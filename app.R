library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(visNetwork)
library(DT)
library(scales)

source("R/03_visual_functions.R")

required_rds <- c(
  "comms_features.rds",
  "rounds_features.rds",
  "network_edges.rds",
  "network_nodes.rds",
  "key_events.rds",
  "causal_chain_nodes.rds",
  "causal_chain_edges.rds",
  "breach_pathway_nodes.rds",
  "breach_pathway_edges.rds",
  "pathway_evidence.rds",
  "app_metrics.rds"
)

processed_dir <- file.path("data", "processed")
missing_rds <- required_rds[!file.exists(file.path(processed_dir, required_rds))]

if (length(missing_rds) > 0) {
  stop(
    paste0(
      "Missing processed RDS files: ",
      paste(missing_rds, collapse = ", "),
      "\nRun:\n",
      "source('R/01_data_prep.R')\n",
      "source('R/02_feature_engineering.R')"
    ),
    call. = FALSE
  )
}

comms_features <- readRDS(file.path(processed_dir, "comms_features.rds"))
rounds_features <- readRDS(file.path(processed_dir, "rounds_features.rds"))
network_edges <- readRDS(file.path(processed_dir, "network_edges.rds"))
network_nodes <- readRDS(file.path(processed_dir, "network_nodes.rds"))
key_events <- readRDS(file.path(processed_dir, "key_events.rds"))
causal_chain_nodes <- readRDS(file.path(processed_dir, "causal_chain_nodes.rds"))
causal_chain_edges <- readRDS(file.path(processed_dir, "causal_chain_edges.rds"))
breach_pathway_nodes <- readRDS(file.path(processed_dir, "breach_pathway_nodes.rds"))
breach_pathway_edges <- readRDS(file.path(processed_dir, "breach_pathway_edges.rds"))
pathway_evidence <- readRDS(file.path(processed_dir, "pathway_evidence.rds"))
app_metrics <- readRDS(file.path(processed_dir, "app_metrics.rds"))

safe_choices <- function(data, col, include_all = TRUE) {
  choices <- if (is.data.frame(data) && col %in% names(data)) {
    sort(unique(na.omit(as.character(data[[col]]))))
  } else {
    character()
  }

  if (include_all) {
    c("All", choices)
  } else {
    choices
  }
}

safe_date_range <- function(data, col = "timestamp") {
  fallback <- as.Date(c("2046-06-01", "2046-06-06"))

  if (!is.data.frame(data) || !col %in% names(data)) {
    return(fallback)
  }

  values <- suppressWarnings(as.Date(data[[col]]))
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    fallback
  } else {
    range(values)
  }
}

metric_value <- function(name, fallback = NA_integer_) {
  if (is.list(app_metrics) && !is.null(app_metrics[[name]])) {
    return(app_metrics[[name]])
  }

  if (is.data.frame(app_metrics) && all(c("metric", "value") %in% names(app_metrics))) {
    value <- app_metrics$value[app_metrics$metric == name]
    if (length(value) > 0) {
      return(value[[1]])
    }
  }

  fallback
}

filter_select <- function(data, input_value, col) {
  if (!is.data.frame(data) || !col %in% names(data) || is.null(input_value) || length(input_value) == 0 || "All" %in% input_value) {
    return(data)
  }

  data %>% filter(as.character(.data[[col]]) %in% input_value)
}

filter_date <- function(data, input_value, col = "timestamp") {
  if (!is.data.frame(data) || !col %in% names(data) || is.null(input_value) || length(input_value) < 2 || any(is.na(input_value))) {
    return(data)
  }

  dates <- suppressWarnings(as.Date(data[[col]]))
  data[!is.na(dates) & dates >= input_value[[1]] & dates <= input_value[[2]], , drop = FALSE]
}

filter_keyword <- function(data, keyword, cols = c("content", "message", "text", "event_label", "anomaly_reason")) {
  if (!is.data.frame(data) || is.null(keyword) || !nzchar(str_squish(keyword))) {
    return(data)
  }

  search_cols <- intersect(cols, names(data))
  if (length(search_cols) == 0) {
    return(data)
  }

  pattern <- fixed(str_squish(keyword), ignore_case = TRUE)
  filtered <- data %>%
    filter(if_any(all_of(search_cols), ~ str_detect(coalesce(as.character(.x), ""), pattern)))

  filtered
}

filter_out_anomaly_events <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(data)
  }

  anomaly_cols <- intersect(c("is_anomaly_event", "is_anomaly", "anomaly_flag"), names(data))
  if (length(anomaly_cols) > 0) {
    return(data %>% filter(!if_any(all_of(anomaly_cols), ~ coalesce(as.logical(.x), FALSE))))
  }

  if ("anomaly_reason" %in% names(data)) {
    return(data %>% filter(is.na(.data$anomaly_reason) | !nzchar(as.character(.data$anomaly_reason))))
  }

  data
}

filter_anomaly_only <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  anomaly_match <- rep(FALSE, nrow(data))
  anomaly_cols <- intersect(c("is_anomaly", "is_anomaly_event", "anomaly_flag"), names(data))
  if (length(anomaly_cols) > 0) {
    anomaly_match <- anomaly_match | (
      data %>%
        transmute(.match = if_any(all_of(anomaly_cols), ~ coalesce(as.logical(.x), FALSE))) %>%
        pull(.match)
    )
  }

  if ("anomaly_reason" %in% names(data)) {
    anomaly_match <- anomaly_match | (!is.na(data$anomaly_reason) & nzchar(as.character(data$anomaly_reason)))
  }

  data[anomaly_match, , drop = FALSE]
}

filter_network_edges <- function(edges, date_range, channel, crisis_phase, keyword, focal_agent) {
  data <- filter_date(edges, date_range)
  data <- filter_select(data, channel, "channel")
  data <- filter_select(data, crisis_phase, "crisis_phase")
  data <- filter_keyword(
    data,
    keyword,
    cols = c("content", "message", "text", "from_agent", "to_agent", "channel", "channel_group", "crisis_phase")
  )

  if (is.data.frame(data) && !is.null(focal_agent) && focal_agent != "All") {
    agent_cols <- intersect(c("from", "to", "from_agent", "to_agent", "agent_clean"), names(data))
    if (length(agent_cols) > 0) {
      data <- data %>% filter(if_any(all_of(agent_cols), ~ as.character(.x) == focal_agent))
    }
  }

  data
}

filter_network_nodes <- function(nodes, edges) {
  if (!is.data.frame(nodes) || !is.data.frame(edges) || nrow(edges) == 0) {
    return(nodes)
  }

  edge_agents <- unique(na.omit(c(
    if ("from" %in% names(edges)) as.character(edges$from),
    if ("to" %in% names(edges)) as.character(edges$to),
    if ("from_agent" %in% names(edges)) as.character(edges$from_agent),
    if ("to_agent" %in% names(edges)) as.character(edges$to_agent)
  )))

  node_id_cols <- intersect(c("id", "agent_clean", "label"), names(nodes))
  if (length(node_id_cols) == 0 || length(edge_agents) == 0) {
    return(nodes)
  }

  node_id_col <- node_id_cols[[1]]
  filtered <- nodes %>% filter(as.character(.data[[node_id_col]]) %in% edge_agents)
  if (nrow(filtered) == 0) nodes else filtered
}

summarise_network_nodes <- function(nodes, comms, edges) {
  nodes_filtered <- filter_network_nodes(nodes, edges)

  if (!is.data.frame(nodes_filtered) || nrow(nodes_filtered) == 0) {
    return(nodes_filtered)
  }

  node_id_cols <- intersect(c("id", "agent_clean", "label"), names(nodes_filtered))

  if (length(node_id_cols) == 0 || !is.data.frame(comms) || !"agent_clean" %in% names(comms) || nrow(comms) == 0) {
    return(nodes_filtered)
  }

  node_id_col <- node_id_cols[[1]]
  public_col <- intersect(c("public_channel", "is_public", "public_post"), names(comms))
  anomaly_cols <- intersect(c("anomaly_flag", "is_anomaly", "is_anomaly_event"), names(comms))
  sensitive_cols <- intersect(c("embargo_sensitive", "crisis_sensitive", "sensitive_message", "is_sensitive"), names(comms))

  summary <- comms %>%
    mutate(agent_clean = as.character(.data$agent_clean))

  summary$.public_post <- if (length(public_col) > 0) {
    coalesce(as.logical(summary[[public_col[[1]]]]), FALSE)
  } else {
    FALSE
  }

  summary$.anomaly_message <- if (length(anomaly_cols) > 0) {
    summary %>% transmute(.value = if_any(all_of(anomaly_cols), ~ coalesce(as.logical(.x), FALSE))) %>% pull(.value)
  } else if ("anomaly_reason" %in% names(summary)) {
    !is.na(summary$anomaly_reason) & nzchar(as.character(summary$anomaly_reason))
  } else {
    FALSE
  }

  summary$.sensitive_message <- if (length(sensitive_cols) > 0) {
    summary %>% transmute(.value = if_any(all_of(sensitive_cols), ~ coalesce(as.logical(.x), FALSE))) %>% pull(.value)
  } else {
    FALSE
  }

  summary <- summary %>%
    group_by(agent_clean) %>%
    summarise(
      total_messages = n(),
      public_posts = sum(.data$.public_post, na.rm = TRUE),
      anomaly_count = sum(.data$.anomaly_message, na.rm = TRUE),
      sensitive_count = sum(.data$.sensitive_message, na.rm = TRUE),
      .groups = "drop"
    )

  nodes_filtered %>%
    select(-any_of(c("total_messages", "public_posts", "anomaly_count", "sensitive_count", "sensitive_message_count"))) %>%
    mutate(.node_key = as.character(.data[[node_id_col]])) %>%
    left_join(summary, by = c(".node_key" = "agent_clean")) %>%
    mutate(
      total_messages = coalesce(.data$total_messages, 0),
      public_posts = coalesce(.data$public_posts, 0),
      anomaly_count = coalesce(.data$anomaly_count, 0),
      sensitive_count = coalesce(.data$sensitive_count, 0)
    ) %>%
    select(-.node_key)
}

aggregate_network_edges <- function(edges, max_detailed_edges = 30) {
  if (!is.data.frame(edges) || nrow(edges) == 0) {
    return(tibble())
  }

  edge_data <- edges %>%
    mutate(
      from_agent = if ("from_agent" %in% names(.)) as.character(.data$from_agent) else if ("from" %in% names(.)) as.character(.data$from) else NA_character_,
      to_agent = if ("to_agent" %in% names(.)) as.character(.data$to_agent) else if ("to" %in% names(.)) as.character(.data$to) else NA_character_,
      crisis_phase = if ("crisis_phase" %in% names(.)) coalesce(as.character(.data$crisis_phase), "Unclassified") else "Unclassified",
      channel_display = if ("channel_group" %in% names(.)) coalesce(as.character(.data$channel_group), "Unspecified") else if ("channel" %in% names(.)) coalesce(as.character(.data$channel), "Unspecified") else "Unspecified",
      interaction_count = if ("weight" %in% names(.)) coalesce(as.numeric(.data$weight), 1) else 1
    ) %>%
    filter(
      !is.na(.data$from_agent),
      !is.na(.data$to_agent),
      nzchar(.data$from_agent),
      nzchar(.data$to_agent),
      .data$from_agent != .data$to_agent
    )

  if (nrow(edge_data) == 0) {
    return(tibble())
  }

  detailed_edges <- edge_data %>%
    group_by(from_agent, to_agent, crisis_phase, channel_display) %>%
    summarise(weight = sum(.data$interaction_count, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      from = .data$from_agent,
      to = .data$to_agent,
      channel = .data$channel_display,
      channel_summary = .data$channel_display,
      phase_summary = .data$crisis_phase
    )

  if (nrow(detailed_edges) <= max_detailed_edges) {
    return(detailed_edges)
  }

  edge_data %>%
    group_by(from_agent, to_agent) %>%
    summarise(
      weight = sum(.data$interaction_count, na.rm = TRUE),
      channel_summary = paste(sort(unique(.data$channel_display)), collapse = ", "),
      phase_summary = paste(sort(unique(.data$crisis_phase)), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(
      from = .data$from_agent,
      to = .data$to_agent,
      channel = .data$channel_summary,
      crisis_phase = .data$phase_summary
    )
}

plotly_panel <- function(plot_obj, tooltip = NULL) {
  if (is.null(tooltip)) {
    ggplotly(plot_obj)
  } else {
    ggplotly(plot_obj, tooltip = tooltip)
  }
}

date_range <- safe_date_range(comms_features)
phase_choices <- safe_choices(comms_features, "crisis_phase")
channel_choices <- safe_choices(comms_features, "channel")
agent_choices <- safe_choices(comms_features, "agent_clean")

kpi_card <- function(value, label) {
  div(
    class = "kpi-card",
    div(class = "kpi-value", comma(value)),
    div(class = "kpi-label", label)
  )
}

section_card <- function(title, ..., class = NULL) {
  card(
    class = paste(c("section-card", "dashboard-card", class), collapse = " "),
    card_header(title),
    ...
  )
}

case_bullet <- function(text) {
  tags$li(text)
}

nav_guide_item <- function(title, text) {
  div(
    class = "nav-guide-item",
    div(class = "nav-guide-title", title),
    div(class = "nav-guide-text", text)
  )
}

ui <- page_navbar(
  title = div(class = "navbar-app-title", "TenantThread AI Crisis"),
  fillable = TRUE,
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  window_title = "Embargo Breach: Tracing TenantThread's AI Crisis",
  navbar_options = navbar_options(collapsible = FALSE, underline = FALSE),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  nav_panel(
    "Landing Page / Overview",
    div(
      class = "app-shell page-shell full-width-tab landing-dashboard",
      div(
        class = "overview-hero compact-overview-hero",
        div(class = "overview-eyebrow", "TenantThread Case File"),
        h1("Embargo Breach: Tracing TenantThread's AI Crisis"),
        p("A visual analytics investigation into how embargo-sensitive information moved through automated agents, governance controls, and public-facing channels before the official release deadline.")
      ),
      div(
        class = "landing-dashboard-grid",
        div(
          class = "landing-left-column",
          section_card(
            "Case Summary",
            div(
              class = "case-summary-list",
              tags$ul(
                case_bullet("TenantThread used automated communication agents and The Judge to manage communications."),
                case_bullet("Project HarborCrest was a confidential CivicLoom merger."),
                case_bullet("The official embargo deadline was 6:00 PM, June 5, 2046."),
                case_bullet("Suspicious public release activity began around 5:00 PM."),
                case_bullet("The app investigates whether this was deliberate leakage or a systemic breakdown.")
              )
            ),
            class = "compact-overview-card"
          ),
          section_card(
            "Identified Breach Window",
            div(class = "breach-window breach-window-risk", strong("Suspected inappropriate release begins:"), " around 5:00 PM, June 5, 2046"),
            div(class = "breach-window", strong("Embargo deadline:"), " 6:00 PM, June 5, 2046"),
            p(
              class = "breach-window-note",
              "This one-hour gap is the key investigation window: it separates suspicious public activity from the authorized release time and frames the search for deliberate leakage or a systemic control breakdown."
            ),
            class = "compact-overview-card breach-overview-card"
          ),
          section_card(
            "Navigation Guide",
            div(
              class = "nav-guide compact-nav-guide",
              nav_guide_item("Crisis Timeline", "Reconstructs the sequence of events."),
              nav_guide_item("Agent Network", "Shows who communicated with whom and how communication patterns shifted."),
              nav_guide_item("Embargo Breach Pathway", "Traces movement of embargo-sensitive information toward public release.")
            ),
            class = "compact-overview-card"
          )
        ),
        div(
          class = "landing-right-column",
          section_card(
            "Case Metrics",
            div(
              class = "kpi-grid compact-kpi-grid",
              kpi_card(metric_value("total_rounds", nrow(rounds_features)), "Rounds"),
              kpi_card(metric_value("total_messages", nrow(comms_features)), "Messages"),
              kpi_card(metric_value("total_agents", length(unique(na.omit(comms_features$agent_clean)))), "Agents"),
              kpi_card(metric_value("total_public_posts", if ("public_channel" %in% names(comms_features)) sum(comms_features$public_channel, na.rm = TRUE) else NA_integer_), "Public posts"),
              kpi_card(metric_value("total_anomalies", if ("anomaly_flag" %in% names(comms_features)) sum(comms_features$anomaly_flag, na.rm = TRUE) else NA_integer_), "Anomaly messages")
            ),
            class = "compact-overview-card"
          ),
          section_card(
            "Investigation Phases",
            div(class = "phase-flow-wrap compact-phase-flow-wrap", plotOutput("phase_flow", height = "190px")),
            class = "compact-overview-card phase-overview-card"
          ),
          div(
            class = "instruction-note compact-instruction-note",
            "Continue to the Crisis Timeline, Agent Network, and Embargo Breach Pathway tabs to inspect the evidence from temporal, relational, and causal perspectives."
          )
        )
      )
    )
  ),
  nav_panel(
    "Crisis Timeline",
    div(
      class = "app-shell page-shell full-width-tab dashboard-tab crisis-timeline-tab",
      div(
        class = "dashboard-layout crisis-timeline-layout",
        div(
          class = "filter-sidebar",
          section_card(
            "Inputs / Filters",
            div(
              class = "timeline-filter-grid",
              dateRangeInput(
                "timeline_dates",
                "Date range",
                start = date_range[[1]],
                end = date_range[[2]],
                min = date_range[[1]],
                max = date_range[[2]],
                width = "100%"
              ),
              selectizeInput("timeline_agents", "Agent", choices = agent_choices, selected = "All", multiple = TRUE),
              selectizeInput("timeline_channels", "Channel", choices = channel_choices, selected = "All", multiple = TRUE),
              textInput("timeline_keyword", "Keyword search"),
              selectizeInput("timeline_phases", "Crisis phase", choices = phase_choices, selected = "All", multiple = TRUE),
              checkboxInput("timeline_show_anomaly", "Show anomaly events", value = TRUE),
              checkboxInput("timeline_show_round_context", "Show Round Context Panel", value = FALSE),
              checkboxInput("timeline_show_event_detail", "Show Linked Event Detail Table", value = FALSE)
            ),
            class = "compact-card filter-sidebar-card filter-card stretch-card scroll-card"
          )
        ),
        div(
          class = "main-dashboard-area timeline-dashboard-area",
          div(
            class = "dashboard-top-row",
            section_card(
              "Interactive Crisis Timeline",
              div(class = "timeline-slider-note", "Use the range slider below the timeline to zoom into specific dates."),
              plotlyOutput("crisis_timeline", height = "100%"),
              class = "chart-card timeline-chart-card stretch-card"
            )
          ),
          div(
            class = "dashboard-card-grid dashboard-bottom-row timeline-support-grid",
            conditionalPanel(
              condition = "input.timeline_show_round_context === true",
              section_card("Round Context Panel", DTOutput("round_context_table"), class = "table-card compact-card stretch-card scroll-card")
            ),
            section_card(
              "Embedded Comparison / Summary",
              layout_columns(
                col_widths = c(6, 6),
                plotlyOutput("message_volume_phase", height = "100%"),
                plotlyOutput("sensitive_keyword_counts", height = "100%")
              ),
              class = "chart-card compact-card stretch-card"
            ),
            conditionalPanel(
              condition = "input.timeline_show_event_detail === true",
              section_card(
                "Linked Event Detail Table",
                DTOutput("timeline_event_table"),
                class = "table-card compact-card stretch-card scroll-card"
              )
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "Agent Network",
    div(
      class = "app-shell page-shell full-width-tab dashboard-tab",
      div(
        class = "dashboard-layout",
        div(
          class = "filter-sidebar",
          section_card(
            "Inputs / Filters",
            div(
              class = "agent-filter-grid",
              selectizeInput("network_focal_agent", "Focal agent", choices = agent_choices, selected = "All", multiple = FALSE),
              dateRangeInput(
                "network_dates",
                "Date range",
                start = date_range[[1]],
                end = date_range[[2]],
                min = date_range[[1]],
                max = date_range[[2]],
                width = "100%"
              ),
              selectizeInput("network_channels", "Channel", choices = channel_choices, selected = "All", multiple = TRUE),
              selectizeInput("network_phases", "Crisis phase", choices = phase_choices, selected = "All", multiple = TRUE),
              textInput("network_keyword", "Keyword search"),
              checkboxInput("network_show_message_table", "Show Linked Message Table", value = FALSE)
            ),
            class = "compact-card filter-sidebar-card"
          )
        ),
        div(
          class = "main-dashboard-area network-dashboard-area",
          div(
            class = "dashboard-card-grid network-main-grid agent-network-main-grid",
            section_card("Causal Chain Diagram", visNetworkOutput("causal_chain_network", height = "100%"), class = "chart-card compact-card network-causal-card"),
            section_card("Interactive Agent Communication Network", visNetworkOutput("agent_network", height = "100%"), class = "chart-card network-star-card agent-network-visual-card"),
            div(
              class = "network-support-column agent-network-support-column",
              section_card("Channel Distribution Summary", plotlyOutput("channel_distribution", height = "100%"), class = "chart-card compact-card network-support-card support-chart-card"),
              section_card(
                "Embedded Comparison Panel",
                plotlyOutput("network_comparison", height = "100%"),
                class = "chart-card compact-card network-support-card support-chart-card"
              ),
              conditionalPanel(
                condition = "input.network_show_message_table",
                section_card("Linked Message Table", DTOutput("network_message_table"), class = "table-card compact-card network-message-card scroll-card")
              )
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "Embargo Breach Pathway",
    div(
      class = "app-shell page-shell full-width-tab dashboard-tab",
      div(
        class = "dashboard-layout",
        div(
          class = "filter-sidebar",
          section_card(
            "Inputs / Filters",
            div(
              class = "pathway-filter-grid",
              selectizeInput("pathway_stage", "Pathway stage", choices = safe_choices(pathway_evidence, "pathway_stage"), selected = "All", multiple = TRUE),
              textInput("pathway_keyword", "Keyword search"),
              selectizeInput("pathway_channel_risk", "Channel risk", choices = safe_choices(pathway_evidence, "channel_risk"), selected = "All", multiple = TRUE),
              selectizeInput("pathway_phases", "Crisis phase", choices = safe_choices(pathway_evidence, "crisis_phase"), selected = "All", multiple = TRUE),
              checkboxInput("pathway_anomaly_only", "Anomaly-only", value = FALSE)
            ),
            class = "compact-card filter-sidebar-card"
          )
        ),
        div(
          class = "main-dashboard-area pathway-dashboard-area",
          div(
            class = "dashboard-card-grid pathway-main-grid",
            section_card(
              "Embargo Breach Pathway / Response Chain",
              visNetworkOutput("breach_pathway_network", height = "100%"),
              class = "chart-card"
            ),
            section_card(
              "Embedded Risk Summary",
              uiOutput("pathway_risk_counts"),
              plotlyOutput("judge_coverage", height = "100%"),
              class = "chart-card compact-card"
            ),
            section_card("Behaviour Comparison Panel", plotlyOutput("pathway_behavior_comparison", height = "100%"), class = "chart-card compact-card"),
            section_card("Linked Evidence Table / Viewer", DTOutput("pathway_evidence_table"), class = "table-card compact-card")
          )
        )
      )
    )
  ),
  nav_spacer(),
  nav_panel(
    "User Guide",
    div(
      class = "app-shell page-shell full-width-tab user-guide-shell",
      div(
        class = "overview-hero user-guide-hero",
        div(class = "overview-eyebrow", "How to Use This App"),
        h1("User Guide"),
        p("This application investigates whether TenantThread's embargo breach was caused by deliberate leakage or systemic breakdown.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        section_card(
          "Landing Page / Overview",
          p("Review the case summary, KPI cards, breach window, and investigation phases before moving into the analytical tabs.")
        ),
        section_card(
          "Crisis Timeline",
          p("Use date, agent, channel, phase, keyword, and anomaly filters to narrow the events. Hover over timeline points for details, and use the range slider below the timeline to zoom into specific dates.")
        ),
        section_card(
          "Agent Network",
          p("Use focal agent, channel, phase, and keyword filters. Read the causal chain first, then use the agent network to inspect communication relationships. Use the linked table to inspect message-level evidence.")
        ),
        section_card(
          "Embargo Breach Pathway",
          p("Use pathway stage, channel risk, phase, keyword, and anomaly filters. Follow the information flow from sensitive internal discussion toward public release. Use Judge coverage and evidence tables to identify where controls weakened.")
        )
      ),
      section_card(
        "Interpretation Note",
        p("The causal chain and pathway diagrams are investigative visual summaries based on message patterns and flagged evidence. They should be interpreted together with the linked evidence tables.")
      ),
      section_card(
        "Suggested Investigation Workflow",
        tags$ol(
          tags$li("Start from Landing Page."),
          tags$li("Use Crisis Timeline to understand when events happened."),
          tags$li("Use Agent Network to understand who was involved."),
          tags$li("Use Embargo Breach Pathway to trace how sensitive information moved toward public release."),
          tags$li("Use evidence tables to support conclusions.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$phase_flow <- renderPlot({
    plot_phase_flow()
  })

  # Crisis Timeline server logic
  timeline_comms_filtered <- reactive({
    data <- comms_features
    data <- filter_date(data, input$timeline_dates)
    data <- filter_select(data, input$timeline_agents, "agent_clean")
    data <- filter_select(data, input$timeline_channels, "channel")
    data <- filter_select(data, input$timeline_phases, "crisis_phase")
    filter_keyword(
      data,
      input$timeline_keyword,
      cols = c("content", "message", "text", "agent_clean", "channel", "channel_group", "crisis_phase", "anomaly_reason")
    )
  })

  timeline_events_filtered <- reactive({
    data <- key_events
    data <- filter_date(data, input$timeline_dates, "event_time")
    data <- filter_select(data, input$timeline_phases, "crisis_phase")
    data <- filter_keyword(
      data,
      input$timeline_keyword,
      cols = c("event_label", "event_type", "crisis_phase", "event_headline", "event_narrative", "anomaly_reason")
    )

    if (!isTRUE(input$timeline_show_anomaly)) {
      data <- filter_out_anomaly_events(data)
    }

    data
  })

  output$crisis_timeline <- renderPlotly({
    build_crisis_timeline_plotly(timeline_events_filtered(), input$timeline_show_anomaly)
  })

  output$round_context_table <- renderDT({
    make_round_context_table(timeline_events_filtered())
  })

  output$message_volume_phase <- renderPlotly({
    plotly_panel(plot_message_volume_by_phase(timeline_comms_filtered()), tooltip = "text")
  })

  output$sensitive_keyword_counts <- renderPlotly({
    plotly_panel(plot_sensitive_keyword_counts(timeline_comms_filtered()), tooltip = "text")
  })

  output$timeline_event_table <- renderDT({
    make_timeline_event_detail_table(timeline_comms_filtered())
  })

  # Agent Network server logic
  network_comms_filtered <- reactive({
    data <- comms_features
    data <- filter_date(data, input$network_dates)
    data <- filter_select(data, input$network_channels, "channel")
    data <- filter_select(data, input$network_phases, "crisis_phase")
    data <- filter_keyword(
      data,
      input$network_keyword,
      cols = c("content", "message", "text", "agent_clean", "channel", "channel_group", "crisis_phase", "anomaly_reason")
    )

    if (!is.null(input$network_focal_agent) && input$network_focal_agent != "All") {
      data <- filter_select(data, input$network_focal_agent, "agent_clean")
    }

    data
  })

  network_edges_filtered <- reactive({
    filter_network_edges(
      network_edges,
      input$network_dates,
      input$network_channels,
      input$network_phases,
      input$network_keyword,
      input$network_focal_agent
    )
  })

  network_edges_aggregated <- reactive({
    aggregate_network_edges(network_edges_filtered())
  })

  output$causal_chain_network <- renderVisNetwork({
    build_causal_chain_network(causal_chain_nodes, causal_chain_edges)
  })

  output$agent_network <- renderVisNetwork({
    edges <- network_edges_aggregated()
    nodes <- summarise_network_nodes(network_nodes, network_comms_filtered(), edges)
    build_agent_network(nodes, edges)
  })

  output$channel_distribution <- renderPlotly({
    plotly_panel(plot_channel_distribution(network_comms_filtered()), tooltip = "text")
  })

  output$network_comparison <- renderPlotly({
    plotly_panel(plot_network_comparison(network_comms_filtered()), tooltip = "text")
  })

  output$network_message_table <- renderDT({
    make_event_detail_table(network_comms_filtered())
  })

  # Embargo Breach Pathway server logic
  # Filters evidence records that trace movement from internal sensitivity through weak controls to public breach.
  pathway_evidence_filtered <- reactive({
    data <- pathway_evidence

    data <- filter_select(data, input$pathway_stage, "pathway_stage")
    data <- filter_keyword(
      data,
      input$pathway_keyword,
      cols = c("content", "anomaly_reason", "pathway_stage", "agent_clean")
    )
    data <- filter_select(data, input$pathway_channel_risk, "channel_risk")
    data <- filter_select(data, input$pathway_phases, "crisis_phase")

    if (isTRUE(input$pathway_anomaly_only)) {
      data <- filter_anomaly_only(data)
    }

    data
  })

  output$breach_pathway_network <- renderVisNetwork({
    build_breach_pathway_network(breach_pathway_nodes, breach_pathway_edges)
  })

  output$pathway_risk_counts <- renderUI({
    data <- pathway_evidence_filtered()

    if (!is.data.frame(data) || nrow(data) == 0) {
      return(div(class = "risk-summary-empty", "No evidence records match the selected filters."))
    }

    status_counts <- if ("judge_monitored_status" %in% names(data)) {
      data %>%
        count(judge_monitored_status, name = "records") %>%
        mutate(judge_monitored_status = coalesce(as.character(.data$judge_monitored_status), "Unclassified"))
    } else {
      tibble(judge_monitored_status = "Unavailable", records = nrow(data))
    }

    anomaly_count <- 0L
    if ("anomaly_flag" %in% names(data)) {
      anomaly_count <- sum(coalesce(as.logical(data$anomaly_flag), FALSE), na.rm = TRUE)
    }
    if ("anomaly_reason" %in% names(data)) {
      anomaly_count <- max(
        anomaly_count,
        sum(!is.na(data$anomaly_reason) & nzchar(as.character(data$anomaly_reason)), na.rm = TRUE)
      )
    }

    div(
      class = "risk-summary",
      div(
        class = "risk-summary-metric",
        div(class = "risk-summary-value", comma(nrow(data))),
        div(class = "risk-summary-label", "Filtered evidence records")
      ),
      div(
        class = "risk-summary-metric",
        div(class = "risk-summary-value", comma(anomaly_count)),
        div(class = "risk-summary-label", "Anomaly records")
      ),
      tags$div(
        class = "risk-summary-status",
        tags$div(class = "risk-summary-status-title", "Judge monitoring status"),
        tags$ul(
          lapply(seq_len(nrow(status_counts)), function(i) {
            tags$li(
              tags$span(status_counts$judge_monitored_status[[i]]),
              tags$strong(comma(status_counts$records[[i]]))
            )
          })
        )
      )
    )
  })

  output$judge_coverage <- renderPlotly({
    plotly_panel(plot_judge_coverage(pathway_evidence_filtered()), tooltip = "text")
  })

  output$pathway_behavior_comparison <- renderPlotly({
    plotly_panel(plot_pathway_behavior_comparison(pathway_evidence_filtered()), tooltip = "text")
  })

  output$pathway_evidence_table <- renderDT({
    make_evidence_table(pathway_evidence_filtered())
  })
}

shinyApp(ui, server)
