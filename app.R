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

  anomaly_cols <- intersect(c("is_anomaly", "is_anomaly_event", "anomaly_flag"), names(data))
  if (length(anomaly_cols) > 0) {
    filtered <- data %>% filter(if_any(all_of(anomaly_cols), ~ coalesce(as.logical(.x), FALSE)))
    return(if (nrow(filtered) == 0) data else filtered)
  }

  if ("anomaly_reason" %in% names(data)) {
    filtered <- data %>% filter(!is.na(.data$anomaly_reason) & nzchar(as.character(.data$anomaly_reason)))
    return(if (nrow(filtered) == 0) data else filtered)
  }

  data
}

filter_network_edges <- function(edges, date_range, channel, crisis_phase, keyword, focal_agent) {
  data <- filter_date(edges, date_range)
  data <- filter_select(data, channel, "channel")
  data <- filter_select(data, crisis_phase, "crisis_phase")
  data <- filter_keyword(data, keyword)

  if (is.data.frame(data) && !is.null(focal_agent) && focal_agent != "All") {
    agent_cols <- intersect(c("from", "to", "from_agent", "to_agent", "agent_clean"), names(data))
    if (length(agent_cols) > 0) {
      filtered <- data %>% filter(if_any(all_of(agent_cols), ~ as.character(.x) == focal_agent))
      if (nrow(filtered) > 0) {
        data <- filtered
      }
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

section_card <- function(title, ...) {
  card(
    class = "section-card",
    card_header(title),
    ...
  )
}

ui <- page_navbar(
  title = "Embargo Breach: Tracing TenantThread's AI Crisis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  nav_panel(
    "Landing Page / Overview",
    div(
      class = "page-shell",
      layout_columns(
        col_widths = c(7, 5),
        section_card(
          "Case Summary",
          p("TenantThread's Project HarborCrest investigation centers on whether embargo-sensitive information escaped before the public deadline and how the information moved through internal, public, and monitored channels."),
          p("This working version organizes the evidence into a timeline, agent network, and breach pathway so analysts can trace the crisis from early governance tension to the suspected release window.")
        ),
        section_card(
          "Identified Breach Window",
          div(class = "breach-window", strong("Suspected release:"), " around 5:00 PM, June 5, 2046"),
          div(class = "breach-window", strong("Embargo deadline:"), " 6:00 PM, June 5, 2046")
        )
      ),
      div(
        class = "kpi-grid",
        kpi_card(metric_value("total_rounds", nrow(rounds_features)), "Total Rounds"),
        kpi_card(metric_value("total_messages", nrow(comms_features)), "Total Messages"),
        kpi_card(metric_value("total_agents", length(unique(na.omit(comms_features$agent_clean)))), "Total Agents"),
        kpi_card(metric_value("total_public_posts", if ("public_channel" %in% names(comms_features)) sum(comms_features$public_channel, na.rm = TRUE) else NA_integer_), "Total Public Posts"),
        kpi_card(metric_value("total_anomalies", if ("anomaly_flag" %in% names(comms_features)) sum(comms_features$anomaly_flag, na.rm = TRUE) else NA_integer_), "Total Anomaly Messages")
      ),
      section_card(
        "Investigation Phases",
        plotlyOutput("phase_flow", height = "260px")
      ),
      div(
        class = "instruction-note",
        "Continue to the Crisis Timeline, Agent Network, and Embargo Breach Pathway tabs to inspect the evidence from temporal, relational, and causal perspectives."
      )
    )
  ),
  nav_panel(
    "Crisis Timeline",
    div(
      class = "page-shell",
      section_card(
        "Inputs / Filters",
        div(
          class = "timeline-filter-grid",
          dateRangeInput("timeline_dates", "Date range", start = date_range[[1]], end = date_range[[2]], min = date_range[[1]], max = date_range[[2]]),
          selectizeInput("timeline_agents", "Agent", choices = agent_choices, selected = "All", multiple = TRUE),
          selectizeInput("timeline_channels", "Channel", choices = channel_choices, selected = "All", multiple = TRUE),
          textInput("timeline_keyword", "Keyword search"),
          selectizeInput("timeline_phases", "Crisis phase", choices = phase_choices, selected = "All", multiple = TRUE),
          checkboxInput("timeline_show_anomaly", "Show anomaly events", value = TRUE)
        )
      ),
      section_card("Interactive Crisis Timeline", plotlyOutput("crisis_timeline", height = "440px")),
      section_card("Round Context Panel", DTOutput("round_context_table")),
      section_card(
        "Embedded Comparison / Summary",
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput("message_volume_phase", height = "330px"),
          plotlyOutput("sensitive_keyword_counts", height = "330px")
        )
      ),
      section_card(
        "Linked Event Detail Table",
        DTOutput("timeline_event_table")
      )
    )
  ),
  nav_panel(
    "Agent Network",
    div(
      class = "page-shell",
      layout_sidebar(
        sidebar = sidebar(
          selectizeInput("network_focal_agent", "Focal agent", choices = agent_choices, selected = "All", multiple = FALSE),
          dateRangeInput("network_dates", "Date range", start = date_range[[1]], end = date_range[[2]], min = date_range[[1]], max = date_range[[2]]),
          selectizeInput("network_channels", "Channel", choices = channel_choices, selected = "All", multiple = TRUE),
          selectizeInput("network_phases", "Crisis phase", choices = phase_choices, selected = "All", multiple = TRUE),
          textInput("network_keyword", "Keyword search"),
          width = 320
        ),
        layout_columns(
          col_widths = c(6, 6),
          section_card("Causal Chain Diagram", visNetworkOutput("causal_chain_network", height = "420px")),
          section_card("Agent Communication Network", visNetworkOutput("agent_network", height = "420px"))
        ),
        layout_columns(
          section_card("Channel Distribution", plotlyOutput("channel_distribution", height = "330px")),
          section_card("Network Comparison", plotlyOutput("network_comparison", height = "330px"))
        ),
        section_card("Linked Messages", DTOutput("network_message_table"))
      )
    )
  ),
  nav_panel(
    "Embargo Breach Pathway",
    div(
      class = "page-shell",
      layout_sidebar(
        sidebar = sidebar(
          selectizeInput("pathway_stage", "Pathway stage", choices = safe_choices(pathway_evidence, "pathway_stage"), selected = "All", multiple = TRUE),
          textInput("pathway_keyword", "Keyword search"),
          selectizeInput("pathway_channel_risk", "Channel risk", choices = safe_choices(pathway_evidence, "channel_risk"), selected = "All", multiple = TRUE),
          selectizeInput("pathway_phases", "Crisis phase", choices = safe_choices(pathway_evidence, "crisis_phase"), selected = "All", multiple = TRUE),
          checkboxInput("pathway_anomaly_only", "Anomaly-only", value = FALSE),
          width = 320
        ),
        section_card("Embargo-Sensitive Information Flow", visNetworkOutput("breach_pathway_network", height = "430px")),
        layout_columns(
          section_card("Judge Coverage Summary", plotlyOutput("judge_coverage", height = "330px")),
          section_card("Pathway Behaviour Comparison", plotlyOutput("pathway_behavior_comparison", height = "330px"))
        ),
        section_card("Linked Evidence", DTOutput("pathway_evidence_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$phase_flow <- renderPlotly({
    plotly_panel(plot_phase_flow())
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
    plotly_panel(plot_crisis_timeline(timeline_events_filtered(), input$timeline_show_anomaly), tooltip = "text")
  })

  output$round_context_table <- renderDT({
    make_round_context_table(timeline_events_filtered())
  })

  output$message_volume_phase <- renderPlotly({
    plotly_panel(plot_message_volume_by_phase(timeline_comms_filtered()))
  })

  output$sensitive_keyword_counts <- renderPlotly({
    plotly_panel(plot_sensitive_keyword_counts(timeline_comms_filtered()))
  })

  output$timeline_event_table <- renderDT({
    make_timeline_event_detail_table(timeline_comms_filtered())
  })

  # Agent Network server logic
  network_comms <- reactive({
    data <- comms_features
    data <- filter_date(data, input$network_dates)
    data <- filter_select(data, input$network_channels, "channel")
    data <- filter_select(data, input$network_phases, "crisis_phase")
    data <- filter_keyword(data, input$network_keyword)

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

  output$causal_chain_network <- renderVisNetwork({
    build_causal_chain_network(causal_chain_nodes, causal_chain_edges)
  })

  output$agent_network <- renderVisNetwork({
    edges <- network_edges_filtered()
    nodes <- filter_network_nodes(network_nodes, edges)
    build_agent_network(nodes, edges)
  })

  output$channel_distribution <- renderPlotly({
    plotly_panel(plot_channel_distribution(network_comms()))
  })

  output$network_comparison <- renderPlotly({
    plotly_panel(plot_network_comparison(network_comms()))
  })

  output$network_message_table <- renderDT({
    make_event_detail_table(network_comms())
  })

  # Embargo Breach Pathway server logic
  pathway_filtered <- reactive({
    data <- pathway_evidence
    data <- filter_select(data, input$pathway_stage, "pathway_stage")
    data <- filter_select(data, input$pathway_channel_risk, "channel_risk")
    data <- filter_select(data, input$pathway_phases, "crisis_phase")
    data <- filter_keyword(data, input$pathway_keyword)

    if (isTRUE(input$pathway_anomaly_only)) {
      data <- filter_anomaly_only(data)
    }

    data
  })

  output$breach_pathway_network <- renderVisNetwork({
    build_breach_pathway_network(breach_pathway_nodes, breach_pathway_edges)
  })

  output$judge_coverage <- renderPlotly({
    plotly_panel(plot_judge_coverage(pathway_filtered()))
  })

  output$pathway_behavior_comparison <- renderPlotly({
    plotly_panel(plot_pathway_behavior_comparison(pathway_filtered()))
  })

  output$pathway_evidence_table <- renderDT({
    make_evidence_table(pathway_filtered())
  })
}

shinyApp(ui, server)
