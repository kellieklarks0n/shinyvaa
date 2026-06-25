# Reusable visualisation functions for the TenantThread Shiny app.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(visNetwork)
  library(DT)
  library(scales)
})

empty_plot <- function(message = "No data available for selected filters") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 5, colour = "grey35") +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void()
}

has_rows <- function(data) {
  is.data.frame(data) && nrow(data) > 0
}

available_cols <- function(data, cols) {
  intersect(cols, names(data))
}

ensure_table_cols <- function(data, cols) {
  if (!is.data.frame(data)) {
    data <- tibble()
  }

  missing_cols <- setdiff(cols, names(data))
  for (col in missing_cols) {
    data[[col]] <- if (nrow(data) == 0) character(0) else NA_character_
  }

  data %>% select(all_of(cols))
}

create_value_box_content <- function(value, subtitle) {
  if (requireNamespace("shiny", quietly = TRUE)) {
    return(
      shiny::tags$div(
        class = "kpi-content",
        shiny::tags$div(class = "kpi-value", value),
        shiny::tags$div(class = "kpi-subtitle", subtitle)
      )
    )
  }

  list(value = value, subtitle = subtitle)
}

plot_phase_flow <- function() {
  phases <- tibble(
    phase = c(
      "Pre-crisis",
      "Governance tension",
      "Media escalation",
      "Embargo-sensitive",
      "Breach / response"
    ),
    x = seq_along(phase),
    y = 1
  )

  ggplot(phases, aes(x = x, y = y)) +
    geom_tile(width = 0.86, height = 0.42, fill = "#edf2f7", colour = "#667085", linewidth = 0.4) +
    geom_text(aes(label = phase), size = 3.6, lineheight = 0.95) +
    geom_segment(
      data = phases %>% filter(x < max(x)),
      aes(x = x + 0.43, xend = x + 0.57, y = y, yend = y),
      arrow = arrow(length = unit(0.16, "cm")),
      inherit.aes = FALSE,
      colour = "#475467",
      linewidth = 0.45
    ) +
    scale_x_continuous(limits = c(0.45, 5.55), expand = expansion(mult = 0.02)) +
    scale_y_continuous(limits = c(0.65, 1.35), expand = expansion(mult = 0)) +
    theme_void()
}

plot_crisis_timeline <- function(key_events, show_anomalies = FALSE) {
  if (!has_rows(key_events) || !"event_time" %in% names(key_events)) {
    return(empty_plot())
  }

  timeline <- key_events %>%
    mutate(
      event_time = as.POSIXct(event_time, tz = "UTC"),
      event_label = if ("event_label" %in% names(.)) coalesce(as.character(.data$event_label), "Event") else "Event",
      event_type = if ("event_type" %in% names(.)) coalesce(as.character(.data$event_type), "Event") else "Event",
      crisis_phase = if ("crisis_phase" %in% names(.)) coalesce(as.character(.data$crisis_phase), "Unclassified") else "Unclassified",
      event_headline = if ("event_headline" %in% names(.)) coalesce(as.character(.data$event_headline), "") else "",
      event_narrative = if ("event_narrative" %in% names(.)) coalesce(as.character(.data$event_narrative), "") else "",
      is_anomaly_event = if ("is_anomaly_event" %in% names(.)) coalesce(.data$is_anomaly_event, FALSE) else FALSE,
      narrative_preview = str_trunc(str_squish(str_c(event_headline, event_narrative, sep = " ")), 180),
      y_value = factor(event_type, levels = rev(unique(event_type))),
      tooltip = str_c(
        "<b>", event_label, "</b>",
        "<br>Time: ", format(event_time, "%Y-%m-%d %H:%M"),
        "<br>Type: ", event_type,
        "<br>Phase: ", crisis_phase,
        if_else(nzchar(narrative_preview), str_c("<br>Context: ", narrative_preview), "")
      )
    ) %>%
    filter(!is.na(event_time))

  if (nrow(timeline) == 0) {
    return(empty_plot())
  }

  labelled_events <- c(
    "AG inquiries",
    "NHPI report",
    "Elena incident",
    "Judge enters",
    "SaltWind piece",
    "ResidentIQ rumor",
    "Embargo breach"
  )

  timeline <- timeline %>%
    mutate(
      label_to_show = if_else(event_label %in% labelled_events, event_label, NA_character_),
      anomaly_display = if (show_anomalies) is_anomaly_event else FALSE
    )

  suspected_release_time <- ymd_hms("2046-06-05 17:00:00", tz = "UTC")
  suspected_release_start <- suspected_release_time - minutes(15)
  suspected_release_end <- suspected_release_time + minutes(15)
  embargo_deadline <- ymd_hms("2046-06-05 18:00:00", tz = "UTC")

  ggplot(timeline, aes(x = event_time, y = y_value, text = tooltip)) +
    annotate(
      "rect",
      xmin = suspected_release_start,
      xmax = suspected_release_end,
      ymin = -Inf,
      ymax = Inf,
      fill = "#f59e0b",
      alpha = 0.12
    ) +
    geom_vline(
      xintercept = suspected_release_time,
      linetype = "dashed",
      colour = "#d97706",
      linewidth = 0.45
    ) +
    geom_vline(
      xintercept = embargo_deadline,
      linetype = "dotted",
      colour = "#b91c1c",
      linewidth = 0.55
    ) +
    geom_point(aes(colour = anomaly_display, size = anomaly_display), alpha = 0.9) +
    geom_point(
      data = timeline %>% filter(anomaly_display),
      shape = 21,
      fill = "#fff7ed",
      colour = "#b91c1c",
      size = 5,
      stroke = 1.2,
      alpha = 0.95,
      inherit.aes = TRUE
    ) +
    geom_text(
      aes(label = label_to_show),
      hjust = -0.05,
      vjust = -0.45,
      size = 3,
      na.rm = TRUE,
      check_overlap = TRUE
    ) +
    scale_colour_manual(
      values = c(`FALSE` = "#2f6f9f", `TRUE` = "#c2410c"),
      labels = c(`FALSE` = "Event", `TRUE` = "Anomaly"),
      name = NULL
    ) +
    scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 4.5), guide = "none") +
    scale_x_datetime(labels = label_date_short()) +
    labs(
      x = NULL,
      y = NULL,
      caption = "Dashed marker: suspected breach window around 5:00 PM, June 5, 2046. Dotted marker: embargo deadline at 6:00 PM, June 5, 2046."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.caption = element_text(colour = "#52616f", hjust = 0)
    )
}

plot_message_volume_by_phase <- function(comms) {
  if (!has_rows(comms) || !all(c("crisis_phase", "channel_group") %in% names(comms))) {
    return(empty_plot())
  }

  comms %>%
    count(crisis_phase, channel_group, name = "messages") %>%
    ggplot(aes(x = crisis_phase, y = messages, fill = channel_group)) +
    geom_col(position = "dodge", width = 0.72) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Messages", fill = "Channel group") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid.minor = element_blank())
}

plot_sensitive_keyword_counts <- function(comms) {
  if (!has_rows(comms) || !"crisis_phase" %in% names(comms)) {
    return(empty_plot())
  }

  keyword_counts <- comms %>%
    mutate(
      embargo_sensitive = if ("embargo_sensitive" %in% names(.)) coalesce(.data$embargo_sensitive, FALSE) else FALSE,
      crisis_sensitive = if ("crisis_sensitive" %in% names(.)) coalesce(.data$crisis_sensitive, FALSE) else FALSE
    ) %>%
    group_by(crisis_phase) %>%
    summarise(
      `Embargo-sensitive` = sum(embargo_sensitive, na.rm = TRUE),
      `Crisis-sensitive` = sum(crisis_sensitive, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-crisis_phase, names_to = "keyword_group", values_to = "messages")

  ggplot(keyword_counts, aes(x = crisis_phase, y = messages, fill = keyword_group)) +
    geom_col(position = "dodge", width = 0.72) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Messages", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid.minor = element_blank())
}

build_causal_chain_network <- function(nodes, edges) {
  if (!has_rows(nodes) || !has_rows(edges)) {
    return(visNetwork(data.frame(id = "empty", label = "No data available"), data.frame()))
  }

  stage_order <- c(
    "Governance concerns",
    "Media escalation",
    "Embargo sensitivity",
    "Judge intervention",
    "Side/private coordination",
    "Public-facing release",
    "Breach response"
  )

  vis_nodes <- nodes %>%
    mutate(
      id = if ("id" %in% names(.)) as.character(.data$id) else as.character(.data$node_id),
      label = if ("label" %in% names(.)) as.character(.data$label) else as.character(.data$stage),
      level = if ("stage_order" %in% names(.)) as.numeric(.data$stage_order) else as.numeric(row_number()),
      level = if_else(label %in% stage_order, as.numeric(match(label, stage_order)), level),
      title = str_c(
        "<b>", label, "</b>",
        "<br>Conceptual evidence stage for explaining crisis escalation.",
        if ("description" %in% names(.)) str_c("<br>", .data$description) else ""
      ),
      shape = "box"
    ) %>%
    arrange(level) %>%
    select(id, label, level, title, shape)

  vis_edges <- edges %>%
    transmute(
      from = as.character(.data$from),
      to = as.character(.data$to),
      arrows = "to",
      width = if ("weight" %in% names(.)) pmax(1, as.numeric(.data$weight)) else 1
    )

  visNetwork(vis_nodes, vis_edges) %>%
    visHierarchicalLayout(direction = "LR", sortMethod = "directed") %>%
    visNodes(color = list(background = "#eef4f8", border = "#2f6f9f"), font = list(size = 18)) %>%
    visEdges(smooth = TRUE, color = list(color = "#52616f")) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE)
}

build_agent_network <- function(nodes, edges) {
  if (!has_rows(nodes) || !has_rows(edges)) {
    return(
      visNetwork(
        data.frame(id = "empty", label = "No communication links for selected filters", shape = "box"),
        data.frame()
      ) %>%
        visNodes(color = list(background = "#f8fafc", border = "#cfd6df"), font = list(size = 16)) %>%
        visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE)
    )
  }

  vis_nodes <- nodes %>%
    mutate(
      id = if ("id" %in% names(.)) as.character(.data$id) else as.character(.data$agent_clean),
      label = if ("label" %in% names(.)) as.character(.data$label) else id,
      total_messages = if ("total_messages" %in% names(.)) coalesce(as.numeric(.data$total_messages), 1) else 1,
      public_posts = if ("public_posts" %in% names(.)) coalesce(as.numeric(.data$public_posts), 0) else 0,
      anomaly_count = if ("anomaly_count" %in% names(.)) coalesce(as.numeric(.data$anomaly_count), 0) else 0,
      sensitive_count = if ("sensitive_count" %in% names(.)) coalesce(as.numeric(.data$sensitive_count), 0) else if ("sensitive_message_count" %in% names(.)) coalesce(as.numeric(.data$sensitive_message_count), 0) else 0,
      value = pmin(34, pmax(18, sqrt(pmax(total_messages + anomaly_count, 1)) * 3.6)),
      title = str_c(
        "<b>", label, "</b>",
        "<br>Total messages: ", comma(total_messages),
        "<br>Public posts: ", comma(public_posts),
        "<br>Anomaly count: ", comma(anomaly_count),
        "<br>Sensitive message count: ", comma(sensitive_count)
      )
    ) %>%
    select(id, label, value, title)

  vis_edges <- edges %>%
    mutate(
      from = if ("from" %in% names(.)) as.character(.data$from) else as.character(.data$from_agent),
      to = if ("to" %in% names(.)) as.character(.data$to) else as.character(.data$to_agent),
      weight = if ("weight" %in% names(.)) coalesce(as.numeric(.data$weight), 1) else 1,
      from_agent = if ("from_agent" %in% names(.)) as.character(.data$from_agent) else from,
      to_agent = if ("to_agent" %in% names(.)) as.character(.data$to_agent) else to,
      channel = if ("channel" %in% names(.)) coalesce(as.character(.data$channel), "Unspecified") else "Unspecified",
      crisis_phase = if ("crisis_phase" %in% names(.)) coalesce(as.character(.data$crisis_phase), "Unclassified") else "Unclassified",
      channel_summary = if ("channel_summary" %in% names(.)) coalesce(as.character(.data$channel_summary), channel) else channel,
      phase_summary = if ("phase_summary" %in% names(.)) coalesce(as.character(.data$phase_summary), crisis_phase) else crisis_phase,
      width = pmin(4, pmax(0.8, log1p(pmax(weight, 1)) * 0.9)),
      title = str_c(
        "<b>", from_agent, " -> ", to_agent, "</b>",
        "<br>from_agent: ", from_agent,
        "<br>to_agent: ", to_agent,
        "<br>channels: ", channel_summary,
        "<br>crisis phases: ", phase_summary,
        "<br>weight: ", comma(weight)
      )
    ) %>%
    filter(!is.na(.data$from), !is.na(.data$to), .data$from != .data$to) %>%
    select(from, to, width, title)

  if (nrow(vis_edges) == 0) {
    return(
      visNetwork(
        data.frame(id = "empty", label = "No communication links for selected filters", shape = "box"),
        data.frame()
      ) %>%
        visNodes(color = list(background = "#f8fafc", border = "#cfd6df"), font = list(size = 16)) %>%
        visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE)
    )
  }

  visNetwork(vis_nodes, vis_edges) %>%
    visLayout(randomSeed = 608, improvedLayout = TRUE) %>%
    visNodes(
      shape = "dot",
      color = list(background = "#eef4f8", border = "#2f6f9f", highlight = list(background = "#dbeafe", border = "#1d4ed8")),
      borderWidth = 2,
      shadow = FALSE,
      font = list(size = 22, face = "arial", color = "#1f2937", vadjust = 30)
    ) %>%
    visEdges(
      smooth = FALSE,
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.35)),
      color = list(color = "#52616f", highlight = "#b42318")
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = TRUE
    ) %>%
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -120,
        centralGravity = 0.005,
        springLength = 260,
        springConstant = 0.02,
        avoidOverlap = 1
      ),
      stabilization = list(enabled = TRUE, iterations = 300),
      minVelocity = 0.75
    ) %>%
    visEvents(stabilizationIterationsDone = "function () { this.setOptions({physics: false}); }")
}

plot_channel_distribution <- function(comms) {
  if (!has_rows(comms) || !"channel_group" %in% names(comms)) {
    return(empty_plot())
  }

  comms %>%
    count(channel_group, name = "messages") %>%
    ggplot(aes(x = reorder(channel_group, messages), y = messages)) +
    geom_col(fill = "#2f6f9f", width = 0.72) +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Messages") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

plot_network_comparison <- function(comms) {
  if (!has_rows(comms) || !all(c("crisis_phase", "channel_risk") %in% names(comms))) {
    return(empty_plot())
  }

  comms %>%
    count(crisis_phase, channel_risk, name = "messages") %>%
    ggplot(aes(x = crisis_phase, y = messages, fill = channel_risk)) +
    geom_col(position = "dodge", width = 0.72) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Messages", fill = "Channel risk") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid.minor = element_blank())
}

build_breach_pathway_network <- function(nodes, edges) {
  if (!has_rows(nodes) || !has_rows(edges)) {
    return(visNetwork(data.frame(id = "empty", label = "No data available"), data.frame()))
  }

  stage_order <- c(
    "Sensitive internal discussion",
    "Side/private coordination",
    "Weak or late enforcement",
    "Public-facing post",
    "Embargo breach"
  )

  vis_nodes <- nodes %>%
    mutate(
      id = if ("id" %in% names(.)) as.character(.data$id) else as.character(.data$node_id),
      .stage = if ("stage" %in% names(.)) as.character(.data$stage) else NA_character_,
      .stage_order = if ("stage_order" %in% names(.)) as.integer(.data$stage_order) else NA_integer_,
      label = if ("label" %in% names(.)) as.character(.data$label) else coalesce(.stage, id),
      level = case_when(
        label %in% stage_order ~ match(label, stage_order),
        .stage %in% stage_order ~ match(.stage, stage_order),
        !is.na(.stage_order) ~ .stage_order,
        TRUE ~ row_number()
      ),
      title = str_c(
        "<b>", label, "</b>",
        if ("description" %in% names(.)) str_c("<br>", .data$description) else ""
      ),
      shape = "box",
      color.background = case_when(
        level == 1 ~ "#e8f2f8",
        level == 2 ~ "#eef4e7",
        level == 3 ~ "#fff4d8",
        level == 4 ~ "#fbe7df",
        level >= 5 ~ "#f7d7d7",
        TRUE ~ "#edf2f7"
      ),
      color.border = case_when(
        level == 1 ~ "#2f6f9f",
        level == 2 ~ "#5f8f4e",
        level == 3 ~ "#b7791f",
        level == 4 ~ "#c05621",
        level >= 5 ~ "#b42318",
        TRUE ~ "#667085"
      ),
      font.size = 18,
      font.face = "bold",
      margin = 14,
      widthConstraint.minimum = 150,
      widthConstraint.maximum = 190
    ) %>%
    arrange(level) %>%
    select(
      id,
      label,
      level,
      title,
      shape,
      color.background,
      color.border,
      font.size,
      font.face,
      margin,
      widthConstraint.minimum,
      widthConstraint.maximum
    )

  vis_edges <- edges %>%
    transmute(
      from = as.character(.data$from),
      to = as.character(.data$to),
      arrows = "to",
      width = if ("weight" %in% names(.)) pmax(1, as.numeric(.data$weight)) else 1
    )

  visNetwork(vis_nodes, vis_edges) %>%
    visHierarchicalLayout(
      direction = "LR",
      sortMethod = "directed",
      levelSeparation = 230,
      nodeSpacing = 180,
      treeSpacing = 220
    ) %>%
    visNodes(shapeProperties = list(borderRadius = 6)) %>%
    visEdges(smooth = list(type = "cubicBezier", forceDirection = "horizontal"), color = list(color = "#667085")) %>%
    visPhysics(enabled = FALSE) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE)
}

plot_judge_coverage <- function(pathway_evidence) {
  if (!has_rows(pathway_evidence) || !all(c("judge_monitored_status", "pathway_stage") %in% names(pathway_evidence))) {
    return(empty_plot())
  }

  pathway_evidence %>%
    count(judge_monitored_status, pathway_stage, name = "records") %>%
    ggplot(aes(x = pathway_stage, y = records, fill = judge_monitored_status)) +
    geom_col(position = "dodge", width = 0.72) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Evidence records", fill = "Judge status") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid.minor = element_blank())
}

plot_pathway_behavior_comparison <- function(pathway_evidence) {
  if (!has_rows(pathway_evidence) || !all(c("crisis_phase", "channel_risk") %in% names(pathway_evidence))) {
    return(empty_plot())
  }

  pathway_evidence %>%
    count(crisis_phase, channel_risk, name = "records") %>%
    ggplot(aes(x = crisis_phase, y = records, fill = channel_risk)) +
    geom_col(position = "dodge", width = 0.72) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Evidence records", fill = "Channel risk") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid.minor = element_blank())
}

make_event_detail_table <- function(data) {
  useful_cols <- c(
    "timestamp",
    "agent_clean",
    "channel",
    "channel_group",
    "crisis_phase",
    "anomaly_reason",
    "content"
  )

  table_data <- if (is.data.frame(data)) {
    ensure_table_cols(data, useful_cols)
  } else {
    ensure_table_cols(tibble(), useful_cols)
  }

  DT::datatable(
    table_data,
    rownames = FALSE,
    options = list(scrollX = TRUE, pageLength = 8)
  )
}

make_round_context_table <- function(data) {
  useful_cols <- c(
    "event_time",
    "event_label",
    "event_headline",
    "event_narrative",
    "crisis_phase",
    "event_type"
  )

  table_data <- if (is.data.frame(data)) {
    ensure_table_cols(data, useful_cols) %>% distinct()
  } else {
    ensure_table_cols(tibble(), useful_cols)
  }

  DT::datatable(
    table_data,
    rownames = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = 5,
      autoWidth = TRUE
    )
  )
}

make_timeline_event_detail_table <- function(data) {
  useful_cols <- c(
    "timestamp",
    "agent_clean",
    "channel",
    "channel_group",
    "crisis_phase",
    "anomaly_reason",
    "content"
  )

  table_data <- if (is.data.frame(data)) {
    ensure_table_cols(data, useful_cols)
  } else {
    ensure_table_cols(tibble(), useful_cols)
  }

  DT::datatable(
    table_data,
    rownames = FALSE,
    filter = "top",
    options = list(
      scrollX = TRUE,
      pageLength = 10,
      autoWidth = TRUE
    )
  )
}

make_evidence_table <- function(data) {
  useful_cols <- c(
    "pathway_stage",
    "timestamp",
    "message_id",
    "agent_clean",
    "channel",
    "channel_group",
    "crisis_phase",
    "channel_risk",
    "judge_monitored_status",
    "anomaly_reason",
    "content",
    "deliberating",
    "rationalizing",
    "reacting"
  )

  table_data <- if (is.data.frame(data)) {
    ensure_table_cols(data, useful_cols)
  } else {
    ensure_table_cols(tibble(), useful_cols)
  }

  DT::datatable(
    table_data,
    rownames = FALSE,
    filter = "top",
    options = list(scrollX = TRUE, pageLength = 8, autoWidth = TRUE)
  )
}
