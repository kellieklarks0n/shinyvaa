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
      event_label = coalesce(as.character(.data$event_label), "Event"),
      event_type = if ("event_type" %in% names(.)) coalesce(as.character(.data$event_type), "Event") else "Event",
      is_anomaly_event = if ("is_anomaly_event" %in% names(.)) coalesce(.data$is_anomaly_event, FALSE) else FALSE,
      y_value = event_type,
      tooltip = str_c(
        "<b>", event_label, "</b>",
        "<br>Time: ", format(event_time, "%Y-%m-%d %H:%M"),
        "<br>Type: ", event_type,
        if ("crisis_phase" %in% names(.)) str_c("<br>Phase: ", .data$crisis_phase) else ""
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

  ggplot(timeline, aes(x = event_time, y = y_value, text = tooltip)) +
    geom_vline(
      xintercept = as.numeric(ymd_hms("2046-06-05 17:00:00", tz = "UTC")),
      linetype = "dashed",
      colour = "#d97706",
      linewidth = 0.45
    ) +
    geom_vline(
      xintercept = as.numeric(ymd_hms("2046-06-05 18:00:00", tz = "UTC")),
      linetype = "dotted",
      colour = "#b91c1c",
      linewidth = 0.55
    ) +
    geom_point(aes(colour = anomaly_display), size = 3, alpha = 0.9) +
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
    scale_x_datetime(labels = label_date_short()) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
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

  vis_nodes <- nodes %>%
    mutate(
      id = if ("id" %in% names(.)) as.character(.data$id) else as.character(.data$node_id),
      label = if ("label" %in% names(.)) as.character(.data$label) else as.character(.data$stage),
      level = if ("stage_order" %in% names(.)) .data$stage_order else row_number(),
      title = if ("stage" %in% names(.)) as.character(.data$stage) else label,
      shape = "box"
    ) %>%
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
    visEdges(smooth = TRUE) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE)
}

build_agent_network <- function(nodes, edges) {
  if (!has_rows(nodes) || !has_rows(edges)) {
    return(visNetwork(data.frame(id = "empty", label = "No data available"), data.frame()))
  }

  vis_nodes <- nodes %>%
    mutate(
      id = if ("id" %in% names(.)) as.character(.data$id) else as.character(.data$agent_clean),
      label = if ("label" %in% names(.)) as.character(.data$label) else id,
      total_messages = if ("total_messages" %in% names(.)) coalesce(as.numeric(.data$total_messages), 1) else 1,
      value = pmax(5, total_messages),
      title = str_c(
        "<b>", label, "</b>",
        if ("total_messages" %in% names(.)) str_c("<br>Messages: ", comma(total_messages)) else "",
        if ("public_posts" %in% names(.)) str_c("<br>Public posts: ", comma(.data$public_posts)) else "",
        if ("anomaly_count" %in% names(.)) str_c("<br>Anomalies: ", comma(.data$anomaly_count)) else "",
        if ("sensitive_count" %in% names(.)) str_c("<br>Sensitive messages: ", comma(.data$sensitive_count)) else ""
      )
    ) %>%
    select(id, label, value, title)

  vis_edges <- edges %>%
    mutate(
      from = if ("from" %in% names(.)) as.character(.data$from) else as.character(.data$from_agent),
      to = if ("to" %in% names(.)) as.character(.data$to) else as.character(.data$to_agent),
      weight = if ("weight" %in% names(.)) coalesce(as.numeric(.data$weight), 1) else 1,
      width = pmax(1, sqrt(weight)),
      title = str_c(
        "Messages: ", comma(weight),
        if ("channel_group" %in% names(.)) str_c("<br>Channel group: ", .data$channel_group) else "",
        if ("channel_risk" %in% names(.)) str_c("<br>Risk: ", .data$channel_risk) else "",
        if ("crisis_phase" %in% names(.)) str_c("<br>Phase: ", .data$crisis_phase) else ""
      ),
      arrows = "to"
    ) %>%
    select(from, to, width, title, arrows)

  visNetwork(vis_nodes, vis_edges) %>%
    visEdges(smooth = TRUE) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visPhysics(stabilization = TRUE)
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

  vis_nodes <- nodes %>%
    mutate(
      id = if ("id" %in% names(.)) as.character(.data$id) else as.character(.data$node_id),
      label = if ("label" %in% names(.)) as.character(.data$label) else as.character(.data$stage),
      level = if ("stage_order" %in% names(.)) .data$stage_order else row_number(),
      title = str_c(
        "<b>", label, "</b>",
        if ("description" %in% names(.)) str_c("<br>", .data$description) else ""
      ),
      shape = "box"
    ) %>%
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
    visEdges(smooth = TRUE) %>%
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
    "event_time",
    "event_label",
    "agent_clean",
    "channel",
    "channel_group",
    "crisis_phase",
    "anomaly_reason",
    "content"
  )

  table_data <- if (is.data.frame(data)) {
    data %>% select(any_of(useful_cols))
  } else {
    tibble()
  }

  DT::datatable(
    table_data,
    rownames = FALSE,
    options = list(scrollX = TRUE, pageLength = 8)
  )
}

make_evidence_table <- function(data) {
  useful_cols <- c(
    "pathway_stage",
    "timestamp",
    "message_id",
    "agent_clean",
    "channel",
    "crisis_phase",
    "channel_risk",
    "anomaly_reason",
    "content",
    "deliberating"
  )

  table_data <- if (is.data.frame(data)) {
    data %>% select(any_of(useful_cols))
  } else {
    tibble()
  }

  DT::datatable(
    table_data,
    rownames = FALSE,
    options = list(scrollX = TRUE, pageLength = 8)
  )
}
