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
    phase_label = str_wrap(phase, width = 12),
    x = seq(1, by = 2.85, length.out = length(phase)),
    y = 1
  )

  ggplot(phases, aes(x = x, y = y)) +
    geom_tile(
      width = 2.2,
      height = 0.66,
      fill = "#f8fafc",
      colour = "#9aa6b2",
      linewidth = 0.45
    ) +
    geom_text(
      aes(label = phase_label),
      colour = "#17202a",
      size = 3.45,
      fontface = "bold",
      lineheight = 0.95
    ) +
    geom_segment(
      data = phases %>% filter(x < max(x)),
      aes(x = x + 1.14, xend = x + 1.7, y = y, yend = y),
      arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
      inherit.aes = FALSE,
      colour = "#52616f",
      linewidth = 0.75,
      lineend = "round"
    ) +
    scale_x_continuous(limits = c(-0.25, max(phases$x) + 1.25), expand = expansion(mult = 0.01)) +
    scale_y_continuous(limits = c(0.52, 1.48), expand = expansion(mult = 0)) +
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
      is_anomaly_event = if ("is_anomaly_event" %in% names(.)) {
        coalesce(as.logical(.data$is_anomaly_event), FALSE)
      } else if ("anomaly_reason" %in% names(.)) {
        !is.na(.data$anomaly_reason) & nzchar(as.character(.data$anomaly_reason))
      } else {
        FALSE
      },
      anomaly_status = if_else(is_anomaly_event, "Anomaly", "Not flagged"),
      tooltip = str_c(
        "<b>", event_label, "</b>",
        "<br>Time: ", format(event_time, "%Y-%m-%d %H:%M"),
        "<br>Type: ", event_type,
        "<br>Phase: ", crisis_phase,
        "<br>Status: ", anomaly_status
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
      y_value = as.numeric(factor(event_type, levels = rev(unique(event_type)))),
      label_to_show = if_else(event_label %in% labelled_events, event_label, NA_character_),
      label_y = y_value + if_else(row_number() %% 2 == 0, 0.24, -0.24),
      anomaly_display = if (show_anomalies) is_anomaly_event else FALSE
    )

  event_type_levels <- rev(unique(timeline$event_type))

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
      data = timeline %>% filter(!is.na(label_to_show)),
      aes(y = label_y, label = label_to_show),
      hjust = -0.05,
      vjust = 0.5,
      size = 3.45,
      fontface = "bold",
      lineheight = 0.95,
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
    scale_y_continuous(
      breaks = seq_along(event_type_levels),
      labels = event_type_levels,
      expand = expansion(add = 0.85)
    ) +
    labs(
      x = NULL,
      y = NULL,
      caption = "Dashed marker: suspected breach window around 5:00 PM, June 5, 2046. Dotted marker: embargo deadline at 6:00 PM, June 5, 2046."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "#e5e9f0", linewidth = 0.35),
      plot.caption = element_text(colour = "#52616f", hjust = 0)
    )
}

build_crisis_timeline_plotly <- function(key_events, show_anomalies = FALSE) {
  if (!has_rows(key_events) || !"event_time" %in% names(key_events)) {
    return(plotly::ggplotly(empty_plot()))
  }

  timeline <- key_events %>%
    mutate(
      event_time = as.POSIXct(event_time, tz = "UTC"),
      event_label = if ("event_label" %in% names(.)) coalesce(as.character(.data$event_label), "Event") else "Event",
      event_type = if ("event_type" %in% names(.)) coalesce(as.character(.data$event_type), "Event") else "Event",
      crisis_phase = if ("crisis_phase" %in% names(.)) coalesce(as.character(.data$crisis_phase), "Unclassified") else "Unclassified",
      is_anomaly_event = if ("is_anomaly_event" %in% names(.)) {
        coalesce(as.logical(.data$is_anomaly_event), FALSE)
      } else if ("anomaly_reason" %in% names(.)) {
        !is.na(.data$anomaly_reason) & nzchar(as.character(.data$anomaly_reason))
      } else {
        FALSE
      },
      anomaly_status = if_else(is_anomaly_event, "Anomaly", "Not flagged")
    ) %>%
    filter(!is.na(event_time))

  if (nrow(timeline) == 0) {
    return(plotly::ggplotly(empty_plot()))
  }

  event_type_levels <- rev(unique(timeline$event_type))
  timeline <- timeline %>%
    mutate(
      y_value = as.numeric(factor(event_type, levels = event_type_levels)),
      trace_group = if_else(show_anomalies & is_anomaly_event, "Anomaly event", "Normal event"),
      tooltip = str_c(
        "<b>", event_label, "</b>",
        "<br>Time: ", format(event_time, "%Y-%m-%d %H:%M"),
        "<br>Type: ", event_type,
        "<br>Phase: ", crisis_phase,
        "<br>Status: ", anomaly_status
      )
    )

  labelled_events <- c(
    "AG inquiries",
    "NHPI report",
    "Elena incident",
    "Judge enters",
    "SaltWind piece",
    "ResidentIQ rumor",
    "Embargo breach"
  )

  label_data <- timeline %>%
    filter(event_label %in% labelled_events) %>%
    arrange(event_time) %>%
    distinct(event_label, .keep_all = TRUE) %>%
    mutate(yshift = rep(c(34, -38, 48, -52), length.out = n()))

  label_annotations <- lapply(seq_len(nrow(label_data)), function(i) {
    list(
      x = label_data$event_time[[i]],
      y = label_data$y_value[[i]],
      text = label_data$event_label[[i]],
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      yshift = label_data$yshift[[i]],
      font = list(size = 13, color = "#17202a", family = "Arial"),
      bgcolor = "rgba(255,255,255,0.88)",
      bordercolor = "rgba(207,214,223,0.9)",
      borderpad = 3
    )
  })

  suspected_release_time <- ymd_hms("2046-06-05 17:00:00", tz = "UTC")
  suspected_release_start <- suspected_release_time - minutes(15)
  suspected_release_end <- suspected_release_time + minutes(15)
  embargo_deadline <- ymd_hms("2046-06-05 18:00:00", tz = "UTC")

  timeline_shapes <- list(
    list(
      type = "rect",
      xref = "x",
      yref = "paper",
      x0 = suspected_release_start,
      x1 = suspected_release_end,
      y0 = 0,
      y1 = 1,
      fillcolor = "rgba(245,158,11,0.12)",
      line = list(width = 0),
      layer = "below"
    ),
    list(
      type = "line",
      xref = "x",
      yref = "paper",
      x0 = suspected_release_time,
      x1 = suspected_release_time,
      y0 = 0,
      y1 = 1,
      line = list(color = "#d97706", width = 1.2, dash = "dash")
    ),
    list(
      type = "line",
      xref = "x",
      yref = "paper",
      x0 = embargo_deadline,
      x1 = embargo_deadline,
      y0 = 0,
      y1 = 1,
      line = list(color = "#b91c1c", width = 1.3, dash = "dot")
    )
  )

  plot_data_normal <- timeline %>% filter(trace_group == "Normal event")
  plot_data_anomaly <- timeline %>% filter(trace_group == "Anomaly event")

  p <- plot_ly()

  if (nrow(plot_data_normal) > 0) {
    p <- p %>%
      add_trace(
        data = plot_data_normal,
        x = ~event_time,
        y = ~y_value,
        type = "scatter",
        mode = "markers",
        name = "Normal event",
        text = ~tooltip,
        hoverinfo = "text",
        marker = list(color = "#2f6f9f", size = 9, opacity = 0.9, line = list(color = "#ffffff", width = 1))
      )
  }

  if (nrow(plot_data_anomaly) > 0) {
    p <- p %>%
      add_trace(
        data = plot_data_anomaly,
        x = ~event_time,
        y = ~y_value,
        type = "scatter",
        mode = "markers",
        name = "Anomaly event",
        text = ~tooltip,
        hoverinfo = "text",
        marker = list(color = "#c2410c", size = 12, opacity = 0.95, symbol = "circle-open", line = list(color = "#b91c1c", width = 2))
      )
  }

  p %>%
    layout(
      annotations = label_annotations,
      shapes = timeline_shapes,
      legend = list(
        orientation = "h",
        x = 0,
        y = 1.12,
        xanchor = "left",
        yanchor = "bottom",
        bgcolor = "rgba(255,255,255,0)"
      ),
      margin = list(l = 150, r = 30, t = 80, b = 115),
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#cfd6df", font = list(color = "#17202a")),
      xaxis = list(
        title = "",
        rangeslider = list(visible = TRUE, thickness = 0.12),
        showgrid = TRUE,
        gridcolor = "#e5e9f0",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        tickmode = "array",
        tickvals = seq_along(event_type_levels),
        ticktext = event_type_levels,
        range = c(0.25, length(event_type_levels) + 0.75),
        showgrid = TRUE,
        gridcolor = "#e5e9f0",
        zeroline = FALSE
      )
    ) %>%
    config(displayModeBar = TRUE)
}

plot_message_volume_by_phase <- function(comms) {
  if (!has_rows(comms) || !all(c("crisis_phase", "channel_group") %in% names(comms))) {
    return(empty_plot())
  }

  comms %>%
    count(crisis_phase, channel_group, name = "messages") %>%
    mutate(
      tooltip = paste0(
        "Crisis phase: ", .data$crisis_phase,
        "<br>Channel group: ", .data$channel_group,
        "<br>Messages: ", comma(.data$messages)
      )
    ) %>%
    ggplot(aes(x = crisis_phase, y = messages, fill = channel_group, text = tooltip)) +
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

  keyword_counts <- keyword_counts %>%
    mutate(
      tooltip = paste0(
        "Crisis phase: ", .data$crisis_phase,
        "<br>Keyword group: ", .data$keyword_group,
        "<br>Messages: ", comma(.data$messages)
      )
    )

  ggplot(keyword_counts, aes(x = crisis_phase, y = messages, fill = keyword_group, text = tooltip)) +
    geom_col(position = "dodge", width = 0.72) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Messages", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid.minor = element_blank())
}

build_causal_chain_network <- function(nodes, edges) {
  stage_order <- c(
    "Governance concerns",
    "Media escalation",
    "Embargo sensitivity",
    "Judge intervention",
    "Side/private coordination",
    "Public-facing release",
    "Breach response"
  )

  fallback_nodes <- tibble(
    id = paste0("causal_", seq_along(stage_order)),
    raw_label = stage_order,
    level = seq_along(stage_order),
    description = "Conceptual evidence stage for explaining crisis escalation."
  )

  use_fallback <- !has_rows(nodes)

  if (!use_fallback) {
    node_id_col <- intersect(c("id", "node_id", "stage_id"), names(nodes))
    node_label_col <- intersect(c("label", "stage", "title", "description"), names(nodes))

    use_fallback <- length(node_id_col) == 0 || length(node_label_col) == 0
  }

  if (use_fallback) {
    node_base <- fallback_nodes
  } else {
    node_id_col <- intersect(c("id", "node_id", "stage_id"), names(nodes))[[1]]
    node_label_col <- intersect(c("label", "stage", "title", "description"), names(nodes))[[1]]
    node_desc_col <- intersect(c("description", "title", "label", "stage"), names(nodes))

    node_base <- nodes %>%
      transmute(
        id = as.character(.data[[node_id_col]]),
        raw_label = as.character(.data[[node_label_col]]),
        level = if ("stage_order" %in% names(nodes)) suppressWarnings(as.numeric(.data$stage_order)) else NA_real_,
        description = if (length(node_desc_col) > 0) as.character(.data[[node_desc_col[[1]]]]) else raw_label
      ) %>%
      mutate(
        raw_label = coalesce(raw_label, id),
        level = case_when(
          raw_label %in% stage_order ~ as.numeric(match(raw_label, stage_order)),
          !is.na(level) ~ level,
          TRUE ~ as.numeric(row_number())
        )
      ) %>%
      arrange(level)

    if (nrow(node_base) == 0 || all(is.na(node_base$id))) {
      node_base <- fallback_nodes
      use_fallback <- TRUE
    }
  }

  vis_nodes <- node_base %>%
    mutate(
      id = if_else(is.na(id) | !nzchar(id), paste0("causal_", row_number()), id),
      raw_label = if_else(is.na(raw_label) | !nzchar(raw_label), id, raw_label),
      label = str_wrap(raw_label, width = 18),
      title = str_c(
        "<b>", raw_label, "</b>",
        if_else(!is.na(description) & nzchar(description), str_c("<br>", description), "")
      ),
      shape = "box",
      color.background = "#f8fafc",
      color.border = "#2f6f9f",
      font.size = 18,
      font.face = "bold",
      font.color = "#17202a",
      margin = 16,
      widthConstraint.minimum = 170,
      widthConstraint.maximum = 220
    ) %>%
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
      font.color,
      margin,
      widthConstraint.minimum,
      widthConstraint.maximum
    )

  edge_base <- tibble()

  if (!use_fallback && has_rows(edges)) {
    from_col <- intersect(c("from", "from_stage", "source"), names(edges))
    to_col <- intersect(c("to", "to_stage", "target"), names(edges))

    if (length(from_col) > 0 && length(to_col) > 0) {
      edge_base <- edges %>%
        transmute(
          from = as.character(.data[[from_col[[1]]]]),
          to = as.character(.data[[to_col[[1]]]])
        ) %>%
        filter(.data$from %in% vis_nodes$id, .data$to %in% vis_nodes$id)
    }
  }

  if (!has_rows(edge_base)) {
    edge_base <- tibble(
      from = head(vis_nodes$id, -1),
      to = tail(vis_nodes$id, -1)
    )
  }

  vis_edges <- edge_base %>%
    mutate(
      arrows = "to",
      width = 2,
      smooth = FALSE,
      title = "Causal sequence"
    )

  visNetwork(vis_nodes, vis_edges) %>%
    visHierarchicalLayout(
      direction = "LR",
      sortMethod = "directed",
      levelSeparation = 260,
      nodeSpacing = 190,
      treeSpacing = 240
    ) %>%
    visNodes(shapeProperties = list(borderRadius = 6)) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.75)),
      color = list(color = "#52616f", highlight = "#b42318"),
      smooth = FALSE
    ) %>%
    visPhysics(enabled = FALSE) %>%
    visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
    visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE)
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
    mutate(
      channel_label = str_wrap(as.character(.data$channel_group), width = 24),
      tooltip = paste0(
        "Channel group: ", .data$channel_group,
        "<br>Messages: ", comma(.data$messages)
      )
    ) %>%
    ggplot(aes(x = reorder(channel_label, messages), y = messages, text = tooltip)) +
    geom_col(fill = "#2f6f9f", width = 0.72) +
    coord_flip() +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Messages") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(lineheight = 0.95),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 18, 12, 8)
    )
}

plot_network_comparison <- function(comms) {
  if (!has_rows(comms) || !all(c("crisis_phase", "channel_risk") %in% names(comms))) {
    return(empty_plot())
  }

  comms %>%
    count(crisis_phase, channel_risk, name = "messages") %>%
    mutate(
      crisis_phase_label = str_wrap(as.character(.data$crisis_phase), width = 22),
      tooltip = paste0(
        "Crisis phase: ", .data$crisis_phase,
        "<br>Channel risk: ", .data$channel_risk,
        "<br>Messages: ", comma(.data$messages)
      )
    ) %>%
    ggplot(aes(x = crisis_phase_label, y = messages, fill = channel_risk, text = tooltip)) +
    geom_col(position = "dodge", width = 0.72) +
    coord_flip() +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
    labs(x = NULL, y = "Messages", fill = "Channel risk") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(lineheight = 0.95),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 18, 14, 8)
    )
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
    mutate(
      tooltip = paste0(
        "Pathway stage: ", .data$pathway_stage,
        "<br>Judge status: ", .data$judge_monitored_status,
        "<br>Evidence records: ", comma(.data$records)
      )
    ) %>%
    ggplot(aes(x = pathway_stage, y = records, fill = judge_monitored_status, text = tooltip)) +
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
    mutate(
      tooltip = paste0(
        "Crisis phase: ", .data$crisis_phase,
        "<br>Channel risk: ", .data$channel_risk,
        "<br>Evidence records: ", comma(.data$records)
      )
    ) %>%
    ggplot(aes(x = crisis_phase, y = records, fill = channel_risk, text = tooltip)) +
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

  table_data <- table_data %>%
    mutate(
      content = str_trunc(str_squish(as.character(.data$content)), 180),
      anomaly_reason = str_trunc(str_squish(as.character(.data$anomaly_reason)), 140)
    )

  DT::datatable(
    table_data,
    rownames = FALSE,
    filter = "top",
    class = "display compact stripe tenantthread-table",
    options = list(
      scrollX = TRUE,
      pageLength = 8,
      autoWidth = TRUE,
      searching = TRUE,
      columnDefs = list(
        list(width = "140px", targets = 0),
        list(width = "130px", targets = 1),
        list(width = "120px", targets = 2),
        list(width = "130px", targets = 3),
        list(width = "150px", targets = 4),
        list(width = "220px", targets = 5),
        list(width = "420px", targets = 6)
      )
    )
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

  table_data <- table_data %>%
    mutate(
      content = str_trunc(str_squish(as.character(.data$content)), 180),
      anomaly_reason = str_trunc(str_squish(as.character(.data$anomaly_reason)), 140)
    )

  DT::datatable(
    table_data,
    rownames = FALSE,
    filter = "top",
    class = "display compact stripe tenantthread-table",
    options = list(
      scrollX = TRUE,
      pageLength = 8,
      autoWidth = TRUE,
      searching = TRUE,
      columnDefs = list(
        list(width = "140px", targets = 0),
        list(width = "135px", targets = 1),
        list(width = "120px", targets = 2),
        list(width = "130px", targets = 3),
        list(width = "120px", targets = 4),
        list(width = "130px", targets = 5),
        list(width = "150px", targets = 6),
        list(width = "130px", targets = 7),
        list(width = "170px", targets = 8),
        list(width = "220px", targets = 9),
        list(width = "430px", targets = 10),
        list(width = "110px", targets = c(11, 12, 13))
      )
    )
  )
}
