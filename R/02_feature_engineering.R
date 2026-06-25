# Feature engineering for the TenantThread Shiny visual analytics app.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(purrr)
})

processed_dir <- file.path("data", "processed")
required_files <- file.path(
  processed_dir,
  c("comms_flat.rds", "rounds_context.rds", "participants.rds")
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Required processed data files are missing:\n",
    paste0("- ", missing_files, collapse = "\n"),
    "\nRun R/01_data_prep.R first, then rerun R/02_feature_engineering.R.",
    call. = FALSE
  )
}

comms_flat <- readRDS(file.path(processed_dir, "comms_flat.rds"))
rounds_context <- readRDS(file.path(processed_dir, "rounds_context.rds"))
participants <- readRDS(file.path(processed_dir, "participants.rds"))

breach_window_start <- ymd_hms("2046-06-05 17:00:00", tz = "UTC")
embargo_deadline <- ymd_hms("2046-06-05 18:00:00", tz = "UTC")

parse_time <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }

  parse_date_time(
    x,
    orders = c("ymd HMS", "ymd HM", "ymd IMS p", "ymd IM p", "ymd", "mdy HMS", "mdy HM"),
    tz = "UTC",
    quiet = TRUE
  )
}

safe_chr <- function(x) {
  coalesce(as.character(x), "")
}

detect_terms <- function(text, terms) {
  pattern <- str_c(str_escape(terms), collapse = "|")
  str_detect(safe_chr(text), regex(pattern, ignore_case = TRUE))
}

classify_phase <- function(event_time, headline = "", narrative = "") {
  text <- str_to_lower(str_c(headline, narrative, sep = " "))

  case_when(
    !is.na(event_time) & event_time >= breach_window_start ~ "Breach / response",
    str_detect(text, "breach|leak|6:00|6 pm|embargo deadline|response") ~ "Breach / response",
    str_detect(text, "embargo|civicloom|harborcrest|merger|acquisition|deal|announcement|no plan b") ~ "Embargo-sensitive",
    str_detect(text, "saltwind|residentiq|media|piece|rumor|re-identification|de-anonymization|elena|judge") ~ "Media escalation",
    str_detect(text, "governance|nhpi|algorithmic|tenantrights|service load|operator misuse|retention optimizer") ~ "Governance tension",
    !is.na(event_time) & as.Date(event_time) >= as.Date("2046-05-30") ~ "Media escalation",
    !is.na(event_time) & as.Date(event_time) >= as.Date("2046-05-25") ~ "Embargo-sensitive",
    !is.na(event_time) & as.Date(event_time) >= as.Date("2046-05-21") ~ "Governance tension",
    TRUE ~ "Pre-crisis"
  )
}

channel_risk_for <- function(channel) {
  channel_clean <- str_to_lower(safe_chr(channel))

  case_when(
    channel_clean == "comms_huddle" ~ "Low risk",
    channel_clean == "one_on_one_chat" ~ "Medium risk",
    channel_clean == "side_huddle" ~ "High risk",
    channel_clean %in% c("official_post", "personal_post", "anonymous_post") ~ "Direct public risk",
    TRUE ~ "Other"
  )
}

public_channel_for <- function(channel) {
  str_to_lower(safe_chr(channel)) %in% c("official_post", "personal_post", "anonymous_post")
}

private_or_side_for <- function(channel) {
  str_to_lower(safe_chr(channel)) %in% c("one_on_one_chat", "side_huddle")
}

judge_status_for <- function(channel) {
  channel_clean <- str_to_lower(safe_chr(channel))

  case_when(
    channel_clean %in% c("official_post", "personal_post", "anonymous_post") ~ "Public channel",
    channel_clean == "comms_huddle" ~ "Likely monitored",
    channel_clean %in% c("one_on_one_chat", "side_huddle") ~ "Likely outside scope",
    TRUE ~ "Unclear"
  )
}

collapse_reasons <- function(...) {
  reasons <- c(...)
  reasons <- reasons[!is.na(reasons) & reasons != ""]
  if (length(reasons) == 0) NA_character_ else str_c(reasons, collapse = "; ")
}

embargo_terms <- c(
  "CivicLoom",
  "HarborCrest",
  "Project HarborCrest",
  "merger",
  "acquisition",
  "embargo",
  "announcement",
  "deal",
  "6:00",
  "6 PM",
  "CivicLoom Realty"
)

crisis_terms <- c(
  "Retention Optimizer",
  "AlgorithmicEviction",
  "SaltWind",
  "TenantRights",
  "NHPI",
  "Service Load Score",
  "de-anonymization",
  "re-identification",
  "Judge",
  "FleX"
)

rounds_features <- rounds_context %>%
  mutate(
    event_time = parse_time(hour),
    event_text = str_c(
      safe_chr(event_headline),
      safe_chr(event_narrative),
      safe_chr(media_events),
      safe_chr(external_actor_actions),
      safe_chr(news),
      sep = " "
    ),
    crisis_phase = classify_phase(event_time, event_headline, event_narrative),
    embargo_sensitive_event = detect_terms(event_text, embargo_terms),
    crisis_sensitive_event = detect_terms(event_text, crisis_terms)
  )

comms_features <- comms_flat %>%
  left_join(
    rounds_features %>%
      select(round_id, round_event_time = event_time, event_headline, event_narrative, round_crisis_phase = crisis_phase),
    by = "round_id"
  ) %>%
  mutate(
    timestamp = parse_time(timestamp),
    event_time = coalesce(timestamp, round_event_time),
    message_text = str_c(
      safe_chr(content),
      safe_chr(reacting),
      safe_chr(rationalizing),
      safe_chr(deliberating),
      sep = " "
    ),
    crisis_phase = classify_phase(event_time, event_headline, event_narrative),
    channel_risk = channel_risk_for(channel),
    public_channel = public_channel_for(channel),
    private_or_side_channel = private_or_side_for(channel),
    internal_channel = !public_channel,
    embargo_sensitive = detect_terms(message_text, embargo_terms),
    crisis_sensitive = detect_terms(message_text, crisis_terms),
    sensitive_content = embargo_sensitive | crisis_sensitive,
    judge_related = detect_terms(message_text, "Judge"),
    judge_enforcement_or_monitoring_issue = judge_related &
      str_detect(
        message_text,
        regex("enforce|monitor|scope|watch|flag|violation|breach|embargo|post|leak|approve|hold", ignore_case = TRUE)
      ),
    junior_agent_involved = str_detect(
      str_c(agent_id, agent_role, agent_label, agent_clean, sep = " "),
      regex("intern|junior", ignore_case = TRUE)
    ),
    risky_channel_during_breach_response = event_time >= breach_window_start &
      channel_risk %in% c("Medium risk", "High risk", "Direct public risk"),
    anomaly_embargo_public = embargo_sensitive & public_channel,
    anomaly_embargo_private_side = embargo_sensitive & private_or_side_channel,
    anomaly_crisis_public = crisis_sensitive & public_channel,
    anomaly_junior_sensitive = junior_agent_involved & sensitive_content,
    anomaly_judge_issue = judge_enforcement_or_monitoring_issue,
    anomaly_reason = pmap_chr(
      list(
        anomaly_embargo_public,
        anomaly_embargo_private_side,
        anomaly_crisis_public,
        risky_channel_during_breach_response,
        anomaly_junior_sensitive,
        anomaly_judge_issue
      ),
      function(embargo_public, embargo_private, crisis_public, risky_breach, junior_sensitive, judge_issue) {
        collapse_reasons(
          if (embargo_public) "Embargo-sensitive content in public channel" else NA_character_,
          if (embargo_private) "Embargo-sensitive content in side or private channel" else NA_character_,
          if (crisis_public) "Crisis-sensitive content in public channel" else NA_character_,
          if (risky_breach) "Risky channel during breach or response period" else NA_character_,
          if (junior_sensitive) "Junior agent involved in sensitive communication" else NA_character_,
          if (judge_issue) "Judge-related enforcement or monitoring issue" else NA_character_
        )
      }
    ),
    anomaly_flag = !is.na(anomaly_reason),
    judge_monitored_status = judge_status_for(channel),
    has_parent_message = !is.na(responding_to) & responding_to != ""
  )

parent_lookup <- comms_features %>%
  select(
    parent_message_id = message_id,
    parent_agent = agent_clean,
    parent_agent_id = agent_id,
    parent_channel = channel,
    parent_responding_to = responding_to
  )

comms_features <- comms_features %>%
  left_join(parent_lookup, by = c("responding_to" = "parent_message_id"))

parent_map <- setNames(parent_lookup$parent_responding_to, parent_lookup$parent_message_id)

response_depth_for <- function(message_id, responding_to) {
  depth <- if_else(!is.na(responding_to) & responding_to != "", 1L, 0L)
  current_parent <- responding_to
  seen <- message_id

  while (!is.na(current_parent) && current_parent != "" && current_parent %in% names(parent_map)) {
    next_parent <- parent_map[[current_parent]]

    if (is.na(next_parent) || next_parent == "" || next_parent %in% seen) {
      break
    }

    depth <- depth + 1L
    seen <- c(seen, current_parent)
    current_parent <- next_parent
  }

  depth
}

comms_features <- comms_features %>%
  mutate(
    response_depth = map2_int(message_id, responding_to, response_depth_for)
  )

network_edges <- comms_features %>%
  filter(!is.na(agent_clean), !is.na(parent_agent), agent_clean != parent_agent) %>%
  count(
    from_agent = agent_clean,
    to_agent = parent_agent,
    channel,
    channel_group,
    channel_risk,
    crisis_phase,
    name = "weight"
  ) %>%
  arrange(desc(weight), from_agent, to_agent)

network_nodes <- comms_features %>%
  group_by(agent_clean) %>%
  summarise(
    total_messages = n(),
    public_posts = sum(public_channel, na.rm = TRUE),
    anomaly_count = sum(anomaly_flag, na.rm = TRUE),
    sensitive_count = sum(sensitive_content, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_messages), agent_clean)

detect_event_labels <- function(text, event_time) {
  text <- str_to_lower(safe_chr(text))
  labels <- tibble(event_label = character(), event_type = character())

  add_label <- function(label, type) {
    tibble(event_label = label, event_type = type)
  }

  if (str_detect(text, "ag inquiries|attorney general|\\bag\\b")) {
    labels <- bind_rows(labels, add_label("AG inquiries", "Regulatory"))
  }
  if (str_detect(text, "nhpi")) {
    labels <- bind_rows(labels, add_label("NHPI report", "Governance"))
  }
  if (str_detect(text, "elena")) {
    labels <- bind_rows(labels, add_label("Elena incident", "Public misstep"))
  }
  if (str_detect(text, "judge")) {
    labels <- bind_rows(labels, add_label("Judge enters", "Enforcement"))
  }
  if (str_detect(text, "saltwind")) {
    labels <- bind_rows(labels, add_label("SaltWind piece", "Media"))
  }
  if (str_detect(text, "residentiq")) {
    labels <- bind_rows(labels, add_label("ResidentIQ rumor", "Competitive rumor"))
  }
  if (
    str_detect(text, "breach|leak|6:00|6 pm|embargo deadline") ||
      (!is.na(event_time) && event_time >= breach_window_start)
  ) {
    labels <- bind_rows(labels, add_label("Embargo breach", "Embargo"))
  }

  distinct(labels)
}

key_events <- rounds_features %>%
  mutate(
    event_text = str_c(
      safe_chr(event_headline),
      safe_chr(event_narrative),
      safe_chr(media_events),
      safe_chr(external_actor_actions),
      safe_chr(social_manager_alerts),
      safe_chr(news),
      sep = " "
    ),
    key_event_label = map2(event_text, event_time, detect_event_labels),
    has_detected_event = map_lgl(key_event_label, ~ nrow(.x) > 0),
    key_event_label = map2(key_event_label, event_headline, function(labels, headline) {
      if (nrow(labels) == 0) {
        return(tibble(event_label = headline, event_type = "Context"))
      }

      labels
    })
  ) %>%
  filter(has_detected_event | embargo_sensitive_event | crisis_sensitive_event) %>%
  unnest(key_event_label) %>%
  transmute(
    event_id = str_c("event_", str_pad(row_number(), 2, pad = "0")),
    event_time,
    event_label,
    event_type,
    crisis_phase,
    event_headline,
    event_narrative,
    is_anomaly_event = event_label %in% c("Elena incident", "Embargo breach") |
      embargo_sensitive_event |
      crisis_sensitive_event
  )

causal_chain_nodes <- tibble(
  stage_order = 1:7,
  stage = c(
    "Governance concerns",
    "Media escalation",
    "Embargo sensitivity",
    "Judge intervention",
    "Side/private coordination",
    "Public-facing release",
    "Breach response"
  )
) %>%
  mutate(node_id = str_c("causal_", stage_order), label = stage) %>%
  select(node_id, stage_order, label, stage)

causal_chain_edges <- tibble(
  from = causal_chain_nodes$node_id[-nrow(causal_chain_nodes)],
  to = causal_chain_nodes$node_id[-1],
  weight = 1
)

breach_pathway_nodes <- tibble(
  stage_order = 1:5,
  stage = c(
    "Sensitive internal discussion",
    "Side/private coordination",
    "Weak or late enforcement",
    "Public-facing post",
    "Embargo breach"
  ),
  description = c(
    "Embargo or crisis-sensitive details appear in internal discussion.",
    "Sensitive details move into one-on-one or side-channel coordination.",
    "Judge coverage or enforcement appears incomplete, delayed, or outside scope.",
    "Sensitive content reaches official, personal, or anonymous public channels.",
    "Embargo-sensitive content appears during the breach or response window."
  )
) %>%
  mutate(node_id = str_c("pathway_", stage_order), label = stage) %>%
  select(node_id, stage_order, label, stage, description)

breach_pathway_edges <- tibble(
  from = breach_pathway_nodes$node_id[-nrow(breach_pathway_nodes)],
  to = breach_pathway_nodes$node_id[-1],
  weight = 1
)

pathway_evidence <- comms_features %>%
  filter(sensitive_content | anomaly_flag) %>%
  mutate(
    pathway_stage = case_when(
      embargo_sensitive & public_channel & event_time >= breach_window_start ~ "Embargo breach",
      sensitive_content & public_channel ~ "Public-facing post",
      judge_enforcement_or_monitoring_issue ~ "Weak or late enforcement",
      sensitive_content & private_or_side_channel ~ "Side/private coordination",
      anomaly_flag & str_detect(safe_chr(anomaly_reason), "Risky channel during breach or response period") ~ "Weak or late enforcement",
      sensitive_content & internal_channel ~ "Sensitive internal discussion",
      anomaly_flag ~ "Weak or late enforcement",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(pathway_stage)) %>%
  left_join(
    breach_pathway_nodes %>% select(pathway_stage = stage, pathway_node_id = node_id, stage_order),
    by = "pathway_stage"
  ) %>%
  transmute(
    message_id,
    round_id,
    timestamp = event_time,
    agent_clean,
    channel,
    channel_risk,
    crisis_phase,
    pathway_node_id,
    pathway_stage,
    stage_order,
    embargo_sensitive,
    crisis_sensitive,
    anomaly_flag,
    anomaly_reason,
    judge_monitored_status,
    content_excerpt = str_trunc(safe_chr(content), 180)
  ) %>%
  arrange(stage_order, timestamp, message_id)

app_metrics <- tibble(
  total_rounds = n_distinct(rounds_features$round_id),
  total_messages = nrow(comms_features),
  total_agents = n_distinct(participants$agent_id, na.rm = TRUE),
  total_public_posts = sum(comms_features$public_channel, na.rm = TRUE),
  total_anomalies = sum(comms_features$anomaly_flag, na.rm = TRUE),
  breach_window_start = breach_window_start,
  embargo_deadline = embargo_deadline
)

saveRDS(comms_features, file.path(processed_dir, "comms_features.rds"))
saveRDS(rounds_features, file.path(processed_dir, "rounds_features.rds"))
saveRDS(network_edges, file.path(processed_dir, "network_edges.rds"))
saveRDS(network_nodes, file.path(processed_dir, "network_nodes.rds"))
saveRDS(key_events, file.path(processed_dir, "key_events.rds"))
saveRDS(causal_chain_nodes, file.path(processed_dir, "causal_chain_nodes.rds"))
saveRDS(causal_chain_edges, file.path(processed_dir, "causal_chain_edges.rds"))
saveRDS(breach_pathway_nodes, file.path(processed_dir, "breach_pathway_nodes.rds"))
saveRDS(breach_pathway_edges, file.path(processed_dir, "breach_pathway_edges.rds"))
saveRDS(pathway_evidence, file.path(processed_dir, "pathway_evidence.rds"))
saveRDS(app_metrics, file.path(processed_dir, "app_metrics.rds"))

cat("\nFeature engineering complete\n")
cat("Anomaly messages: ", sum(comms_features$anomaly_flag, na.rm = TRUE), "\n", sep = "")
cat("Embargo-sensitive messages: ", sum(comms_features$embargo_sensitive, na.rm = TRUE), "\n", sep = "")
cat("Crisis-sensitive messages: ", sum(comms_features$crisis_sensitive, na.rm = TRUE), "\n", sep = "")
cat("Network edges: ", nrow(network_edges), "\n", sep = "")
cat("Key events: ", nrow(key_events), "\n", sep = "")
cat("Pathway evidence records: ", nrow(pathway_evidence), "\n", sep = "")
