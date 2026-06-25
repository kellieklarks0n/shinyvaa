# Data preparation for VAST Challenge 2026 MC1.

suppressPackageStartupMessages({
  library(jsonlite)
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(purrr)
})

raw_path <- file.path("data", "raw", "MC1_final_00.json")
processed_dir <- file.path("data", "processed")

if (!file.exists(raw_path)) {
  stop(
    "Raw data file not found: ", raw_path,
    "\nPlace MC1_final_00.json in data/raw/ before running R/01_data_prep.R.",
    call. = FALSE
  )
}

if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

raw_data <- jsonlite::fromJSON(raw_path, flatten = TRUE)

if (is.null(raw_data$rounds)) {
  stop("The JSON file does not contain a top-level 'rounds' array.", call. = FALSE)
}

rounds <- raw_data$rounds

# Helpers keep the script resilient when optional JSON fields are NULL or absent.
null_to_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  x
}

first_value <- function(x) {
  x <- null_to_na(x)

  if (is.data.frame(x)) {
    return(collapse_value(x))
  }

  if (is.list(x) && !is.data.frame(x)) {
    return(collapse_value(x))
  }

  value <- x[[1]]
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    NA_character_
  } else {
    as.character(value)
  }
}

collapse_value <- function(x, sep = " | ") {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  if (is.data.frame(x)) {
    if (nrow(x) == 0) {
      return(NA_character_)
    }

    rows <- purrr::pmap_chr(x, function(...) {
      vals <- list(...)
      vals <- vals[!purrr::map_lgl(vals, ~ is.null(.x) || length(.x) == 0)]

      if (length(vals) == 0) {
        return(NA_character_)
      }

      vals <- purrr::imap_chr(vals, function(value, name) {
        value <- collapse_value(value, sep = ", ")
        if (is.na(value) || value == "") {
          return(NA_character_)
        }
        paste0(name, ": ", value)
      })

      paste(stats::na.omit(vals), collapse = "; ")
    })

    rows <- rows[!is.na(rows) & rows != ""]
    if (length(rows) == 0) NA_character_ else paste(rows, collapse = sep)
  } else if (is.list(x)) {
    vals <- purrr::map_chr(x, collapse_value, sep = ", ")
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) == 0) NA_character_ else paste(vals, collapse = sep)
  } else {
    vals <- as.character(x)
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) == 0) NA_character_ else paste(vals, collapse = sep)
  }
}

get_col <- function(df, col, default = NA_character_) {
  if (!is.data.frame(df) || !col %in% names(df)) {
    return(rep(default, nrow(df)))
  }

  df[[col]]
}

get_round_field <- function(round_row, field) {
  if (!field %in% names(round_row)) {
    return(NA_character_)
  }

  collapse_value(round_row[[field]][[1]])
}

clean_agent <- function(agent_id, agent_role, agent_label) {
  lookup_text <- str_to_lower(str_c(agent_id, agent_role, agent_label, sep = " "))

  case_when(
    str_detect(lookup_text, "legal") ~ "Legal",
    str_detect(lookup_text, "quality|platform[_ -]?trust") ~ "Platform Trust",
    str_detect(lookup_text, "social[_ -]?manager") ~ "Social Manager",
    str_detect(lookup_text, "judge|eval") ~ "Judge",
    str_detect(lookup_text, "pr[_ -]?intern") ~ "PR Intern",
    str_detect(lookup_text, "\\bpr\\b|pr[_ -]?agent") ~ "PR",
    str_detect(lookup_text, "intern") ~ "Intern",
    TRUE ~ "Other"
  )
}

clean_channel_group <- function(channel) {
  channel_clean <- str_to_lower(coalesce(as.character(channel), ""))

  case_when(
    str_detect(channel_clean, "huddle|meeting|war_room|war room|legal|board|memo|email") ~ "Formal internal",
    str_detect(channel_clean, "slack|teams|chat|thread|group|comms") ~ "Semi-private",
    str_detect(channel_clean, "dm|direct|private|1:1|one_on_one|signal|text") ~ "Private",
    str_detect(channel_clean, "official|press|statement|newsroom|tenantthread") ~ "Official public",
    str_detect(channel_clean, "personal|linkedin|facebook|instagram|tiktok|x_|twitter") ~ "Personal public",
    str_detect(channel_clean, "anonymous|reddit|blind|forum|leak") ~ "Anonymous public",
    TRUE ~ "Other"
  )
}

parse_timestamp <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c("ymd HMS", "ymd HM", "ymd IMS p", "ymd IM p", "ymd", "mdy HMS", "mdy HM"),
    tz = "UTC",
    quiet = TRUE
  )
}

rounds_context <- purrr::map_dfr(seq_len(nrow(rounds)), function(i) {
  round_row <- rounds[i, , drop = FALSE]

  tibble(
    round_id = i,
    hour = get_round_field(round_row, "hour"),
    event_narrative = get_round_field(round_row, "environment_context.event_narrative"),
    event_headline = get_round_field(round_row, "environment_context.event_headline"),
    stock_price = get_round_field(round_row, "environment_context.market_snapshot.stock_price"),
    percent_change = get_round_field(round_row, "environment_context.market_snapshot.percent_change"),
    sentiment = get_round_field(round_row, "environment_context.market_snapshot.sentiment"),
    social_state = get_round_field(round_row, "environment_context.social_state"),
    media_events = get_round_field(round_row, "environment_context.media_events"),
    external_actor_actions = get_round_field(round_row, "environment_context.external_actor_actions"),
    social_manager_alerts = get_round_field(round_row, "environment_context.social_manager_alerts"),
    critical_deadlines = get_round_field(round_row, "environment_context.critical_deadlines"),
    news = get_round_field(round_row, "environment_context.news")
  )
})

extract_communications <- function(round_row, round_id, round_hour) {
  if ("communications" %in% names(round_row) && length(round_row$communications[[1]]) > 0) {
    comms <- round_row$communications[[1]]
  } else if ("agent_outputs" %in% names(round_row) && length(round_row$agent_outputs[[1]]) > 0) {
    agent_outputs <- round_row$agent_outputs[[1]]

    comms <- purrr::map_dfr(seq_len(nrow(agent_outputs)), function(i) {
      agent_row <- agent_outputs[i, , drop = FALSE]
      agent_comms <- agent_row$communications[[1]]

      if (is.null(agent_comms) || length(agent_comms) == 0 || nrow(agent_comms) == 0) {
        return(tibble())
      }

      agent_comms %>%
        mutate(
          agent_id = first_value(agent_row$agent_id),
          agent_role = first_value(agent_row$agent_role),
          agent_label = first_value(agent_row$agent_label),
          internal_state.reacting = get_round_field(agent_row, "internal_state.reacting"),
          internal_state.rationalizing = get_round_field(agent_row, "internal_state.rationalizing"),
          internal_state.deliberating = get_round_field(agent_row, "internal_state.deliberating")
        )
    })
  } else {
    return(tibble())
  }

  if (is.null(comms) || length(comms) == 0 || nrow(comms) == 0) {
    return(tibble())
  }

  tibble(
    round_id = round_id,
    round_hour = round_hour,
    message_id = get_col(comms, "message_id"),
    agent_id = get_col(comms, "agent_id"),
    agent_role = get_col(comms, "agent_role"),
    agent_label = get_col(comms, "agent_label"),
    channel = get_col(comms, "channel"),
    recipients = purrr::map_chr(get_col(comms, "recipients", vector("list", nrow(comms))), collapse_value),
    message_type = get_col(comms, "message_type"),
    responding_to = get_col(comms, "responding_to"),
    content = coalesce(get_col(comms, "content"), get_col(comms, "message_text")),
    timestamp = get_col(comms, "timestamp"),
    reacting = get_col(comms, "internal_state.reacting"),
    rationalizing = get_col(comms, "internal_state.rationalizing"),
    deliberating = get_col(comms, "internal_state.deliberating")
  )
}

comms_flat <- purrr::map_dfr(seq_len(nrow(rounds)), function(i) {
  round_row <- rounds[i, , drop = FALSE]
  extract_communications(round_row, i, get_round_field(round_row, "hour"))
}) %>%
  mutate(
    timestamp = parse_timestamp(timestamp),
    date = as.Date(timestamp),
    content_lower = str_to_lower(coalesce(as.character(content), "")),
    agent_clean = clean_agent(agent_id, agent_role, agent_label),
    channel_group = clean_channel_group(channel)
  )

extract_participants <- function(round_row, round_id) {
  if ("agent_outputs" %in% names(round_row) && length(round_row$agent_outputs[[1]]) > 0) {
    agent_outputs <- round_row$agent_outputs[[1]]

    return(tibble(
      round_id = round_id,
      agent_id = get_col(agent_outputs, "agent_id"),
      agent_role = get_col(agent_outputs, "agent_role"),
      agent_label = get_col(agent_outputs, "agent_label"),
      declared_action = get_col(agent_outputs, "declared_action")
    ))
  }

  if ("communications" %in% names(round_row) && length(round_row$communications[[1]]) > 0) {
    comms <- round_row$communications[[1]]

    if (is.null(comms) || nrow(comms) == 0) {
      return(tibble())
    }

    return(
      tibble(
        round_id = round_id,
        agent_id = get_col(comms, "agent_id"),
        agent_role = get_col(comms, "agent_role"),
        agent_label = get_col(comms, "agent_label"),
        declared_action = get_col(comms, "declared_action")
      ) %>%
        distinct()
    )
  }

  tibble()
}

participants <- purrr::map_dfr(seq_len(nrow(rounds)), function(i) {
  extract_participants(rounds[i, , drop = FALSE], i)
})

saveRDS(rounds_context, file.path(processed_dir, "rounds_context.rds"))
saveRDS(comms_flat, file.path(processed_dir, "comms_flat.rds"))
saveRDS(participants, file.path(processed_dir, "participants.rds"))

date_range <- range(comms_flat$date, na.rm = TRUE)
date_range_text <- if (all(is.finite(date_range))) {
  paste(date_range, collapse = " to ")
} else {
  "No valid communication dates"
}

cat("\nData preparation complete\n")
cat("Rounds: ", nrow(rounds_context), "\n", sep = "")
cat("Communications: ", nrow(comms_flat), "\n", sep = "")
cat("Participants: ", nrow(participants), "\n", sep = "")
cat("Date range: ", date_range_text, "\n", sep = "")

cat("\nTop channels:\n")
comms_flat %>%
  count(channel, sort = TRUE) %>%
  slice_head(n = 10) %>%
  print(n = 10)

cat("\nTop agents:\n")
comms_flat %>%
  count(agent_clean, agent_id, sort = TRUE) %>%
  slice_head(n = 10) %>%
  print(n = 10)
