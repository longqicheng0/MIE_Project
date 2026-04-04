#!/usr/bin/env Rscript

# Build participant-level and condition-level summaries from cleaned experimental trials.

required_packages <- c("dplyr", "tidyr", "stringr", "tibble", "janitor", "readr")

check_packages <- function(packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Missing required R packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install them before running this script.",
      call. = FALSE
    )
  }
}

check_packages(required_packages)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(janitor)
  library(readr)
})

data_file <- file.path("data", "processed", "experimental_trials_clean.csv")
processed_dir <- file.path("data", "processed")
logs_dir <- file.path("outputs", "logs")
tables_dir <- file.path("outputs", "tables")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_file)) {
  stop(
    "Missing cleaned trial file at ",
    data_file,
    ". Run scripts/01_load_clean_combine.R first.",
    call. = FALSE
  )
}

message("Reading cleaned experimental trial file: ", data_file)

experimental_trials <- read_csv(
  data_file,
  col_types = cols(.default = col_guess()),
  na = c("", "NA", "NaN"),
  progress = FALSE,
  show_col_types = FALSE,
  trim_ws = TRUE
) %>%
  clean_names() %>%
  mutate(
    participant_id = str_squish(as.character(participant_id)),
    condition = str_to_lower(str_squish(as.character(condition))),
    trial_number = as.integer(trial_number),
    trial_type = str_to_lower(str_squish(as.character(trial_type))),
    target_x = as.numeric(target_x),
    target_y = as.numeric(target_y),
    target_radius = as.numeric(target_radius),
    target_onset_timestamp_ms = as.numeric(target_onset_timestamp_ms),
    click_timestamp_ms = as.numeric(click_timestamp_ms),
    reaction_time_ms = as.numeric(reaction_time_ms),
    session_elapsed_ms = as.numeric(session_elapsed_ms),
    hit = as.integer(hit),
    cumulative_score = as.integer(cumulative_score)
  )

condition_levels <- c("auditory", "visual")

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

safe_median <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else median(x)
}

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) NA_real_ else sd(x)
}

mean_ci <- function(x, conf_level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) {
    return(tibble(ci_low = NA_real_, ci_high = NA_real_))
  }
  m <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  alpha <- 1 - conf_level
  critical <- stats::qt(1 - alpha / 2, df = n - 1)
  tibble(
    ci_low = m - critical * se,
    ci_high = m + critical * se
  )
}

participant_summary <- experimental_trials %>%
  filter(trial_type == "experimental") %>%
  mutate(condition = factor(condition, levels = condition_levels)) %>%
  group_by(participant_id, condition) %>%
  summarise(
    n_experimental_trials = n(),
    n_hits = sum(hit == 1L, na.rm = TRUE),
    n_misses = sum(hit == 0L, na.rm = TRUE),
    n_successful_trials = sum(hit == 1L, na.rm = TRUE),
    n_rt_observations = sum(hit == 1L & !is.na(reaction_time_ms)),
    hit_rate_pct = 100 * n_hits / n_experimental_trials,
    mean_rt_ms = safe_mean(reaction_time_ms[hit == 1L]),
    median_rt_ms = safe_median(reaction_time_ms[hit == 1L]),
    sd_rt_ms = safe_sd(reaction_time_ms[hit == 1L]),
    .groups = "drop"
  ) %>%
  mutate(condition = as.character(condition)) %>%
  arrange(condition, participant_id)

condition_summary <- participant_summary %>%
  pivot_longer(
    cols = c(mean_rt_ms, hit_rate_pct),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      mean_rt_ms = "participant_mean_rt_ms",
      hit_rate_pct = "participant_hit_rate_pct"
    )
  ) %>%
  group_by(condition, metric) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = safe_mean(value),
    sd = safe_sd(value),
    median = safe_median(value),
    min = if_else(n > 0, min(value, na.rm = TRUE), NA_real_),
    max = if_else(n > 0, max(value, na.rm = TRUE), NA_real_),
    ci = list(mean_ci(value)),
    .groups = "drop"
  ) %>%
  unnest_wider(ci) %>%
  arrange(condition, metric)

participant_output <- file.path(processed_dir, "participant_summary.csv")
condition_output <- file.path(processed_dir, "condition_summary.csv")

write_csv(participant_summary, participant_output)
write_csv(condition_summary, condition_output)

quality_notes <- c(
  "Participant summary creation complete.",
  paste0("Experimental rows read: ", nrow(experimental_trials)),
  paste0("Participant-condition rows written: ", nrow(participant_summary)),
  paste0("Condition-metric summary rows written: ", nrow(condition_summary)),
  paste0("Participant summary file: ", participant_output),
  paste0("Condition summary file: ", condition_output)
)

writeLines(quality_notes, file.path(logs_dir, "participant_summary_summary.txt"))

message("Wrote participant-level summary to ", participant_output)
message("Wrote condition-level summary to ", condition_output)
