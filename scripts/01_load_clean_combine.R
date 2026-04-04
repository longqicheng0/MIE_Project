#!/usr/bin/env Rscript

# Load, clean, validate, and combine raw trial-level CSV files from data/.

required_packages <- c("dplyr", "tidyr", "purrr", "stringr", "tibble", "janitor", "readr")

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
  library(purrr)
  library(stringr)
  library(tibble)
  library(janitor)
  library(readr)
})

data_dir <- "data"
processed_dir <- file.path(data_dir, "processed")
logs_dir <- file.path("outputs", "logs")
tables_dir <- file.path("outputs", "tables")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

expected_columns <- c(
  "participant_id",
  "condition",
  "trial_number",
  "trial_type",
  "target_x",
  "target_y",
  "target_radius",
  "target_onset_timestamp_ms",
  "click_timestamp_ms",
  "reaction_time_ms",
  "session_elapsed_ms",
  "hit",
  "cumulative_score"
)

csv_files <- list.files(
  path = data_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

if (length(csv_files) == 0) {
  stop("No CSV files were found in data/.", call. = FALSE)
}

message("Found ", length(csv_files), " raw CSV file(s) in data/.")

parse_binary <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  case_when(
    x_chr %in% c("1", "true", "t", "yes", "y") ~ 1L,
    x_chr %in% c("0", "false", "f", "no", "n") ~ 0L,
    TRUE ~ NA_integer_
  )
}

safe_read_csv <- function(file_path) {
  file_name <- basename(file_path)

  parsed <- tryCatch(
    read_csv(
      file_path,
      col_types = cols(
        participant_id = col_character(),
        condition = col_character(),
        trial_number = col_integer(),
        trial_type = col_character(),
        target_x = col_double(),
        target_y = col_double(),
        target_radius = col_double(),
        target_onset_timestamp_ms = col_double(),
        click_timestamp_ms = col_double(),
        reaction_time_ms = col_double(),
        session_elapsed_ms = col_double(),
        hit = col_character(),
        cumulative_score = col_integer(),
        .default = col_guess()
      ),
      na = c("", "NA", "NaN"),
      progress = FALSE,
      show_col_types = FALSE,
      trim_ws = TRUE
    ),
    error = function(e) {
      return(list(
        data = NULL,
        read_problems = tibble(
          source_file = file_name,
          row = NA_integer_,
          col = NA_character_,
          expected = NA_character_,
          actual = NA_character_,
          issue = e$message
        ),
        file_issues = tibble(source_file = file_name, issue = e$message)
      ))
    }
  )

  if (
    is.list(parsed) &&
      all(c("data", "read_problems", "file_issues") %in% names(parsed)) &&
      is.null(parsed$data)
  ) {
    return(parsed)
  }

  data <- clean_names(parsed)

  missing_cols <- setdiff(expected_columns, names(data))
  parse_problems <- problems(parsed) %>%
    mutate(source_file = file_name, .before = 1)

  if (length(missing_cols) > 0) {
    return(list(
      data = NULL,
      read_problems = parse_problems,
      file_issues = tibble(
        source_file = file_name,
        issue = paste("Missing expected column(s):", paste(missing_cols, collapse = ", "))
      )
    ))
  }

  cleaned <- data %>%
    select(all_of(expected_columns)) %>%
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
      hit = parse_binary(hit),
      cumulative_score = as.integer(cumulative_score),
      source_file = file_name
    )

  list(
    data = cleaned,
    read_problems = parse_problems,
    file_issues = tibble(source_file = file_name, issue = NA_character_)
  )
}

file_results <- map(csv_files, safe_read_csv)

raw_trials <- file_results %>%
  map("data") %>%
  compact() %>%
  bind_rows()

read_problem_log <- file_results %>%
  map("read_problems") %>%
  compact() %>%
  bind_rows()

if (nrow(read_problem_log) == 0) {
  read_problem_log <- tibble(
    source_file = character(),
    row = integer(),
    col = character(),
    expected = character(),
    actual = character(),
    issue = character()
  )
}

file_issue_log <- file_results %>%
  map("file_issues") %>%
  compact() %>%
  bind_rows()

if (nrow(file_issue_log) == 0 || !"issue" %in% names(file_issue_log)) {
  file_issue_log <- tibble(source_file = character(), issue = character())
} else {
  file_issue_log <- file_issue_log %>%
    filter(!is.na(issue))
}

if (nrow(raw_trials) == 0) {
  stop("None of the CSV files could be read successfully.", call. = FALSE)
}

message("Read ", nrow(raw_trials), " total row(s) across all source files.")

missing_values_log <- raw_trials %>%
  summarise(across(all_of(expected_columns), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  mutate(pct_missing = n_missing / nrow(raw_trials) * 100) %>%
  arrange(desc(n_missing), column)

rows_with_missing_values <- raw_trials %>%
  filter(if_any(all_of(expected_columns), is.na)) %>%
  arrange(participant_id, condition, trial_number)

duplicate_key_rows <- raw_trials %>%
  group_by(participant_id, condition, trial_number) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(participant_id, condition, trial_number, source_file)

impossible_rt_rows <- raw_trials %>%
  filter(!is.na(reaction_time_ms) & reaction_time_ms <= 0) %>%
  arrange(participant_id, condition, trial_number)

suspicious_rt_rows <- raw_trials %>%
  filter(!is.na(reaction_time_ms) & reaction_time_ms > 5000) %>%
  arrange(participant_id, condition, trial_number)

qc_summary <- tibble(
  total_rows_read = nrow(raw_trials),
  source_files = length(csv_files),
  rows_with_any_missing = nrow(rows_with_missing_values),
  duplicate_participant_condition_trial_rows = nrow(duplicate_key_rows),
  impossible_rt_rows = nrow(impossible_rt_rows),
  suspicious_rt_rows_over_5000_ms = nrow(suspicious_rt_rows)
)

experimental_trials_clean <- raw_trials %>%
  filter(trial_type == "experimental") %>%
  filter(reaction_time_ms > 0 | is.na(reaction_time_ms)) %>%
  filter(if_all(all_of(expected_columns), ~ !is.na(.x))) %>%
  arrange(participant_id, condition, trial_number, source_file) %>%
  distinct(participant_id, condition, trial_number, .keep_all = TRUE) %>%
  select(all_of(expected_columns))

output_file <- file.path(processed_dir, "experimental_trials_clean.csv")
write_csv(experimental_trials_clean, output_file)

write_csv(missing_values_log, file.path(logs_dir, "missing_values_by_column.csv"))
write_csv(rows_with_missing_values, file.path(logs_dir, "rows_with_missing_values.csv"))
write_csv(duplicate_key_rows, file.path(logs_dir, "duplicate_participant_condition_trial_rows.csv"))
write_csv(impossible_rt_rows, file.path(logs_dir, "impossible_rt_rows.csv"))
write_csv(suspicious_rt_rows, file.path(logs_dir, "suspicious_rt_rows_over_5000_ms.csv"))
write_csv(read_problem_log, file.path(logs_dir, "read_csv_parse_problems.csv"))
write_csv(file_issue_log, file.path(logs_dir, "file_read_issues.csv"))
write_csv(qc_summary, file.path(logs_dir, "quality_check_summary.csv"))

summary_lines <- c(
  "Raw trial cleaning summary",
  paste0("Source files read: ", length(csv_files)),
  paste0("Combined raw rows: ", nrow(raw_trials)),
  paste0("Rows with any missing value: ", nrow(rows_with_missing_values)),
  paste0("Duplicate participant-condition-trial rows: ", nrow(duplicate_key_rows)),
  paste0("Impossible RT rows (<= 0 ms): ", nrow(impossible_rt_rows)),
  paste0("Suspicious RT rows (> 5000 ms): ", nrow(suspicious_rt_rows)),
  paste0("Clean experimental rows written: ", nrow(experimental_trials_clean)),
  paste0("Output file: ", output_file)
)

writeLines(summary_lines, file.path(logs_dir, "quality_check_summary.txt"))

message("Wrote cleaned experimental trial-level data to ", output_file)
message("Quality-check logs saved to outputs/logs/")
