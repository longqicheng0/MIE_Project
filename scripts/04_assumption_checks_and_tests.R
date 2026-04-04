#!/usr/bin/env Rscript

# Run participant-level descriptives, assumption checks, inferential tests, and effect sizes.

required_packages <- c("dplyr", "tidyr", "stringr", "tibble", "readr", "janitor", "broom", "effectsize")

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
  library(readr)
  library(janitor)
  library(broom)
  library(effectsize)
})

summary_file <- file.path("data", "processed", "participant_summary.csv")
tables_dir <- file.path("outputs", "tables")
logs_dir <- file.path("outputs", "logs")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(summary_file)) {
  stop(
    "Missing participant summary file at ",
    summary_file,
    ". Run scripts/02_participant_summary.R first.",
    call. = FALSE
  )
}

message("Reading participant summary: ", summary_file)

participant_summary <- read_csv(
  summary_file,
  col_types = cols(.default = col_guess()),
  na = c("", "NA", "NaN"),
  progress = FALSE,
  show_col_types = FALSE,
  trim_ws = TRUE
) %>%
  clean_names() %>%
  mutate(
    participant_id = str_squish(as.character(participant_id)),
    condition = factor(str_to_lower(str_squish(as.character(condition))), levels = c("auditory", "visual"))
  )

rt_data <- participant_summary %>%
  filter(!is.na(condition), !is.na(mean_rt_ms))

hit_rate_data <- participant_summary %>%
  filter(!is.na(condition), !is.na(hit_rate_pct))

correlation_data <- participant_summary %>%
  filter(!is.na(condition), !is.na(mean_rt_ms), !is.na(hit_rate_pct))

if (n_distinct(rt_data$condition) < 2) {
  stop("At least two conditions are required for the reaction-time analyses.", call. = FALSE)
}

if (n_distinct(hit_rate_data$condition) < 2) {
  stop("At least two conditions are required for the hit-rate analyses.", call. = FALSE)
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) NA_real_ else sd(x)
}

safe_median <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else median(x)
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

descriptives_rt <- rt_data %>%
  group_by(condition) %>%
  summarise(
    n = sum(!is.na(mean_rt_ms)),
    mean = safe_mean(mean_rt_ms),
    sd = safe_sd(mean_rt_ms),
    median = safe_median(mean_rt_ms),
    min = if_else(n > 0, min(mean_rt_ms, na.rm = TRUE), NA_real_),
    max = if_else(n > 0, max(mean_rt_ms, na.rm = TRUE), NA_real_),
    ci = list(mean_ci(mean_rt_ms)),
    .groups = "drop"
  ) %>%
  unnest_wider(ci) %>%
  mutate(metric = "participant_mean_rt_ms", .before = 1)

descriptives_hit_rate <- hit_rate_data %>%
  group_by(condition) %>%
  summarise(
    n = sum(!is.na(hit_rate_pct)),
    mean = safe_mean(hit_rate_pct),
    sd = safe_sd(hit_rate_pct),
    median = safe_median(hit_rate_pct),
    min = if_else(n > 0, min(hit_rate_pct, na.rm = TRUE), NA_real_),
    max = if_else(n > 0, max(hit_rate_pct, na.rm = TRUE), NA_real_),
    ci = list(mean_ci(hit_rate_pct)),
    .groups = "drop"
  ) %>%
  unnest_wider(ci) %>%
  mutate(metric = "participant_hit_rate_pct", .before = 1)

descriptives_by_condition <- bind_rows(descriptives_rt, descriptives_hit_rate) %>%
  arrange(metric, condition)

shapiro_rt <- rt_data %>%
  group_by(condition) %>%
  summarise(
    n = sum(!is.na(mean_rt_ms)),
    shapiro = list({
      x <- mean_rt_ms
      x <- x[!is.na(x)]
      if (length(x) < 3 || length(unique(x)) < 3 || length(x) > 5000) {
        tibble(statistic = NA_real_, p_value = NA_real_)
      } else {
        test <- shapiro.test(x)
        tibble(statistic = unname(test$statistic), p_value = unname(test$p.value))
      }
    }),
    .groups = "drop"
  ) %>%
  unnest_wider(shapiro) %>%
  mutate(metric = "participant_mean_rt_ms", .before = 1)

shapiro_hit_rate <- hit_rate_data %>%
  group_by(condition) %>%
  summarise(
    n = sum(!is.na(hit_rate_pct)),
    shapiro = list({
      x <- hit_rate_pct
      x <- x[!is.na(x)]
      if (length(x) < 3 || length(unique(x)) < 3 || length(x) > 5000) {
        tibble(statistic = NA_real_, p_value = NA_real_)
      } else {
        test <- shapiro.test(x)
        tibble(statistic = unname(test$statistic), p_value = unname(test$p.value))
      }
    }),
    .groups = "drop"
  ) %>%
  unnest_wider(shapiro) %>%
  mutate(metric = "participant_hit_rate_pct", .before = 1)

run_levene <- function(data, value_col, metric_name) {
  centered <- data %>%
    group_by(condition) %>%
    mutate(group_median = median(.data[[value_col]], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(abs_dev = abs(.data[[value_col]] - group_median))

  fit <- stats::aov(abs_dev ~ condition, data = centered)
  tab <- summary(fit)[[1]]

  tibble(
    metric = metric_name,
    term = rownames(tab),
    Df = as.numeric(tab[, "Df"]),
    `F value` = as.numeric(tab[, "F value"]),
    `Pr(>F)` = as.numeric(tab[, "Pr(>F)"])
  )
}

rt_levene <- run_levene(rt_data, "mean_rt_ms", "participant_mean_rt_ms")
hit_levene <- run_levene(hit_rate_data, "hit_rate_pct", "participant_hit_rate_pct")

rt_t_test <- broom::tidy(t.test(mean_rt_ms ~ condition, data = rt_data)) %>%
  mutate(metric = "participant_mean_rt_ms", .before = 1)

hit_t_test <- broom::tidy(t.test(hit_rate_pct ~ condition, data = hit_rate_data)) %>%
  mutate(metric = "participant_hit_rate_pct", .before = 1)

rt_wilcox <- broom::tidy(wilcox.test(mean_rt_ms ~ condition, data = rt_data, exact = FALSE, conf.int = TRUE)) %>%
  mutate(metric = "participant_mean_rt_ms", .before = 1)

hit_wilcox <- broom::tidy(wilcox.test(hit_rate_pct ~ condition, data = hit_rate_data, exact = FALSE, conf.int = TRUE)) %>%
  mutate(metric = "participant_hit_rate_pct", .before = 1)

rt_d <- effectsize::cohens_d(mean_rt_ms ~ condition, data = rt_data, pooled_sd = TRUE, ci = 0.95) %>%
  as_tibble() %>%
  mutate(metric = "participant_mean_rt_ms", .before = 1)

hit_d <- effectsize::cohens_d(hit_rate_pct ~ condition, data = hit_rate_data, pooled_sd = TRUE, ci = 0.95) %>%
  as_tibble() %>%
  mutate(metric = "participant_hit_rate_pct", .before = 1)

pearson_rt_hit <- broom::tidy(cor.test(correlation_data$mean_rt_ms, correlation_data$hit_rate_pct, method = "pearson")) %>%
  mutate(metric = "mean_rt_ms_vs_hit_rate_pct", .before = 1)

descriptive_output <- file.path(tables_dir, "condition_descriptives.csv")
shapiro_output <- file.path(tables_dir, "shapiro_wilk_tests.csv")
levene_output <- file.path(tables_dir, "levene_tests.csv")
t_test_output <- file.path(tables_dir, "independent_samples_t_tests.csv")
wilcox_output <- file.path(tables_dir, "wilcoxon_rank_sum_tests.csv")
effect_size_output <- file.path(tables_dir, "cohens_d_effect_sizes.csv")
correlation_output <- file.path(tables_dir, "pearson_correlation.csv")

write_csv(descriptives_by_condition, descriptive_output)
write_csv(bind_rows(shapiro_rt, shapiro_hit_rate), shapiro_output)
write_csv(bind_rows(rt_levene, hit_levene), levene_output)
write_csv(bind_rows(rt_t_test, hit_t_test), t_test_output)
write_csv(bind_rows(rt_wilcox, hit_wilcox), wilcox_output)
write_csv(bind_rows(rt_d, hit_d), effect_size_output)
write_csv(pearson_rt_hit, correlation_output)

format_stat <- function(value, digits = 3) {
  ifelse(is.na(value), "NA", formatC(value, digits = digits, format = "f"))
}

rt_desc <- descriptives_by_condition %>% filter(metric == "participant_mean_rt_ms")
hit_desc <- descriptives_by_condition %>% filter(metric == "participant_hit_rate_pct")

summary_lines <- c(
  "Rapid Target Clicking Experiment: Main Results",
  "",
  "Participant-level descriptives",
  paste0(
    "RT - ", rt_desc$condition, ": n=", rt_desc$n,
    ", mean=", format_stat(rt_desc$mean),
    ", sd=", format_stat(rt_desc$sd),
    ", median=", format_stat(rt_desc$median),
    ", min=", format_stat(rt_desc$min),
    ", max=", format_stat(rt_desc$max),
    ", 95% CI=[", format_stat(rt_desc$ci_low), ", ", format_stat(rt_desc$ci_high), "]"
  ),
  paste0(
    "Hit rate - ", hit_desc$condition, ": n=", hit_desc$n,
    ", mean=", format_stat(hit_desc$mean),
    ", sd=", format_stat(hit_desc$sd),
    ", median=", format_stat(hit_desc$median),
    ", min=", format_stat(hit_desc$min),
    ", max=", format_stat(hit_desc$max),
    ", 95% CI=[", format_stat(hit_desc$ci_low), ", ", format_stat(hit_desc$ci_high), "]"
  ),
  "",
  "Shapiro-Wilk tests",
  paste0(
    "RT - ", shapiro_rt$condition,
    ": W=", format_stat(shapiro_rt$statistic),
    ", p=", format_stat(shapiro_rt$p_value)
  ),
  paste0(
    "Hit rate - ", shapiro_hit_rate$condition,
    ": W=", format_stat(shapiro_hit_rate$statistic),
    ", p=", format_stat(shapiro_hit_rate$p_value)
  ),
  "",
  "Levene tests",
  paste0(
    "RT: F=", format_stat(rt_levene$`F value`[1]),
    ", df1=", rt_levene$Df[1],
    ", df2=", rt_levene$Df[2],
    ", p=", format_stat(rt_levene$`Pr(>F)`[1])
  ),
  paste0(
    "Hit rate: F=", format_stat(hit_levene$`F value`[1]),
    ", df1=", hit_levene$Df[1],
    ", df2=", hit_levene$Df[2],
    ", p=", format_stat(hit_levene$`Pr(>F)`[1])
  ),
  "",
  "Independent-samples t-tests",
  paste0(
    "RT: t=", format_stat(rt_t_test$statistic),
    ", df=", format_stat(rt_t_test$parameter),
    ", p=", format_stat(rt_t_test$p.value),
    ", mean difference=", format_stat(rt_t_test$estimate1 - rt_t_test$estimate2)
  ),
  paste0(
    "Hit rate: t=", format_stat(hit_t_test$statistic),
    ", df=", format_stat(hit_t_test$parameter),
    ", p=", format_stat(hit_t_test$p.value),
    ", mean difference=", format_stat(hit_t_test$estimate1 - hit_t_test$estimate2)
  ),
  "",
  "Wilcoxon rank-sum tests",
  paste0(
    "RT: W=", format_stat(rt_wilcox$statistic),
    ", p=", format_stat(rt_wilcox$p.value)
  ),
  paste0(
    "Hit rate: W=", format_stat(hit_wilcox$statistic),
    ", p=", format_stat(hit_wilcox$p.value)
  ),
  "",
  "Effect sizes",
  paste0(
    "RT Cohen's d=", format_stat(rt_d$Cohens_d),
    ", 95% CI=[", format_stat(rt_d$CI_low), ", ", format_stat(rt_d$CI_high), "]"
  ),
  paste0(
    "Hit rate Cohen's d=", format_stat(hit_d$Cohens_d),
    ", 95% CI=[", format_stat(hit_d$CI_low), ", ", format_stat(hit_d$CI_high), "]"
  ),
  "",
  "Pearson correlation",
  paste0(
    "RT vs hit rate: r=", format_stat(pearson_rt_hit$estimate),
    ", p=", format_stat(pearson_rt_hit$p.value),
    ", 95% CI=[", format_stat(pearson_rt_hit$conf.low), ", ", format_stat(pearson_rt_hit$conf.high), "]"
  ),
  "",
  paste0("Participant rows analyzed for correlation: ", nrow(correlation_data))
)

writeLines(summary_lines, file.path(tables_dir, "main_results_summary.txt"))

assumption_lines <- c(
  "Assumption Check Summary",
  paste0("Shapiro RT p-values: ", paste(format_stat(shapiro_rt$p_value), collapse = ", ")),
  paste0("Shapiro hit rate p-values: ", paste(format_stat(shapiro_hit_rate$p_value), collapse = ", ")),
  paste0("Levene RT p-value: ", format_stat(rt_levene$`Pr(>F)`[1])),
  paste0("Levene hit rate p-value: ", format_stat(hit_levene$`Pr(>F)`[1]))
)

writeLines(assumption_lines, file.path(tables_dir, "assumption_checks_summary.txt"))

message("Wrote statistical tables to outputs/tables/")
message("Wrote console-friendly summary files to outputs/tables/")
