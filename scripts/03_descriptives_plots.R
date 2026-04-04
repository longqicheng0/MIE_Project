#!/usr/bin/env Rscript

# Generate participant-level descriptive plots by condition.

required_packages <- c("dplyr", "stringr", "readr", "ggplot2", "janitor")

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
  library(stringr)
  library(readr)
  library(ggplot2)
  library(janitor)
})

summary_file <- file.path("data", "processed", "participant_summary.csv")
figures_dir <- file.path("outputs", "figures")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

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

theme_experiment <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

condition_palette <- c(auditory = "#2A6F97", visual = "#C05640")

save_plot <- function(plot_object, filename, width = 8, height = 5.5, dpi = 300) {
  ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

mean_rt_box <- ggplot(participant_summary, aes(x = condition, y = mean_rt_ms, fill = condition)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.alpha = 0.7) +
  geom_jitter(width = 0.08, alpha = 0.55, size = 1.8, color = "grey25") +
  scale_fill_manual(values = condition_palette, drop = FALSE) +
  labs(
    title = "Participant Mean Reaction Time by Condition",
    x = "Condition",
    y = "Mean reaction time (ms)"
  ) +
  theme_experiment

hit_rate_box <- ggplot(participant_summary, aes(x = condition, y = hit_rate_pct, fill = condition)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.alpha = 0.7) +
  geom_jitter(width = 0.08, alpha = 0.55, size = 1.8, color = "grey25") +
  scale_fill_manual(values = condition_palette, drop = FALSE) +
  labs(
    title = "Participant Hit Rate by Condition",
    x = "Condition",
    y = "Hit rate (%)"
  ) +
  theme_experiment

mean_rt_hist <- ggplot(participant_summary, aes(x = mean_rt_ms, fill = condition)) +
  geom_histogram(bins = 12, color = "white", alpha = 0.9) +
  facet_wrap(~ condition, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = condition_palette, drop = FALSE) +
  labs(
    title = "Distribution of Participant Mean Reaction Time",
    x = "Mean reaction time (ms)",
    y = "Count"
  ) +
  theme_experiment +
  theme(legend.position = "none")

hit_rate_hist <- ggplot(participant_summary, aes(x = hit_rate_pct, fill = condition)) +
  geom_histogram(bins = 12, color = "white", alpha = 0.9) +
  facet_wrap(~ condition, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = condition_palette, drop = FALSE) +
  labs(
    title = "Distribution of Participant Hit Rate",
    x = "Hit rate (%)",
    y = "Count"
  ) +
  theme_experiment +
  theme(legend.position = "none")

rt_vs_hit_scatter <- ggplot(participant_summary, aes(x = mean_rt_ms, y = hit_rate_pct, color = condition)) +
  geom_point(size = 2.6, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, fullrange = FALSE) +
  scale_color_manual(values = condition_palette, drop = FALSE) +
  labs(
    title = "Participant Mean Reaction Time vs Hit Rate",
    x = "Mean reaction time (ms)",
    y = "Hit rate (%)"
  ) +
  theme_experiment

qq_rt <- ggplot(participant_summary, aes(sample = mean_rt_ms, color = condition)) +
  stat_qq(alpha = 0.8, size = 1.7) +
  stat_qq_line(linewidth = 0.8) +
  facet_wrap(~ condition, ncol = 1, scales = "free") +
  scale_color_manual(values = condition_palette, drop = FALSE) +
  labs(
    title = "Q-Q Plots for Participant Mean Reaction Time",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_experiment

qq_hit_rate <- ggplot(participant_summary, aes(sample = hit_rate_pct, color = condition)) +
  stat_qq(alpha = 0.8, size = 1.7) +
  stat_qq_line(linewidth = 0.8) +
  facet_wrap(~ condition, ncol = 1, scales = "free") +
  scale_color_manual(values = condition_palette, drop = FALSE) +
  labs(
    title = "Q-Q Plots for Participant Hit Rate",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_experiment

save_plot(mean_rt_box, "participant_mean_rt_boxplot_by_condition.png", width = 7.5, height = 5.5)
save_plot(hit_rate_box, "participant_hit_rate_boxplot_by_condition.png", width = 7.5, height = 5.5)
save_plot(mean_rt_hist, "participant_mean_rt_histogram_by_condition.png", width = 8, height = 6.5)
save_plot(hit_rate_hist, "participant_hit_rate_histogram_by_condition.png", width = 8, height = 6.5)
save_plot(rt_vs_hit_scatter, "participant_mean_rt_vs_hit_rate_scatter.png", width = 7.5, height = 5.5)
save_plot(qq_rt, "participant_mean_rt_qq_plots_by_condition.png", width = 8, height = 6.5)
save_plot(qq_hit_rate, "participant_hit_rate_qq_plots_by_condition.png", width = 8, height = 6.5)

message("Saved descriptive plots to outputs/figures/")
