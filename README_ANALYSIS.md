# Analysis Workflow

This repository includes a participant-level analysis pipeline for the rapid target clicking experiment. The raw session files live in `data/`, while processed analysis files and outputs are written to dedicated subfolders.

## Script Order

Run the scripts in this order from the repository root:

1. `scripts/01_load_clean_combine.R`
2. `scripts/02_participant_summary.R`
3. `scripts/03_descriptives_plots.R`
4. `scripts/04_assumption_checks_and_tests.R`

## What Each Script Does

`scripts/01_load_clean_combine.R`
- Reads all raw CSV session files directly from `data/`.
- Ignores non-CSV files safely.
- Standardizes column types and column names.
- Checks for missing values, duplicate participant-condition-trial rows, impossible RT values, and suspiciously large RT values.
- Filters to experimental trials only and removes invalid rows with missing required fields or non-positive reaction times.
- Writes the cleaned trial-level file to `data/processed/experimental_trials_clean.csv`.
- Writes quality-check logs to `outputs/logs/`.

`scripts/02_participant_summary.R`
- Reads the cleaned experimental trial-level file.
- Creates participant-level outcomes for the between-subjects analysis.
- Computes mean RT from successful experimental trials only (`hit == 1`).
- Computes hit rate as the percent of experimental trials with `hit == 1`.
- Also computes median RT, SD of RT, number of hits, number of misses, number of experimental trials, and number of successful RT observations.
- Writes `data/processed/participant_summary.csv` and `data/processed/condition_summary.csv`.

`scripts/03_descriptives_plots.R`
- Reads `data/processed/participant_summary.csv`.
- Generates all requested participant-level plots.
- Writes figures to `outputs/figures/`.

`scripts/04_assumption_checks_and_tests.R`
- Reads `data/processed/participant_summary.csv`.
- Runs descriptives, Shapiro-Wilk tests, Levene’s tests, independent-samples t-tests, Wilcoxon rank-sum tests, Pearson correlation, and Cohen’s d effect sizes.
- Writes statistical tables and summary text files to `outputs/tables/`.

## Files Produced

### Cleaned Data

- `data/processed/experimental_trials_clean.csv`
- `data/processed/participant_summary.csv`
- `data/processed/condition_summary.csv`

### Quality Checks

- `outputs/logs/missing_values_by_column.csv`
- `outputs/logs/rows_with_missing_values.csv`
- `outputs/logs/duplicate_participant_condition_trial_rows.csv`
- `outputs/logs/impossible_rt_rows.csv`
- `outputs/logs/suspicious_rt_rows_over_5000_ms.csv`
- `outputs/logs/read_csv_parse_problems.csv`
- `outputs/logs/file_read_issues.csv`
- `outputs/logs/quality_check_summary.csv`
- `outputs/logs/quality_check_summary.txt`

### Figures

- `outputs/figures/participant_mean_rt_boxplot_by_condition.png`
- `outputs/figures/participant_hit_rate_boxplot_by_condition.png`
- `outputs/figures/participant_mean_rt_histogram_by_condition.png`
- `outputs/figures/participant_hit_rate_histogram_by_condition.png`
- `outputs/figures/participant_mean_rt_vs_hit_rate_scatter.png`
- `outputs/figures/participant_mean_rt_qq_plots_by_condition.png`
- `outputs/figures/participant_hit_rate_qq_plots_by_condition.png`

### Statistical Tables and Summaries

- `outputs/tables/condition_descriptives.csv`
- `outputs/tables/shapiro_wilk_tests.csv`
- `outputs/tables/levene_tests.csv`
- `outputs/tables/independent_samples_t_tests.csv`
- `outputs/tables/wilcoxon_rank_sum_tests.csv`
- `outputs/tables/cohens_d_effect_sizes.csv`
- `outputs/tables/pearson_correlation.csv`
- `outputs/tables/main_results_summary.txt`
- `outputs/tables/assumption_checks_summary.txt`

## How The Participant-Level Dataset Maps To The Report Outcomes

The final inferential tests are run on `data/processed/participant_summary.csv`, not on trial rows.

- `mean_rt_ms`: participant mean reaction time in milliseconds, calculated from successful experimental trials only (`hit == 1`).
- `hit_rate_pct`: participant hit rate in percent, calculated as `100 * n_hits / n_experimental_trials`.

These are the two primary dependent variables for the report and are the only variables used in the between-subjects hypothesis tests.
