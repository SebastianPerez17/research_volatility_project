# Research Volatility Project

## Overview

This repository provides a modular research framework for empirical analysis of financial market volatility. The application focuses on daily S&P 500 returns and compares ARCH-family and score-driven volatility models under alternative distributional assumptions. The workflow includes data preparation, exploratory analysis, in-sample model comparison, distribution selection, and out-of-sample forecast evaluation with Diebold-Mariano tests.

Core features:
1. Downloads and explores daily S&P 500 return data
2. Selects ARCH(q) specifications via residual diagnostics
3. Estimates benchmark models (ARCH, GARCH, EGARCH, TGARCH)
4. Estimates score-driven models (GAS) for comparison
5. Selects optimal distribution (Normal vs Student t) across all models
6. Generates rolling 1-step-ahead forecasts and evaluates performance

## Quick Start

1. Edit `scripts/00_config.R` to set parameters (data range, window size, model selection criteria)
2. Run the pipeline:
   ```bash
   Rscript run_all.R
   ```
3. Check output directories under `results/` for generated tables and plots

## Project Structure

Analysis workflow scripts executed in order:

- **00_config.R**: Central configuration (data range, model parameters, criteria)
- **01_packages.R**: Load required R packages
- **02_benchmark_helpers.R**: Output directories and utility functions
- **03_data_download.R**: Download S&P 500 data from Yahoo Finance
- **04_exploratory.R**: Descriptive statistics, unit root tests, baseline OLS
- **05_arch_selection.R**: ARCH(q) selection using residual diagnostics
- **06_benchmark_models.R**: Fit ARCH, GARCH, EGARCH, TGARCH with normal and Student t distributions
- **07_comparison_helpers.R**: Helpers for GAS models and forecast comparison
- **08_in_sample_candidates.R**: Fit GAS models and compare with rugarch models
- **09_distribution_choice.R**: Choose common distribution for forecasting
- **10_rolling_forecasts.R**: Generate 1-step-ahead rolling forecasts and forecast evaluation
- **11_final_summary.R**: Compilation of outputs and analysis notes

## Output Structure

```
results/
├── benchmark_models/
│   ├── tables/          # Model estimates, diagnostics, rankings
│   ├── plots/           # Volatility paths, ACF plots, QQ plots
│   └── logs/            # Execution notes and data summaries
│
└── forecast_comparison/
    ├── tables/          # Rolling forecast evaluations, Diebold-Mariano tests
    ├── plots/           # Rolling variance forecasts with realized proxy
    └── logs/            # Forecast design and stability notes
```

## Key Outputs

### In-Sample Analysis (Benchmark Models)
- **results/benchmark_models/tables/model_candidates.csv**: List of 8 candidate models (ARCH/GARCH/EGARCH/TGARCH × Normal/Student t)
- **results/benchmark_models/tables/in_sample_fit_status.csv**: Convergence status for each model
- **results/benchmark_models/tables/in_sample_params.csv**: Parameter estimates with standard errors and t-statistics
- **results/benchmark_models/tables/in_sample_information_criteria.csv**: AIC and BIC values for model comparison
- **results/benchmark_models/tables/in_sample_diagnostics.csv**: Residual diagnostics (Ljung-Box, ARCH-LM p-values)
- **results/benchmark_models/tables/in_sample_ranking_help.csv**: Model rankings by diagnostic test pass rate and AIC
- **results/benchmark_models/tables/arch_q_selection.csv**: ARCH(q) order selection results

### In-Sample Analysis (GAS Models)
- **results/forecast_comparison/tables/model_candidates.csv**: List of 6 candidate models (GAS/EGARCH/TGARCH × Normal/Student t)
- **results/forecast_comparison/tables/in_sample_fit_status.csv**: Convergence and usability status for GAS and rugarch models
- **results/forecast_comparison/tables/in_sample_params.csv**: Parameter estimates for all candidate models
- **results/forecast_comparison/tables/in_sample_information_criteria.csv**: AIC/BIC for model comparison
- **results/forecast_comparison/tables/in_sample_diagnostics.csv**: Residual diagnostic test results
- **results/forecast_comparison/tables/in_sample_ranking_help.csv**: Model rankings by diagnostic pass rate

### Distribution Choice
- **results/forecast_comparison/tables/distribution_choice_by_model.csv**: Per-model comparison of Normal vs Student t
- **results/forecast_comparison/tables/distribution_choice_total_ic.csv**: Aggregate IC differences across models
- **results/forecast_comparison/tables/distribution_choice_summary.csv**: Final distribution choice with voting summary

### Rolling Forecasts & Evaluation
- **results/forecast_comparison/tables/rolling_forecasts_long.csv**: All 1-step-ahead forecasts (long format with Date, model, variance forecast, loss metrics)
- **results/forecast_comparison/tables/rolling_forecasts_wide.csv**: Forecasts pivoted to wide format by model
- **results/forecast_comparison/tables/rolling_validity_summary.csv**: Forecast validity and stability by model
- **results/forecast_comparison/tables/rolling_invalid_forecasts.csv**: Forecasts flagged as numerically unstable
- **results/forecast_comparison/tables/forecast_evaluation_summary.csv**: Out-of-sample loss comparison and rankings
- **results/forecast_comparison/tables/forecast_evaluation_summary_common_dates.csv**: Loss comparison on dates with all models valid
- **results/forecast_comparison/tables/dm_tests_qlike.csv**: Diebold-Mariano pairwise test results on QLIKE loss

## Configuration

Edit `scripts/00_config.R` to customize:
```r
start_date <- as.Date("2020-01-03")     # Data start date
end_date <- as.Date("2025-12-03")       # Data end date
window_length <- 1000L                  # Rolling forecast window
run_rolling <- TRUE                      # Enable rolling forecasts
```

See `scripts/00_config.R` comments for all available settings.

## Workflow

### Terminal
```bash
Rscript run_all.R
```

