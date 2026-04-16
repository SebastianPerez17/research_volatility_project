# A Rolling VaR and Expected Shortfall Framework Using TGARCH-t, GAS-t, and EGARCH-t

## Overview

This repository is a modular **market-risk / tail-risk forecasting framework** for daily S&P 500 returns.

The primary contribution is out-of-sample, 1-day-ahead **VaR** and **Expected Shortfall (ES)** forecasting and backtesting using:

- **Benchmark**: `TGARCH-t`
- **Challenger**: `GAS-t`
- **Control**: `EGARCH-t`

The core question is tail-risk credibility, not only variance fit:

- Old emphasis: which model forecasts variance better?
- New emphasis: which model provides more credible tail-risk forecasts and passes backtests?

Variance forecast evaluation (QLIKE/MSE/log-score + DM tests) is retained as supporting analysis.

## Quick Start

1. Edit `scripts/00_config.R` for dates and rolling settings.
2. Confirm tail-risk settings in the same file (`run_tail_risk`, `tail_probs`, `tail_risk_models`, `tail_risk_dist`, `stress_quantile`).
3. Run the full pipeline from project root:

```bash
Rscript run_all.R
```

4. Inspect outputs under:

- `results/benchmark_models/`
- `results/forecast_comparison/`

## Project Structure

Pipeline scripts (run in order):

- `scripts/00_config.R`: Central configuration (data, rolling, tail-risk settings)
- `scripts/01_packages.R`: Package loading and install checks
- `scripts/02_benchmark_helpers.R`: Shared benchmark utilities and output helpers
- `scripts/03_data_download.R`: Yahoo Finance download and cleaning
- `scripts/04_exploratory.R`: Exploratory return analysis and baseline diagnostics
- `scripts/05_arch_selection.R`: ARCH(q) diagnostics and order choice
- `scripts/06_benchmark_models.R`: ARCH-family benchmark estimation
- `scripts/07_comparison_helpers.R`: GAS/rugarch, rolling-forecast, and VaR/ES backtest helpers
- `scripts/08_in_sample_candidates.R`: In-sample candidate comparison (GAS, EGARCH, TGARCH)
- `scripts/09_distribution_choice.R`: Legacy common-distribution decision for variance comparison
- `scripts/10_rolling_forecasts.R`: Rolling variance forecasts and dedicated Student-t tail-risk base forecasts
- `scripts/12_tail_risk_backtests.R`: VaR/ES construction, backtests, regime analysis, and tail-risk plots
- `scripts/11_final_summary.R`: Output presence/status summary log (runs last)
- `run_all.R`: Full pipeline runner

## Key Outputs

### Supporting Volatility Outputs

- `results/forecast_comparison/tables/rolling_forecasts_long.csv`
- `results/forecast_comparison/tables/rolling_forecasts_wide.csv`
- `results/forecast_comparison/tables/forecast_evaluation_summary.csv`
- `results/forecast_comparison/tables/forecast_evaluation_summary_common_dates.csv`
- `results/forecast_comparison/tables/dm_tests_qlike.csv`

### Tail-Risk Core Outputs

- `results/forecast_comparison/tables/tail_risk_base_forecasts_long.csv`
  - Dedicated Student-t rolling base for `TGARCH`, `GAS`, and `EGARCH`
- `results/forecast_comparison/tables/tail_risk_forecasts_long.csv`
  - One row per `Date x model x alpha`, including VaR/ES/hits
- `results/forecast_comparison/tables/tail_risk_forecasts_wide.csv`
- `results/forecast_comparison/tables/var_es_summary.csv`
  - Per `model x alpha`: usability (`n_total_oos`, `n_valid_forecasts`, `n_invalid_forecasts`, `share_valid`) and calibration (`n_exceptions`, hit-rate metrics)
- `results/forecast_comparison/tables/var_backtests.csv`
  - Kupiec UC, Christoffersen independence, Christoffersen conditional coverage
- `results/forecast_comparison/tables/es_backtests.csv`
  - McNeil-Frey ES backtest
- `results/forecast_comparison/tables/tail_risk_backtests_by_regime.csv`
  - Calm vs stress comparison (stress = top 20% realized variance)
- `results/forecast_comparison/tables/tail_risk_exceptions.csv`

Backtest conditioning note:

- Invalid forecasts are excluded from formal VaR/ES backtests because no meaningful VaR/ES exists on those dates.
- Calibration outputs are therefore conditional on successful forecasts.
- Read model comparison jointly through usability (consistently producing valid forecasts) and calibration (passing VaR/ES backtests when forecasts are available).

### Tail-Risk Plots

Saved in `results/forecast_comparison/plots/`:

- `var95_paths_recent.png`
- `var99_paths_recent.png`
- `var95_exceptions_timeline.png`
- `var99_exceptions_timeline.png`
- `tail_risk_stress_vs_calm.png`

## Workflow

1. Build benchmark and candidate models in-sample.
2. Run rolling 1-day forecasts.
3. Generate legacy variance comparison outputs.
4. Build Student-t tail-risk forecast panel for `TGARCH`, `GAS`, and `EGARCH`.
5. Compute VaR/ES at **95% and 99%** levels (`alpha = 0.05, 0.01`).
6. Backtest VaR and ES forecasts.
7. Compare behavior across **calm vs stressed** periods using a reproducible realized-variance rule.

## Configuration

Key settings in `scripts/00_config.R`:

```r
# Core dates and rolling window
start_date <- as.Date("2020-01-03")
end_date <- as.Date("2025-12-03")
window_length <- 750L
run_rolling <- TRUE

# Tail-risk settings
run_tail_risk <- TRUE
tail_probs <- c(0.05, 0.01)   # 95% and 99% VaR/ES
tail_risk_models <- c("TGARCH", "GAS", "EGARCH")
tail_risk_dist <- "t"
loss_sign_convention <- "positive_loss"
stress_quantile <- 0.80
```

Tail-risk stage is intentionally pinned to **Student t** for `TGARCH`, `GAS`, and `EGARCH`, independent of the legacy common-distribution selection used in variance-comparison outputs.
