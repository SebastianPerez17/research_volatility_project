# A Rolling VaR and Expected Shortfall Framework Using TGARCH-t, GAS-t, and EGARCH-t

## Overview

This repository contains a modular empirical framework for **volatility forecasting** and **tail-risk forecasting** using daily **S&P 500 returns**.

The project combines two connected layers of analysis:

1. a **supporting volatility layer**, where alternative conditional variance models are estimated, diagnosed, and compared through information criteria and out-of-sample variance forecast losses
2. a **core tail-risk layer**, where selected models are used to produce and backtest **1-day-ahead Value at Risk (VaR)** and **Expected Shortfall (ES)** forecasts

The project is designed as a full pipeline rather than a single script. It starts from data download and preprocessing, proceeds through exploratory analysis and in-sample model comparison, and ends with rolling forecasts, VaR/ES construction, formal backtesting, and output summaries.

The main modelling focus is on three Student-t specifications in the tail-risk stage:

- **Benchmark**: `TGARCH-t`
- **Challenger**: `GAS-t`
- **Control**: `EGARCH-t`

This design reflects the central research shift of the project:

- the supporting question is: **which model forecasts conditional variance more effectively?**
- the main question is: **which model delivers more credible tail-risk forecasts and performs better in VaR/ES backtesting?**

Variance forecast evaluation remains part of the project, but it is treated as supporting evidence rather than the final objective.

## Research Question

The project studies whether alternative volatility dynamics and innovation distributions lead to materially different conclusions about market risk.

More precisely, it asks:

- whether symmetric and asymmetric volatility models differ in their adequacy for daily equity-index returns
- whether allowing for heavy-tailed innovations improves the empirical description of return dynamics
- whether models that appear reasonable in-sample also remain reliable in **out-of-sample rolling forecasts**
- whether those models provide **usable and well-calibrated VaR and ES forecasts**

The project therefore moves beyond a purely in-sample model-comparison exercise. Its main contribution is to compare models not only by fit, but also by their ability to generate risk forecasts that remain credible when formally backtested.

## Data

The default data source is **Yahoo Finance**, and the default asset is the **S&P 500 index** (`^GSPC`).

The pipeline downloads adjusted closing prices, constructs log prices, and computes daily log returns scaled by a configurable factor. By default, returns are scaled by `100`, so they are expressed in percentage terms.

All core data settings are configurable in `scripts/00_config.R`, including:

- ticker symbol
- start date
- end date
- return scaling
- rolling window length
- tail-risk settings

The current default configuration uses:

- `ticker = "^GSPC"`
- `start_date = as.Date("2020-01-03")`
- `end_date = as.Date("2025-12-03")`
- `window_length = 750L`

These are **default project settings**, not hard-coded conceptual restrictions. The framework is written so the user can modify the empirical design from the configuration file.

## Methodology

The methodology is organized in sequential stages.

### 1. Data download and preprocessing

The pipeline downloads daily price data, extracts the adjusted close series, computes log prices, and then derives daily log returns. Invalid or non-finite observations are removed before analysis begins.

### 2. Exploratory analysis

The exploratory stage provides a first statistical description of the series through:

- descriptive statistics
- return and squared-return plots
- autocorrelation analysis
- Ljung-Box tests
- ADF and Phillips-Perron stationarity tests
- a baseline constant-mean OLS specification

The purpose of this stage is to verify the standard stylized facts of financial returns, especially low linear autocorrelation in returns and persistence in squared returns.

### 3. ARCH(q) order selection

Before fitting the broader benchmark family, the project selects an ARCH order using a practical adequacy-first rule.

For each candidate `q`, an ARCH model is estimated and the standardized residuals are tested with ARCH-LM tests at lags 5 and 10. The preferred order is the smallest `q` for which the remaining ARCH evidence is no longer statistically significant at the chosen significance level.

This step is meant to ground the benchmark layer in residual diagnostics rather than relying only on information criteria.

### 4. Benchmark ARCH-family estimation

The project then estimates a benchmark set of ARCH-family models:

- `ARCH(q)`
- `GARCH(1,1)`
- `EGARCH(1,1)`
- `TGARCH(1,1)`

These models are estimated under alternative innovation distributions, specifically Normal and Student t where relevant.

For each benchmark model, the pipeline reports:

- convergence status
- parameter estimates
- information criteria
- residual diagnostic tests
- standardized residual plots, ACF plots, and Q-Q plots

### 5. In-sample candidate comparison

The broader candidate comparison focuses on:

- `GAS`
- `EGARCH`
- `TGARCH`

Each is estimated under Normal and Student t versions where applicable. This stage is used to compare score-driven and ARCH-type volatility updating mechanisms within a common framework.

The in-sample comparison is not based on information criteria alone. The project also checks whether the fitted models leave substantial remaining dependence in standardized residuals or squared standardized residuals.

### 6. Distribution choice for the supporting variance layer

The script `09_distribution_choice.R` selects a common forecasting distribution for the supporting rolling variance-comparison exercise. This decision is based on information-criterion evidence across candidate models.

This distribution-choice stage belongs to the **supporting volatility layer** of the project.

### 7. Rolling 1-step-ahead forecasts

The script `10_rolling_forecasts.R` performs rolling 1-step-ahead forecasting using a fixed rolling window.

For each out-of-sample date, the model is re-estimated on the previous window and used to generate forecasts for the next observation. The rolling framework produces:

- mean forecasts
- volatility forecasts
- variance forecasts
- shape parameters where relevant
- log-score, QLIKE, and MSE loss measures

The project also performs pairwise **Diebold-Mariano tests** on QLIKE loss to compare predictive performance across models.

### 8. Dedicated tail-risk forecast layer

The tail-risk stage is intentionally separated from the legacy common-distribution decision used for the supporting variance-comparison outputs.

For the tail-risk layer, the framework uses a **dedicated Student-t forecasting base** for:

- `TGARCH`
- `GAS`
- `EGARCH`

This means the core VaR/ES exercise is deliberately pinned to Student t innovations, even if the earlier supporting variance-comparison layer considered broader distribution choice logic.

### 9. VaR and Expected Shortfall construction

Using the rolling Student-t forecast base, the project computes 1-day-ahead:

- **95% VaR and ES**
- **99% VaR and ES**

The sign convention is explicitly defined so that losses are treated as positive quantities in the tail-risk stage.

### 10. VaR and ES backtesting

The script `12_tail_risk_backtests.R` evaluates risk forecasts using formal backtests.

For **VaR**, the project reports:

- **Kupiec unconditional coverage test**
- **Christoffersen independence test**
- **Christoffersen conditional coverage test**

For **ES**, the project reports:

- **McNeil-Frey ES backtest**

In addition, the project compares tail-risk behavior across **calm** and **stress** regimes, where stress periods are defined using a realized-variance threshold based on the upper tail of the realized-variance distribution.

## Interpretation Logic: Usability vs Calibration

A central feature of the project is that model evaluation in the tail-risk stage is based on **two dimensions**, not one.

### Forecast usability

Usability refers to whether a model is able to generate valid risk forecasts consistently. The project reports:

- total out-of-sample forecast count
- valid forecast count
- invalid forecast count
- share of valid forecasts

This matters because a model that is theoretically attractive but frequently fails numerically is not operationally strong in a risk-management setting.

### Conditional calibration

Calibration refers to how well the model performs **when valid forecasts are available**. This includes:

- exception counts
- observed hit rates
- VaR backtests
- ES backtests

### Why both dimensions matter

Formal VaR/ES backtests are run only on valid forecasts, because no meaningful VaR or ES exists on dates where the forecast itself is invalid.

For that reason, model quality must be read jointly through:

- **usability**: can the model deliver forecasts reliably?
- **calibration**: when forecasts are available, do they pass the relevant tail-risk checks?

This interpretation rule is one of the key design principles of the project.

## Project Structure

The full pipeline is executed through `run_all.R`, which sources the scripts in order from the project root.

### Execution order

- `scripts/00_config.R`  
  Central configuration file. Defines data settings, rolling settings, model settings, numerical safeguards, and tail-risk options.

- `scripts/01_packages.R`  
  Loads required packages and optionally installs missing ones.

- `scripts/02_benchmark_helpers.R`  
  Creates benchmark output folders and provides shared helper functions for logging, plotting, descriptive statistics, and diagnostics.

- `scripts/03_data_download.R`  
  Downloads Yahoo Finance data, computes log prices and returns, validates data integrity, and saves the cleaned dataset.

- `scripts/04_exploratory.R`  
  Produces descriptive statistics, return plots, stationarity tests, baseline OLS output, and first-pass diagnostics.

- `scripts/05_arch_selection.R`  
  Selects the ARCH order using standardized residual diagnostics and ARCH-LM tests.

- `scripts/06_benchmark_models.R`  
  Estimates the benchmark ARCH-family models and exports coefficients, information criteria, diagnostics, and plots.

- `scripts/07_comparison_helpers.R`  
  Defines helper functions used in the score-driven and rolling-forecast stages, including forecasting, diagnostics, loss functions, VaR/ES computation, and backtest utilities.

- `scripts/08_in_sample_candidates.R`  
  Fits and compares the main candidate models (`GAS`, `EGARCH`, `TGARCH`) under alternative distributions.

- `scripts/09_distribution_choice.R`  
  Chooses the common distribution used for the supporting variance-comparison forecasting layer.

- `scripts/10_rolling_forecasts.R`  
  Runs rolling 1-step-ahead forecasts, computes loss metrics, exports forecast-evaluation tables, performs Diebold-Mariano tests, and builds the dedicated Student-t base forecasts for the tail-risk stage.

- `scripts/12_tail_risk_backtests.R`  
  Constructs VaR and ES forecasts, computes hit indicators, runs backtests, compares calm and stress regimes, and exports tail-risk plots and tables.

- `scripts/11_final_summary.R`  
  Produces a final output-presence summary and logs the status of the main generated artifacts.

- `run_all.R`  
  Pipeline runner for the entire project.

## How to Run the Project

Run the project from the repository root:

```bash
Rscript run_all.R
The runner attempts to identify the project root robustly and then sources each script in the required order.
Configuration
All important empirical settings are centralized in scripts/00_config.R.
Core data settings
ticker      <- "^GSPC"
start_date  <- as.Date("2020-01-03")
end_date    <- as.Date("2025-12-03")
return_scale <- 100
Benchmark-selection settings
max_q_try <- 12
alpha_archlm <- 0.05
archlm_lags_check <- c(5, 10)
ac_lag <- 20
Rolling-forecast settings
window_length <- 750L
progress_every <- 25L
run_rolling <- TRUE
forecast_dist_override <- NULL
Tail-risk settings
run_tail_risk <- TRUE
tail_probs <- c(0.05, 0.01)
tail_risk_models <- c("TGARCH", "GAS", "EGARCH")
tail_risk_dist <- "t"
loss_sign_convention <- "positive_loss"
stress_quantile <- 0.80
Numerical safeguards
forecast_var_floor <- 1e-12
forecast_var_cap   <- 500
student_shape_min  <- 2.05
min_diag_obs_buffer <- 5L
The configuration file is intended to make the framework easy to adapt without editing the analytical logic in the main scripts.
Output Structure
The project writes results under results/.
results/
├── benchmark_models/
│   ├── tables/
│   ├── plots/
│   └── logs/
└── forecast_comparison/
    ├── tables/
    ├── plots/
    └── logs/
Benchmark outputs
The results/benchmark_models/ folder contains the benchmark estimation layer, including:
cleaned data
descriptive statistics
stationarity-test logs
ARCH-order selection outputs
benchmark-model fit summaries
information criteria
diagnostic tables
residual and volatility plots
Forecast-comparison outputs
The results/forecast_comparison/ folder contains the candidate-comparison, rolling-forecast, and tail-risk outputs.
Important tables include:
Supporting volatility outputs
rolling_forecasts_long.csv
rolling_forecasts_wide.csv
rolling_validity_summary.csv
rolling_invalid_forecasts.csv
forecast_evaluation_summary.csv
forecast_evaluation_summary_common_dates.csv
dm_tests_qlike.csv
Tail-risk core outputs
tail_risk_base_forecasts_long.csv
tail_risk_forecasts_long.csv
tail_risk_forecasts_wide.csv
var_es_summary.csv
var_backtests.csv
es_backtests.csv
tail_risk_backtests_by_regime.csv
tail_risk_exceptions.csv
Plot outputs
The project also exports plots for both the variance and tail-risk layers, including:
rolling variance-forecast plots
recent-subsample variance plots
VaR path plots
VaR exceptions timelines
calm-vs-stress comparison plots
Main Outputs and How to Read Them
forecast_evaluation_summary.csv
This file summarizes out-of-sample variance-forecast performance using loss functions such as QLIKE, MSE, and log-score.
It is useful for comparing predictive variance performance across models, but it is not the final decision object for the project.
forecast_evaluation_summary_common_dates.csv
This file repeats the variance-forecast comparison on the subset of dates where all models delivered valid forecasts. It is useful for fairer comparison when models differ in numerical stability.
dm_tests_qlike.csv
This file reports pairwise Diebold-Mariano tests based on QLIKE loss. It provides a formal pairwise comparison of predictive variance performance.
var_es_summary.csv
This is one of the central tail-risk outputs. It combines:
forecast-usability information
exception counts
observed hit rates
average VaR and ES measures
It should be read together with the formal backtest tables rather than in isolation.
var_backtests.csv
This file reports the VaR backtest results:
Kupiec unconditional coverage
Christoffersen independence
Christoffersen conditional coverage
In short:
Kupiec checks whether the exception frequency is consistent with the nominal VaR level
Christoffersen independence checks whether exceptions cluster over time
conditional coverage combines both ideas
es_backtests.csv
This file reports the McNeil-Frey ES backtest, which evaluates whether Expected Shortfall forecasts are compatible with the realized losses observed in the exceedance region.
tail_risk_backtests_by_regime.csv
This file compares model behavior in calm and stress periods. It helps assess whether model performance deteriorates in volatile conditions, which is especially relevant in a market-risk context.
Current Scope of the Project
This repository is currently designed as a single-asset empirical framework centered on daily S&P 500 returns.
Its contribution is therefore methodological and empirical within that scope:
comparison of benchmark and score-driven volatility models
comparison of Normal and Student-t innovation assumptions in the supporting layer
rolling out-of-sample forecast evaluation
dedicated Student-t VaR/ES construction and backtesting
explicit separation between forecast usability and conditional calibration
The project is not presented as a universal claim about all assets or all market conditions. It is a modular framework applied to a specific asset and sample configuration, with settings that can be changed through the configuration file.
Reproducibility Notes
The project is intended to be run from the repository root through Rscript run_all.R.
The package-loading step can optionally install missing packages if install_if_missing <- TRUE is kept in the configuration.
Because the project uses rolling re-estimation, runtime may depend materially on:
sample length
rolling-window length
model set
package versions
system performance
For faster testing, users can reduce the date range or rolling window in scripts/00_config.R.
Extension Paths
Although the current repository documents the framework as it is now, it is naturally extensible. Possible future extensions include:
applying the same pipeline to other equity indices or asset classes
adding additional forecast windows
introducing alternative innovation distributions
comparing against simpler market-risk benchmarks
expanding the backtesting section with additional scoring or comparative procedures
Summary
This project provides a structured empirical framework for studying volatility and tail risk in daily financial returns.
Its supporting layer compares conditional variance models through diagnostics and forecasting losses. Its main layer evaluates whether selected models produce usable and credible VaR/ES forecasts under a rolling out-of-sample design.
The result is a modular workflow that is both academically interpretable and practically relevant for market-risk analysis.