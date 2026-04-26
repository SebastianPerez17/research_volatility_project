# Research volatility pipeline configuration
# Run from the project root with: Rscript run_all.R

# Core data settings ------------------------------------------------------
ticker      <- "^GSPC"
start_date  <- as.Date("2005-01-03")
end_date    <- as.Date("2025-12-03")
return_scale <- 100

# Unified output structure ------------------------------------------------
results_root <- file.path("results", "sp500_2005_2025_w750")
benchmark_output_dir <- file.path(results_root, "benchmark_models")
forecast_subdir <- "forecast_comparison"
allow_overwrite_final_run <- FALSE

# ARCH(q) selection -------------------------------------------------------
max_q_try <- 12
alpha_archlm <- 0.05
archlm_lags_check <- c(5, 10)
ac_lag <- 20

# Rolling forecast settings ----------------------------------------------
window_length <- 750L
progress_every <- 25L
run_rolling <- TRUE
forecast_dist_override <- NULL

# Tail-risk settings ------------------------------------------------------
run_tail_risk <- TRUE
tail_probs <- c(0.05, 0.01)   # 95% and 99% VaR/ES
tail_risk_models <- c("TGARCH", "GAS", "EGARCH")
tail_risk_dist <- "t"
loss_sign_convention <- "positive_loss"
stress_quantile <- 0.80

# Simple risk benchmark settings -----------------------------------------
run_simple_risk_benchmarks <- TRUE
simple_risk_ewma_lambda <- 0.94
run_filtered_historical_simulation <- TRUE

# Robustness check settings -----------------------------------------------
run_robustness_checks <- TRUE
robustness_window_lengths <- c(500L, 750L, 1000L)
robustness_start_dates <- as.Date(c("2005-01-03", "2010-01-04", "2020-01-03"))
robustness_mean_specs <- c("zero", "constant")
robustness_distributions <- c("norm", "t")
robustness_student_t_shape <- 6

# GAS settings ------------------------------------------------------------
gas_scaling <- "unit"
require_valid_gas_hessian_for_se <- TRUE

# Numerical safeguards ----------------------------------------------------
forecast_var_floor <- 1e-12
forecast_var_cap   <- 500
student_shape_min  <- 2.05
min_diag_obs_buffer <- 5L

# General -----------------------------------------------------------------
install_if_missing <- TRUE
req_pkgs_bonus <- c("quantmod", "xts", "zoo", "dplyr", "tibble", "readr", 
                    "lubridate", "tidyr", "ggplot2", "lmtest", "FinTS", 
                    "rugarch", "gasmodel")
