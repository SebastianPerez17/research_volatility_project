# Research volatility pipeline configuration
# Run from the project root with: Rscript run_all.R

# Core data settings ------------------------------------------------------
ticker      <- "^GSPC"
start_date  <- as.Date("2020-01-03")
end_date    <- as.Date("2025-12-03")
return_scale <- 100

# Unified output structure ------------------------------------------------
results_root <- file.path("results")
benchmark_output_dir <- file.path(results_root, "benchmark_models")
bonus_subdir <- "forecast_comparison"

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
