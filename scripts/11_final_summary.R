# Final summary

# Validate required variables from config
required_vars <- c("run_rolling", "bonus_dir")
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check if previous scripts executed successfully."))
  }
}

# Validate function exists
if (!exists("write_log_bonus") || !is.function(write_log_bonus)) {
  stop("write_log_bonus function not found. Check if 07_comparison_helpers.R executed successfully.")
}

# Validate output directory exists
if (!dir.exists(bonus_dir)) {
  stop(paste0("Output directory does not exist: ", bonus_dir))
}
if (!is.logical(run_rolling) || length(run_rolling) != 1 || is.na(run_rolling)) {
  stop("run_rolling must be TRUE or FALSE.")
}

output_line <- function(rel_path) {
  full_path <- file.path(bonus_dir, rel_path)
  status <- if (file.exists(full_path)) "OK" else "MISSING"
  paste0("- [", status, "] ", rel_path)
}

# Compile summary of analysis outputs
summary_lines <- c(
  "Analysis completed.",
  "",
  "Main outputs status:",
  output_line(file.path("tables", "model_candidates.csv")),
  output_line(file.path("tables", "in_sample_fit_status.csv")),
  output_line(file.path("tables", "in_sample_params.csv")),
  output_line(file.path("tables", "in_sample_information_criteria.csv")),
  output_line(file.path("tables", "in_sample_diagnostics.csv")),
  output_line(file.path("tables", "in_sample_ranking_help.csv")),
  output_line(file.path("tables", "distribution_choice_by_model.csv")),
  output_line(file.path("tables", "distribution_choice_total_ic.csv")),
  output_line(file.path("tables", "distribution_choice_summary.csv"))
)

if (isTRUE(run_rolling)) {
  summary_lines <- c(
    summary_lines,
    "",
    "Rolling forecast outputs status:",
    output_line(file.path("tables", "rolling_forecasts_long.csv")),
    output_line(file.path("tables", "rolling_forecasts_wide.csv")),
    output_line(file.path("tables", "rolling_validity_summary.csv")),
    output_line(file.path("tables", "rolling_invalid_forecasts.csv")),
    output_line(file.path("tables", "forecast_evaluation_summary.csv")),
    output_line(file.path("tables", "forecast_evaluation_summary_common_dates.csv")),
    output_line(file.path("tables", "dm_tests_qlike.csv")),
    "",
    "Information criteria notes:",
    "- AIC/BIC harmonized across GAS and rugarch models",
    "- saved columns: AIC_total, BIC_total, AIC_per_obs, BIC_per_obs",
    "",
    "Rolling forecast notes:",
    "- numerically unstable forecasts flagged and excluded from loss evaluation",
    "- plots show only valid forecasts from each model"
  )
} else {
  summary_lines <- c(summary_lines, "", "Rolling forecast section skipped (run_rolling = FALSE).")
}

# Add plot summary
summary_lines <- c(
  summary_lines,
  "",
  "Visualization outputs:",
  "- ACF plots: standardized residuals and squared residuals per in-sample model",
  "- QQ plots: standardized residuals per in-sample model",
  "- Diagnostics plots: fitted volatility paths per in-sample model"
)

if (isTRUE(run_rolling)) {
  summary_lines <- c(
    summary_lines,
    output_line(file.path("plots", "rolling_variance_forecasts_full_proxy_red.png")),
    output_line(file.path("plots", "rolling_variance_forecasts_recent_proxy_red.png")),
    output_line(file.path("plots", "rolling_variance_forecasts_recent_proxy_red_ymax40.png")),
    output_line(file.path("plots", "rolling_variance_forecasts_last_year_proxy_red_ymax20.png")),
    "- Rolling plots include realized volatility proxy series"
  )
}

# Write summary log
tryCatch(
  write_log_bonus("bonus_summary.txt", summary_lines),
  error = function(e) cat("WARNING: Failed to write summary log:", e$message, "\n")
)

# Print final progress message
cat("\n=== ANALYSIS COMPLETED ===\n")
cat("Output directory:", bonus_dir, "\n")
cat("Summary log:", file.path(bonus_dir, "logs", "bonus_summary.txt"), "\n")
cat("Check logs/ directory for detailed execution notes\n")
cat("===========================\n")
