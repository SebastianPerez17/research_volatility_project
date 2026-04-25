# Final summary

# Validate required variables from config
required_vars <- c("run_rolling", "run_tail_risk", "forecast_dir")
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check if previous scripts executed successfully."))
  }
}

# Validate function exists
if (!exists("write_log_forecast") || !is.function(write_log_forecast)) {
  stop("write_log_forecast function not found. Check if 07_comparison_helpers.R executed successfully.")
}

# Validate output directory exists
if (!dir.exists(forecast_dir)) {
  stop(paste0("Output directory does not exist: ", forecast_dir))
}
if (!is.logical(run_rolling) || length(run_rolling) != 1 || is.na(run_rolling)) {
  stop("run_rolling must be TRUE or FALSE.")
}
if (!is.logical(run_tail_risk) || length(run_tail_risk) != 1 || is.na(run_tail_risk)) {
  stop("run_tail_risk must be TRUE or FALSE.")
}

output_line <- function(rel_path) {
  full_path <- file.path(forecast_dir, rel_path)
  status <- if (file.exists(full_path)) "OK" else "MISSING"
  paste0("- [", status, "] ", rel_path)
}

read_forecast_table <- function(filename) {
  path <- file.path(forecast_dir, "tables", filename)
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE)
}

combine_forecast_tables <- function(filenames) {
  tbls <- lapply(filenames, read_forecast_table)
  tbls <- Filter(Negate(is.null), tbls)
  if (length(tbls) == 0L) {
    return(NULL)
  }
  bind_rows(tbls)
}

pick_alpha_rows <- function(tbl, alpha_use, value_col, output_col, test_filter = NULL, regime_filter = NULL) {
  if (is.null(tbl) || !all(c("model", "alpha", value_col) %in% names(tbl))) {
    return(tibble(model = character(), !!output_col := numeric()))
  }

  out <- tbl %>%
    filter(abs(alpha - alpha_use) < 1e-8)

  if (!is.null(test_filter) && "test" %in% names(out)) {
    out <- out %>% filter(test == test_filter)
  }
  if (!is.null(regime_filter) && "regime" %in% names(out)) {
    out <- out %>% filter(regime == regime_filter)
  }

  out %>%
    transmute(model, !!output_col := .data[[value_col]]) %>%
    distinct(model, .keep_all = TRUE)
}

rank_metric <- function(x, higher_is_better = FALSE, target = NULL) {
  if (!is.null(target)) {
    rank(abs(x - target), ties.method = "average", na.last = "keep")
  } else if (isTRUE(higher_is_better)) {
    rank(-x, ties.method = "average", na.last = "keep")
  } else {
    rank(x, ties.method = "average", na.last = "keep")
  }
}

summarise_dm_for_models <- function(dm_tbl, models) {
  if (is.null(dm_tbl) || nrow(dm_tbl) == 0L ||
      !all(c("comparison", "p_value", "preferred_model") %in% names(dm_tbl))) {
    return(tibble(model = models, dm_qlike_result = "No pairwise DM test available"))
  }

  dm_long <- dm_tbl %>%
    mutate(
      model_1 = sub(" vs .*", "", comparison),
      model_2 = sub(".* vs ", "", comparison),
      significant_5pct = is.finite(p_value) & p_value < 0.05
    ) %>%
    select(comparison, p_value, preferred_model, model_1, model_2, significant_5pct)

  tibble(model = models) %>%
    rowwise() %>%
    mutate(
      dm_qlike_result = {
        rows <- dm_long %>% filter(model_1 == model | model_2 == model)
        if (nrow(rows) == 0L) {
          "No pairwise DM test available"
        } else {
          sig_rows <- rows %>% filter(significant_5pct)
          if (nrow(sig_rows) == 0L) {
            "No significant QLIKE DM differences at 5%"
          } else {
            wins <- sig_rows %>%
              filter(preferred_model == model) %>%
              mutate(opponent = ifelse(model_1 == model, model_2, model_1)) %>%
              pull(opponent)
            losses <- sig_rows %>%
              filter(preferred_model != model) %>%
              mutate(opponent = ifelse(model_1 == model, model_2, model_1)) %>%
              pull(opponent)

            win_txt <- if (length(wins) > 0L) {
              paste0("significant wins vs ", paste(wins, collapse = ", "))
            } else {
              "no significant wins"
            }
            loss_txt <- if (length(losses) > 0L) {
              paste0("significant losses vs ", paste(losses, collapse = ", "))
            } else {
              "no significant losses"
            }
            paste(win_txt, loss_txt, sep = "; ")
          }
        }
      }
    ) %>%
    ungroup()
}

build_final_model_comparison <- function() {
  include_tail_outputs <- exists("run_tail_risk") && isTRUE(run_tail_risk)
  include_simple_outputs <- include_tail_outputs &&
    exists("run_simple_risk_benchmarks") && isTRUE(run_simple_risk_benchmarks)

  forecast_eval_tbl <- combine_forecast_tables(c(
    "forecast_evaluation_summary.csv",
    if (include_simple_outputs) "simple_risk_benchmark_evaluation_summary.csv" else character()
  ))
  if (is.null(forecast_eval_tbl)) {
    return(NULL)
  }

  rolling_validity_tbl <- combine_forecast_tables(c(
    "rolling_validity_summary.csv",
    if (include_simple_outputs) "simple_risk_benchmark_validity_summary.csv" else character()
  ))
  var_es_summary_tbl <- combine_forecast_tables(c(
    if (include_tail_outputs) "var_es_summary.csv" else character(),
    if (include_simple_outputs) "simple_risk_benchmark_var_es_summary.csv" else character()
  ))
  var_backtests_tbl <- combine_forecast_tables(c(
    if (include_tail_outputs) "var_backtests.csv" else character(),
    if (include_simple_outputs) "simple_risk_benchmark_var_backtests.csv" else character()
  ))
  es_backtests_tbl <- combine_forecast_tables(c(
    if (include_tail_outputs) "es_backtests.csv" else character(),
    if (include_simple_outputs) "simple_risk_benchmark_es_backtests.csv" else character()
  ))
  regime_tbl <- combine_forecast_tables(c(
    if (include_tail_outputs) "tail_risk_backtests_by_regime.csv" else character(),
    if (include_simple_outputs) "simple_risk_benchmark_backtests_by_regime.csv" else character()
  ))
  dm_tbl <- read_forecast_table("dm_tests_qlike.csv")

  base_tbl <- forecast_eval_tbl %>%
    select(model, distribution, avg_qlike, avg_mse, avg_log_score) %>%
    distinct(model, distribution, .keep_all = TRUE)

  if (!is.null(rolling_validity_tbl) &&
      all(c("model", "distribution", "n_total", "n_valid", "n_invalid", "share_valid") %in% names(rolling_validity_tbl))) {
    base_tbl <- base_tbl %>%
      left_join(
        rolling_validity_tbl %>% select(model, distribution, n_total, n_valid, n_invalid, share_valid),
        by = c("model", "distribution")
      )
  } else {
    base_tbl <- base_tbl %>%
      mutate(
        n_total = NA_integer_,
        n_valid = NA_integer_,
        n_invalid = NA_integer_,
        share_valid = NA_real_
      )
  }

  operational_reliability_tbl <- base_tbl %>%
    transmute(
      model,
      distribution,
      number_of_forecasts = n_total,
      valid_forecasts = n_valid,
      invalid_forecasts = n_invalid,
      share_valid
    ) %>%
    arrange(desc(share_valid), desc(valid_forecasts), model)

  variance_density_tbl <- base_tbl %>%
    left_join(summarise_dm_for_models(dm_tbl, unique(base_tbl$model)), by = "model") %>%
    transmute(
      model,
      distribution,
      avg_qlike,
      avg_mse,
      avg_log_score,
      dm_qlike_result
    ) %>%
    arrange(avg_qlike, avg_mse, model)

  final_tbl <- base_tbl %>%
    left_join(
      pick_alpha_rows(var_es_summary_tbl, 0.05, "observed_hit_rate_valid_only", "VaR_95_hit_rate"),
      by = "model"
    ) %>%
    left_join(
      pick_alpha_rows(var_es_summary_tbl, 0.01, "observed_hit_rate_valid_only", "VaR_99_hit_rate"),
      by = "model"
    ) %>%
    left_join(
      pick_alpha_rows(var_backtests_tbl, 0.05, "p_value", "Kupiec_95_p", test_filter = "kupiec_uc"),
      by = "model"
    ) %>%
    left_join(
      pick_alpha_rows(
        var_backtests_tbl,
        0.05,
        "p_value",
        "Christoffersen_95_p",
        test_filter = "christoffersen_independence"
      ),
      by = "model"
    ) %>%
    left_join(
      pick_alpha_rows(es_backtests_tbl, 0.05, "p_value", "ES_95_p", test_filter = "mcneil_frey_es"),
      by = "model"
    ) %>%
    left_join(
      pick_alpha_rows(
        regime_tbl,
        0.05,
        "observed_hit_rate_valid_only",
        "stress_hit_rate_95",
        regime_filter = "stress"
      ),
      by = "model"
    )

  tail_risk_calibration_tbl <- final_tbl %>%
    transmute(
      model,
      distribution,
      VaR_95_hit_rate,
      VaR_99_hit_rate,
      Kupiec_95_p,
      Christoffersen_95_p,
      ES_95_p,
      stress_hit_rate_95
    ) %>%
    arrange(abs(VaR_95_hit_rate - 0.05), model)

  write_csv(
    operational_reliability_tbl,
    file.path(forecast_dir, "tables", "final_operational_reliability.csv")
  )
  write_csv(
    variance_density_tbl,
    file.path(forecast_dir, "tables", "final_variance_density_forecast.csv")
  )
  write_csv(
    tail_risk_calibration_tbl,
    file.path(forecast_dir, "tables", "final_tail_risk_calibration.csv")
  )

  final_tbl <- final_tbl %>%
    mutate(
      rank_share_valid = rank_metric(share_valid, higher_is_better = TRUE),
      rank_avg_qlike = rank_metric(avg_qlike),
      rank_avg_log_score = rank_metric(avg_log_score, higher_is_better = TRUE),
      rank_VaR_95_hit_rate = rank_metric(VaR_95_hit_rate, target = 0.05),
      rank_VaR_99_hit_rate = rank_metric(VaR_99_hit_rate, target = 0.01),
      rank_Kupiec_95_p = rank_metric(Kupiec_95_p, higher_is_better = TRUE),
      rank_Christoffersen_95_p = rank_metric(Christoffersen_95_p, higher_is_better = TRUE),
      rank_ES_95_p = rank_metric(ES_95_p, higher_is_better = TRUE),
      rank_stress_hit_rate_95 = rank_metric(stress_hit_rate_95, target = 0.05)
    ) %>%
    rowwise() %>%
    mutate(
      final_score = mean(
        c_across(starts_with("rank_")),
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    mutate(final_rank = rank(final_score, ties.method = "min", na.last = "keep")) %>%
    arrange(final_rank, avg_qlike) %>%
    select(
      model,
      distribution,
      share_valid,
      avg_qlike,
      avg_log_score,
      VaR_95_hit_rate,
      VaR_99_hit_rate,
      Kupiec_95_p,
      Christoffersen_95_p,
      ES_95_p,
      stress_hit_rate_95,
      final_rank
    )

  write_csv(final_tbl, file.path(forecast_dir, "tables", "final_model_comparison.csv"))
  final_tbl
}

final_model_comparison_tbl <- if (isTRUE(run_rolling)) build_final_model_comparison() else NULL

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
    output_line(file.path("tables", "final_operational_reliability.csv")),
    output_line(file.path("tables", "final_variance_density_forecast.csv")),
    output_line(file.path("tables", "final_tail_risk_calibration.csv")),
    output_line(file.path("tables", "final_model_comparison.csv")),
    output_line(file.path("tables", "dm_tests_qlike.csv")),
    "",
    "Information criteria notes:",
    "- AIC/BIC harmonized across GAS and rugarch models",
    "- saved columns: AIC_total, BIC_total, AIC_per_obs, BIC_per_obs",
    "",
    "Rolling forecast notes:",
    "- numerically unstable forecasts flagged and excluded from loss evaluation",
    "- plots show only valid forecasts from each model",
    "- final_model_comparison.csv is a compact reader-facing summary; use the three final component tables as the main evidence"
  )

  if (isTRUE(run_tail_risk)) {
    summary_lines <- c(
      summary_lines,
      "",
      "Tail-risk outputs status:",
      output_line(file.path("tables", "tail_risk_base_forecasts_long.csv")),
      output_line(file.path("tables", "tail_risk_forecasts_long.csv")),
      output_line(file.path("tables", "tail_risk_forecasts_wide.csv")),
      output_line(file.path("tables", "var_es_summary.csv")),
      output_line(file.path("tables", "var_backtests.csv")),
      output_line(file.path("tables", "es_backtests.csv")),
      output_line(file.path("tables", "tail_risk_backtests_by_regime.csv")),
      output_line(file.path("tables", "tail_risk_exceptions.csv")),
      "",
      "Tail-risk interpretation note:",
      "- formal VaR/ES backtests exclude invalid forecasts (no meaningful risk measure on those dates)",
      "- calibration results are conditional on successful forecasts only",
      "- compare models jointly through forecast usability (share_valid) and conditional calibration"
    )
  } else {
    summary_lines <- c(summary_lines, "", "Tail-risk section skipped (run_tail_risk = FALSE).")
  }

  if (isTRUE(run_tail_risk) && exists("run_simple_risk_benchmarks") && isTRUE(run_simple_risk_benchmarks)) {
    summary_lines <- c(
      summary_lines,
      "",
      "Simple risk benchmark outputs status:",
      output_line(file.path("tables", "simple_risk_benchmark_forecasts_long.csv")),
      output_line(file.path("tables", "simple_risk_benchmark_validity_summary.csv")),
      output_line(file.path("tables", "simple_risk_benchmark_evaluation_summary.csv")),
      output_line(file.path("tables", "simple_risk_benchmark_tail_forecasts_long.csv")),
      output_line(file.path("tables", "simple_risk_benchmark_var_es_summary.csv")),
      output_line(file.path("tables", "simple_risk_benchmark_var_backtests.csv")),
      output_line(file.path("tables", "simple_risk_benchmark_es_backtests.csv")),
      output_line(file.path("tables", "simple_risk_benchmark_backtests_by_regime.csv")),
      "",
      "Simple risk benchmark notes:",
      "- Historical Simulation uses empirical rolling-window loss quantiles",
      "- EWMA uses normal VaR/ES with the configured decay parameter",
      if (exists("run_filtered_historical_simulation") && isTRUE(run_filtered_historical_simulation)) {
        "- Filtered Historical Simulation uses EWMA-standardized empirical returns"
      } else {
        "- Filtered Historical Simulation skipped"
      }
    )
  } else {
    summary_lines <- c(summary_lines, "", "Simple risk benchmark section skipped.")
  }

  if (exists("run_robustness_checks") && isTRUE(run_robustness_checks)) {
    summary_lines <- c(
      summary_lines,
      "",
      "Robustness check outputs status:",
      output_line(file.path("tables", "robustness_check_design.csv")),
      output_line(file.path("tables", "robustness_check_results.csv")),
      output_line(file.path("tables", "robustness_common_valid_dates.csv")),
      "",
      "Robustness check notes:",
      "- window length, start date, mean, distribution, and VaR-level checks are computed for fast risk benchmarks",
      "- full-sample versus common-valid-date checks use the existing rolling forecast outputs",
      "- model-based TGARCH/GAS/EGARCH window and start-date variants require separate heavy reruns"
    )
  } else {
    summary_lines <- c(summary_lines, "", "Robustness check section skipped.")
  }
} else {
  summary_lines <- c(summary_lines, "", "Rolling forecast section skipped (run_rolling = FALSE).")
  if (isTRUE(run_tail_risk)) {
    summary_lines <- c(summary_lines, "Tail-risk section skipped because rolling forecasts were not run.")
  }
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

  if (isTRUE(run_tail_risk)) {
    summary_lines <- c(
      summary_lines,
      output_line(file.path("plots", "var95_paths_recent.png")),
      output_line(file.path("plots", "var99_paths_recent.png")),
      output_line(file.path("plots", "var95_exceptions_timeline.png")),
      output_line(file.path("plots", "var99_exceptions_timeline.png")),
      output_line(file.path("plots", "tail_risk_stress_vs_calm.png"))
    )
  }
}

# Write summary log
tryCatch(
  write_log_forecast("bonus_summary.txt", summary_lines),
  error = function(e) cat("WARNING: Failed to write summary log:", e$message, "\n")
)

# Print final progress message
cat("\n=== ANALYSIS COMPLETED ===\n")
cat("Output directory:", forecast_dir, "\n")
cat("Summary log:", file.path(forecast_dir, "logs", "bonus_summary.txt"), "\n")
cat("Check logs/ directory for detailed execution notes\n")
cat("===========================\n")
