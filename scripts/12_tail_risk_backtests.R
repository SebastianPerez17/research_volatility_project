# Tail-risk forecasts and backtests

# Validate required variables from config
required_vars <- c(
  "run_rolling", "run_tail_risk", "tail_probs", "tail_risk_models", "tail_risk_dist",
  "loss_sign_convention", "stress_quantile", "bonus_dir"
)
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check if previous scripts executed successfully."))
  }
}

# Validate output directory exists
if (!dir.exists(bonus_dir)) {
  stop(paste0("Output directory does not exist: ", bonus_dir))
}
dir.create(file.path(bonus_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(bonus_dir, "plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(bonus_dir, "logs"), showWarnings = FALSE, recursive = TRUE)

# Validate helper functions exist
helper_funcs <- c(
  "norm_label", "pretty_dist_label", "write_log_bonus", "save_plot_bonus",
  "tail_alpha_label_bonus", "compute_var_es_from_forecasts_bonus", "compute_hit_indicator_bonus",
  "summarize_var_es_by_group_bonus", "run_var_backtests_group_bonus", "run_es_backtests_group_bonus",
  "split_calm_stress_bonus", "safe_mean"
)
missing_funcs <- helper_funcs[!vapply(helper_funcs, exists, logical(1), mode = "function")]
if (length(missing_funcs) > 0) {
  stop(paste0(
    "Missing helper functions: ", paste(missing_funcs, collapse = ", "),
    ". Check if 07_comparison_helpers.R was sourced successfully."
  ))
}

if (!is.logical(run_rolling) || length(run_rolling) != 1 || is.na(run_rolling)) {
  stop("run_rolling must be TRUE or FALSE.")
}
if (!is.logical(run_tail_risk) || length(run_tail_risk) != 1 || is.na(run_tail_risk)) {
  stop("run_tail_risk must be TRUE or FALSE.")
}

if (!isTRUE(run_rolling)) {
  write_log_bonus(
    "tail_risk_backtests.txt",
    c("Tail-risk stage skipped: run_rolling = FALSE")
  )
} else if (!isTRUE(run_tail_risk)) {
  write_log_bonus(
    "tail_risk_backtests.txt",
    c("Tail-risk stage skipped: run_tail_risk = FALSE")
  )
} else {
  if (!is.numeric(tail_probs) || length(tail_probs) < 1 || any(!is.finite(tail_probs)) ||
      any(tail_probs <= 0) || any(tail_probs >= 1)) {
    stop("tail_probs must be a non-empty numeric vector with values in (0, 1).")
  }
  tail_probs <- sort(unique(as.numeric(tail_probs)), decreasing = TRUE)

  if (!is.character(tail_risk_models) || length(tail_risk_models) < 1 || any(is.na(tail_risk_models))) {
    stop("tail_risk_models must be a non-empty character vector.")
  }

  if (!is.character(tail_risk_dist) || length(tail_risk_dist) != 1 || is.na(tail_risk_dist)) {
    stop("tail_risk_dist must be a single character value.")
  }
  tail_risk_dist_simple <- norm_label(tail_risk_dist)
  if (tail_risk_dist_simple != "t") {
    stop('tail_risk_dist must be "t" for this tail-risk pipeline.')
  }

  if (!is.character(loss_sign_convention) || length(loss_sign_convention) != 1 ||
      is.na(loss_sign_convention) || !identical(loss_sign_convention, "positive_loss")) {
    stop('loss_sign_convention must be exactly "positive_loss".')
  }

  if (!is.numeric(stress_quantile) || length(stress_quantile) != 1 || !is.finite(stress_quantile) ||
      stress_quantile <= 0 || stress_quantile >= 1) {
    stop("stress_quantile must be a scalar in (0, 1).")
  }

  tables_dir <- file.path(bonus_dir, "tables")
  plots_dir <- file.path(bonus_dir, "plots")

  base_path <- file.path(tables_dir, "tail_risk_base_forecasts_long.csv")
  if (!file.exists(base_path)) {
    stop(paste0(
      "Required tail-risk base file not found: ", base_path,
      ". Run scripts/10_rolling_forecasts.R first."
    ))
  }

  tail_base_tbl <- read_csv(base_path, show_col_types = FALSE)

  required_cols <- c(
    "Date", "model", "distribution", "fit_status", "forecast_valid",
    "realized_return", "realized_var", "mean_forecast", "sigma_forecast",
    "variance_forecast", "shape", "log_score"
  )
  missing_cols <- setdiff(required_cols, names(tail_base_tbl))
  if (length(missing_cols) > 0) {
    stop(paste0("tail_risk_base_forecasts_long.csv missing columns: ", paste(missing_cols, collapse = ", ")))
  }

  tail_base_tbl <- tail_base_tbl %>%
    mutate(Date = as.Date(Date))

  tail_models_use <- unique(as.character(tail_risk_models))
  tail_models_use <- tail_models_use[tail_models_use %in% c("TGARCH", "GAS", "EGARCH")]
  if (length(tail_models_use) == 0) {
    stop('tail_risk_models must include at least one of: "TGARCH", "GAS", "EGARCH".')
  }

  tail_candidate_tbl <- tail_base_tbl %>%
    mutate(dist_simple = norm_label(distribution)) %>%
    filter(model %in% tail_models_use, dist_simple == "t") %>%
    select(-dist_simple) %>%
    arrange(Date, model)

  if (nrow(tail_candidate_tbl) == 0) {
    stop("No Student t tail-risk candidate forecasts found after filtering models/distribution.")
  }

  tail_counts_tbl <- tail_candidate_tbl %>%
    group_by(model) %>%
    summarise(
      n_total_oos = n(),
      n_valid_forecasts = sum(forecast_valid, na.rm = TRUE),
      n_invalid_forecasts = n_total_oos - n_valid_forecasts,
      share_valid = ifelse(n_total_oos > 0, n_valid_forecasts / n_total_oos, NA_real_),
      .groups = "drop"
    )

  tail_valid_tbl <- tail_candidate_tbl %>%
    filter(!is.na(forecast_valid) & forecast_valid)

  if (nrow(tail_valid_tbl) == 0) {
    stop("No valid tail-risk forecasts available after filtering forecast_valid == TRUE.")
  }

  tail_forecasts_long <- bind_rows(lapply(tail_probs, function(alpha_i) {
    var_es_tbl <- compute_var_es_from_forecasts_bonus(
      mean_forecast = tail_valid_tbl$mean_forecast,
      sigma_forecast = tail_valid_tbl$sigma_forecast,
      alpha = alpha_i,
      distribution = tail_valid_tbl$distribution,
      shape = tail_valid_tbl$shape,
      loss_sign_convention = loss_sign_convention
    )

    out <- bind_cols(tail_valid_tbl, var_es_tbl) %>%
      mutate(
        alpha = as.numeric(alpha_i),
        realized_loss = -realized_return,
        hit = compute_hit_indicator_bonus(realized_loss = realized_loss, var_loss = VaR)
      )

    out
  })) %>%
    select(
      Date, model, distribution, alpha,
      fit_status, forecast_valid,
      realized_return, realized_loss, realized_var,
      mean_forecast, sigma_forecast, variance_forecast, shape,
      log_score, qlike, mse,
      return_quantile, VaR, ES, hit, var_es_note
    ) %>%
    arrange(Date, model, alpha)

  write_csv(
    tail_forecasts_long,
    file.path(tables_dir, "tail_risk_forecasts_long.csv")
  )

  tail_forecasts_wide <- tail_forecasts_long %>%
    mutate(alpha_label = tail_alpha_label_bonus(alpha)) %>%
    select(Date, model, distribution, realized_return, realized_loss, alpha_label, VaR, ES, hit) %>%
    pivot_wider(
      names_from = alpha_label,
      values_from = c(VaR, ES, hit),
      names_sep = "_"
    ) %>%
    arrange(Date, model)

  write_csv(
    tail_forecasts_wide,
    file.path(tables_dir, "tail_risk_forecasts_wide.csv")
  )

  var_es_calibration_tbl <- tail_forecasts_long %>%
    group_by(model, alpha) %>%
    summarise(
      n_exceptions = sum(hit == 1, na.rm = TRUE),
      avg_VaR = safe_mean(VaR),
      avg_ES = safe_mean(ES),
      avg_realized_loss = safe_mean(realized_loss),
      avg_realized_loss_exceptions = safe_mean(realized_loss[hit == 1]),
      .groups = "drop"
    )

  var_es_summary_tbl <- var_es_calibration_tbl %>%
    left_join(tail_counts_tbl, by = "model") %>%
    mutate(
      observed_hit_rate_valid_only = ifelse(n_valid_forecasts > 0, n_exceptions / n_valid_forecasts, NA_real_),
      exception_rate_full_oos = ifelse(n_total_oos > 0, n_exceptions / n_total_oos, NA_real_),
      expected_hit_rate = alpha,
      # Backward-compatible aliases
      n_forecasts = n_total_oos,
      observed_hit_rate = observed_hit_rate_valid_only
    ) %>%
    select(
      model,
      alpha,
      n_total_oos,
      n_valid_forecasts,
      n_invalid_forecasts,
      share_valid,
      n_exceptions,
      observed_hit_rate_valid_only,
      exception_rate_full_oos,
      expected_hit_rate,
      avg_VaR,
      avg_ES,
      avg_realized_loss,
      avg_realized_loss_exceptions,
      n_forecasts,
      observed_hit_rate
    ) %>%
    arrange(alpha, model)

  write_csv(
    var_es_summary_tbl,
    file.path(tables_dir, "var_es_summary.csv")
  )

  var_backtests_tbl <- run_var_backtests_group_bonus(
    tail_forecasts_long,
    group_vars = c("model", "alpha")
  ) %>%
    select(model, alpha, test, statistic, p_value, decision_note, n, n_exceptions, observed_hit_rate) %>%
    arrange(alpha, model, test)

  write_csv(
    var_backtests_tbl,
    file.path(tables_dir, "var_backtests.csv")
  )

  es_backtests_tbl <- run_es_backtests_group_bonus(
    tail_forecasts_long,
    group_vars = c("model", "alpha")
  ) %>%
    select(model, alpha, test, statistic, p_value, decision_note, n, n_exceptions, observed_hit_rate, mean_exceedance_residual) %>%
    arrange(alpha, model)

  write_csv(
    es_backtests_tbl,
    file.path(tables_dir, "es_backtests.csv")
  )

  regime_lookup <- tail_candidate_tbl %>%
    select(Date, realized_var) %>%
    distinct() %>%
    arrange(Date) %>%
    split_calm_stress_bonus(realized_var_col = "realized_var", stress_quantile = stress_quantile) %>%
    select(Date, regime, stress_threshold)

  tail_forecasts_regime_tbl <- tail_forecasts_long %>%
    left_join(regime_lookup %>% select(Date, regime), by = "Date")

  regime_counts_tbl <- tail_candidate_tbl %>%
    select(Date, model, forecast_valid) %>%
    left_join(regime_lookup %>% select(Date, regime), by = "Date") %>%
    filter(!is.na(regime)) %>%
    group_by(model, regime) %>%
    summarise(
      n_forecasts = n(),
      n_valid_forecasts = sum(forecast_valid, na.rm = TRUE),
      n_invalid_forecasts = n_forecasts - n_valid_forecasts,
      share_valid = ifelse(n_forecasts > 0, n_valid_forecasts / n_forecasts, NA_real_),
      .groups = "drop"
    )

  regime_summary_tbl <- summarize_var_es_by_group_bonus(
    tail_forecasts_regime_tbl %>% filter(!is.na(regime)),
    group_vars = c("model", "alpha", "regime")
  ) %>%
    left_join(regime_counts_tbl, by = c("model", "regime"))

  var_backtests_regime_tbl <- run_var_backtests_group_bonus(
    tail_forecasts_regime_tbl %>% filter(!is.na(regime)),
    group_vars = c("model", "alpha", "regime")
  )

  es_backtests_regime_tbl <- run_es_backtests_group_bonus(
    tail_forecasts_regime_tbl %>% filter(!is.na(regime)),
    group_vars = c("model", "alpha", "regime")
  )

  var_backtests_regime_wide <- var_backtests_regime_tbl %>%
    select(model, alpha, regime, test, p_value, decision_note) %>%
    pivot_wider(
      names_from = test,
      values_from = c(p_value, decision_note),
      names_glue = "{test}_{.value}"
    )

  es_backtests_regime_wide <- es_backtests_regime_tbl %>%
    select(model, alpha, regime, p_value, decision_note, statistic) %>%
    rename(
      mcneil_frey_es_p_value = p_value,
      mcneil_frey_es_decision_note = decision_note,
      mcneil_frey_es_statistic = statistic
    )

  tail_risk_backtests_by_regime_tbl <- regime_summary_tbl %>%
    transmute(
      model,
      alpha,
      regime,
      n_forecasts,
      n_valid_forecasts,
      n_invalid_forecasts,
      share_valid,
      n_exceptions,
      observed_hit_rate_valid_only = observed_hit_rate,
      observed_hit_rate = observed_hit_rate_valid_only,
      exception_rate_full_oos = ifelse(n_forecasts > 0, n_exceptions / n_forecasts, NA_real_),
      expected_hit_rate = alpha,
      avg_VaR,
      avg_ES,
      avg_realized_loss,
      avg_realized_loss_exceptions
    ) %>%
    left_join(var_backtests_regime_wide, by = c("model", "alpha", "regime")) %>%
    left_join(es_backtests_regime_wide, by = c("model", "alpha", "regime")) %>%
    arrange(alpha, model, regime)

  write_csv(
    tail_risk_backtests_by_regime_tbl,
    file.path(tables_dir, "tail_risk_backtests_by_regime.csv")
  )

  tail_risk_exceptions_tbl <- tail_forecasts_long %>%
    filter(hit == 1) %>%
    arrange(Date, model, alpha)

  write_csv(
    tail_risk_exceptions_tbl,
    file.path(tables_dir, "tail_risk_exceptions.csv")
  )

  # Plots -----------------------------------------------------------------
  recent_cutoff_date <- tail_forecasts_long %>%
    distinct(Date) %>%
    arrange(Date) %>%
    summarise(cutoff = Date[ceiling(0.80 * n())]) %>%
    pull(cutoff)

  recent_tbl <- tail_forecasts_long %>%
    filter(Date >= recent_cutoff_date)

  make_var_path_plot <- function(df, alpha_use, title_txt, file_name) {
    p_df <- df %>%
      filter(alpha == alpha_use) %>%
      select(Date, model, realized_loss, VaR) %>%
      pivot_longer(
        cols = c(realized_loss, VaR),
        names_to = "series",
        values_to = "value"
      ) %>%
      mutate(
        series = recode(series, realized_loss = "Realized loss", VaR = "VaR")
      )

    p <- ggplot(p_df, aes(x = Date, y = value, color = series, linetype = series, group = series)) +
      geom_line(linewidth = 0.4, na.rm = TRUE) +
      facet_wrap(~ model, ncol = 1, scales = "free_y") +
      labs(
        title = title_txt,
        x = NULL,
        y = "Loss",
        color = "Series",
        linetype = "Series"
      ) +
      scale_color_manual(values = c("Realized loss" = "red", "VaR" = "black")) +
      scale_linetype_manual(values = c("Realized loss" = "solid", "VaR" = "dashed")) +
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
      theme_minimal()

    save_plot_bonus(p, file_name, w = 11, h = 7)
  }

  make_exceptions_timeline_plot <- function(df, alpha_use, title_txt, file_name) {
    p_df <- df %>%
      filter(alpha == alpha_use, hit == 1)

    p <- ggplot(p_df, aes(x = Date, y = model, color = model)) +
      geom_point(size = 1.9, alpha = 0.85, na.rm = TRUE) +
      labs(
        title = title_txt,
        x = NULL,
        y = "Model",
        color = "Model"
      ) +
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
      theme_minimal()

    save_plot_bonus(p, file_name, w = 11, h = 5)
  }

  make_var_path_plot(
    df = recent_tbl,
    alpha_use = 0.05,
    title_txt = "Recent sample: realized loss and 95% VaR paths",
    file_name = "var95_paths_recent.png"
  )

  make_var_path_plot(
    df = recent_tbl,
    alpha_use = 0.01,
    title_txt = "Recent sample: realized loss and 99% VaR paths",
    file_name = "var99_paths_recent.png"
  )

  make_exceptions_timeline_plot(
    df = tail_forecasts_long,
    alpha_use = 0.05,
    title_txt = "95% VaR exceptions timeline",
    file_name = "var95_exceptions_timeline.png"
  )

  make_exceptions_timeline_plot(
    df = tail_forecasts_long,
    alpha_use = 0.01,
    title_txt = "99% VaR exceptions timeline",
    file_name = "var99_exceptions_timeline.png"
  )

  stress_calm_plot_tbl <- tail_risk_backtests_by_regime_tbl %>%
    select(model, alpha, regime, observed_hit_rate_valid_only)

  alpha_lines_tbl <- stress_calm_plot_tbl %>%
    distinct(alpha)

  p_stress_calm <- ggplot(
    stress_calm_plot_tbl,
    aes(x = model, y = observed_hit_rate_valid_only, fill = regime)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    geom_hline(
      data = alpha_lines_tbl,
      aes(yintercept = alpha),
      linetype = "dashed",
      color = "black"
    ) +
    facet_wrap(~ alpha, scales = "free_y") +
    labs(
      title = "Observed VaR hit rates: stress vs calm",
      subtitle = "Dashed line shows expected hit rate (alpha)",
      x = NULL,
      y = "Observed hit rate",
      fill = "Regime"
    ) +
    theme_minimal()

  save_plot_bonus(p_stress_calm, "tail_risk_stress_vs_calm.png", w = 10, h = 5)

  stress_threshold_value <- regime_lookup %>%
    summarise(threshold = first(stress_threshold)) %>%
    pull(threshold)

  write_log_bonus(
    "tail_risk_backtests.txt",
    c(
      "Tail-risk backtesting completed.",
      paste0("Input base file: ", base_path),
      paste0("Models used: ", paste(sort(unique(tail_models_use)), collapse = ", ")),
      paste0("Distribution used: ", pretty_dist_label(tail_risk_dist_simple)),
      paste0("Tail probabilities: ", paste(tail_probs, collapse = ", ")),
      paste0("Loss convention: ", loss_sign_convention),
      paste0("Stress quantile rule: top ", round((1 - stress_quantile) * 100), "% by realized variance"),
      paste0("Stress threshold (realized variance): ", stress_threshold_value),
      "",
      "Interpretation rule: usability vs calibration",
      "- Formal VaR/ES backtests use valid forecasts only.",
      "- Invalid forecasts are excluded because no meaningful VaR/ES exists on those dates.",
      "- Read model quality jointly through:",
      "  * Forecast usability: n_valid_forecasts, n_invalid_forecasts, share_valid",
      "  * Tail calibration (conditional): VaR/ES backtests and observed_hit_rate_valid_only",
      "  * Full OOS exception pressure: exception_rate_full_oos",
      paste0("Rows in tail_risk_forecasts_long.csv: ", nrow(tail_forecasts_long)),
      paste0("Rows in tail_risk_exceptions.csv: ", nrow(tail_risk_exceptions_tbl))
    )
  )
}
