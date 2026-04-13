# Rolling forecasts and forecast comparison

# Validate required variables from config
required_vars <- c("run_rolling", "window_length", "progress_every", "chosen_dist_simple", 
                   "bonus_dir", "forecast_var_floor", "forecast_var_cap", "student_shape_min", "df", "r")
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
helper_funcs <- c("forecast_one_gas_bonus", "forecast_one_rugarch_bonus", "rugarch_dist_from_simple", 
                  "norm_label", "pretty_dist_label", "qlike_loss", "mse_loss", 
                  "paired_losses_by_date", "dm_test_1step", "safe_mean", "safe_max_num", 
                  "safe_quantile_num", "safe_median_num", "make_variance_plot_bonus", 
                  "save_plot_bonus", "write_log_bonus")
missing_funcs <- helper_funcs[!vapply(helper_funcs, exists, logical(1), mode = "function")]
if (length(missing_funcs) > 0) {
  stop(paste0("Missing helper functions: ", paste(missing_funcs, collapse = ", "), 
              ". Check if 07_comparison_helpers.R was sourced successfully."))
}

# Validate that data is available
if (!is.numeric(r) || length(r) == 0) {
  stop("r must be a non-empty numeric vector.")
}
if (!is.data.frame(df) || !"Date" %in% names(df)) {
  stop("df must be a data frame containing a Date column.")
}
if (!inherits(df$Date, "Date")) {
  stop("df$Date must be of class Date.")
}
if (length(r) != nrow(df)) {
  stop("Inconsistent objects: length(r) != nrow(df). Use a fresh R session.")
}
if (!is.logical(run_rolling) || length(run_rolling) != 1 || is.na(run_rolling)) {
  stop("run_rolling must be TRUE or FALSE.")
}
if (!is.numeric(window_length) || length(window_length) != 1 || !is.finite(window_length) ||
    window_length < 1 || window_length != as.integer(window_length)) {
  stop("window_length must be a single positive integer.")
}
if (!is.numeric(progress_every) || length(progress_every) != 1 || !is.finite(progress_every) ||
    progress_every < 1 || progress_every != as.integer(progress_every)) {
  stop("progress_every must be a single positive integer.")
}
if (!is.character(chosen_dist_simple) || length(chosen_dist_simple) != 1 || is.na(chosen_dist_simple)) {
  stop('chosen_dist_simple must be a single value representing "norm" or "t".')
}
chosen_dist_simple <- norm_label(as.character(chosen_dist_simple))
if (!chosen_dist_simple %in% c("norm", "t")) {
  stop('chosen_dist_simple must be "norm" or "t".')
}

if (isTRUE(run_rolling)) {
  n_total <- length(r)
  
  if (window_length >= n_total) {
    stop("window_length must be strictly smaller than the number of return observations.")
  }
  
  oos_index <- seq.int(from = window_length + 1L, to = n_total)
  oos_dates <- df$Date[oos_index]
  
  chosen_dist_rugarch <- rugarch_dist_from_simple(chosen_dist_simple)
  chosen_dist_gas <- norm_label(chosen_dist_simple)
  
  # Initialize forecast container: 3 models per step (GAS, EGARCH, TGARCH)
  forecast_rows <- vector("list", length(oos_index) * 3L)
  row_counter <- 1L
  
  # Rolling window: fit on past window_length observations, forecast next return
  for (j in seq_along(oos_index)) {
    idx_out <- oos_index[j]
    train_idx <- seq.int(from = idx_out - window_length, to = idx_out - 1L)
    
    y_train <- r[train_idx]
    y_next <- r[idx_out]
    date_next <- df$Date[idx_out]
    rv_next <- y_next^2
    
    # Progress indicator: report at start, first, and every progress_every steps
    if (j %% progress_every == 0L || j == 1L || j == length(oos_index)) {
      cat("Step", j, "of", length(oos_index), "Forecast date:", as.character(date_next), "\n")
    }
    
    # Generate forecasts from each model
    fc_gas <- forecast_one_gas_bonus(
      y_train = y_train,
      y_next = y_next,
      dist_model = chosen_dist_gas
    ) %>%
      mutate(
        Date = date_next,
        realized_return = y_next,
        realized_var = rv_next
      )
    
    fc_egarch <- forecast_one_rugarch_bonus(
      y_train = y_train,
      y_next = y_next,
      variance_model = "EGARCH",
      dist_model = chosen_dist_rugarch
    ) %>%
      mutate(
        Date = date_next,
        realized_return = y_next,
        realized_var = rv_next
      )
    
    fc_tgarch <- forecast_one_rugarch_bonus(
      y_train = y_train,
      y_next = y_next,
      variance_model = "TGARCH",
      dist_model = chosen_dist_rugarch
    ) %>%
      mutate(
        Date = date_next,
        realized_return = y_next,
        realized_var = rv_next
      )
    
    forecast_rows[[row_counter]] <- fc_gas
    forecast_rows[[row_counter + 1L]] <- fc_egarch
    forecast_rows[[row_counter + 2L]] <- fc_tgarch
    row_counter <- row_counter + 3L
  }
  
  rolling_long_tbl <- bind_rows(forecast_rows) %>%
    mutate(
      distribution = pretty_dist_label(distribution),
      forecast_valid = is.finite(variance_forecast),
      qlike = ifelse(forecast_valid, qlike_loss(realized_var, variance_forecast), NA_real_),
      mse = ifelse(forecast_valid, mse_loss(realized_var, variance_forecast), NA_real_)
    ) %>%
    select(
      Date, model, distribution, fit_status, forecast_valid,
      realized_return, realized_var,
      mean_forecast, sigma_forecast, variance_forecast, shape,
      log_score, qlike, mse
    )
  
  write_csv(rolling_long_tbl, file.path(bonus_dir, "tables", "rolling_forecasts_long.csv"))
  
  rolling_wide_tbl <- rolling_long_tbl %>%
    select(Date, model, realized_return, realized_var, variance_forecast, log_score, qlike, mse) %>%
    pivot_wider(
      names_from = model,
      values_from = c(variance_forecast, log_score, qlike, mse),
      names_sep = "_"
    ) %>%
    arrange(Date)
  
  write_csv(rolling_wide_tbl, file.path(bonus_dir, "tables", "rolling_forecasts_wide.csv"))
  
  rolling_validity_tbl <- rolling_long_tbl %>%
    group_by(model, distribution) %>%
    summarise(
      n_total = n(),
      n_valid = sum(forecast_valid, na.rm = TRUE),
      n_invalid = n_total - n_valid,
      share_valid = n_valid / n_total,
      max_valid_variance = safe_max_num(variance_forecast),
      p99_valid_variance = safe_quantile_num(variance_forecast, 0.99),
      median_valid_variance = safe_median_num(variance_forecast),
      mean_valid_variance = safe_mean(variance_forecast),
      .groups = "drop"
    )
  
  write_csv(rolling_validity_tbl, file.path(bonus_dir, "tables", "rolling_validity_summary.csv"))
  
  rolling_invalid_tbl <- rolling_long_tbl %>%
    filter(!forecast_valid) %>%
    select(Date, model, distribution, fit_status, mean_forecast, shape)
  
  write_csv(rolling_invalid_tbl, file.path(bonus_dir, "tables", "rolling_invalid_forecasts.csv"))
  
  # Compute out-of-sample model performance: loss functions and rankings
  forecast_eval_tbl <- rolling_long_tbl %>%
    group_by(model, distribution) %>%
    summarise(
      n_total = n(),
      n_valid = sum(forecast_valid, na.rm = TRUE),
      n_invalid = n_total - n_valid,
      avg_qlike = safe_mean(qlike),
      avg_mse = safe_mean(mse),
      avg_log_score = safe_mean(log_score),
      .groups = "drop"
    ) %>%
    mutate(
      qlike_rank = rank(avg_qlike, ties.method = "min", na.last = "keep"),
      mse_rank = rank(avg_mse, ties.method = "min", na.last = "keep"),
      log_score_rank = rank(-avg_log_score, ties.method = "min", na.last = "keep")
    ) %>%
    arrange(qlike_rank, mse_rank, log_score_rank)
  
  write_csv(
    forecast_eval_tbl,
    file.path(bonus_dir, "tables", "forecast_evaluation_summary.csv")
  )
  
  common_valid_dates <- rolling_long_tbl %>%
    group_by(Date) %>%
    summarise(
      all_models_valid = all(forecast_valid),
      .groups = "drop"
    ) %>%
    filter(all_models_valid) %>%
    pull(Date)
  
  forecast_eval_common_tbl <- rolling_long_tbl %>%
    filter(Date %in% common_valid_dates) %>%
    group_by(model, distribution) %>%
    summarise(
      n_common = n(),
      avg_qlike = safe_mean(qlike),
      avg_mse = safe_mean(mse),
      avg_log_score = safe_mean(log_score),
      .groups = "drop"
    ) %>%
    mutate(
      qlike_rank = rank(avg_qlike, ties.method = "min", na.last = "keep"),
      mse_rank = rank(avg_mse, ties.method = "min", na.last = "keep"),
      log_score_rank = rank(-avg_log_score, ties.method = "min", na.last = "keep")
    ) %>%
    arrange(qlike_rank, mse_rank, log_score_rank)
  
  write_csv(
    forecast_eval_common_tbl,
    file.path(bonus_dir, "tables", "forecast_evaluation_summary_common_dates.csv")
  )
  
  # Diebold-Mariano tests for pairwise model comparisons on QLIKE loss
  gas_egarch_losses <- paired_losses_by_date(rolling_long_tbl, "qlike", "GAS", "EGARCH")
  gas_tgarch_losses <- paired_losses_by_date(rolling_long_tbl, "qlike", "GAS", "TGARCH")
  egarch_tgarch_losses <- paired_losses_by_date(rolling_long_tbl, "qlike", "EGARCH", "TGARCH")
  
  dm_tbl <- bind_rows(
    dm_test_1step(
      loss_model_1 = gas_egarch_losses$loss_1,
      loss_model_2 = gas_egarch_losses$loss_2,
      model_1 = "GAS",
      model_2 = "EGARCH"
    ),
    dm_test_1step(
      loss_model_1 = gas_tgarch_losses$loss_1,
      loss_model_2 = gas_tgarch_losses$loss_2,
      model_1 = "GAS",
      model_2 = "TGARCH"
    ),
    dm_test_1step(
      loss_model_1 = egarch_tgarch_losses$loss_1,
      loss_model_2 = egarch_tgarch_losses$loss_2,
      model_1 = "EGARCH",
      model_2 = "TGARCH"
    )
  )
  
  write_csv(dm_tbl, file.path(bonus_dir, "tables", "dm_tests_qlike.csv"))
  
  # Create variance forecast plots with realized volatility proxy
  plot_df_models <- rolling_long_tbl %>%
    transmute(
      Date = Date,
      series = model,
      value = variance_forecast
    )
  
  realized_df <- rolling_long_tbl %>%
    group_by(Date) %>%
    summarise(
      realized_var = dplyr::first(realized_var),
      .groups = "drop"
    ) %>%
    arrange(Date)
  
  plot_df_all <- bind_rows(
    plot_df_models,
    realized_df %>%
      transmute(
        Date = Date,
        series = "Proxy",
        value = realized_var
      )
  ) %>%
    mutate(
      series = factor(series, levels = c("EGARCH", "GAS", "TGARCH", "Proxy"))
    ) %>%
    arrange(Date)
  
  # Full sample
  p_full_proxy_red <- make_variance_plot_bonus(
    plot_df = plot_df_all,
    title_txt = paste0(
      "Rolling 1-step-ahead variance forecasts (",
      pretty_dist_label(chosen_dist_simple), " distribution)"
    )
  )
  
  save_plot_bonus(
    p_full_proxy_red,
    "rolling_variance_forecasts_full_proxy_red.png",
    w = 11,
    h = 6
  )
  
  # Recent subsample
  cutoff_date <- realized_df %>%
    summarise(cutoff = Date[ceiling(0.80 * n())]) %>%
    pull(cutoff)
  
  plot_df_recent_all <- plot_df_all %>%
    filter(Date >= cutoff_date)
  
  p_recent_proxy_red <- make_variance_plot_bonus(
    plot_df = plot_df_recent_all,
    title_txt = "Rolling variance forecasts: recent subsample"
  )
  
  save_plot_bonus(
    p_recent_proxy_red,
    "rolling_variance_forecasts_recent_proxy_red.png",
    w = 11,
    h = 6
  )
  
  # Recent subsample y <= 40
  p_recent_proxy_red_y40 <- make_variance_plot_bonus(
    plot_df = plot_df_recent_all,
    title_txt = "Rolling variance forecasts: recent subsample (y-axis up to 40)",
    ymax = 40
  )
  
  save_plot_bonus(
    p_recent_proxy_red_y40,
    "rolling_variance_forecasts_recent_proxy_red_ymax40.png",
    w = 11,
    h = 6
  )
  
  # Last year y <= 20
  last_year_cutoff <- max(plot_df_all$Date, na.rm = TRUE) %m-% years(1)
  
  plot_df_last_year_all <- plot_df_all %>%
    filter(Date >= last_year_cutoff)
  
  p_last_year_proxy_red_y20 <- make_variance_plot_bonus(
    plot_df = plot_df_last_year_all,
    title_txt = "Rolling variance forecasts: last year (y-axis up to 20)",
    ymax = 20
  )
  
  save_plot_bonus(
    p_last_year_proxy_red_y20,
    "rolling_variance_forecasts_last_year_proxy_red_ymax20.png",
    w = 11,
    h = 6
  )
  
  write_log_bonus(
    "forecast_design.txt",
    c(
      "Rolling forecast design",
      paste0("Window length: ", window_length),
      paste0("Out-of-sample forecasts: ", length(oos_index)),
      paste0("First forecast date: ", min(oos_dates)),
      paste0("Last forecast date: ", max(oos_dates)),
      paste0("Chosen common distribution: ", pretty_dist_label(chosen_dist_simple)),
      "Forecasted models: GAS, EGARCH, TGARCH",
      "Primary loss: QLIKE",
      "Secondary loss: MSE on squared returns",
      "Density metric: predictive log score",
      "Statistical comparison: Diebold-Mariano tests on QLIKE loss differentials",
      "",
      "Rolling numerical-safeguard rule",
      paste0("- forecast variance floor: ", forecast_var_floor),
      paste0("- forecast variance cap: ", forecast_var_cap),
      paste0("- minimum Student t shape: ", student_shape_min)
    )
  )
  
  write_log_bonus(
    "rolling_stability.txt",
    c(
      "Rolling forecast validity summary",
      capture.output(print(rolling_validity_tbl, n = nrow(rolling_validity_tbl))),
      "",
      "Interpretation note",
      "- forecasts flagged as numerically unstable are retained in the long table as rows",
      "- but their forecast values and losses are set to missing",
      "- evaluation tables, DM tests, and plots use only valid forecasts"
    )
  )
  
  write_log_bonus(
    "rolling_forecast_note.txt",
    c(
      "Rolling forecast generation note",
      "",
      "GAS variance scale transformation:",
      "- GAS models forecast the dynamic variance state on the variance scale",
      "- rolling forecasts correctly extract and scale these to forecast variance",
      "- previous versions may have saved forecasts on incorrect scale",
      "- rolling forecast outputs should not be mixed across different versions"
    )
  )
}
