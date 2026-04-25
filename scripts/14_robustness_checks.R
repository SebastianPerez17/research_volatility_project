# Robustness checks for forecast and tail-risk conclusions

required_vars <- c(
  "run_robustness_checks", "forecast_dir", "df", "r", "tail_probs",
  "stress_quantile", "forecast_var_floor", "forecast_var_cap",
  "simple_risk_ewma_lambda", "robustness_window_lengths",
  "robustness_start_dates", "robustness_mean_specs",
  "robustness_distributions", "robustness_student_t_shape"
)
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check if previous scripts executed successfully."))
  }
}

if (!dir.exists(forecast_dir)) {
  stop(paste0("Output directory does not exist: ", forecast_dir))
}
dir.create(file.path(forecast_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(forecast_dir, "logs"), showWarnings = FALSE, recursive = TRUE)

helper_funcs <- c(
  "norm_label", "write_log_forecast", "compute_var_es_point_bonus",
  "compute_hit_indicator_bonus", "run_var_backtests_group_bonus",
  "run_es_backtests_group_bonus", "split_calm_stress_bonus",
  "qlike_loss", "mse_loss", "safe_mean"
)
missing_funcs <- helper_funcs[!vapply(helper_funcs, exists, logical(1), mode = "function")]
if (length(missing_funcs) > 0) {
  stop(paste0(
    "Missing helper functions: ", paste(missing_funcs, collapse = ", "),
    ". Check if 07_comparison_helpers.R was sourced successfully."
  ))
}

tables_dir <- file.path(forecast_dir, "tables")

robustness_design_tbl <- tibble::tribble(
  ~robustness_check, ~levels, ~why, ~implementation,
  "window length", "500, 750, 1000", "tests sensitivity to estimation window", "fast benchmark VaR/ES grid recomputed for each window; model-based rerun required for TGARCH/GAS/EGARCH alternatives",
  "start date", "2005, 2010, 2020", "tests sample dependence", "fast benchmark grid recomputed when the loaded data cover the requested start date; otherwise flagged as unavailable",
  "mean specification", "zero mean, constant mean", "tests mean specification", "EWMA and filtered historical simulation recomputed with zero and rolling-constant mean forecasts",
  "distribution", "Normal, Student t", "tests distribution dependence", "EWMA parametric VaR/ES recomputed with Normal and fixed-shape Student t innovations",
  "VaR level", "95%, 99%", "tests tail depth", "VaR/ES hit rates and backtests reported at alpha = 0.05 and 0.01",
  "sample scope", "full sample, common valid dates", "tests numerical-failure bias", "existing model and benchmark forecast losses compared on full and common-valid forecast dates"
)

write_csv(
  robustness_design_tbl,
  file.path(tables_dir, "robustness_check_design.csv")
)

if (!isTRUE(run_robustness_checks)) {
  write_log_forecast(
    "robustness_checks.txt",
    c("Robustness check stage skipped: run_robustness_checks = FALSE")
  )
} else {
  if (!all(c("Date", "Return") %in% names(df))) {
    stop('df must contain columns: "Date" and "Return".')
  }
  if (!inherits(df$Date, "Date")) {
    df <- df %>% mutate(Date = as.Date(Date))
  }
  if (!is.numeric(r) || length(r) != nrow(df)) {
    stop("r must be numeric and have the same length as nrow(df).")
  }
  if (!is.numeric(tail_probs) || length(tail_probs) < 1 || any(!is.finite(tail_probs)) ||
      any(tail_probs <= 0) || any(tail_probs >= 1)) {
    stop("tail_probs must be a non-empty numeric vector with values in (0, 1).")
  }
  if (!is.numeric(robustness_window_lengths) || length(robustness_window_lengths) < 1 ||
      any(!is.finite(robustness_window_lengths)) || any(robustness_window_lengths < 20)) {
    stop("robustness_window_lengths must contain window lengths >= 20.")
  }
  if (!inherits(robustness_start_dates, "Date")) {
    robustness_start_dates <- as.Date(robustness_start_dates)
  }
  if (!all(robustness_mean_specs %in% c("zero", "constant"))) {
    stop('robustness_mean_specs must contain only "zero" and/or "constant".')
  }
  robustness_dist_simple <- unique(vapply(robustness_distributions, norm_label, character(1)))
  if (!all(robustness_dist_simple %in% c("norm", "t"))) {
    stop('robustness_distributions must map to "norm" and/or "t".')
  }
  if (!is.numeric(robustness_student_t_shape) || length(robustness_student_t_shape) != 1 ||
      !is.finite(robustness_student_t_shape) || robustness_student_t_shape <= 2) {
    stop("robustness_student_t_shape must be a scalar greater than 2.")
  }

  clamp_variance <- function(x) {
    x <- as.numeric(x)
    ifelse(is.finite(x), pmin(pmax(x, forecast_var_floor), forecast_var_cap), NA_real_)
  }

  ewma_variance_path <- function(x, lambda) {
    x <- as.numeric(x)
    n <- length(x)
    if (n < 2L || any(!is.finite(x))) {
      return(list(sigma_train = rep(NA_real_, n), variance_next = NA_real_))
    }

    init_var <- stats::var(x)
    if (!is.finite(init_var) || init_var <= 0) {
      init_var <- mean(x^2)
    }
    init_var <- clamp_variance(init_var)

    var_train <- rep(NA_real_, n)
    var_train[1L] <- init_var
    if (n >= 2L) {
      for (i in 2L:n) {
        var_train[i] <- clamp_variance(lambda * var_train[i - 1L] + (1 - lambda) * x[i - 1L]^2)
      }
    }

    list(
      sigma_train = sqrt(var_train),
      variance_next = clamp_variance(lambda * var_train[n] + (1 - lambda) * x[n]^2)
    )
  }

  empirical_var_es <- function(sim_returns, alpha) {
    sim_returns <- as.numeric(sim_returns)
    sim_returns <- sim_returns[is.finite(sim_returns)]
    if (length(sim_returns) < 20L) {
      return(tibble(return_quantile = NA_real_, VaR = NA_real_, ES = NA_real_, var_es_note = "Insufficient data"))
    }

    q_ret <- as.numeric(stats::quantile(sim_returns, probs = alpha, na.rm = TRUE, names = FALSE, type = 7))
    var_loss <- -q_ret
    tail_losses <- -sim_returns[sim_returns <= q_ret]

    tibble(
      return_quantile = q_ret,
      VaR = var_loss,
      ES = if (length(tail_losses) > 0L) mean(tail_losses) else var_loss,
      var_es_note = NA_character_
    )
  }

  log_score_student_t <- function(y, mean_forecast, sigma_forecast, shape) {
    if (!is.finite(y) || !is.finite(mean_forecast) || !is.finite(sigma_forecast) ||
        sigma_forecast <= 0 || !is.finite(shape) || shape <= 2) {
      return(NA_real_)
    }
    t_scale <- sqrt((shape - 2) / shape)
    raw_z <- ((y - mean_forecast) / sigma_forecast) / t_scale
    stats::dt(raw_z, df = shape, log = TRUE) - log(sigma_forecast * t_scale)
  }

  compute_grid_one <- function(start_requested, window_length_use, mean_spec, dist_simple) {
    min_data_date <- min(df$Date, na.rm = TRUE)
    start_effective <- max(as.Date(start_requested), min_data_date)
    requested_before_data_year <- as.integer(format(as.Date(start_requested), "%Y")) <
      as.integer(format(min_data_date, "%Y"))
    df_use <- df %>%
      filter(Date >= start_effective) %>%
      arrange(Date)
    r_use <- df_use$Return

    base_cols <- tibble(
      start_date_requested = as.Date(start_requested),
      start_date_effective = as.Date(start_effective),
      window_length = as.integer(window_length_use),
      mean_spec = as.character(mean_spec),
      distribution_check = ifelse(dist_simple == "t", "Student t", "Normal")
    )

    if (requested_before_data_year) {
      return(base_cols %>%
        mutate(
          model = NA_character_,
          distribution = NA_character_,
          alpha = NA_real_,
          n_total = 0L,
          n_valid = 0L,
          share_valid = NA_real_,
          avg_qlike = NA_real_,
          avg_log_score = NA_real_,
          VaR_hit_rate = NA_real_,
          Kupiec_p = NA_real_,
          Christoffersen_p = NA_real_,
          ES_p = NA_real_,
          stress_hit_rate = NA_real_,
          status = "unavailable",
          note = paste0("Loaded data start at ", min_data_date, "; rerun with earlier data for this start-date check.")
        ))
    }

    if (nrow(df_use) <= window_length_use + 5L) {
      return(base_cols %>%
        mutate(
          model = NA_character_,
          distribution = NA_character_,
          alpha = NA_real_,
          n_total = nrow(df_use),
          n_valid = 0L,
          share_valid = NA_real_,
          avg_qlike = NA_real_,
          avg_log_score = NA_real_,
          VaR_hit_rate = NA_real_,
          Kupiec_p = NA_real_,
          Christoffersen_p = NA_real_,
          ES_p = NA_real_,
          stress_hit_rate = NA_real_,
          status = "unavailable",
          note = "Not enough observations for this window length."
        ))
    }

    oos_index <- seq.int(window_length_use + 1L, nrow(df_use))
    forecast_rows <- vector("list", length(oos_index) * 3L)
    tail_rows <- vector("list", length(oos_index) * length(tail_probs) * 3L)
    f_counter <- 1L
    t_counter <- 1L

    for (j in seq_along(oos_index)) {
      idx_out <- oos_index[j]
      train_idx <- seq.int(idx_out - window_length_use, idx_out - 1L)
      y_train <- r_use[train_idx]
      y_next <- r_use[idx_out]
      date_next <- df_use$Date[idx_out]

      mean_forecast <- if (identical(mean_spec, "constant")) safe_mean(y_train) else 0
      mean_forecast <- as.numeric(mean_forecast)[1]
      resid_train <- y_train - mean_forecast
      ewma_obj <- ewma_variance_path(resid_train, simple_risk_ewma_lambda)
      ewma_var <- as.numeric(ewma_obj$variance_next)[1]
      ewma_sigma <- if (is.finite(ewma_var) && ewma_var > 0) sqrt(ewma_var) else NA_real_
      hist_var <- as.numeric(clamp_variance(stats::var(resid_train)))[1]

      dist_label <- ifelse(dist_simple == "t", "Student t", "Normal")
      param_model <- paste0("EWMA ", dist_label)
      param_log_score <- if (dist_simple == "t") {
        log_score_student_t(y_next, mean_forecast, ewma_sigma, robustness_student_t_shape)
      } else if (is.finite(ewma_sigma) && ewma_sigma > 0) {
        stats::dnorm(y_next, mean = mean_forecast, sd = ewma_sigma, log = TRUE)
      } else {
        NA_real_
      }
      param_log_score <- as.numeric(param_log_score)[1]

      fc_tbl <- tibble(
        Date = date_next,
        model = c("Historical Simulation", param_model, "Filtered Historical Simulation"),
        distribution = c("Empirical", dist_label, "Filtered empirical"),
        forecast_valid = is.finite(c(hist_var, ewma_var, ewma_var)),
        realized_return = y_next,
        realized_var = y_next^2,
        mean_forecast = c(mean_forecast, mean_forecast, mean_forecast),
        sigma_forecast = sqrt(c(hist_var, ewma_var, ewma_var)),
        variance_forecast = c(hist_var, ewma_var, ewma_var),
        log_score = c(NA_real_, param_log_score, NA_real_)
      ) %>%
        mutate(
          qlike = ifelse(forecast_valid, qlike_loss(realized_var, variance_forecast), NA_real_),
          mse = ifelse(forecast_valid, mse_loss(realized_var, variance_forecast), NA_real_)
        )

      for (row_i in seq_len(nrow(fc_tbl))) {
        forecast_rows[[f_counter]] <- fc_tbl[row_i, ]
        f_counter <- f_counter + 1L
      }

      z_keep <- is.finite(resid_train) & is.finite(ewma_obj$sigma_train) & ewma_obj$sigma_train > 0
      z_hist <- resid_train[z_keep] / ewma_obj$sigma_train[z_keep]
      sim_fhs <- mean_forecast + ewma_sigma * z_hist
      sim_hs <- mean_forecast + resid_train

      for (alpha_i in tail_probs) {
        param_ve <- compute_var_es_point_bonus(
          mean_forecast = mean_forecast,
          sigma_forecast = ewma_sigma,
          alpha = alpha_i,
          distribution = dist_simple,
          shape = ifelse(dist_simple == "t", robustness_student_t_shape, NA_real_),
          loss_sign_convention = "positive_loss"
        ) %>%
          as_tibble()

        ve_tbl <- bind_rows(
          empirical_var_es(sim_hs, alpha_i),
          param_ve,
          empirical_var_es(sim_fhs, alpha_i)
        )

        for (row_i in seq_len(nrow(fc_tbl))) {
          realized_loss <- -y_next
          tail_rows[[t_counter]] <- fc_tbl[row_i, ] %>%
            mutate(
              alpha = as.numeric(alpha_i),
              realized_loss = realized_loss,
              return_quantile = ve_tbl$return_quantile[row_i],
              VaR = ve_tbl$VaR[row_i],
              ES = ve_tbl$ES[row_i],
              hit = compute_hit_indicator_bonus(realized_loss, VaR),
              var_es_note = ve_tbl$var_es_note[row_i]
            )
          t_counter <- t_counter + 1L
        }
      }
    }

    forecast_tbl <- bind_rows(forecast_rows)
    tail_tbl <- bind_rows(tail_rows)

    eval_tbl <- forecast_tbl %>%
      group_by(model, distribution) %>%
      summarise(
        n_total = n(),
        n_valid = sum(forecast_valid, na.rm = TRUE),
        share_valid = ifelse(n_total > 0, n_valid / n_total, NA_real_),
        avg_qlike = safe_mean(qlike),
        avg_log_score = safe_mean(log_score),
        .groups = "drop"
      )

    var_summary_tbl <- tail_tbl %>%
      group_by(model, distribution, alpha) %>%
      summarise(
        VaR_hit_rate = safe_mean(hit),
        .groups = "drop"
      )

    var_bt_wide <- run_var_backtests_group_bonus(
      tail_tbl,
      group_vars = c("model", "distribution", "alpha")
    ) %>%
      select(model, distribution, alpha, test, p_value) %>%
      tidyr::pivot_wider(names_from = test, values_from = p_value)

    es_bt_tbl <- run_es_backtests_group_bonus(
      tail_tbl,
      group_vars = c("model", "distribution", "alpha")
    ) %>%
      transmute(model, distribution, alpha, ES_p = p_value)

    regime_lookup <- forecast_tbl %>%
      select(Date, realized_var) %>%
      distinct() %>%
      split_calm_stress_bonus(realized_var_col = "realized_var", stress_quantile = stress_quantile) %>%
      select(Date, regime)

    stress_tbl <- tail_tbl %>%
      left_join(regime_lookup, by = "Date") %>%
      filter(regime == "stress") %>%
      group_by(model, distribution, alpha) %>%
      summarise(stress_hit_rate = safe_mean(hit), .groups = "drop")

    eval_tbl %>%
      tidyr::crossing(alpha = sort(unique(as.numeric(tail_probs)), decreasing = TRUE)) %>%
      left_join(var_summary_tbl, by = c("model", "distribution", "alpha")) %>%
      left_join(var_bt_wide, by = c("model", "distribution", "alpha")) %>%
      left_join(es_bt_tbl, by = c("model", "distribution", "alpha")) %>%
      left_join(stress_tbl, by = c("model", "distribution", "alpha")) %>%
      mutate(
        start_date_requested = as.Date(start_requested),
        start_date_effective = as.Date(start_effective),
        window_length = as.integer(window_length_use),
        mean_spec = mean_spec,
        distribution_check = ifelse(dist_simple == "t", "Student t", "Normal"),
        Kupiec_p = kupiec_uc,
        Christoffersen_p = christoffersen_independence,
        status = "computed",
        note = NA_character_
      ) %>%
      select(
        start_date_requested,
        start_date_effective,
        window_length,
        mean_spec,
        distribution_check,
        model,
        distribution,
        alpha,
        n_total,
        n_valid,
        share_valid,
        avg_qlike,
        avg_log_score,
        VaR_hit_rate,
        Kupiec_p,
        Christoffersen_p,
        ES_p,
        stress_hit_rate,
        status,
        note
      )
  }

  robustness_grid <- tidyr::crossing(
    start_date_requested = robustness_start_dates,
    window_length = as.integer(robustness_window_lengths),
    mean_spec = robustness_mean_specs,
    distribution_check = robustness_dist_simple
  )

  robustness_results_tbl <- bind_rows(lapply(seq_len(nrow(robustness_grid)), function(i) {
    compute_grid_one(
      start_requested = robustness_grid$start_date_requested[i],
      window_length_use = robustness_grid$window_length[i],
      mean_spec = robustness_grid$mean_spec[i],
      dist_simple = robustness_grid$distribution_check[i]
    )
  })) %>%
    arrange(start_date_requested, window_length, mean_spec, distribution_check, alpha, model)

  write_csv(
    robustness_results_tbl,
    file.path(tables_dir, "robustness_check_results.csv")
  )

  read_optional_table <- function(filename) {
    path <- file.path(tables_dir, filename)
    if (!file.exists(path)) return(NULL)
    read_csv(path, show_col_types = FALSE)
  }

  combined_forecast_tbl <- bind_rows(
    read_optional_table("rolling_forecasts_long.csv"),
    read_optional_table("simple_risk_benchmark_forecasts_long.csv")
  )

  if (nrow(combined_forecast_tbl) > 0 && all(c("Date", "model", "forecast_valid") %in% names(combined_forecast_tbl))) {
    common_valid_dates <- combined_forecast_tbl %>%
      group_by(Date) %>%
      summarise(all_models_valid = all(forecast_valid), .groups = "drop") %>%
      filter(all_models_valid) %>%
      pull(Date)

    common_valid_tbl <- bind_rows(
      combined_forecast_tbl %>% mutate(sample_scope = "full sample"),
      combined_forecast_tbl %>% filter(Date %in% common_valid_dates) %>% mutate(sample_scope = "common valid dates")
    ) %>%
      group_by(sample_scope, model, distribution) %>%
      summarise(
        n_total = n(),
        n_valid = sum(forecast_valid, na.rm = TRUE),
        share_valid = ifelse(n_total > 0, n_valid / n_total, NA_real_),
        avg_qlike = safe_mean(qlike),
        avg_log_score = safe_mean(log_score),
        .groups = "drop"
      ) %>%
      arrange(model, distribution, sample_scope)
  } else {
    common_valid_tbl <- tibble(
      sample_scope = character(),
      model = character(),
      distribution = character(),
      n_total = integer(),
      n_valid = integer(),
      share_valid = numeric(),
      avg_qlike = numeric(),
      avg_log_score = numeric()
    )
  }

  write_csv(
    common_valid_tbl,
    file.path(tables_dir, "robustness_common_valid_dates.csv")
  )

  write_log_forecast(
    "robustness_checks.txt",
    c(
      "Robustness checks completed.",
      "Outputs:",
      "- tables/robustness_check_design.csv",
      "- tables/robustness_check_results.csv",
      "- tables/robustness_common_valid_dates.csv",
      paste0("Rows in robustness_check_results.csv: ", nrow(robustness_results_tbl)),
      paste0("Rows in robustness_common_valid_dates.csv: ", nrow(common_valid_tbl))
    )
  )
}
