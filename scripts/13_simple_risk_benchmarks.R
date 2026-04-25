# Simple risk benchmarks: historical simulation, EWMA, and filtered historical simulation

# Validate required variables from config
required_vars <- c(
  "run_rolling", "run_tail_risk", "run_simple_risk_benchmarks",
  "run_filtered_historical_simulation", "tail_probs", "loss_sign_convention",
  "stress_quantile", "forecast_dir", "window_length", "forecast_var_floor",
  "forecast_var_cap", "simple_risk_ewma_lambda", "df", "r"
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
  "norm_label", "pretty_dist_label", "write_log_forecast",
  "tail_alpha_label_bonus", "compute_var_es_point_bonus",
  "compute_hit_indicator_bonus", "summarize_var_es_by_group_bonus",
  "run_var_backtests_group_bonus", "run_es_backtests_group_bonus",
  "split_calm_stress_bonus", "qlike_loss", "mse_loss", "safe_mean",
  "safe_max_num", "safe_quantile_num", "safe_median_num"
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
if (!is.logical(run_simple_risk_benchmarks) || length(run_simple_risk_benchmarks) != 1 ||
    is.na(run_simple_risk_benchmarks)) {
  stop("run_simple_risk_benchmarks must be TRUE or FALSE.")
}
if (!is.logical(run_filtered_historical_simulation) ||
    length(run_filtered_historical_simulation) != 1 ||
    is.na(run_filtered_historical_simulation)) {
  stop("run_filtered_historical_simulation must be TRUE or FALSE.")
}
if (!is.numeric(simple_risk_ewma_lambda) || length(simple_risk_ewma_lambda) != 1 ||
    !is.finite(simple_risk_ewma_lambda) ||
    simple_risk_ewma_lambda <= 0 || simple_risk_ewma_lambda >= 1) {
  stop("simple_risk_ewma_lambda must be a scalar in (0, 1).")
}

tables_dir <- file.path(forecast_dir, "tables")

if (!isTRUE(run_rolling)) {
  write_log_forecast(
    "simple_risk_benchmarks.txt",
    c("Simple risk benchmark stage skipped: run_rolling = FALSE")
  )
} else if (!isTRUE(run_tail_risk)) {
  write_log_forecast(
    "simple_risk_benchmarks.txt",
    c("Simple risk benchmark stage skipped: run_tail_risk = FALSE")
  )
} else if (!isTRUE(run_simple_risk_benchmarks)) {
  write_log_forecast(
    "simple_risk_benchmarks.txt",
    c("Simple risk benchmark stage skipped: run_simple_risk_benchmarks = FALSE")
  )
} else {
  if (!is.numeric(tail_probs) || length(tail_probs) < 1 || any(!is.finite(tail_probs)) ||
      any(tail_probs <= 0) || any(tail_probs >= 1)) {
    stop("tail_probs must be a non-empty numeric vector with values in (0, 1).")
  }
  tail_probs <- sort(unique(as.numeric(tail_probs)), decreasing = TRUE)

  if (!is.character(loss_sign_convention) || length(loss_sign_convention) != 1 ||
      is.na(loss_sign_convention) || !identical(loss_sign_convention, "positive_loss")) {
    stop('loss_sign_convention must be exactly "positive_loss".')
  }
  if (!all(c("Date", "Return") %in% names(df))) {
    stop('df must contain columns: "Date" and "Return".')
  }
  if (!is.numeric(r) || length(r) != nrow(df)) {
    stop("r must be numeric and have the same length as nrow(df).")
  }
  if (nrow(df) <= window_length + 5L) {
    stop("Not enough observations for the requested rolling window.")
  }

  clamp_variance <- function(x) {
    x <- as.numeric(x)
    ifelse(
      is.finite(x),
      pmin(pmax(x, forecast_var_floor), forecast_var_cap),
      NA_real_
    )
  }

  sample_variance_forecast <- function(y_train) {
    y_train <- as.numeric(y_train)
    y_train <- y_train[is.finite(y_train)]
    if (length(y_train) < 2L) return(NA_real_)
    clamp_variance(stats::var(y_train))
  }

  ewma_variance_path <- function(y_train, lambda) {
    y_train <- as.numeric(y_train)
    n <- length(y_train)
    if (n < 2L || any(!is.finite(y_train))) {
      return(list(sigma_train = rep(NA_real_, n), variance_next = NA_real_))
    }

    init_var <- stats::var(y_train)
    if (!is.finite(init_var) || init_var <= 0) {
      init_var <- mean(y_train^2)
    }
    init_var <- clamp_variance(init_var)

    var_train <- rep(NA_real_, n)
    var_train[1L] <- init_var
    if (n >= 2L) {
      for (i in 2L:n) {
        var_train[i] <- clamp_variance(lambda * var_train[i - 1L] + (1 - lambda) * y_train[i - 1L]^2)
      }
    }

    variance_next <- clamp_variance(lambda * var_train[n] + (1 - lambda) * y_train[n]^2)
    list(
      sigma_train = sqrt(var_train),
      variance_next = variance_next
    )
  }

  empirical_var_es <- function(y_train, alpha) {
    y_train <- as.numeric(y_train)
    y_train <- y_train[is.finite(y_train)]
    if (length(y_train) < 20L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
      return(tibble(return_quantile = NA_real_, VaR = NA_real_, ES = NA_real_, var_es_note = "Insufficient data"))
    }

    q_ret <- as.numeric(quantile(y_train, probs = alpha, na.rm = TRUE, names = FALSE, type = 7))
    var_loss <- -q_ret
    loss_train <- -y_train
    tail_losses <- loss_train[is.finite(loss_train) & loss_train >= var_loss]
    es_loss <- if (length(tail_losses) > 0L) mean(tail_losses) else var_loss

    tibble(
      return_quantile = q_ret,
      VaR = var_loss,
      ES = as.numeric(es_loss),
      var_es_note = NA_character_
    )
  }

  filtered_empirical_var_es <- function(y_train, sigma_train, sigma_next, alpha) {
    y_train <- as.numeric(y_train)
    sigma_train <- as.numeric(sigma_train)
    keep <- is.finite(y_train) & is.finite(sigma_train) & sigma_train > 0
    z <- y_train[keep] / sigma_train[keep]
    z <- z[is.finite(z)]

    if (length(z) < 20L || !is.finite(sigma_next) || sigma_next <= 0 ||
        !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
      return(tibble(return_quantile = NA_real_, VaR = NA_real_, ES = NA_real_, var_es_note = "Insufficient data"))
    }

    q_z <- as.numeric(quantile(z, probs = alpha, na.rm = TRUE, names = FALSE, type = 7))
    q_ret <- sigma_next * q_z
    var_loss <- -q_ret
    tail_losses_std <- -z[z <= q_z]
    es_loss <- if (length(tail_losses_std) > 0L) sigma_next * mean(tail_losses_std) else var_loss

    tibble(
      return_quantile = q_ret,
      VaR = var_loss,
      ES = as.numeric(es_loss),
      var_es_note = NA_character_
    )
  }

  make_forecast_row <- function(model, distribution, date_next, y_next, variance_forecast,
                                mean_forecast = 0, fit_status = "OK") {
    variance_forecast <- clamp_variance(variance_forecast)
    forecast_valid <- is.finite(variance_forecast) && variance_forecast > 0
    sigma_forecast <- if (forecast_valid) sqrt(variance_forecast) else NA_real_
    log_score <- if (forecast_valid && identical(distribution, "Normal")) {
      stats::dnorm(y_next, mean = mean_forecast, sd = sigma_forecast, log = TRUE)
    } else {
      NA_real_
    }

    tibble(
      Date = date_next,
      model = model,
      distribution = distribution,
      fit_status = fit_status,
      forecast_valid = forecast_valid,
      realized_return = y_next,
      realized_var = y_next^2,
      mean_forecast = mean_forecast,
      sigma_forecast = sigma_forecast,
      variance_forecast = variance_forecast,
      shape = NA_real_,
      log_score = log_score,
      qlike = ifelse(forecast_valid, qlike_loss(y_next^2, variance_forecast), NA_real_),
      mse = ifelse(forecast_valid, mse_loss(y_next^2, variance_forecast), NA_real_)
    )
  }

  oos_index <- seq.int(from = window_length + 1L, to = nrow(df))
  forecast_rows <- vector("list", length(oos_index) * ifelse(run_filtered_historical_simulation, 3L, 2L))
  tail_rows <- vector("list", length(oos_index) * length(tail_probs) * ifelse(run_filtered_historical_simulation, 3L, 2L))
  forecast_counter <- 1L
  tail_counter <- 1L

  for (j in seq_along(oos_index)) {
    idx_out <- oos_index[j]
    train_idx <- seq.int(from = idx_out - window_length, to = idx_out - 1L)
    y_train <- r[train_idx]
    y_next <- r[idx_out]
    date_next <- df$Date[idx_out]

    ewma_obj <- ewma_variance_path(y_train, simple_risk_ewma_lambda)
    ewma_var_next <- ewma_obj$variance_next
    ewma_sigma_next <- if (is.finite(ewma_var_next) && ewma_var_next > 0) sqrt(ewma_var_next) else NA_real_

    benchmark_rows <- list(
      make_forecast_row(
        model = "Historical Simulation",
        distribution = "Empirical",
        date_next = date_next,
        y_next = y_next,
        variance_forecast = sample_variance_forecast(y_train),
        mean_forecast = safe_mean(y_train)
      ),
      make_forecast_row(
        model = "EWMA",
        distribution = "Normal",
        date_next = date_next,
        y_next = y_next,
        variance_forecast = ewma_var_next,
        mean_forecast = 0
      )
    )

    if (isTRUE(run_filtered_historical_simulation)) {
      benchmark_rows[[length(benchmark_rows) + 1L]] <- make_forecast_row(
        model = "Filtered Historical Simulation",
        distribution = "Filtered empirical",
        date_next = date_next,
        y_next = y_next,
        variance_forecast = ewma_var_next,
        mean_forecast = 0
      )
    }

    for (row_i in benchmark_rows) {
      forecast_rows[[forecast_counter]] <- row_i
      forecast_counter <- forecast_counter + 1L
    }

    for (alpha_i in tail_probs) {
      hs_var_es <- empirical_var_es(y_train, alpha_i)
      ewma_var_es <- compute_var_es_point_bonus(
        mean_forecast = 0,
        sigma_forecast = ewma_sigma_next,
        alpha = alpha_i,
        distribution = "Normal",
        shape = NA_real_,
        loss_sign_convention = loss_sign_convention
      ) %>%
        as_tibble()

      tail_specs <- list(
        list(model = "Historical Simulation", distribution = "Empirical", values = hs_var_es),
        list(model = "EWMA", distribution = "Normal", values = ewma_var_es)
      )

      if (isTRUE(run_filtered_historical_simulation)) {
        tail_specs[[length(tail_specs) + 1L]] <- list(
          model = "Filtered Historical Simulation",
          distribution = "Filtered empirical",
          values = filtered_empirical_var_es(y_train, ewma_obj$sigma_train, ewma_sigma_next, alpha_i)
        )
      }

      for (spec in tail_specs) {
        fc_row <- bind_rows(benchmark_rows) %>%
          filter(model == spec$model) %>%
          slice(1)
        realized_loss <- -y_next
        tail_rows[[tail_counter]] <- fc_row %>%
          mutate(
            alpha = as.numeric(alpha_i),
            realized_loss = realized_loss,
            return_quantile = spec$values$return_quantile[1],
            VaR = spec$values$VaR[1],
            ES = spec$values$ES[1],
            hit = compute_hit_indicator_bonus(realized_loss = realized_loss, var_loss = VaR),
            var_es_note = spec$values$var_es_note[1]
          ) %>%
          select(
            Date, model, distribution, alpha,
            fit_status, forecast_valid,
            realized_return, realized_loss, realized_var,
            mean_forecast, sigma_forecast, variance_forecast, shape,
            log_score, qlike, mse,
            return_quantile, VaR, ES, hit, var_es_note
          )
        tail_counter <- tail_counter + 1L
      }
    }
  }

  simple_forecasts_tbl <- bind_rows(forecast_rows) %>%
    arrange(Date, model)

  write_csv(
    simple_forecasts_tbl,
    file.path(tables_dir, "simple_risk_benchmark_forecasts_long.csv")
  )

  simple_validity_tbl <- simple_forecasts_tbl %>%
    group_by(model, distribution) %>%
    summarise(
      n_total = n(),
      n_valid = sum(forecast_valid, na.rm = TRUE),
      n_invalid = n_total - n_valid,
      share_valid = ifelse(n_total > 0, n_valid / n_total, NA_real_),
      max_valid_variance = safe_max_num(variance_forecast[forecast_valid]),
      p99_valid_variance = safe_quantile_num(variance_forecast[forecast_valid], 0.99),
      median_valid_variance = safe_median_num(variance_forecast[forecast_valid]),
      mean_valid_variance = safe_mean(variance_forecast[forecast_valid]),
      .groups = "drop"
    )

  write_csv(
    simple_validity_tbl,
    file.path(tables_dir, "simple_risk_benchmark_validity_summary.csv")
  )

  simple_eval_tbl <- simple_forecasts_tbl %>%
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
    simple_eval_tbl,
    file.path(tables_dir, "simple_risk_benchmark_evaluation_summary.csv")
  )

  simple_tail_forecasts_tbl <- bind_rows(tail_rows) %>%
    arrange(Date, model, alpha)

  write_csv(
    simple_tail_forecasts_tbl,
    file.path(tables_dir, "simple_risk_benchmark_tail_forecasts_long.csv")
  )

  simple_tail_forecasts_wide_tbl <- simple_tail_forecasts_tbl %>%
    mutate(alpha_label = tail_alpha_label_bonus(alpha)) %>%
    select(Date, model, distribution, realized_return, realized_loss, alpha_label, VaR, ES, hit) %>%
    pivot_wider(
      names_from = alpha_label,
      values_from = c(VaR, ES, hit),
      names_sep = "_"
    ) %>%
    arrange(Date, model)

  write_csv(
    simple_tail_forecasts_wide_tbl,
    file.path(tables_dir, "simple_risk_benchmark_tail_forecasts_wide.csv")
  )

  simple_counts_tbl <- simple_forecasts_tbl %>%
    group_by(model) %>%
    summarise(
      n_total_oos = n(),
      n_valid_forecasts = sum(forecast_valid, na.rm = TRUE),
      n_invalid_forecasts = n_total_oos - n_valid_forecasts,
      share_valid = ifelse(n_total_oos > 0, n_valid_forecasts / n_total_oos, NA_real_),
      .groups = "drop"
    )

  simple_var_es_calibration_tbl <- simple_tail_forecasts_tbl %>%
    group_by(model, alpha) %>%
    summarise(
      n_exceptions = sum(hit == 1, na.rm = TRUE),
      avg_VaR = safe_mean(VaR),
      avg_ES = safe_mean(ES),
      avg_realized_loss = safe_mean(realized_loss),
      avg_realized_loss_exceptions = safe_mean(realized_loss[hit == 1]),
      .groups = "drop"
    )

  simple_var_es_summary_tbl <- simple_var_es_calibration_tbl %>%
    left_join(simple_counts_tbl, by = "model") %>%
    mutate(
      observed_hit_rate_valid_only = ifelse(n_valid_forecasts > 0, n_exceptions / n_valid_forecasts, NA_real_),
      exception_rate_full_oos = ifelse(n_total_oos > 0, n_exceptions / n_total_oos, NA_real_),
      expected_hit_rate = alpha,
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
    simple_var_es_summary_tbl,
    file.path(tables_dir, "simple_risk_benchmark_var_es_summary.csv")
  )

  simple_var_backtests_tbl <- run_var_backtests_group_bonus(
    simple_tail_forecasts_tbl,
    group_vars = c("model", "alpha")
  ) %>%
    select(model, alpha, test, statistic, p_value, decision_note, n, n_exceptions, observed_hit_rate) %>%
    arrange(alpha, model, test)

  write_csv(
    simple_var_backtests_tbl,
    file.path(tables_dir, "simple_risk_benchmark_var_backtests.csv")
  )

  simple_es_backtests_tbl <- run_es_backtests_group_bonus(
    simple_tail_forecasts_tbl,
    group_vars = c("model", "alpha")
  ) %>%
    select(model, alpha, test, statistic, p_value, decision_note, n, n_exceptions, observed_hit_rate, mean_exceedance_residual) %>%
    arrange(alpha, model)

  write_csv(
    simple_es_backtests_tbl,
    file.path(tables_dir, "simple_risk_benchmark_es_backtests.csv")
  )

  regime_lookup <- simple_forecasts_tbl %>%
    select(Date, realized_var) %>%
    distinct() %>%
    arrange(Date) %>%
    split_calm_stress_bonus(realized_var_col = "realized_var", stress_quantile = stress_quantile) %>%
    select(Date, regime, stress_threshold)

  simple_tail_regime_tbl <- simple_tail_forecasts_tbl %>%
    left_join(regime_lookup %>% select(Date, regime), by = "Date")

  simple_regime_counts_tbl <- simple_forecasts_tbl %>%
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

  simple_regime_summary_tbl <- summarize_var_es_by_group_bonus(
    simple_tail_regime_tbl %>% filter(!is.na(regime)),
    group_vars = c("model", "alpha", "regime")
  ) %>%
    left_join(simple_regime_counts_tbl, by = c("model", "regime"))

  simple_var_backtests_regime_tbl <- run_var_backtests_group_bonus(
    simple_tail_regime_tbl %>% filter(!is.na(regime)),
    group_vars = c("model", "alpha", "regime")
  )

  simple_es_backtests_regime_tbl <- run_es_backtests_group_bonus(
    simple_tail_regime_tbl %>% filter(!is.na(regime)),
    group_vars = c("model", "alpha", "regime")
  )

  simple_var_backtests_regime_wide <- simple_var_backtests_regime_tbl %>%
    select(model, alpha, regime, test, p_value, decision_note) %>%
    pivot_wider(
      names_from = test,
      values_from = c(p_value, decision_note),
      names_glue = "{test}_{.value}"
    )

  simple_es_backtests_regime_wide <- simple_es_backtests_regime_tbl %>%
    select(model, alpha, regime, p_value, decision_note, statistic) %>%
    rename(
      mcneil_frey_es_p_value = p_value,
      mcneil_frey_es_decision_note = decision_note,
      mcneil_frey_es_statistic = statistic
    )

  simple_backtests_by_regime_tbl <- simple_regime_summary_tbl %>%
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
    left_join(simple_var_backtests_regime_wide, by = c("model", "alpha", "regime")) %>%
    left_join(simple_es_backtests_regime_wide, by = c("model", "alpha", "regime")) %>%
    arrange(alpha, model, regime)

  write_csv(
    simple_backtests_by_regime_tbl,
    file.path(tables_dir, "simple_risk_benchmark_backtests_by_regime.csv")
  )

  write_log_forecast(
    "simple_risk_benchmarks.txt",
    c(
      "Simple risk benchmark stage completed.",
      paste0(
        "Models: ",
        paste(
          c(
            "Historical Simulation",
            "EWMA",
            if (isTRUE(run_filtered_historical_simulation)) "Filtered Historical Simulation" else character()
          ),
          collapse = ", "
        )
      ),
      paste0("Filtered Historical Simulation enabled: ", run_filtered_historical_simulation),
      paste0("Rolling window length: ", window_length),
      paste0("EWMA lambda: ", simple_risk_ewma_lambda),
      paste0("Tail probabilities: ", paste(tail_probs, collapse = ", ")),
      paste0("Rows in simple_risk_benchmark_forecasts_long.csv: ", nrow(simple_forecasts_tbl)),
      paste0("Rows in simple_risk_benchmark_tail_forecasts_long.csv: ", nrow(simple_tail_forecasts_tbl))
    )
  )
}
