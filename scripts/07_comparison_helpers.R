# Score-driven / forecast-comparison setup

# Validate required variables from config
required_vars <- c(
  "results_root", "bonus_subdir", "forecast_dist_override", "install_if_missing",
  "req_pkgs_bonus", "window_length", "run_rolling", "forecast_var_floor",
  "forecast_var_cap", "student_shape_min", "min_diag_obs_buffer",
  "gas_scaling", "require_valid_gas_hessian_for_se", "archlm_lags_check"
)
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check 00_config.R"))
  }
}

# Validate forecast parameter constraints
if (!is.numeric(forecast_var_floor) || length(forecast_var_floor) != 1 || !is.finite(forecast_var_floor) ||
    forecast_var_floor < 0) {
  stop("forecast_var_floor must be a non-negative number")
}
if (!is.numeric(forecast_var_cap) || length(forecast_var_cap) != 1 || !is.finite(forecast_var_cap) ||
    forecast_var_cap <= forecast_var_floor) {
  stop("forecast_var_cap must be > forecast_var_floor")
}
if (!is.numeric(student_shape_min) || length(student_shape_min) != 1 || !is.finite(student_shape_min) ||
    student_shape_min <= 2) {
  stop("student_shape_min must be > 2 for valid Student t distribution")
}
if (!is.numeric(min_diag_obs_buffer) || length(min_diag_obs_buffer) != 1 || !is.finite(min_diag_obs_buffer) ||
    min_diag_obs_buffer < 0 || min_diag_obs_buffer != as.integer(min_diag_obs_buffer)) {
  stop("min_diag_obs_buffer must be a non-negative integer")
}
if (!is.character(gas_scaling) || length(gas_scaling) != 1 || is.na(gas_scaling) || !nzchar(gas_scaling)) {
  stop("gas_scaling must be a non-empty character scalar")
}
if (!is.logical(require_valid_gas_hessian_for_se) || length(require_valid_gas_hessian_for_se) != 1 || is.na(require_valid_gas_hessian_for_se)) {
  stop("require_valid_gas_hessian_for_se must be TRUE or FALSE")
}
if (!is.numeric(window_length) || length(window_length) != 1 || !is.finite(window_length) ||
    window_length < 1 || window_length != as.integer(window_length)) {
  stop("window_length must be a single positive integer")
}
if (!is.logical(run_rolling) || length(run_rolling) != 1 || is.na(run_rolling)) {
  stop("run_rolling must be TRUE or FALSE")
}
if (!is.numeric(archlm_lags_check) || length(archlm_lags_check) != 2 ||
    any(!is.finite(archlm_lags_check)) || any(archlm_lags_check < 1) ||
    any(archlm_lags_check != as.integer(archlm_lags_check)) ||
    length(unique(as.integer(archlm_lags_check))) != 2) {
  stop("archlm_lags_check must be two distinct positive integers")
}

archlm_lag_1 <- as.integer(archlm_lags_check[1L])
archlm_lag_2 <- as.integer(archlm_lags_check[2L])
archlm_col_1 <- paste0("archlm_p_lag", archlm_lag_1)
archlm_col_2 <- paste0("archlm_p_lag", archlm_lag_2)

output_dir <- results_root

# Fallback settings if benchmark objects do not already exist
if (!exists("ticker")) ticker <- "^GSPC"
if (!exists("start_date")) start_date <- as.Date("2005-01-03")
if (!exists("end_date")) end_date <- as.Date("2025-12-03")
if (!exists("return_scale")) return_scale <- 100
if (!exists("ac_lag")) ac_lag <- 20
if (!is.numeric(ac_lag) || length(ac_lag) != 1 || !is.finite(ac_lag) ||
    ac_lag < 1 || ac_lag != as.integer(ac_lag)) {
  stop("ac_lag must be a single positive integer")
}

# Validate distribution override parameter
if (!is.null(forecast_dist_override)) {
  if (length(forecast_dist_override) != 1 || is.na(forecast_dist_override)) {
    stop('forecast_dist_override must be NULL, "norm", "t", or "std".')
  }
  forecast_dist_override <- tolower(as.character(forecast_dist_override))
  if (!forecast_dist_override %in% c("norm", "t", "std")) {
    stop('forecast_dist_override must be NULL, "norm", "t", or "std".')
  }
}


if (isTRUE(install_if_missing)) {
  ip <- rownames(installed.packages())
  for (p in req_pkgs_bonus) {
    if (!p %in% ip) install.packages(p, dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  library(quantmod)
  library(xts); library(zoo)
  library(dplyr); library(tibble); library(readr); library(lubridate); library(tidyr)
  library(ggplot2)
  library(lmtest); library(FinTS)
  library(rugarch)
  library(gasmodel)
})

# Validate GAS scaling argument early to avoid late fit failures
gas_scaling_check_fn <- tryCatch(getFromNamespace("check_my_scaling", "gasmodel"), error = function(e) NULL)
if (!is.null(gas_scaling_check_fn)) {
  checked_scaling <- tryCatch(gas_scaling_check_fn(gas_scaling), error = function(e) e)
  if (inherits(checked_scaling, "error")) {
    stop(paste0(
      "Invalid gas_scaling value: \"", gas_scaling, "\". Allowed values are: ",
      paste(
        c(
          "unit", "fisher_inv", "fisher_inv_sqrt", "diag_fisher_inv",
          "diag_fisher_inv_sqrt", "full_fisher_inv", "full_fisher_inv_sqrt"
        ),
        collapse = ", "
      ),
      "."
    ))
  }
  gas_scaling <- as.character(checked_scaling)
}

# Create output directory structure

bonus_dir <- file.path(output_dir, bonus_subdir)
dir.create(bonus_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(bonus_dir, "plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(bonus_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(bonus_dir, "logs"), showWarnings = FALSE, recursive = TRUE)

# Log writer for bonus analysis
write_log_bonus <- function(filename, lines) {
  writeLines(as.character(lines), con = file.path(bonus_dir, "logs", filename))
}

# Plot saver for bonus analysis
save_plot_bonus <- function(p, filename, w = 10, h = 6) {
  ggsave(
    filename = file.path(bonus_dir, "plots", filename),
    plot = p,
    width = w,
    height = h,
    dpi = 300
  )
}

# Safe name function: replace non-alphanumeric characters
safe_name_bonus <- function(x) {
  gsub("[^A-Za-z0-9_\\-]+", "_", x)
}

# Reuse or reload data if needed

if (!exists("df") || !all(c("Date", "Price", "LogPrice", "Return") %in% names(df))) {
  px_xts <- tryCatch(
    getSymbols(
      Symbols = ticker,
      src = "yahoo",
      from = start_date,
      to = end_date,
      auto.assign = FALSE
    ),
    error = function(e) e
  )
  
  if (inherits(px_xts, "error")) {
    stop(paste0("Yahoo download failed in bonus script: ", px_xts$message))
  }
  
  adj <- Ad(px_xts)
  colnames(adj) <- "AdjClose"
  
  df <- tibble(
    Date = as.Date(index(adj)),
    Price = as.numeric(coredata(adj))
  ) %>%
    arrange(Date) %>%
    filter(Date >= start_date, Date <= end_date) %>%
    mutate(
      LogPrice = log(Price),
      Return = return_scale * (LogPrice - dplyr::lag(LogPrice))
    ) %>%
    filter(is.finite(Return), is.finite(LogPrice), is.finite(Price))
}

if (!exists("r")) r <- df$Return

# Validate data integrity
if (!is.numeric(r)) {
  stop("r must be a numeric vector of returns.")
}
if (!all(c("Date", "Price", "LogPrice", "Return") %in% names(df))) {
  stop('df must contain columns: "Date", "Price", "LogPrice", "Return".')
}
if (length(r) != nrow(df)) {
  stop("Inconsistent objects: length(r) != nrow(df). Use a fresh R session.")
}
if (!isTRUE(all.equal(as.numeric(r), as.numeric(df$Return), tolerance = 0))) {
  stop("Inconsistent objects: r is not exactly equal to df$Return. Use a fresh R session.")
}
if (!all(is.finite(r))) {
  stop("r contains non-finite values.")
}
if (!inherits(df$Date, "Date")) {
  stop("df$Date must be of class Date.")
}
if (nrow(df) <= window_length + 5L && isTRUE(run_rolling)) {
  stop("Not enough observations for the requested rolling window.")
}

write_log_bonus(
  "bonus_data_info.txt",
  c(
    paste0("Ticker: ", ticker),
    paste0("Start date setting: ", start_date),
    paste0("End date setting: ", end_date),
    paste0("Return observations available: ", length(r)),
    paste0("First return date in df: ", min(df$Date)),
    paste0("Last return date in df: ", max(df$Date))
  )
)

# Helper functions for distribution normalization and formatting
norm_label <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("norm", "Normal") ~ "norm",
    x %in% c("t", "std", "Student t", "student", "student_t") ~ "t",
    TRUE ~ x
  )
}

pretty_dist_label <- function(x) {
  nx <- norm_label(x)
  dplyr::case_when(
    nx == "t" ~ "Student t",
    nx == "norm" ~ "Normal",
    TRUE ~ as.character(x)
  )
}

rugarch_dist_from_simple <- function(x) {
  ifelse(norm_label(x) == "t", "std", "norm")
}

# Safe statistical functions: return NA_real_ if no valid data
safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_quantile_num <- function(x, prob) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
}

safe_max_num <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

safe_median_num <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  median(x)
}

first_num_or_na <- function(x) {
  if (length(x) == 0) return(NA_real_)
  as.numeric(x[1])
}

best_dist_by_ic <- function(dist_vec, ic_vec) {
  ok <- is.finite(ic_vec) & !is.na(dist_vec)
  if (!any(ok)) return(NA_character_)
  as.character(dist_vec[ok][which.min(ic_vec[ok])][1])
}

append_status <- function(status_txt, extra_txt) {
  if (is.na(extra_txt) || !nzchar(extra_txt)) return(status_txt)
  paste0(status_txt, " | ", extra_txt)
}

make_diag_row_bonus <- function(model_name, dist_name, lb_resid_p, lb_resid2_p, archlm_p_1, archlm_p_2, status_txt) {
  out <- tibble(
    model = model_name,
    distribution = dist_name,
    lb_resid_p = as.numeric(lb_resid_p),
    lb_resid2_p = as.numeric(lb_resid2_p),
    status = as.character(status_txt)
  )
  out[[archlm_col_1]] <- as.numeric(archlm_p_1)
  out[[archlm_col_2]] <- as.numeric(archlm_p_2)
  out %>%
    select(model, distribution, lb_resid_p, lb_resid2_p, all_of(archlm_col_1), all_of(archlm_col_2), status)
}

ljung_box_p_bonus <- function(x, lag = 20) {
  stats::Box.test(x, lag = lag, type = "Ljung-Box")$p.value
}

qlike_loss <- function(realized_var, forecast_var, eps = 1e-12) {
  rv <- pmax(realized_var, eps)
  fv <- pmax(forecast_var, eps)
  rv / fv - log(rv / fv) - 1
}

mse_loss <- function(realized_var, forecast_var) {
  (realized_var - forecast_var)^2
}

dm_test_1step <- function(loss_model_1, loss_model_2, model_1, model_2) {
  d <- loss_model_1 - loss_model_2
  d <- d[is.finite(d)]
  
  n <- length(d)
  if (n < 5) {
    return(tibble(
      comparison = paste(model_1, "vs", model_2),
      n = n,
      mean_loss_diff = NA_real_,
      dm_statistic = NA_real_,
      p_value = NA_real_,
      preferred_model = NA_character_,
      note = "Too few paired non-missing observations"
    ))
  }
  
  gamma0 <- stats::var(d)
  if (!is.finite(gamma0) || gamma0 <= 0) {
    return(tibble(
      comparison = paste(model_1, "vs", model_2),
      n = n,
      mean_loss_diff = mean(d),
      dm_statistic = NA_real_,
      p_value = NA_real_,
      preferred_model = ifelse(mean(d) < 0, model_1, model_2),
      note = "Variance of loss differential not positive"
    ))
  }
  
  dm_raw <- mean(d) / sqrt(gamma0 / n)
  
  h <- 1
  adj <- sqrt((n + 1 - 2 * h + h * (h - 1) / n) / n)
  dm_stat <- adj * dm_raw
  p_val <- 2 * pt(-abs(dm_stat), df = n - 1)
  
  tibble(
    comparison = paste(model_1, "vs", model_2),
    n = n,
    mean_loss_diff = mean(d),
    dm_statistic = dm_stat,
    p_value = p_val,
    preferred_model = ifelse(mean(d) < 0, model_1, model_2),
    note = "Negative mean_loss_diff favors the first model"
  )
}

paired_losses_by_date <- function(rolling_tbl, loss_col, model_1, model_2) {
  paired <- rolling_tbl %>%
    filter(model %in% c(model_1, model_2)) %>%
    transmute(Date, model, loss = .data[[loss_col]]) %>%
    distinct(Date, model, .keep_all = TRUE) %>%
    pivot_wider(names_from = model, values_from = loss) %>%
    arrange(Date)
  
  loss_1 <- if (model_1 %in% names(paired)) paired[[model_1]] else rep(NA_real_, nrow(paired))
  loss_2 <- if (model_2 %in% names(paired)) paired[[model_2]] else rep(NA_real_, nrow(paired))
  
  list(loss_1 = loss_1, loss_2 = loss_2)
}

save_acf_png <- function(x, main, filename, lag = 20) {
  png(file.path(bonus_dir, "plots", filename), width = 1200, height = 800)
  on.exit(dev.off(), add = TRUE)
  acf(x, main = main, lag.max = lag)
}

save_qq_plot_assumed <- function(z, dist_label, shape = NA_real_, main, filename) {
  z <- z[is.finite(z)]
  if (length(z) < 10) return(invisible(NULL))
  
  p <- ppoints(length(z))
  z_sorted <- sort(z)
  
  if (norm_label(dist_label) == "t" && is.finite(shape) && shape > 2) {
    theo <- qt(p, df = shape) * sqrt((shape - 2) / shape)
    xlab_txt <- paste0("Theoretical standardized t quantiles (df=", round(shape, 3), ")")
  } else {
    theo <- qnorm(p)
    xlab_txt <- "Theoretical Normal quantiles"
  }
  
  qq_df <- tibble(theoretical = theo, sample = z_sorted)
  
  p_qq <- ggplot(qq_df, aes(x = theoretical, y = sample)) +
    geom_point(size = 1.1, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1) +
    labs(title = main, x = xlab_txt, y = "Sample quantiles") +
    theme_minimal()
  
  save_plot_bonus(p_qq, filename, w = 8, h = 6)
  invisible(NULL)
}

compute_ic_common_bonus <- function(loglik, k, n) {
  if (!is.finite(loglik) || !is.finite(k) || !is.finite(n) || n <= 0 || k < 0) {
    return(tibble(
      logLik = NA_real_,
      n_obs = NA_integer_,
      n_params = NA_integer_,
      AIC_total = NA_real_,
      BIC_total = NA_real_,
      AIC_per_obs = NA_real_,
      BIC_per_obs = NA_real_
    ))
  }
  
  aic_total <- -2 * loglik + 2 * k
  bic_total <- -2 * loglik + log(n) * k
  
  tibble(
    logLik = as.numeric(loglik),
    n_obs = as.integer(n),
    n_params = as.integer(k),
    AIC_total = as.numeric(aic_total),
    BIC_total = as.numeric(bic_total),
    AIC_per_obs = as.numeric(aic_total / n),
    BIC_per_obs = as.numeric(bic_total / n)
  )
}

validate_forecast_bonus <- function(var_fc, sigma_fc, dist_model, shape_fc) {
  reasons <- character(0)
  
  if (!is.finite(sigma_fc) || sigma_fc <= 0) {
    reasons <- c(reasons, "INVALID SIGMA")
  }
  if (!is.finite(var_fc) || var_fc <= forecast_var_floor) {
    reasons <- c(reasons, "INVALID VARIANCE")
  }
  if (is.finite(var_fc) && var_fc > forecast_var_cap) {
    reasons <- c(reasons, "NUMERICALLY UNSTABLE FORECAST")
  }
  if (norm_label(dist_model) == "t") {
    if (!is.finite(shape_fc) || shape_fc <= student_shape_min) {
      reasons <- c(reasons, paste0("INVALID T SHAPE <= ", student_shape_min))
    }
  }
  
  if (length(reasons) == 0) return(NA_character_)
  paste(unique(reasons), collapse = "; ")
}

make_variance_plot_bonus <- function(plot_df, title_txt, ymax = NULL) {
  p <- ggplot(
    plot_df,
    aes(
      x = Date,
      y = value,
      color = series,
      linetype = series,
      group = series
    )
  ) +
    geom_line(linewidth = 0.4, na.rm = TRUE) +
    labs(
      title = title_txt,
      x = NULL,
      y = "Variance / squared return proxy",
      color = "Series",
      linetype = "Series",
      caption = "Proxy = squared return. Forecasts flagged as numerically unstable are omitted from model lines."
    ) +
    scale_color_manual(
      values = c(
        "EGARCH" = "black",
        "GAS"    = "black",
        "TGARCH" = "black",
        "Proxy"  = "red"
      ),
      breaks = c("EGARCH", "GAS", "TGARCH", "Proxy"),
      drop = FALSE
    ) +
    scale_linetype_manual(
      values = c(
        "EGARCH" = "solid",
        "GAS"    = "dotted",
        "TGARCH" = "dashed",
        "Proxy"  = "longdash"
      ),
      breaks = c("EGARCH", "GAS", "TGARCH", "Proxy"),
      drop = FALSE
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal()
  
  if (!is.null(ymax)) {
    p <- p + coord_cartesian(ylim = c(0, ymax))
  }
  
  p
}


# Rugarch helper functions: specification, fitting, convergence checking, parameter extraction

make_rugarch_spec_bonus <- function(variance_model, dist_model) {
  if (variance_model == "EGARCH") {
    v <- list(model = "eGARCH", garchOrder = c(1, 1))
  } else if (variance_model == "TGARCH") {
    v <- list(model = "gjrGARCH", garchOrder = c(1, 1))
  } else {
    stop("Unknown rugarch variance_model")
  }
  
  ugarchspec(
    variance.model = v,
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = dist_model
  )
}

fit_rugarch_bonus <- function(y, variance_model, dist_model) {
  spec <- make_rugarch_spec_bonus(variance_model, dist_model)
  tryCatch(
    ugarchfit(spec = spec, data = y, solver = "hybrid"),
    error = function(e) e
  )
}

rugarch_conv_code_bonus <- function(fit) {
  if (inherits(fit, "error")) return(NA_integer_)
  tryCatch(as.integer(fit@fit$convergence), error = function(e) NA_integer_)
}

get_rugarch_status_bonus <- function(fit) {
  if (inherits(fit, "error")) return(paste0("ERROR: ", fit$message))
  conv <- rugarch_conv_code_bonus(fit)
  if (is.na(conv)) return("UNKNOWN CONVERGENCE")
  if (conv == 0) return("OK")
  paste0("NON-CONVERGED (code ", conv, ")")
}

rugarch_is_usable_bonus <- function(fit) {
  if (inherits(fit, "error")) return(FALSE)
  conv <- rugarch_conv_code_bonus(fit)
  if (is.na(conv) || conv != 0) return(FALSE)
  cf <- tryCatch(coef(fit), error = function(e) NULL)
  if (is.null(cf) || length(cf) == 0) return(FALSE)
  all(is.finite(as.numeric(cf)))
}

extract_rugarch_params_bonus <- function(fit, model_name, dist_name) {
  status_txt <- get_rugarch_status_bonus(fit)
  
  if (!rugarch_is_usable_bonus(fit)) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      param = NA_character_,
      estimate = NA_real_,
      se = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      status = status_txt
    ))
  }
  
  mat <- tryCatch(fit@fit$matcoef, error = function(e) NULL)
  if (is.null(mat) || nrow(as.matrix(mat)) == 0) {
    cf <- coef(fit)
    return(tibble(
      model = model_name,
      distribution = dist_name,
      param = names(cf),
      estimate = as.numeric(cf),
      se = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      status = append_status(status_txt, "OK (estimates only)")
    ))
  }
  
  tab <- as.data.frame(mat)
  tab$param <- rownames(tab)
  rownames(tab) <- NULL
  names(tab) <- trimws(names(tab))
  
  tibble(
    model = model_name,
    distribution = dist_name,
    param = tab$param,
    estimate = as.numeric(tab$Estimate),
    se = as.numeric(tab$`Std. Error`),
    z_value = as.numeric(tab$`t value`),
    p_value = as.numeric(tab$`Pr(>|t|)`),
    status = status_txt
  )
}

extract_rugarch_ic_bonus <- function(fit, model_name, dist_name, n_obs) {
  status_txt <- get_rugarch_status_bonus(fit)
  
  if (!rugarch_is_usable_bonus(fit)) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      logLik = NA_real_,
      n_obs = NA_integer_,
      n_params = NA_integer_,
      AIC_total = NA_real_,
      BIC_total = NA_real_,
      AIC_per_obs = NA_real_,
      BIC_per_obs = NA_real_,
      status = status_txt
    ))
  }
  
  ll <- tryCatch(as.numeric(likelihood(fit)), error = function(e) NA_real_)
  k  <- tryCatch(length(coef(fit)), error = function(e) NA_integer_)
  
  ic_tbl <- compute_ic_common_bonus(loglik = ll, k = k, n = n_obs)
  
  tibble(model = model_name, distribution = dist_name) %>%
    bind_cols(ic_tbl) %>%
    mutate(status = status_txt)
}

diagnostics_rugarch_bonus <- function(fit, model_name, dist_name, lag = 20) {
  status_txt <- get_rugarch_status_bonus(fit)
  
  if (!rugarch_is_usable_bonus(fit)) {
    return(make_diag_row_bonus(
      model_name = model_name,
      dist_name = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      archlm_p_1 = NA_real_,
      archlm_p_2 = NA_real_,
      status_txt = status_txt
    ))
  }
  
  z <- tryCatch(as.numeric(residuals(fit, standardize = TRUE)), error = function(e) NULL)
  if (is.null(z)) {
    return(make_diag_row_bonus(
      model_name = model_name,
      dist_name = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      archlm_p_1 = NA_real_,
      archlm_p_2 = NA_real_,
      status_txt = append_status(status_txt, "NO STANDARDIZED RESIDUALS")
    ))
  }
  
  z <- z[is.finite(z)]
  min_n <- max(c(lag, archlm_lag_1, archlm_lag_2)) + min_diag_obs_buffer
  if (length(z) <= min_n) {
    return(make_diag_row_bonus(
      model_name = model_name,
      dist_name = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      archlm_p_1 = NA_real_,
      archlm_p_2 = NA_real_,
      status_txt = append_status(status_txt, "TOO FEW RESIDUALS")
    ))
  }
  
  make_diag_row_bonus(
    model_name = model_name,
    dist_name = dist_name,
    lb_resid_p = tryCatch(ljung_box_p_bonus(z, lag), error = function(e) NA_real_),
    lb_resid2_p = tryCatch(ljung_box_p_bonus(z^2, lag), error = function(e) NA_real_),
    archlm_p_1 = tryCatch(as.numeric(FinTS::ArchTest(z, lags = archlm_lag_1)$p.value), error = function(e) NA_real_),
    archlm_p_2 = tryCatch(as.numeric(FinTS::ArchTest(z, lags = archlm_lag_2)$p.value), error = function(e) NA_real_),
    status_txt = status_txt
  )
}

save_rugarch_plots_bonus <- function(fit, key, model_name, dist_name, dates, lag = 20) {
  if (!rugarch_is_usable_bonus(fit)) return(invisible(NULL))
  
  sig <- tryCatch(as.numeric(sigma(fit)), error = function(e) NULL)
  if (!is.null(sig) && length(sig) > 1) {
    vol_df <- tibble(
      Date = tail(dates, length(sig)),
      sigma = sig
    )
    
    p_vol <- ggplot(vol_df, aes(x = Date, y = sigma)) +
      geom_line(linewidth = 0.3) +
      labs(
        title = paste0("Conditional sigma: ", model_name, " with ", pretty_dist_label(dist_name)),
        x = NULL,
        y = "sigma_t"
      ) +
      theme_minimal()
    
    save_plot_bonus(p_vol, paste0("volatility_", safe_name_bonus(key), ".png"))
  }
  
  z <- tryCatch(as.numeric(residuals(fit, standardize = TRUE)), error = function(e) NULL)
  if (is.null(z)) return(invisible(NULL))
  z <- z[is.finite(z)]
  if (length(z) < 10) return(invisible(NULL))
  
  save_acf_png(
    z,
    main = paste0("ACF standardized residuals: ", key),
    filename = paste0("acf_stdres_", safe_name_bonus(key), ".png"),
    lag = lag
  )
  
  save_acf_png(
    z^2,
    main = paste0("ACF squared standardized residuals: ", key),
    filename = paste0("acf_stdres2_", safe_name_bonus(key), ".png"),
    lag = lag
  )
  
  shape <- if (norm_label(dist_name) == "t") {
    tryCatch(as.numeric(coef(fit)["shape"]), error = function(e) NA_real_)
  } else {
    NA_real_
  }
  
  save_qq_plot_assumed(
    z = z,
    dist_label = dist_name,
    shape = shape,
    main = paste0("QQ plot standardized residuals: ", key),
    filename = paste0("qq_stdres_", safe_name_bonus(key), ".png")
  )
  
  invisible(NULL)
}

forecast_one_rugarch_bonus <- function(y_train, y_next, variance_model, dist_model) {
  fit <- fit_rugarch_bonus(y_train, variance_model, dist_model)
  status_txt <- get_rugarch_status_bonus(fit)
  
  if (!rugarch_is_usable_bonus(fit)) {
    return(tibble(
      model = variance_model,
      distribution = dist_model,
      fit_status = status_txt,
      mean_forecast = NA_real_,
      variance_forecast = NA_real_,
      sigma_forecast = NA_real_,
      shape = NA_real_,
      log_score = NA_real_
    ))
  }
  
  fc <- tryCatch(ugarchforecast(fit, n.ahead = 1), error = function(e) e)
  if (inherits(fc, "error")) {
    return(tibble(
      model = variance_model,
      distribution = dist_model,
      fit_status = append_status(status_txt, paste0("FORECAST ERROR: ", fc$message)),
      mean_forecast = NA_real_,
      variance_forecast = NA_real_,
      sigma_forecast = NA_real_,
      shape = NA_real_,
      log_score = NA_real_
    ))
  }
  
  mu_fc <- tryCatch(as.numeric(fitted(fc))[1], error = function(e) NA_real_)
  if (!is.finite(mu_fc)) mu_fc <- safe_mean(y_train)
  
  sigma_fc <- tryCatch(as.numeric(sigma(fc))[1], error = function(e) NA_real_)
  var_fc <- sigma_fc^2
  
  shape_fc <- if (norm_label(dist_model) == "t") {
    tryCatch(as.numeric(coef(fit)["shape"]), error = function(e) NA_real_)
  } else {
    NA_real_
  }
  
  invalid_reason <- validate_forecast_bonus(
    var_fc = var_fc,
    sigma_fc = sigma_fc,
    dist_model = dist_model,
    shape_fc = shape_fc
  )
  
  if (!is.na(invalid_reason)) {
    return(tibble(
      model = variance_model,
      distribution = dist_model,
      fit_status = append_status(status_txt, invalid_reason),
      mean_forecast = mu_fc,
      variance_forecast = NA_real_,
      sigma_forecast = NA_real_,
      shape = shape_fc,
      log_score = NA_real_
    ))
  }
  
  dens_args <- list(
    distribution = dist_model,
    y = y_next,
    mu = mu_fc,
    sigma = sigma_fc,
    log = TRUE
  )
  if (norm_label(dist_model) == "t") {
    dens_args$shape <- shape_fc
  }
  
  log_sc <- tryCatch(do.call(rugarch::ddist, dens_args), error = function(e) NA_real_)
  
  tibble(
    model = variance_model,
    distribution = dist_model,
    fit_status = status_txt,
    mean_forecast = mu_fc,
    variance_forecast = var_fc,
    sigma_forecast = sigma_fc,
    shape = shape_fc,
    log_score = as.numeric(log_sc)
  )
}


# GAS helper functions: specification, fitting, parameter extraction, forecasting

gas_static_flags <- function(dist_model) {
  if (norm_label(dist_model) == "norm") {
    c(TRUE, FALSE)
  } else {
    c(TRUE, FALSE, TRUE)
  }
}

fit_gas_bonus <- function(y, dist_model, for_forecast = FALSE) {
  dist_use <- norm_label(dist_model)
  
  gas_args <- list(
    y = as.numeric(y),
    distr = dist_use,
    param = "meanvar",
    scaling = gas_scaling,
    regress = "joint",
    p = 1L,
    q = 1L,
    par_static = gas_static_flags(dist_use),
    print_progress = FALSE
  )
  
  if (isTRUE(for_forecast)) {
    gas_args$hessian_function <- NULL
  }
  
  tryCatch(
    do.call(gasmodel::gas, gas_args),
    error = function(e) e
  )
}

get_gas_status_bonus <- function(fit) {
  if (inherits(fit, "error")) return(paste0("ERROR: ", fit$message))
  status_optim <- tryCatch(as.character(fit$solution$status_optim), error = function(e) "UNKNOWN")
  status_hess  <- tryCatch(as.character(fit$solution$status_hessian), error = function(e) "UNKNOWN")
  paste0("optim=", status_optim, "; hessian=", status_hess)
}

gas_status_ok_bonus <- function(status_obj) {
  if (is.null(status_obj) || length(status_obj) == 0 || all(is.na(status_obj))) return(FALSE)
  
  if (is.numeric(status_obj)) {
    return(all(is.finite(status_obj)) && all(status_obj == 0))
  }
  
  s <- trimws(tolower(paste(status_obj, collapse = " ")))
  if (!nzchar(s)) return(FALSE)
  
  bad_pattern <- paste(
    c("fail", "error", "singular", "not available", "not computed",
      "could not", "stopped", "false", "non.?converg",
      "maxeval", "roundoff", "nan", "inf"),
    collapse = "|"
  )
  if (grepl(bad_pattern, s)) return(FALSE)
  
  good_pattern <- paste(
    c("^0$", "ok", "success", "successful", "computed", "available",
      "xtol", "ftol", "tolerance_reached", "variables_tolerance_reached"),
    collapse = "|"
  )
  grepl(good_pattern, s)
}

gas_optim_ok_bonus <- function(fit) {
  if (inherits(fit, "error")) return(FALSE)
  status_optim <- tryCatch(fit$solution$status_optim, error = function(e) NULL)
  gas_status_ok_bonus(status_optim)
}

gas_hessian_ok_bonus <- function(fit) {
  if (inherits(fit, "error")) return(FALSE)
  status_hess <- tryCatch(fit$solution$status_hessian, error = function(e) NULL)
  gas_status_ok_bonus(status_hess)
}

gas_is_usable_bonus <- function(fit) {
  if (inherits(fit, "error")) return(FALSE)
  if (!gas_optim_ok_bonus(fit)) return(FALSE)
  
  cf <- tryCatch(coef(fit), error = function(e) NULL)
  if (is.null(cf) || length(cf) == 0) return(FALSE)
  if (!all(is.finite(as.numeric(cf)))) return(FALSE)
  
  var_tv <- tryCatch(as.numeric(fit$fit$var_tv), error = function(e) NULL)
  if (is.null(var_tv)) return(FALSE)
  if (!any(is.finite(var_tv))) return(FALSE)
  
  TRUE
}

gas_has_valid_vcov_bonus <- function(fit) {
  if (!gas_is_usable_bonus(fit)) return(FALSE)
  if (!gas_hessian_ok_bonus(fit)) return(FALSE)
  vc <- tryCatch(vcov(fit), error = function(e) NULL)
  if (is.null(vc)) return(FALSE)
  all(is.finite(diag(as.matrix(vc))))
}

extract_gas_params_bonus <- function(fit, model_name, dist_name) {
  status_txt <- get_gas_status_bonus(fit)
  
  if (!gas_is_usable_bonus(fit)) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      param = NA_character_,
      estimate = NA_real_,
      se = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      status = status_txt
    ))
  }
  
  cf <- coef(fit)
  
  if (isTRUE(require_valid_gas_hessian_for_se) && !gas_has_valid_vcov_bonus(fit)) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      param = names(cf),
      estimate = as.numeric(cf),
      se = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      status = append_status(status_txt, "NO VALID HESSIAN/VCOV")
    ))
  }
  
  vc <- tryCatch(vcov(fit), error = function(e) NULL)
  
  if (is.null(vc)) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      param = names(cf),
      estimate = as.numeric(cf),
      se = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      status = append_status(status_txt, "NO VCOV")
    ))
  }
  
  se_raw <- as.numeric(diag(vc))
  se <- sqrt(pmax(se_raw, 0))
  zval <- ifelse(is.finite(se) & se > 0, as.numeric(cf) / se, NA_real_)
  pval <- ifelse(is.finite(zval), 2 * pnorm(-abs(zval)), NA_real_)
  
  tibble(
    model = model_name,
    distribution = dist_name,
    param = names(cf),
    estimate = as.numeric(cf),
    se = as.numeric(se),
    z_value = as.numeric(zval),
    p_value = as.numeric(pval),
    status = status_txt
  )
}

extract_gas_ic_bonus <- function(fit, model_name, dist_name, n_obs) {
  status_txt <- get_gas_status_bonus(fit)
  
  if (!gas_is_usable_bonus(fit)) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      logLik = NA_real_,
      n_obs = NA_integer_,
      n_params = NA_integer_,
      AIC_total = NA_real_,
      BIC_total = NA_real_,
      AIC_per_obs = NA_real_,
      BIC_per_obs = NA_real_,
      status = status_txt
    ))
  }
  
  ll <- tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_)
  k  <- tryCatch(length(coef(fit)), error = function(e) NA_integer_)
  
  ic_tbl <- compute_ic_common_bonus(loglik = ll, k = k, n = n_obs)
  
  tibble(model = model_name, distribution = dist_name) %>%
    bind_cols(ic_tbl) %>%
    mutate(status = status_txt)
}

get_gas_standardized_resid_bonus <- function(fit) {
  if (!gas_is_usable_bonus(fit)) return(NULL)
  
  res <- tryCatch(as.numeric(residuals(fit)), error = function(e) NULL)
  var_tv <- tryCatch(as.numeric(fit$fit$var_tv), error = function(e) NULL)
  
  if (is.null(res) || is.null(var_tv)) return(NULL)
  z <- res / sqrt(pmax(var_tv, forecast_var_floor))
  z[is.finite(z)]
}

diagnostics_gas_bonus <- function(fit, model_name, dist_name, lag = 20) {
  status_txt <- get_gas_status_bonus(fit)
  
  z <- get_gas_standardized_resid_bonus(fit)
  if (is.null(z)) {
    return(make_diag_row_bonus(
      model_name = model_name,
      dist_name = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      archlm_p_1 = NA_real_,
      archlm_p_2 = NA_real_,
      status_txt = append_status(status_txt, "NO STANDARDIZED RESIDUALS")
    ))
  }
  
  min_n <- max(c(lag, archlm_lag_1, archlm_lag_2)) + min_diag_obs_buffer
  if (length(z) <= min_n) {
    return(make_diag_row_bonus(
      model_name = model_name,
      dist_name = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      archlm_p_1 = NA_real_,
      archlm_p_2 = NA_real_,
      status_txt = append_status(status_txt, "TOO FEW RESIDUALS")
    ))
  }
  
  make_diag_row_bonus(
    model_name = model_name,
    dist_name = dist_name,
    lb_resid_p = tryCatch(ljung_box_p_bonus(z, lag), error = function(e) NA_real_),
    lb_resid2_p = tryCatch(ljung_box_p_bonus(z^2, lag), error = function(e) NA_real_),
    archlm_p_1 = tryCatch(as.numeric(FinTS::ArchTest(z, lags = archlm_lag_1)$p.value), error = function(e) NA_real_),
    archlm_p_2 = tryCatch(as.numeric(FinTS::ArchTest(z, lags = archlm_lag_2)$p.value), error = function(e) NA_real_),
    status_txt = status_txt
  )
}

get_gas_shape_bonus <- function(fit) {
  if (!gas_is_usable_bonus(fit)) return(NA_real_)
  
  cf <- tryCatch(coef(fit), error = function(e) NULL)
  if (is.null(cf) || length(cf) == 0) return(NA_real_)
  
  nm <- names(cf)
  idx <- grep("(^|_)(df|nu|shape|dof)($|_)", nm, ignore.case = TRUE)
  
  if (length(idx) == 1) {
    val <- as.numeric(cf[idx])
    return(ifelse(is.finite(val), val, NA_real_))
  }
  
  NA_real_
}

get_gas_mean_bonus <- function(fit) {
  if (!gas_is_usable_bonus(fit)) return(NA_real_)
  
  mean_tv <- tryCatch(as.numeric(fit$fit$mean_tv), error = function(e) NULL)
  if (!is.null(mean_tv)) {
    mean_tv <- mean_tv[is.finite(mean_tv)]
    if (length(mean_tv) > 0) return(tail(mean_tv, 1))
  }
  
  cf <- tryCatch(coef(fit), error = function(e) NULL)
  if (is.null(cf) || length(cf) == 0) return(NA_real_)
  
  nm <- names(cf)
  idx <- grep("mean", nm, ignore.case = TRUE)
  if (length(idx) > 0) return(as.numeric(cf[idx[1]]))
  
  dynamic_pat <- "_(omega|alpha[0-9]+|phi[0-9]+|beta[0-9]+)$"
  static_cf <- cf[!grepl(dynamic_pat, nm)]
  if (length(static_cf) >= 1) return(as.numeric(static_cf[1]))
  
  NA_real_
}

extract_gas_variance_forecast_bonus <- function(fit, fc) {
  empty_out <- list(
    variance_forecast = NA_real_,
    variance_state_forecast = NA_real_,
    linked_variance = NA,
    variance_parameter_name = NA_character_
  )
  
  par_mat <- tryCatch(fc$forecast$par_tv_ahead_mean, error = function(e) NULL)
  par_mat <- tryCatch(as.matrix(par_mat), error = function(e) NULL)
  
  if (is.null(par_mat) || nrow(par_mat) < 1L || ncol(par_mat) < 1L) {
    return(empty_out)
  }
  
  # robust 1-step-ahead extraction
  if (nrow(par_mat) == 1L) {
    par_fc <- as.numeric(par_mat[1, , drop = TRUE])
    nm <- colnames(par_mat)
    alt_nm <- rownames(par_mat)
  } else if (ncol(par_mat) == 1L) {
    par_fc <- as.numeric(par_mat[, 1, drop = TRUE])
    nm <- rownames(par_mat)
    alt_nm <- colnames(par_mat)
  } else {
    # fallback: take first row, which should correspond to first horizon
    par_fc <- as.numeric(par_mat[1, , drop = TRUE])
    nm <- colnames(par_mat)
    alt_nm <- rownames(par_mat)
  }
  
  if (is.null(nm) || length(nm) != length(par_fc)) nm <- alt_nm
  if (is.null(nm) || length(nm) != length(par_fc)) nm <- rep(NA_character_, length(par_fc))
  names(par_fc) <- nm
  
  # find the variance entry by name, not by vector length
  var_idx <- grep("^log\\(var\\)$|(^|_)(var|variance|sigma)(_|$)", names(par_fc), ignore.case = TRUE)
  
  if (length(var_idx) != 1L) {
    nm_txt <- names(par_fc)
    nm_txt <- nm_txt[!is.na(nm_txt) & nzchar(nm_txt)]
    return(list(
      variance_forecast = NA_real_,
      variance_state_forecast = NA_real_,
      linked_variance = NA,
      variance_parameter_name = if (length(nm_txt) > 0L) paste(nm_txt, collapse = ",") else NA_character_
    ))
  }
  
  state_fc <- as.numeric(par_fc[var_idx])
  var_name <- names(par_fc)[var_idx]
  
  # use full par_link if dimensions match the forecasted parameter vector
  full_links <- tryCatch(as.logical(fit$model$par_link), error = function(e) NULL)
  
  if (!is.null(full_links) && length(full_links) == length(par_fc) && !is.na(full_links[var_idx])) {
    linked_var <- isTRUE(full_links[var_idx])
  } else if (!is.na(var_name) && grepl("^log\\(var\\)$|log.?var|log.?variance|log.?sigma", var_name, ignore.case = TRUE)) {
    linked_var <- TRUE
  } else {
    linked_var <- FALSE
  }
  
  var_fc <- if (!is.finite(state_fc)) {
    NA_real_
  } else if (isTRUE(linked_var)) {
    exp(state_fc)
  } else {
    state_fc
  }
  
  list(
    variance_forecast = as.numeric(var_fc),
    variance_state_forecast = as.numeric(state_fc),
    linked_variance = linked_var,
    variance_parameter_name = var_name
  )
}

save_gas_plots_bonus <- function(fit, key, model_name, dist_name, dates, lag = 20) {
  if (!gas_is_usable_bonus(fit)) return(invisible(NULL))
  
  var_tv <- tryCatch(as.numeric(fit$fit$var_tv), error = function(e) NULL)
  if (!is.null(var_tv) && length(var_tv) > 1) {
    vol_df <- tibble(
      Date = tail(dates, length(var_tv)),
      sigma = sqrt(pmax(var_tv, forecast_var_floor))
    )
    
    p_vol <- ggplot(vol_df, aes(x = Date, y = sigma)) +
      geom_line(linewidth = 0.3) +
      labs(
        title = paste0("Conditional sigma: ", model_name, " with ", pretty_dist_label(dist_name)),
        x = NULL,
        y = "sigma_t"
      ) +
      theme_minimal()
    
    save_plot_bonus(p_vol, paste0("volatility_", safe_name_bonus(key), ".png"))
  }
  
  z <- get_gas_standardized_resid_bonus(fit)
  if (is.null(z) || length(z) < 10) return(invisible(NULL))
  
  save_acf_png(
    z,
    main = paste0("ACF standardized residuals: ", key),
    filename = paste0("acf_stdres_", safe_name_bonus(key), ".png"),
    lag = lag
  )
  
  save_acf_png(
    z^2,
    main = paste0("ACF squared standardized residuals: ", key),
    filename = paste0("acf_stdres2_", safe_name_bonus(key), ".png"),
    lag = lag
  )
  
  shape <- if (norm_label(dist_name) == "t") get_gas_shape_bonus(fit) else NA_real_
  
  save_qq_plot_assumed(
    z = z,
    dist_label = dist_name,
    shape = shape,
    main = paste0("QQ plot standardized residuals: ", key),
    filename = paste0("qq_stdres_", safe_name_bonus(key), ".png")
  )
  
  invisible(NULL)
}

forecast_one_gas_bonus <- function(y_train, y_next, dist_model) {
  fit <- fit_gas_bonus(y_train, dist_model = dist_model, for_forecast = TRUE)
  status_txt <- get_gas_status_bonus(fit)
  
  if (!gas_is_usable_bonus(fit)) {
    return(tibble(
      model = "GAS",
      distribution = norm_label(dist_model),
      fit_status = status_txt,
      mean_forecast = NA_real_,
      variance_forecast = NA_real_,
      sigma_forecast = NA_real_,
      shape = NA_real_,
      log_score = NA_real_
    ))
  }
  
  fc <- tryCatch(
    gasmodel::gas_forecast(fit, method = "mean_path", t_ahead = 1L),
    error = function(e) e
  )
  
  if (inherits(fc, "error")) {
    return(tibble(
      model = "GAS",
      distribution = norm_label(dist_model),
      fit_status = append_status(status_txt, paste0("FORECAST ERROR: ", fc$message)),
      mean_forecast = NA_real_,
      variance_forecast = NA_real_,
      sigma_forecast = NA_real_,
      shape = NA_real_,
      log_score = NA_real_
    ))
  }
  
  gas_var_obj <- extract_gas_variance_forecast_bonus(fit, fc)
  var_fc <- gas_var_obj$variance_forecast
  
  if (!is.finite(var_fc)) {
    var_label <- gas_var_obj$variance_parameter_name
    extra_reason <- paste0(
      "GAS VAR EXTRACTION FAILED",
      if (length(var_label) == 1L && !is.na(var_label) && nzchar(var_label)) {
        paste0(": ", var_label)
      } else {
        ""
      }
    )
    return(tibble(
      model = "GAS",
      distribution = norm_label(dist_model),
      fit_status = append_status(status_txt, extra_reason),
      mean_forecast = NA_real_,
      variance_forecast = NA_real_,
      sigma_forecast = NA_real_,
      shape = if (norm_label(dist_model) == "t") get_gas_shape_bonus(fit) else NA_real_,
      log_score = NA_real_
    ))
  }
  
  mu_fc <- tryCatch(as.numeric(fc$forecast$y_ahead_mean)[1], error = function(e) NA_real_)
  if (!is.finite(mu_fc)) mu_fc <- get_gas_mean_bonus(fit)
  
  shape_fc <- if (norm_label(dist_model) == "t") get_gas_shape_bonus(fit) else NA_real_
  sigma_fc <- if (is.finite(var_fc)) sqrt(pmax(var_fc, forecast_var_floor)) else NA_real_
  
  invalid_reason <- validate_forecast_bonus(
    var_fc = var_fc,
    sigma_fc = sigma_fc,
    dist_model = dist_model,
    shape_fc = shape_fc
  )
  
  if (!is.na(invalid_reason)) {
    return(tibble(
      model = "GAS",
      distribution = norm_label(dist_model),
      fit_status = append_status(status_txt, invalid_reason),
      mean_forecast = mu_fc,
      variance_forecast = NA_real_,
      sigma_forecast = NA_real_,
      shape = shape_fc,
      log_score = NA_real_
    ))
  }
  
  if (norm_label(dist_model) == "norm") {
    f_vec <- c(mu_fc, var_fc)
    log_sc <- tryCatch(
      gasmodel::distr_density(
        y = y_next,
        f = f_vec,
        distr = "norm",
        param = "meanvar",
        par_link = c(FALSE, FALSE),
        trans = "log"
      ),
      error = function(e) NA_real_
    )
  } else {
    f_vec <- c(mu_fc, var_fc, shape_fc)
    log_sc <- tryCatch(
      gasmodel::distr_density(
        y = y_next,
        f = f_vec,
        distr = "t",
        param = "meanvar",
        par_link = c(FALSE, FALSE, FALSE),
        trans = "log"
      ),
      error = function(e) NA_real_
    )
  }
  
  tibble(
    model = "GAS",
    distribution = norm_label(dist_model),
    fit_status = status_txt,
    mean_forecast = mu_fc,
    variance_forecast = var_fc,
    sigma_forecast = sigma_fc,
    shape = shape_fc,
    log_score = as.numeric(log_sc)
  )
}
