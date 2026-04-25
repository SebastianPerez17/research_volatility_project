# In-sample score-driven and benchmark comparison

# Validate required variables from config
required_vars <- c("forecast_dir", "ac_lag", "archlm_lags_check", "alpha_archlm", "df", "r")
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check if previous scripts executed successfully."))
  }
}

# Validate output directory exists
if (!dir.exists(forecast_dir)) {
  stop(paste0("Output directory does not exist: ", forecast_dir))
}

# Validate data consistency
if (!is.numeric(r) || length(r) == 0) {
  stop("r must be a numeric vector of returns. Check if 03_data_download.R executed successfully.")
}
if (!is.data.frame(df) || !"Date" %in% names(df)) {
  stop("df must contain a Date column. Check if 03_data_download.R executed successfully.")
}
if (length(r) != nrow(df)) {
  stop("Inconsistent objects: length(r) != nrow(df). Use a fresh R session.")
}
if (!is.numeric(ac_lag) || length(ac_lag) != 1 || !is.finite(ac_lag) ||
    ac_lag < 1 || ac_lag != as.integer(ac_lag)) {
  stop("ac_lag must be a single positive integer.")
}
if (!is.numeric(alpha_archlm) || length(alpha_archlm) != 1 || !is.finite(alpha_archlm) ||
    alpha_archlm <= 0 || alpha_archlm >= 1) {
  stop("alpha_archlm must be a single number strictly between 0 and 1.")
}
if (!is.numeric(archlm_lags_check) || length(archlm_lags_check) != 2 ||
    any(!is.finite(archlm_lags_check)) || any(archlm_lags_check < 1) ||
    any(archlm_lags_check != as.integer(archlm_lags_check)) ||
    length(unique(as.integer(archlm_lags_check))) != 2) {
  stop("archlm_lags_check must be two distinct positive integers.")
}

archlm_lag_1 <- as.integer(archlm_lags_check[1L])
archlm_lag_2 <- as.integer(archlm_lags_check[2L])
archlm_col_1 <- paste0("archlm_p_lag", archlm_lag_1)
archlm_col_2 <- paste0("archlm_p_lag", archlm_lag_2)
pass_arch_col_1 <- paste0("pass_arch", archlm_lag_1)
pass_arch_col_2 <- paste0("pass_arch", archlm_lag_2)

# Validate helper functions exist
helper_funcs <- c("fit_gas_bonus", "get_gas_status_bonus", "extract_gas_params_bonus", 
                  "extract_gas_ic_bonus", "diagnostics_gas_bonus", "save_gas_plots_bonus",
                  "fit_rugarch_bonus", "get_rugarch_status_bonus", "extract_rugarch_params_bonus",
                  "extract_rugarch_ic_bonus", "diagnostics_rugarch_bonus", "save_rugarch_plots_bonus",
                  "gas_is_usable_bonus", "gas_has_valid_vcov_bonus", "rugarch_is_usable_bonus")
missing_funcs <- helper_funcs[!vapply(helper_funcs, exists, logical(1), mode = "function")]
if (length(missing_funcs) > 0) {
  stop(paste0("Missing helper functions: ", paste(missing_funcs, collapse = ", "), 
              ". Check if 07_comparison_helpers.R was sourced successfully."))
}

# Define candidate models: GAS, EGARCH, TGARCH with normal and Student t distributions
bonus_candidates <- tribble(
  ~model,   ~distribution,
  "GAS",    "norm",
  "GAS",    "t",
  "EGARCH", "norm",
  "EGARCH", "std",
  "TGARCH", "norm",
  "TGARCH", "std"
)

write_csv(bonus_candidates, file.path(forecast_dir, "tables", "model_candidates.csv"))

# Initialize result containers
insample_status <- list()
insample_params <- list()
insample_ic <- list()
insample_diag <- list()
insample_fits <- list()

# Fit each candidate model and store results
for (i in seq_len(nrow(bonus_candidates))) {
  model_i <- bonus_candidates$model[i]
  dist_i <- bonus_candidates$distribution[i]
  key_i <- paste0(model_i, "_", dist_i)
  
  cat("\nFitting:", key_i, "...\n")
  
  # GAS model fit
  if (model_i == "GAS") {
    fit_i <- fit_gas_bonus(r, dist_model = dist_i, for_forecast = FALSE)
    insample_status[[key_i]] <- tibble(
      model = model_i,
      distribution = dist_i,
      status = get_gas_status_bonus(fit_i),
      usable_fit = gas_is_usable_bonus(fit_i),
      valid_hessian_for_se = gas_has_valid_vcov_bonus(fit_i)
    )
    insample_params[[key_i]] <- extract_gas_params_bonus(fit_i, model_i, dist_i)
    insample_ic[[key_i]] <- extract_gas_ic_bonus(fit_i, model_i, dist_i, n_obs = length(r))
    insample_diag[[key_i]] <- diagnostics_gas_bonus(fit_i, model_i, dist_i, lag = ac_lag)
    save_gas_plots_bonus(fit_i, key_i, model_i, dist_i, df$Date, lag = ac_lag)
  # EGARCH or TGARCH via rugarch
  } else {
    fit_i <- fit_rugarch_bonus(r, variance_model = model_i, dist_model = dist_i)
    insample_status[[key_i]] <- tibble(
      model = model_i,
      distribution = dist_i,
      status = get_rugarch_status_bonus(fit_i),
      usable_fit = rugarch_is_usable_bonus(fit_i),
      valid_hessian_for_se = NA
    )
    insample_params[[key_i]] <- extract_rugarch_params_bonus(fit_i, model_i, dist_i)
    insample_ic[[key_i]] <- extract_rugarch_ic_bonus(fit_i, model_i, dist_i, n_obs = length(r))
    insample_diag[[key_i]] <- diagnostics_rugarch_bonus(fit_i, model_i, dist_i, lag = ac_lag)
    save_rugarch_plots_bonus(fit_i, key_i, model_i, dist_i, df$Date, lag = ac_lag)
  }
  
  insample_fits[[key_i]] <- fit_i
}

# Combine results into tables
insample_status_tbl <- bind_rows(insample_status)
insample_params_tbl <- bind_rows(insample_params)
insample_ic_tbl <- bind_rows(insample_ic)
insample_diag_tbl <- bind_rows(insample_diag)

write_csv(insample_status_tbl, file.path(forecast_dir, "tables", "in_sample_fit_status.csv"))
write_csv(insample_params_tbl, file.path(forecast_dir, "tables", "in_sample_params.csv"))
write_csv(insample_ic_tbl, file.path(forecast_dir, "tables", "in_sample_information_criteria.csv"))
write_csv(insample_diag_tbl, file.path(forecast_dir, "tables", "in_sample_diagnostics.csv"))

required_diag_cols <- c("lb_resid_p", "lb_resid2_p", archlm_col_1, archlm_col_2)
missing_diag_cols <- setdiff(required_diag_cols, names(insample_diag_tbl))
if (length(missing_diag_cols) > 0) {
  stop(
    paste0(
      "Missing diagnostic columns in in-sample diagnostics table: ",
      paste(missing_diag_cols, collapse = ", "),
      ". Check if 07_comparison_helpers.R uses archlm_lags_check consistently."
    )
  )
}

# Rank models by diagnostic test pass rate and information criteria
insample_rank_tbl <- insample_diag_tbl %>%
  left_join(
    insample_ic_tbl %>%
      select(
        model, distribution, logLik, n_obs, n_params,
        AIC_total, BIC_total, AIC_per_obs, BIC_per_obs
      ),
    by = c("model", "distribution")
  ) %>%
  mutate(
    pass_lb_resid  = as.integer(!is.na(lb_resid_p) & lb_resid_p > 0.05),
    pass_lb_resid2 = as.integer(!is.na(lb_resid2_p) & lb_resid2_p > 0.05),
    !!pass_arch_col_1 := as.integer(!is.na(.data[[archlm_col_1]]) & .data[[archlm_col_1]] > alpha_archlm),
    !!pass_arch_col_2 := as.integer(!is.na(.data[[archlm_col_2]]) & .data[[archlm_col_2]] > alpha_archlm)
  ) %>%
  mutate(
    pass_sum = pass_lb_resid + pass_lb_resid2 + .data[[pass_arch_col_1]] + .data[[pass_arch_col_2]]
  ) %>%
  arrange(desc(pass_sum), AIC_total)

write_csv(insample_rank_tbl, file.path(forecast_dir, "tables", "in_sample_ranking_help.csv"))
