# Benchmark ARCH-family models

# Validate required variables from config
required_vars <- c("ARCH_q", "archlm_lags_check", "output_dir", "ac_lag", "r", "df")
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check 00_config.R"))
  }
}

# Validate helper functions from 02_benchmark_helpers.R
helper_funcs <- c("save_plot", "safe_name", "ljung_box_p", "write_log")
missing_funcs <- helper_funcs[!vapply(helper_funcs, exists, logical(1), mode = "function")]
if (length(missing_funcs) > 0) {
  stop(paste0("Missing helper functions: ", paste(missing_funcs, collapse = ", "),
              ". Check if 02_benchmark_helpers.R was sourced successfully."))
}

# Validate data and parameter structure
if (!is.numeric(ARCH_q) || length(ARCH_q) != 1 || !is.finite(ARCH_q) ||
    ARCH_q < 1 || ARCH_q != as.integer(ARCH_q)) {
  stop("ARCH_q must be a single positive integer.")
}
if (!is.numeric(ac_lag) || length(ac_lag) != 1 || !is.finite(ac_lag) ||
    ac_lag < 1 || ac_lag != as.integer(ac_lag)) {
  stop("ac_lag must be a single positive integer.")
}
if (!is.numeric(archlm_lags_check) || length(archlm_lags_check) != 2 ||
    any(!is.finite(archlm_lags_check)) || any(archlm_lags_check < 1) ||
    any(archlm_lags_check != as.integer(archlm_lags_check)) ||
    length(unique(as.integer(archlm_lags_check))) != 2) {
  stop("archlm_lags_check must contain two distinct positive integer lags.")
}
arch_lag_1 <- as.integer(archlm_lags_check[1])
arch_lag_2 <- as.integer(archlm_lags_check[2])
arch_col_1 <- paste0("archlm_p_lag", arch_lag_1)
arch_col_2 <- paste0("archlm_p_lag", arch_lag_2)
pass_col_1 <- paste0("pass_arch", arch_lag_1)
pass_col_2 <- paste0("pass_arch", arch_lag_2)

if (!is.numeric(r) || length(r) == 0) {
  stop("r must be a numeric vector of returns. Check if 03_data_download.R executed successfully.")
}
if (!all(is.finite(r))) {
  stop("r contains non-finite values.")
}
if (!is.data.frame(df) || !"Date" %in% names(df)) {
  stop("df must contain a Date column. Check if 03_data_download.R executed successfully.")
}
if (!inherits(df$Date, "Date")) {
  stop("df$Date must be of class Date.")
}
if (length(r) != nrow(df)) {
  stop("Inconsistent objects: length(r) != nrow(df). Use a fresh R session.")
}

# Ensure output folders exist
dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "logs"), recursive = TRUE, showWarnings = FALSE)

# Fit candidate models: ARCH(q), GARCH(1,1), EGARCH(1,1), TGARCH/GJR(1,1)
# Test with distributions: norm, std (8 candidate models total)

# Build model specification based on variance model type
make_spec <- function(variance_model, dist_model, arch_q = 1) {
  if (variance_model == "ARCH") {
    v <- list(model = "sGARCH", garchOrder = c(arch_q, 0))
  } else if (variance_model == "GARCH") {
    v <- list(model = "sGARCH", garchOrder = c(1, 1))
  } else if (variance_model == "EGARCH") {
    v <- list(model = "eGARCH", garchOrder = c(1, 1))
  } else if (variance_model == "TGARCH") {
    v <- list(model = "gjrGARCH", garchOrder = c(1, 1))
  } else {
    stop("Unknown variance_model")
  }
  
  ugarchspec(
    variance.model = v,
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = dist_model
  )
}

# Fit any model specification safely, returning model or error object
fit_one <- function(spec, r) {
  tryCatch(
    ugarchfit(spec = spec, data = r, solver = "hybrid"),
    error = function(e) e
  )
}

# Extract convergence code from model (0 = success)
get_conv_code <- function(fit) {
  if (inherits(fit, "error")) return(NA_integer_)
  tryCatch(as.integer(fit@fit$convergence), error = function(e) NA_integer_)
}

# Convert convergence code to readable status
get_fit_status <- function(fit) {
  if (inherits(fit, "error")) {
    return(paste0("ERROR: ", fit$message))
  }
  
  conv <- get_conv_code(fit)
  
  if (is.na(conv)) {
    return("UNKNOWN CONVERGENCE")
  } else if (conv == 0) {
    return("OK")
  } else {
    return(paste0("NON-CONVERGED (code ", conv, ")"))
  }
}

# Extract model coefficients in tidy format
extract_params <- function(fit, model_name, dist_name) {
  status_txt <- get_fit_status(fit)
  
  # Return NA row for errors
  if (inherits(fit, "error")) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      param = NA_character_,
      estimate = NA_real_,
      se = NA_real_,
      t_value = NA_real_,
      p_value = NA_real_,
      status = status_txt
    ))
  }
  
  # Return NA row for non-converged fits
  conv <- get_conv_code(fit)
  if (!is.na(conv) && conv != 0) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      param = NA_character_,
      estimate = NA_real_,
      se = NA_real_,
      t_value = NA_real_,
      p_value = NA_real_,
      status = status_txt
    ))
  }
  # Try to extract coefficient matrix
  mat <- tryCatch(fit@fit$matcoef, error = function(e) NULL) 
  
  # Fallback to coef() if matcoef unavailable
  if (is.null(mat) || length(mat) == 0 || nrow(as.matrix(mat)) == 0) {
    cf <- tryCatch(coef(fit), error = function(e) NULL)
    
    if (is.null(cf) || length(cf) == 0) {
      return(tibble(
        model = model_name,
        distribution = dist_name,
        param = NA_character_,
        estimate = NA_real_,
        se = NA_real_,
        t_value = NA_real_,
        p_value = NA_real_,
        status = "NO PARAMETER TABLE AVAILABLE"
      ))
    }
    
    # Use coef() to get parameter estimates when matcoef unavailable
    return(tibble(
      model = model_name,
      distribution = dist_name,
      param = names(cf),
      estimate = as.numeric(cf),
      se = NA_real_,
      t_value = NA_real_,
      p_value = NA_real_,
      status = "OK (estimates only, no matcoef)"
    ))
  }
  
  # Parse coefficient matrix
  tab <- as.data.frame(mat)
  tab$param <- rownames(tab)
  rownames(tab) <- NULL
  names(tab) <- trimws(names(tab))
  
  # Find column positions by name
  est_col <- if ("Estimate" %in% names(tab)) "Estimate" else NA_character_
  se_col  <- if ("Std. Error" %in% names(tab)) "Std. Error" else NA_character_
  t_col   <- if ("t value" %in% names(tab)) "t value" else NA_character_
  p_col   <- if ("Pr(>|t|)" %in% names(tab)) "Pr(>|t|)" else NA_character_
  
  # Build tidy coefficient table
  tibble(
    model = model_name,
    distribution = dist_name,
    param = tab$param,
    estimate = if (!is.na(est_col)) as.numeric(tab[[est_col]]) else NA_real_,
    se = if (!is.na(se_col)) as.numeric(tab[[se_col]]) else NA_real_,
    t_value = if (!is.na(t_col)) as.numeric(tab[[t_col]]) else NA_real_,
    p_value = if (!is.na(p_col)) as.numeric(tab[[p_col]]) else NA_real_,
    status = if (all(!is.na(c(est_col, se_col, t_col, p_col)))) "OK" else "OK (partial coefficient table)"
  )
}

# Extract information criteria (AIC, BIC)
extract_ic <- function(fit, model_name, dist_name) {
  status_txt <- get_fit_status(fit)
  
  # Return NA row for errors
  if (inherits(fit, "error")) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      AIC = NA_real_,
      BIC = NA_real_,
      status = status_txt
    ))
  }
  
  # Return NA row for non-converged fits
  conv <- get_conv_code(fit)
  if (!is.na(conv) && conv != 0) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      AIC = NA_real_,
      BIC = NA_real_,
      status = status_txt
    ))
  }
  
  # Extract information criteria
  ic <- tryCatch(infocriteria(fit), error = function(e) NULL)
  
  if (is.null(ic) || length(ic) == 0) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      AIC = NA_real_,
      BIC = NA_real_,
      status = "NO INFORMATION CRITERIA AVAILABLE"
    ))
  }
  
  ic_vec <- as.numeric(ic)
  ic_names <- names(ic)
  
  aic_val <- NA_real_
  bic_val <- NA_real_
  
  # Try matching by name
  if (!is.null(ic_names)) {
    if ("Akaike" %in% ic_names) aic_val <- unname(ic["Akaike"])
    if ("Bayes"  %in% ic_names) bic_val <- unname(ic["Bayes"])
  }
  
  # Fallback to position if names unavailable
  if (is.na(aic_val) && length(ic_vec) >= 1) aic_val <- ic_vec[1]
  if (is.na(bic_val) && length(ic_vec) >= 2) bic_val <- ic_vec[2]
  
  tibble(
    model = model_name,
    distribution = dist_name,
    AIC = aic_val,
    BIC = bic_val,
    status = "OK"
  )
}

# Test whether estimated volatility model is adequate
# Computes: Ljung-Box (residuals and squared residuals), ARCH-LM (configured lags)
model_diagnostics <- function(fit, model_name, dist_name, lag = 20, arch_lags = c(5L, 10L)) {
  status_txt <- get_fit_status(fit)
  
  # Return NA row for errors
  if (inherits(fit, "error")) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      !!arch_col_1 := NA_real_,
      !!arch_col_2 := NA_real_,
      status = status_txt
    ))
  }
  
  # Return NA row for non-converged fits
  conv <- get_conv_code(fit)
  if (!is.na(conv) && conv != 0) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      !!arch_col_1 := NA_real_,
      !!arch_col_2 := NA_real_,
      status = status_txt
    ))
  }
  
  # Get standardized residuals
  z <- tryCatch(as.numeric(residuals(fit, standardize = TRUE)), error = function(e) NULL)
  
  if (is.null(z)) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      !!arch_col_1 := NA_real_,
      !!arch_col_2 := NA_real_,
      status = "NO STANDARDIZED RESIDUALS"
    ))
  }
  
  # Remove non-finite values
  z <- z[is.finite(z)]
  
  # Check sufficient sample size for tests
  if (length(z) <= max(c(lag, arch_lags))) {
    return(tibble(
      model = model_name,
      distribution = dist_name,
      lb_resid_p = NA_real_,
      lb_resid2_p = NA_real_,
      !!arch_col_1 := NA_real_,
      !!arch_col_2 := NA_real_,
      status = "TOO FEW RESIDUALS"
    ))
  }
  
  # Ljung-Box tests
  lb1 <- tryCatch(ljung_box_p(z, lag = lag), error = function(e) NA_real_)
  lb2 <- tryCatch(ljung_box_p(z^2, lag = lag), error = function(e) NA_real_)
  
  # ARCH-LM tests on standardized residuals
  t_arch_1 <- tryCatch(FinTS::ArchTest(z, lags = arch_lags[1]), error = function(e) NULL)
  t_arch_2 <- tryCatch(FinTS::ArchTest(z, lags = arch_lags[2]), error = function(e) NULL)
  
  tibble(
    model = model_name,
    distribution = dist_name,
    lb_resid_p = lb1,
    lb_resid2_p = lb2,
    !!arch_col_1 := if (!is.null(t_arch_1)) unname(t_arch_1$p.value) else NA_real_,
    !!arch_col_2 := if (!is.null(t_arch_2)) unname(t_arch_2$p.value) else NA_real_,
    status = "OK"
  )
}

#saves graphical diagnostics for each valid model
save_model_plots <- function(fit, key, model_name, dist_name, dates, lag = 20) {
  if (inherits(fit, "error")) return(invisible(NULL))
  
  conv <- get_conv_code(fit)
  if (!is.na(conv) && conv != 0) return(invisible(NULL))
  
  # 1) Conditional sigma plot, extracts the estimated conditional standard deviation
  sig <- tryCatch(as.numeric(sigma(fit)), error = function(e) NULL)
  
  if (!is.null(sig)) {
    sig <- sig[is.finite(sig)]
    # aligns estimated sigma with dates. Then it plots sigma over time. useful for interpretation.
    if (length(sig) > 1) {
      vol_df <- tibble(
        Date = tail(dates, length(sig)),
        sigma = sig
      )
      
      p_vol <- ggplot(vol_df, aes(x = Date, y = sigma)) +
        geom_line(linewidth = 0.3) +
        labs(
          title = paste0("Conditional sigma: ", model_name, " with ", dist_name),
          x = NULL,
          y = "sigma_t"
        ) +
        theme_minimal()
      
      save_plot(p_vol, paste0("volatility_", safe_name(key), ".png"))
    }
  }
  
  # 2) Standardized residual plots, checks remaining linear dependence.
  z <- tryCatch(as.numeric(residuals(fit, standardize = TRUE)), error = function(e) NULL)
  
  if (is.null(z)) return(invisible(NULL))
  
  z <- z[is.finite(z)]
  
  if (length(z) > 1) {
    # Standardized residual ACF plot
    png(
      file.path(output_dir, "plots", paste0("acf_stdres_", safe_name(key), ".png")),
      width = 1200, height = 800
    )
    acf(z, main = paste0("ACF standardized residuals: ", key), lag.max = lag)
    dev.off()
    # Squared standardized residual ACF plot
    png(
      file.path(output_dir, "plots", paste0("acf_stdres2_", safe_name(key), ".png")),
      width = 1200, height = 800
    )
    acf(z^2, main = paste0("ACF standardized residuals^2: ", key), lag.max = lag)
    dev.off()
    # QQ plot
    png(
      file.path(output_dir, "plots", paste0("qq_stdres_", safe_name(key), ".png")),
      width = 1200, height = 800
    )
    qqnorm(z, main = paste0("QQ plot standardized residuals: ", key))
    qqline(z)
    dev.off()
  }
  
  invisible(NULL)
}

# Candidate model table
candidates <- tribble(
  ~model,   ~dist,
  "ARCH",   "norm",
  "ARCH",   "std",
  "GARCH",  "norm",
  "GARCH",  "std",
  "EGARCH", "norm",
  "EGARCH", "std",
  "TGARCH", "norm",
  "TGARCH", "std"
)

write_csv(candidates, file.path(output_dir, "tables", "model_candidates.csv"))

# Storage objects
fits <- list()
fit_status_tbl <- list()
param_tbl <- list()
ic_tbl <- list()
diag_tbl2 <- list()

# Main estimation loop
# For each candidate model:
## -identify the model and distribution
## -build the specification
## -estimate it
## -check convergence
## -extract results
## -save diagnostics and plots
for (i in seq_len(nrow(candidates))) {
  # Get model name and distribution
  m <- candidates$model[i]
  d <- candidates$dist[i]
  key <- paste0(m, "_", d)
  # Print progress to console
  cat("\n------------------------------\n")
  cat("Fitting model:", key, "\n")
  # Build and fit the model
  spec <- make_spec(m, d, arch_q = ARCH_q)
  fit  <- fit_one(spec, r)
  fits[[key]] <- fit
  # Status and convergence code
  fit_status <- get_fit_status(fit)
  conv_code <- get_conv_code(fit)
  cat("Status:", fit_status, "\n")
  # Extract status, parameters, information criteria, and diagnostics
  fit_status_tbl[[key]] <- tibble(
    model = m,
    distribution = d,
    convergence_code = conv_code,
    status = fit_status
  )
  
  param_tbl[[key]] <- extract_params(fit, m, d)
  ic_tbl[[key]]    <- extract_ic(fit, m, d)
  diag_tbl2[[key]] <- model_diagnostics(
    fit,
    m,
    d,
    lag = ac_lag,
    arch_lags = c(arch_lag_1, arch_lag_2)
  )
  # Save plots
  save_model_plots(
    fit = fit,
    key = key,
    model_name = m,
    dist_name = d,
    dates = df$Date,
    lag = ac_lag
  )
}

#Combine all model outputs
fit_status_all <- bind_rows(fit_status_tbl)
params_all     <- bind_rows(param_tbl)
ic_all         <- bind_rows(ic_tbl)
diag_all       <- bind_rows(diag_tbl2)

# 4 important output files:
# - in_sample_fit_status.csv: Convergence status and fit validity
# - in_sample_params.csv: Coefficient estimates and inference
# - in_sample_information_criteria.csv: AIC and BIC for model comparison
# - in_sample_diagnostics.csv: Residual diagnostic tests
write_csv(fit_status_all, file.path(output_dir, "tables", "in_sample_fit_status.csv"))
write_csv(params_all,     file.path(output_dir, "tables", "in_sample_params.csv"))
write_csv(ic_all,         file.path(output_dir, "tables", "in_sample_information_criteria.csv"))
write_csv(diag_all,       file.path(output_dir, "tables", "in_sample_diagnostics.csv"))

# Helpful ranking table: pass count + AIC
rank_tbl <- diag_all %>%
  left_join(
    ic_all %>% select(model, distribution, AIC, BIC),
    by = c("model", "distribution")
  ) %>%
  mutate(
    pass_lb_resid  = as.integer(!is.na(lb_resid_p)      & lb_resid_p > 0.05),
    pass_lb_resid2 = as.integer(!is.na(lb_resid2_p)     & lb_resid2_p > 0.05),
    !!pass_col_1 := as.integer(!is.na(.data[[arch_col_1]]) & .data[[arch_col_1]] > 0.05),
    !!pass_col_2 := as.integer(!is.na(.data[[arch_col_2]]) & .data[[arch_col_2]] > 0.05),
    pass_sum = pass_lb_resid + pass_lb_resid2 + .data[[pass_col_1]] + .data[[pass_col_2]]
  ) %>%
  arrange(desc(pass_sum), AIC)

write_csv(rank_tbl, file.path(output_dir, "tables", "in_sample_ranking_help.csv"))
