# ARCH(q) selection

# Validate required variables from config
required_vars <- c("df", "max_q_try", "archlm_lags_check", "alpha_archlm", "output_dir")
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check 00_config.R"))
  }
}

# Validate helper functions from 02_benchmark_helpers.R
if (!exists("write_log", mode = "function")) {
  stop("write_log function not found. Check if 02_benchmark_helpers.R executed successfully.")
}

# Validate data and parameter integrity
if (!is.data.frame(df) || !"Return" %in% names(df)) {
  stop('df must be a data frame containing a "Return" column.')
}
if (!is.numeric(df$Return)) {
  stop("df$Return must be numeric.")
}
if (!is.numeric(max_q_try) || length(max_q_try) != 1 || !is.finite(max_q_try) ||
    max_q_try < 1 || max_q_try != as.integer(max_q_try)) {
  stop("max_q_try must be a single positive integer.")
}
if (!is.numeric(archlm_lags_check) || length(archlm_lags_check) != 2 ||
    any(!is.finite(archlm_lags_check)) || any(archlm_lags_check < 1) ||
    any(archlm_lags_check != as.integer(archlm_lags_check))) {
  stop("archlm_lags_check must be a numeric vector of two positive integers.")
}
if (!is.numeric(alpha_archlm) || length(alpha_archlm) != 1 || !is.finite(alpha_archlm) ||
    alpha_archlm <= 0 || alpha_archlm >= 1) {
  stop("alpha_archlm must be a single number strictly between 0 and 1.")
}
dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "logs"), recursive = TRUE, showWarnings = FALSE)

# Fit ARCH(q) models with constant mean and test for remaining ARCH structure
# Choose smallest q where residual ARCH-LM p-values exceed significance threshold
r <- df$Return
r <- r[is.finite(r)]
if (length(r) == 0) {
  stop("No finite return observations available for ARCH(q) selection.")
}

# ARCH specification builder
make_arch_spec <- function(q, dist_model = "norm") {
  ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(q, 0)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = dist_model
  )
}

# Fit one ARCH(q) and test residual ARCH structure
fit_arch_q <- function(q) {
  spec <- make_arch_spec(q, dist_model = "norm")
  fit <- tryCatch(
    ugarchfit(spec = spec, data = r, solver = "hybrid"),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(list(ok = FALSE, message = fit$message, p5 = NA_real_, p10 = NA_real_))
  }

  conv_code <- tryCatch(as.integer(fit@fit$convergence), error = function(e) NA_integer_)
  if (is.na(conv_code)) {
    return(list(ok = FALSE, message = "UNKNOWN CONVERGENCE", p5 = NA_real_, p10 = NA_real_))
  }
  if (conv_code != 0L) {
    return(list(ok = FALSE, message = paste0("NON-CONVERGED (code ", conv_code, ")"), p5 = NA_real_, p10 = NA_real_))
  }
  
  # Standardized residuals
  z <- as.numeric(residuals(fit, standardize = TRUE))
  z <- z[is.finite(z)]
  if (length(z) <= max(archlm_lags_check)) {
    return(list(ok = FALSE, message = "TOO FEW STANDARDIZED RESIDUALS", p5 = NA_real_, p10 = NA_real_))
  }
  
  # ARCH-LM tests: null = no ARCH effect
  t5  <- tryCatch(FinTS::ArchTest(z, lags = archlm_lags_check[1]), error = function(e) e)
  t10 <- tryCatch(FinTS::ArchTest(z, lags = archlm_lags_check[2]), error = function(e) e)
  
  # Extract p-values safely
  p5  <- if (inherits(t5, "error"))  NA_real_ else unname(t5$p.value)
  p10 <- if (inherits(t10, "error")) NA_real_ else unname(t10$p.value)
  
  list(ok = TRUE, message = "OK", p5 = p5, p10 = p10)
}

# Fit ARCH(q) for q = 1 to max_q_try
# Model passes if: estimated successfully AND both ARCH-LM p-values > alpha_archlm
arch_select <- vector("list", max_q_try)

for (q in 1:max_q_try) {
  resq <- fit_arch_q(q)
  arch_select[[q]] <- tibble(
    q = q,
    archlm_p_lag5 = resq$p5,
    archlm_p_lag10 = resq$p10,
    ok = resq$ok,
    status = resq$message,
    pass = isTRUE(resq$ok) && !is.na(resq$p5) && !is.na(resq$p10) &&
      (resq$p5 > alpha_archlm) && (resq$p10 > alpha_archlm)
  )
}

arch_select_tbl <- bind_rows(arch_select)
write_csv(arch_select_tbl, file.path(output_dir, "tables", "arch_q_selection.csv"))

# Decision rule: If any model passes, choose smallest q; otherwise choose q with best worst-case p-value
if (any(arch_select_tbl$pass, na.rm = TRUE)) {
  ARCH_q <- min(arch_select_tbl$q[arch_select_tbl$pass])
} else {
  # Fallback: choose q that maximizes min(p5, p10) among successful fits
  arch_select_tbl <- arch_select_tbl %>%
    mutate(min_p = pmin(archlm_p_lag5, archlm_p_lag10, na.rm = TRUE))
  cand <- arch_select_tbl %>% filter(ok, is.finite(min_p))
  if (nrow(cand) > 0) {
    ARCH_q <- cand$q[which.max(cand$min_p)]
  } else {
    stop("ARCH(q) selection failed: no usable fitted candidates.")
  }
}

write_log(
  "arch_q_choice.txt",
  c(
    paste0("ARCH(q) selection rule: smallest q with residual ARCH-LM not significant at lags ",
           paste(archlm_lags_check, collapse = " and "), " (alpha=", alpha_archlm, ")."),
    paste0("Chosen q: ", ARCH_q)
  )
)
