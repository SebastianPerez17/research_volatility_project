# Descriptives, stationarity, and baseline OLS

# Validate required variables from previous scripts/config
required_vars <- c("ac_lag", "df", "output_dir")
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check if previous scripts executed successfully."))
  }
}

# Validate helper functions from 02_benchmark_helpers.R
helper_funcs <- c("desc_stats", "save_plot", "ljung_box_p", "white_test", "write_log")
missing_funcs <- helper_funcs[!vapply(helper_funcs, exists, logical(1), mode = "function")]
if (length(missing_funcs) > 0) {
  stop(paste0("Missing helper functions: ", paste(missing_funcs, collapse = ", "),
              ". Check if 02_benchmark_helpers.R was sourced successfully."))
}

# Validate data and parameters
if (!is.data.frame(df)) {
  stop("df must be a data frame.")
}
if (!all(c("Date", "LogPrice", "Return") %in% names(df))) {
  stop('df must contain columns: "Date", "LogPrice", "Return".')
}
if (!inherits(df$Date, "Date")) {
  stop("df$Date must be of class Date.")
}
if (!is.numeric(df$LogPrice) || !is.numeric(df$Return)) {
  stop("df$LogPrice and df$Return must be numeric.")
}
if (!is.numeric(ac_lag) || length(ac_lag) != 1 || !is.finite(ac_lag) ||
    ac_lag < 1 || ac_lag != as.integer(ac_lag)) {
  stop("ac_lag must be a single positive integer.")
}

# Ensure output subdirectories exist
dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "logs"), recursive = TRUE, showWarnings = FALSE)

if (nrow(df) < 3) {
  stop("Not enough observations for exploratory diagnostics (need at least 3).")
}

# Compute descriptive statistics for log-price and returns
stats_logprice <- desc_stats(df$LogPrice)
stats_return <- desc_stats(df$Return)

if (is.null(stats_logprice) || is.null(stats_return)) {
  stop("Failed to compute descriptive statistics (no finite values)")
}

stats_all <- bind_rows(
  stats_logprice %>% mutate(series = "LogPrice"),
  stats_return %>% mutate(series = "Return")
) %>% select(series, everything())

write_csv(stats_all, file.path(output_dir, "tables", "descriptive_stats.csv"))

# Time series plot of returns
p_ret <- ggplot(df, aes(x = Date, y = Return)) +
  geom_line(linewidth = 0.3) +
  labs(title = "Daily S&P 500 log returns (percent)", x = NULL, y = "Return (%)") +
  theme_minimal()

# Squared returns: visualize volatility clustering
p_sq <- ggplot(df, aes(x = Date, y = Return^2)) +
  geom_line(linewidth = 0.3) +
  labs(title = "Squared returns (volatility clustering)", x = NULL, y = "Return^2") +
  theme_minimal()

save_plot(p_ret, "returns_timeseries.png")
save_plot(p_sq,  "squared_returns_timeseries.png")

# ACF: returns typically show little autocorrelation
png(file.path(output_dir, "plots", "acf_returns.png"), width = 1200, height = 800)
acf(df$Return, main = "ACF of returns", lag.max = ac_lag)
dev.off()

# ACF: squared returns typically show persistence (volatility clustering)
png(file.path(output_dir, "plots", "acf_squared_returns.png"), width = 1200, height = 800)
acf(df$Return^2, main = "ACF of squared returns", lag.max = ac_lag)
dev.off()

# Ljung-Box test: Return (linear autocorrelation), Return^2 (volatility dependence)
# Significant p-value for Return^2 supports ARCH/GARCH modeling
lb_tbl <- tibble(
  series = c("Return", "Return^2"),
  lag = ac_lag,
  ljung_box_p_value = c(
    tryCatch(ljung_box_p(df$Return, ac_lag), error = function(e) NA_real_),
    tryCatch(ljung_box_p(df$Return^2, ac_lag), error = function(e) NA_real_)
  )
) %>%
  mutate(
    ljung_box_p_value_display = ifelse(
      ljung_box_p_value == 0,
      paste0("< ", format(.Machine$double.eps, scientific = TRUE)),
      format.pval(ljung_box_p_value, digits = 6, eps = .Machine$double.eps)
    )
  )

write_csv(lb_tbl, file.path(output_dir, "tables", "ljung_box.csv"))

# Unit root tests: LogPrice (typically nonstationary), Returns (typically stationary)
adf_logprice <- tryCatch(tseries::adf.test(df$LogPrice), error = function(e) e)
adf_return   <- tryCatch(tseries::adf.test(df$Return), error = function(e) e)

pp_logprice  <- tryCatch(tseries::pp.test(df$LogPrice), error = function(e) e)
pp_return    <- tryCatch(tseries::pp.test(df$Return), error = function(e) e)

# Augmented Dickey-Fuller with drift and trend
ur_adf_logprice_drift <- tryCatch(urca::ur.df(df$LogPrice, type = "drift", lags = 5), error = function(e) e)
ur_adf_logprice_trend <- tryCatch(urca::ur.df(df$LogPrice, type = "trend", lags = 5), error = function(e) e)
ur_adf_return_drift   <- tryCatch(urca::ur.df(df$Return,   type = "drift", lags = 5), error = function(e) e)

# Phillips-Perron tests
ur_pp_logprice <- tryCatch(
  urca::ur.pp(df$LogPrice, type = "Z-tau", model = "constant", lags = "short"),
  error = function(e) e
)
ur_pp_return <- tryCatch(
  urca::ur.pp(df$Return, type = "Z-tau", model = "constant", lags = "short"),
  error = function(e) e
)

format_test_p_line <- function(label, test_obj) {
  if (inherits(test_obj, "error")) {
    return(paste0(label, ": ERROR - ", test_obj$message))
  }
  p_val <- tryCatch(as.numeric(test_obj$p.value), error = function(e) NA_real_)
  if (!is.finite(p_val)) return(paste0(label, ": NA"))
  paste0(label, ": ", signif(p_val, 4))
}

format_test_summary_lines <- function(label, test_obj) {
  if (inherits(test_obj, "error")) {
    return(paste0(label, ": ERROR - ", test_obj$message))
  }
  c(paste0(label, ":"), capture.output(summary(test_obj)))
}

stationarity_lines <- c(
  "ADF (tseries::adf.test) p-values",
  format_test_p_line("LogPrice", adf_logprice),
  format_test_p_line("Return", adf_return),
  "",
  "PP (tseries::pp.test) p-values",
  format_test_p_line("LogPrice", pp_logprice),
  format_test_p_line("Return", pp_return),
  "",
  "ADF (urca::ur.df) summaries",
  format_test_summary_lines("LogPrice drift", ur_adf_logprice_drift),
  "",
  format_test_summary_lines("LogPrice trend", ur_adf_logprice_trend),
  "",
  format_test_summary_lines("Return drift", ur_adf_return_drift),
  "",
  "PP (urca::ur.pp) summaries",
  format_test_summary_lines("LogPrice", ur_pp_logprice),
  "",
  format_test_summary_lines("Return", ur_pp_return)
)
write_log("stationarity_tests.txt", stationarity_lines)

# Baseline OLS: constant mean model (Return_t = mu + eps_t)
ols <- lm(Return ~ 1, data = df)
res <- resid(ols)

write_log("ols_summary.txt", capture.output(summary(ols)))

# OLS diagnostics: BP and White tests require at least one regressor
# For intercept-only models, skip these tests
has_regressor <- ncol(model.matrix(ols)) > 1

# If there is a regressor, run BP and White. Otherwise skip them
if (has_regressor) {
  bp <- lmtest::bptest(ols)
  wt_p <- white_test(ols)  # white_test returns p-value directly
  bp_p <- bp$p.value
  bp_note <- "OK"
  wt_note <- "OK"
} else {
  bp_p <- NA_real_
  wt_p <- NA_real_
  bp_note <- "Not defined for intercept-only mean model"
  wt_note <- "Not defined for intercept-only mean model"
}

# tryCatch(...):  try to run the test, if there is an error, do not stop the whole script
# instead, store the error object
# Run Durbin-Watson test for autocorrelation
dw <- tryCatch(lmtest::dwtest(ols), error = function(e) e)
# Breusch-Godfrey test for autocorrelation
bg <- tryCatch(lmtest::bgtest(ols, order = min(10, ac_lag)), error = function(e) e)
# Normality test on residuals Jarque-Bera
jb <- tryCatch(tseries::jarque.bera.test(res), error = function(e) e)

# Extract p-values safely
dw_p <- if (inherits(dw, "error")) NA_real_ else dw$p.value
bg_p <- if (inherits(bg, "error")) NA_real_ else bg$p.value
dw_note <- if (inherits(dw, "error")) paste("ERROR:", dw$message) else "OK"
bg_note <- if (inherits(bg, "error")) paste("ERROR:", bg$message) else "OK"

# plot ACF of OLS residuals
png(file.path(output_dir, "plots", "acf_ols_residuals.png"), width = 1200, height = 800)
acf(res, main = "ACF of OLS residuals", lag.max = ac_lag)
dev.off()

# plot QQ plot of residuals
png(file.path(output_dir, "plots", "qq_ols_residuals.png"), width = 1200, height = 800)
qqnorm(res, main = "QQ plot: OLS residuals")
qqline(res)
dev.off()

# Initial ARCH-LM on OLS residuals: this is the key variance diagnostic here
# tests whether the OLS residuals still contain ARCH effects.
archlm0_5  <- tryCatch(FinTS::ArchTest(res, lags = 5), error = function(e) e)
archlm0_10 <- tryCatch(FinTS::ArchTest(res, lags = 10), error = function(e) e)

get_test_p_value <- function(test_obj) {
  if (inherits(test_obj, "error")) return(NA_real_)
  p_val <- tryCatch(as.numeric(test_obj$p.value), error = function(e) NA_real_)
  ifelse(is.finite(p_val), p_val, NA_real_)
}

get_test_statistic <- function(test_obj) {
  if (inherits(test_obj, "error")) return(NA_real_)
  stat <- tryCatch(as.numeric(test_obj$statistic), error = function(e) NA_real_)
  ifelse(is.finite(stat), stat, NA_real_)
}

jb_p <- get_test_p_value(jb)
jb_note <- if (inherits(jb, "error")) paste("ERROR:", jb$message) else "OK"
arch5_p <- get_test_p_value(archlm0_5)
arch10_p <- get_test_p_value(archlm0_10)
arch5_stat <- get_test_statistic(archlm0_5)
arch10_stat <- get_test_statistic(archlm0_10)
arch5_note <- if (inherits(archlm0_5, "error")) paste("ERROR:", archlm0_5$message) else "Main heteroskedasticity diagnostic for conditional variance"
arch10_note <- if (inherits(archlm0_10, "error")) paste("ERROR:", archlm0_10$message) else "Main heteroskedasticity diagnostic for conditional variance"

#store values in one table
# useful for "OLS assumptions checks" section.
diag_tbl <- tibble(
  item = c(
    "Residual mean",
    "Breusch-Pagan p-value",
    "White test p-value",
    "Durbin-Watson p-value",
    paste0("Breusch-Godfrey p-value (order=", min(10, ac_lag), ")"),
    "Jarque-Bera p-value",
    "ARCH LM p-value (lag 5)",
    "ARCH LM p-value (lag 10)"
  ),
  value = c(
    mean(res),
    bp_p,
    wt_p,
    dw_p,
    bg_p,
    jb_p,
    arch5_p,
    arch10_p
  ),
  note = c(
    "Should be approximately zero with intercept",
    bp_note,
    wt_note,
    dw_note,
    bg_note,
    jb_note,
    arch5_note,
    arch10_note
  )
)
# save diagnostic table
write_csv(diag_tbl, file.path(output_dir, "tables", "ols_diagnostics.csv"))

# creates a separate ARCH-LM table
arch0_tbl <- tibble(
  test = c("ARCH LM on OLS residuals", "ARCH LM on OLS residuals"),
  lags = c(5, 10),
  statistic = c(arch5_stat, arch10_stat),
  p_value = c(arch5_p, arch10_p)
)
#save ARCH-LM diagnostic table
write_csv(arch0_tbl, file.path(output_dir, "tables", "archlm_ols_residuals.csv"))
