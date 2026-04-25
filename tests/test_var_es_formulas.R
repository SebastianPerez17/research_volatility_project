# Unit tests for VaR/ES formulas and tail-risk shape handling.
# Run from the project root with:
#   Rscript tests/test_var_es_formulas.R

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_path <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[1]) else file.path("tests", "test_var_es_formulas.R")
project_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
if (!file.exists(file.path(project_root, "scripts", "07_comparison_helpers.R"))) {
  project_root <- normalizePath(getwd(), mustWork = TRUE)
}
setwd(project_root)

expect_true <- function(value, label) {
  if (!isTRUE(value)) {
    stop(paste0("FAILED: ", label), call. = FALSE)
  }
  invisible(TRUE)
}

expect_equal <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(paste0("FAILED: ", label, "\nactual: ", actual, "\nexpected: ", expected), call. = FALSE)
  }
  invisible(TRUE)
}

expect_close <- function(actual, expected, label, tolerance = 1e-10) {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance))) {
    stop(
      paste0(
        "FAILED: ", label,
        "\nactual: ", format(actual, digits = 16),
        "\nexpected: ", format(expected, digits = 16)
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

source(file.path("scripts", "00_config.R"), local = FALSE)

# Keep this a unit test: no downloads, no full pipeline outputs.
results_root <- tempfile("var_es_unit_results_")
forecast_subdir <- "forecast_comparison"
install_if_missing <- FALSE
run_rolling <- FALSE
window_length <- 5L
df <- tibble::tibble(
  Date = as.Date("2020-01-01") + 0:19,
  Price = 100 + 0:19,
  LogPrice = log(100 + 0:19),
  Return = c(
    -0.40, 0.15, -0.25, 0.08, 0.04,
    -0.12, 0.19, -0.06, 0.03, -0.18,
    0.11, -0.07, 0.05, -0.09, 0.13,
    -0.03, 0.02, -0.04, 0.06, -0.01
  )
)
r <- df$Return

source(file.path("scripts", "07_comparison_helpers.R"), local = FALSE)

alpha <- 0.05
mu <- 0.10
sigma <- 2.00

normal_out <- compute_var_es_point_bonus(
  mean_forecast = mu,
  sigma_forecast = sigma,
  alpha = alpha,
  distribution = "Normal",
  loss_sign_convention = "positive_loss"
)
z_alpha <- qnorm(alpha)
normal_q_ret <- mu + sigma * z_alpha
normal_var <- -normal_q_ret
normal_es <- -mu + sigma * dnorm(z_alpha) / alpha

expect_close(normal_out$return_quantile, normal_q_ret, "Normal return quantile")
expect_close(normal_out$VaR, normal_var, "Normal positive-loss VaR formula")
expect_close(normal_out$ES, normal_es, "Normal positive-loss ES formula")
expect_true(normal_out$VaR > 0, "positive-loss sign convention produces positive left-tail VaR")
expect_true(normal_out$ES > normal_out$VaR, "positive-loss ES is more conservative than VaR")

shape <- 7
t_out <- compute_var_es_point_bonus(
  mean_forecast = mu,
  sigma_forecast = sigma,
  alpha = alpha,
  distribution = "Student t",
  shape = shape,
  loss_sign_convention = "positive_loss"
)
q_raw <- qt(alpha, df = shape)
t_scale <- sqrt((shape - 2) / shape)
t_q_ret <- mu + sigma * t_scale * q_raw
t_var <- -t_q_ret
t_es <- -mu + sigma * t_scale * ((shape + q_raw^2) / (shape - 1)) * dt(q_raw, df = shape) / alpha

expect_close(t_out$return_quantile, t_q_ret, "Student-t standardized return quantile")
expect_close(t_out$VaR, t_var, "Student-t positive-loss VaR formula")
expect_close(t_out$ES, t_es, "Student-t positive-loss ES formula")
expect_true(t_out$VaR > 0, "Student-t positive-loss VaR is positive for left-tail loss")
expect_true(t_out$ES > t_out$VaR, "Student-t positive-loss ES is more conservative than VaR")

bad_shape_out <- compute_var_es_point_bonus(
  mean_forecast = mu,
  sigma_forecast = sigma,
  alpha = alpha,
  distribution = "Student t",
  shape = 1.99,
  loss_sign_convention = "positive_loss"
)
expect_true(!is.finite(bad_shape_out$return_quantile), "shape below 2 invalidates return quantile")
expect_true(!is.finite(bad_shape_out$VaR), "shape below 2 invalidates VaR")
expect_true(!is.finite(bad_shape_out$ES), "shape below 2 invalidates ES")
expect_equal(bad_shape_out$var_es_note, "Invalid Student t shape (must be > 2)", "shape below 2 returns explicit invalid note")

mock_gas_fit <- structure(
  list(
    solution = list(status_optim = "success"),
    fit = list(var_tv = c(1.0, 1.1, 1.2)),
    coefficients = c(location = 0, scale = 1, nu = 8.5)
  ),
  class = "mock_gas_fit"
)
coef.mock_gas_fit <- function(object, ...) object$coefficients

gas_shape <- get_gas_shape_bonus(mock_gas_fit)
expect_close(gas_shape, 8.5, "GAS-t shape extraction finds nu parameter")

mock_gas_no_shape <- mock_gas_fit
mock_gas_no_shape$coefficients <- c(location = 0, scale = 1)
missing_shape <- get_gas_shape_bonus(mock_gas_no_shape)
expect_true(!is.finite(missing_shape), "GAS-t missing shape returns NA instead of an arbitrary fallback")

missing_shape_validation <- validate_forecast_bonus(
  var_fc = 1,
  sigma_fc = 1,
  dist_model = "t",
  shape_fc = missing_shape
)
expect_true(!is.na(missing_shape_validation), "GAS-t missing shape is rejected by forecast validation")
expect_true(
  grepl("INVALID T SHAPE", missing_shape_validation),
  "GAS-t missing shape rejection gives an explicit reason"
)

cat("All VaR/ES unit tests passed.\n")
