# Benchmark-model output setup and shared helper functions

# Initialize output directory structure
if (!exists("benchmark_output_dir")) {
  stop("benchmark_output_dir not defined. Check 00_config.R")
}

output_dir <- benchmark_output_dir

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "logs"), showWarnings = FALSE, recursive = TRUE)

write_log <- function(filename, lines) {
  writeLines(lines, con = file.path(output_dir, "logs", filename))
}

# Saves a ggplot graph as a PNG.
save_plot <- function(p, filename, w = 10, h = 6) {
  ggsave(
    filename = file.path(output_dir, "plots", filename),
    plot = p, width = w, height = h, dpi = 300
  )
}

# Cleans names (removes special characters)
safe_name <- function(x) {
  gsub("[^A-Za-z0-9_\\-]+", "_", x)
}

# Computes descriptive statistics
desc_stats <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    warning("No finite values in x")
    return(NULL)
  }
  tibble(
    n = length(x),
    mean = mean(x),
    sd = sd(x),
    min = min(x),
    max = max(x),
    skewness = moments::skewness(x),
    kurtosis = moments::kurtosis(x)
  )
}

# p-value of the Ljung-Box test (tests for autocorrelation)
ljung_box_p <- function(x, lag = 20) {
  stats::Box.test(x, lag = lag, type = "Ljung-Box")$p.value
}

# White-type heteroskedasticity test using fitted values and squared fitted values
white_test <- function(lm_obj) {
  f <- fitted(lm_obj)
  result <- lmtest::bptest(lm_obj, ~ f + I(f^2))
  return(result$p.value)
}
