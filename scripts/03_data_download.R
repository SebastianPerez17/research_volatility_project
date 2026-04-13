# Download and clean data

# Validate required variables from config
required_vars <- c("ticker", "start_date", "end_date", "return_scale", "output_dir")
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check 00_config.R"))
  }
}

# Validate helper functions from benchmark helpers
if (!exists("write_log", mode = "function")) {
  stop("write_log function not found. Check if 02_benchmark_helpers.R executed successfully.")
}

# Download S&P 500 data from Yahoo Finance
px_xts <- tryCatch(
  getSymbols(
    Symbols = ticker, src = "yahoo",
    from = start_date, to = end_date,
    auto.assign = FALSE
  ),
  error = function(e) e
)

if (inherits(px_xts, "error")) {
  stop(paste0("Yahoo download failed: ", px_xts$message))
}

# Extract adjusted close price and compute log returns
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

# Validate data was successfully processed
if (nrow(df) == 0) {
  stop("No valid data rows after filtering")
}

# Save cleaned data
write_csv(df, file.path(output_dir, "tables", "data_clean.csv"))

# Write summary info
write_log(
  "data_info.txt",
  c(
    paste0("Ticker: ", ticker),
    paste0("Start:  ", start_date),
    paste0("End:    ", end_date),
    paste0("Obs (returns): ", nrow(df)),
    paste0("First date in df: ", min(df$Date)),
    paste0("Last  date in df: ", max(df$Date))
  )
)
