# Shared package loader for the full research pipeline

required_packages <- c(
  "quantmod", "xts", "zoo",
  "dplyr", "tibble", "readr", "lubridate", "tidyr",
  "ggplot2",
  "tseries", "urca",
  "lmtest", "sandwich",
  "moments", "FinTS",
  "rugarch", "gasmodel"
)

if (isTRUE(install_if_missing)) {
  ip <- rownames(installed.packages())
  for (p in required_packages) {
    if (!p %in% ip) install.packages(p, dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
  library(zoo)
  library(dplyr)
  library(tibble)
  library(readr)
  library(lubridate)
  library(tidyr)
  library(ggplot2)
  library(tseries)
  library(urca)
  library(lmtest)
  library(sandwich)
  library(moments)
  library(FinTS)
  library(rugarch)
  library(gasmodel)
})
