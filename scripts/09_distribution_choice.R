# Distribution choice

# Validate required variables from config
required_vars <- c("insample_ic_tbl", "forecast_dist_override", "forecast_dir")
for (var in required_vars) {
  if (!exists(var)) {
    stop(paste0(var, " not defined. Check if previous scripts executed successfully."))
  }
}

# Validate output directory exists
if (!dir.exists(forecast_dir)) {
  stop(paste0("Output directory does not exist: ", forecast_dir))
}

# Validate helper functions exist
helper_funcs <- c("norm_label", "best_dist_by_ic", "first_num_or_na", "pretty_dist_label", "write_log_forecast")
missing_funcs <- helper_funcs[!vapply(helper_funcs, exists, logical(1), mode = "function")]
if (length(missing_funcs) > 0) {
  stop(paste0("Missing helper functions: ", paste(missing_funcs, collapse = ", "), 
              ". Check if 07_comparison_helpers.R was sourced successfully."))
}

# Validate data integrity
if (!is.data.frame(insample_ic_tbl) || nrow(insample_ic_tbl) == 0) {
  stop("insample_ic_tbl must be a non-empty data frame.")
}
if (!all(c("model", "distribution", "AIC_total", "BIC_total", "AIC_per_obs", "BIC_per_obs") %in% names(insample_ic_tbl))) {
  stop("insample_ic_tbl missing required columns.")
}
ic_cols <- c("AIC_total", "BIC_total", "AIC_per_obs", "BIC_per_obs")
if (!all(vapply(insample_ic_tbl[ic_cols], is.numeric, logical(1)))) {
  stop("insample_ic_tbl IC columns must be numeric.")
}

# Validate override explicitly as a single normalized value
if (!is.null(forecast_dist_override)) {
  if (length(forecast_dist_override) != 1 || is.na(forecast_dist_override)) {
    stop('forecast_dist_override must be a single value: NULL, "norm", "t", or "std".')
  }
  override_norm <- norm_label(as.character(forecast_dist_override))
  if (!override_norm %in% c("norm", "t")) {
    stop('forecast_dist_override must be NULL, "norm", "t", or "std".')
  }
  forecast_dist_override <- override_norm
}

min_finite_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

# Collapse possible duplicate rows per model/distribution deterministically
insample_ic_compact_tbl <- insample_ic_tbl %>%
  mutate(dist_simple = norm_label(distribution)) %>%
  filter(dist_simple %in% c("norm", "t")) %>%
  group_by(model, dist_simple) %>%
  summarise(
    AIC_total = min_finite_or_na(AIC_total),
    BIC_total = min_finite_or_na(BIC_total),
    AIC_per_obs = min_finite_or_na(AIC_per_obs),
    BIC_per_obs = min_finite_or_na(BIC_per_obs),
    .groups = "drop"
  )

if (nrow(insample_ic_compact_tbl) == 0) {
  stop("No usable Normal/Student-t IC rows found in insample_ic_tbl.")
}

# Compare distributions (Normal vs Student t) within each model based on information criteria
dist_choice_tbl <- insample_ic_compact_tbl %>%
  group_by(model) %>%
  summarise(
    best_AIC_dist = best_dist_by_ic(dist_simple, AIC_total),
    best_BIC_dist = best_dist_by_ic(dist_simple, BIC_total),
    AIC_norm = first_num_or_na(AIC_total[dist_simple == "norm"]),
    AIC_t = first_num_or_na(AIC_total[dist_simple == "t"]),
    BIC_norm = first_num_or_na(BIC_total[dist_simple == "norm"]),
    BIC_t = first_num_or_na(BIC_total[dist_simple == "t"]),
    AIC_per_obs_norm = first_num_or_na(AIC_per_obs[dist_simple == "norm"]),
    AIC_per_obs_t = first_num_or_na(AIC_per_obs[dist_simple == "t"]),
    BIC_per_obs_norm = first_num_or_na(BIC_per_obs[dist_simple == "norm"]),
    BIC_per_obs_t = first_num_or_na(BIC_per_obs[dist_simple == "t"]),
    dAIC_t_minus_norm = AIC_t - AIC_norm,
    dBIC_t_minus_norm = BIC_t - BIC_norm,
    dAIC_per_obs_t_minus_norm = AIC_per_obs_t - AIC_per_obs_norm,
    dBIC_per_obs_t_minus_norm = BIC_per_obs_t - BIC_per_obs_norm,
    .groups = "drop"
  )

votes <- c(dist_choice_tbl$best_AIC_dist, dist_choice_tbl$best_BIC_dist)
votes <- votes[!is.na(votes) & votes %in% c("norm", "t")]

vote_t <- sum(votes == "t")
vote_norm <- sum(votes == "norm")

sum_finite_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  sum(x)
}

has_vote_signal <- length(votes) > 0
has_pairwise_ic_signal <- any(is.finite(dist_choice_tbl$dAIC_t_minus_norm)) ||
  any(is.finite(dist_choice_tbl$dBIC_t_minus_norm))

if (is.null(forecast_dist_override) && !has_vote_signal && !has_pairwise_ic_signal) {
  stop("No valid information-criterion evidence is available to choose a forecasting distribution.")
}

# Aggregate IC differences across models for tie-breaking
total_ic_tbl <- dist_choice_tbl %>%
  summarise(
    total_dAIC_t_minus_norm = sum_finite_or_na(dAIC_t_minus_norm),
    total_dBIC_t_minus_norm = sum_finite_or_na(dBIC_t_minus_norm),
    total_dAIC_per_obs_t_minus_norm = sum_finite_or_na(dAIC_per_obs_t_minus_norm),
    total_dBIC_per_obs_t_minus_norm = sum_finite_or_na(dBIC_per_obs_t_minus_norm)
  )

# Decision rule: override > vote_t > vote_norm > aggregate IC
chosen_dist_simple <- if (!is.null(forecast_dist_override)) {
  norm_label(forecast_dist_override)
} else if (vote_t > vote_norm) {
  "t"
} else if (vote_norm > vote_t) {
  "norm"
} else {
  # Tie-break using aggregate information criteria
  if (is.finite(total_ic_tbl$total_dAIC_t_minus_norm) && total_ic_tbl$total_dAIC_t_minus_norm < 0) {
    "t"
  } else if (is.finite(total_ic_tbl$total_dAIC_t_minus_norm) && total_ic_tbl$total_dAIC_t_minus_norm > 0) {
    "norm"
  } else if (is.finite(total_ic_tbl$total_dBIC_t_minus_norm) && total_ic_tbl$total_dBIC_t_minus_norm < 0) {
    "t"
  } else if (is.finite(total_ic_tbl$total_dBIC_t_minus_norm) && total_ic_tbl$total_dBIC_t_minus_norm > 0) {
    "norm"
  } else {
    stop("Distribution votes are tied and tie-break IC differences are unavailable or exactly zero.")
  }
}

chosen_dist_export <- dplyr::case_when(
  chosen_dist_simple == "t" ~ "student_t",
  chosen_dist_simple == "norm" ~ "norm",
  TRUE ~ as.character(chosen_dist_simple)
)

dist_choice_summary_tbl <- tibble(
  chosen_distribution_for_forecasting = chosen_dist_export,
  chosen_distribution_pretty = pretty_dist_label(chosen_dist_simple),
  vote_t = vote_t,
  vote_norm = vote_norm,
  override_used = !is.null(forecast_dist_override),
  tie_break_total_dAIC_t_minus_norm = total_ic_tbl$total_dAIC_t_minus_norm,
  tie_break_total_dBIC_t_minus_norm = total_ic_tbl$total_dBIC_t_minus_norm
)

write_csv(dist_choice_tbl, file.path(forecast_dir, "tables", "distribution_choice_by_model.csv"))
write_csv(total_ic_tbl, file.path(forecast_dir, "tables", "distribution_choice_total_ic.csv"))
write_csv(dist_choice_summary_tbl, file.path(forecast_dir, "tables", "distribution_choice_summary.csv"))

write_log_forecast(
  "distribution_choice.txt",
  c(
    "Distribution choice rule:",
    "1. If override specified, use it",
    "2. Otherwise, compare Normal vs Student t within each model using AIC and BIC",
    "3. Count votes: best AIC distribution + best BIC distribution per model",
    "4. If tied, use summed within-model IC differences across models",
    "5. Negative difference favors Student t, positive favors Normal",
    "",
    paste0("Votes for Student t: ", vote_t),
    paste0("Votes for Normal: ", vote_norm),
    paste0("Tie-break total dAIC (t - norm): ", total_ic_tbl$total_dAIC_t_minus_norm),
    paste0("Tie-break total dBIC (t - norm): ", total_ic_tbl$total_dBIC_t_minus_norm),
    paste0("Chosen distribution code for forecasting: ", chosen_dist_export),
    paste0("Chosen distribution for forecasting: ", pretty_dist_label(chosen_dist_simple))
  )
)
