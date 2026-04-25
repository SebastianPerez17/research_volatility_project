# Run the full research volatility pipeline

# Helper function to get the directory of the current script
this_file <- function() {
  is_project_runall <- function(path) {
    if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) return(FALSE)
    normalized <- tryCatch(normalizePath(path, mustWork = TRUE), error = function(e) NA_character_)
    if (is.na(normalized)) return(FALSE)
    identical(basename(normalized), "run_all.R") && dir.exists(file.path(dirname(normalized), "scripts"))
  }

  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    file_path <- sub("^--file=", "", file_arg[1])
    if (is_project_runall(file_path)) {
      return(normalizePath(file_path, mustWork = TRUE))
    }
  }

  # When sourced(), an execution frame often carries the source file path in $ofile
  n_frames <- sys.nframe()
  if (n_frames > 0) {
    for (i in rev(seq_len(n_frames))) {
      frame_i <- sys.frame(i)
      if (is_project_runall(frame_i$ofile)) {
        return(normalizePath(frame_i$ofile, mustWork = TRUE))
      }
    }
  }

  # Additional sourced() fallback: inspect source(...) calls in the stack
  calls <- sys.calls()
  if (length(calls) > 0) {
    for (i in rev(seq_along(calls))) {
      call_i <- calls[[i]]
      if (!is.call(call_i)) next
      call_head <- paste(deparse(call_i[[1]]), collapse = "")
      if (!grepl("(^|::)source$", call_head)) next

      # First, try extracting a quoted run_all.R path directly from the call text
      call_txt <- paste(deparse(call_i), collapse = " ")
      m <- regexpr("\"[^\"]*run_all\\.R\"", call_txt)
      if (m[1] > 0) {
        src_path_txt <- regmatches(call_txt, m)
        src_path_txt <- sub("^\"", "", sub("\"$", "", src_path_txt))
        if (is_project_runall(src_path_txt)) {
          return(normalizePath(src_path_txt, mustWork = TRUE))
        }
      }

      src_arg <- NULL
      call_names <- names(call_i)
      if (!is.null(call_names) && "file" %in% call_names) {
        src_arg <- call_i[["file"]]
      } else if (length(call_i) >= 2) {
        src_arg <- call_i[[2]]
      }

      src_path <- tryCatch(eval(src_arg, envir = parent.frame()), error = function(e) NULL)
      if (is_project_runall(src_path)) {
        return(normalizePath(src_path, mustWork = TRUE))
      }
    }
  }

  # Fallback: search upward from current directory for project root marker
  probe <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    if (file.exists(file.path(probe, "run_all.R")) && dir.exists(file.path(probe, "scripts"))) {
      return(normalizePath(file.path(probe, "run_all.R"), mustWork = TRUE))
    }
    parent <- dirname(probe)
    if (identical(parent, probe)) break
    probe <- parent
  }

  stop("Cannot determine run_all.R path. Run with `Rscript run_all.R` from project root or source the file from within the repository.")
}

# Set working directory to project root
project_root <- dirname(this_file())

run_pipeline <- function() {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)

  # Pipeline stages:
  source(file.path("scripts", "00_config.R"), local = FALSE)
  source(file.path("scripts", "01_packages.R"), local = FALSE)
  source(file.path("scripts", "02_benchmark_helpers.R"), local = FALSE)
  source(file.path("scripts", "03_data_download.R"), local = FALSE)
  source(file.path("scripts", "04_exploratory.R"), local = FALSE)
  source(file.path("scripts", "05_arch_selection.R"), local = FALSE)
  source(file.path("scripts", "06_benchmark_models.R"), local = FALSE)
  source(file.path("scripts", "07_comparison_helpers.R"), local = FALSE)
  source(file.path("scripts", "08_in_sample_candidates.R"), local = FALSE)
  source(file.path("scripts", "09_distribution_choice.R"), local = FALSE)
  source(file.path("scripts", "10_rolling_forecasts.R"), local = FALSE)
  source(file.path("scripts", "12_tail_risk_backtests.R"), local = FALSE)
  source(file.path("scripts", "13_simple_risk_benchmarks.R"), local = FALSE)
  source(file.path("scripts", "14_robustness_checks.R"), local = FALSE)
  source(file.path("scripts", "11_final_summary.R"), local = FALSE)
}

run_pipeline()
