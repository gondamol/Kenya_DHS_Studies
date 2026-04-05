# Study: ST03_NCD_Insurance_Service_Use
# Script: run_st03_workflow.R
# Author: Nichodemus Werre Amollo
# Date: 2026-03-29
# Purpose: Execute the modular ST03 reproducible workflow end-to-end.

find_study_root_runner <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    required_dirs <- c("01_protocol", "03_scripts", "06_manuscript")
    if (all(dir.exists(file.path(current_dir, required_dirs)))) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Unable to locate the ST03 study root for workflow execution.")
    }
    current_dir <- parent_dir
  }
}

study_root <- find_study_root_runner()
script_dir <- file.path(study_root, "03_scripts")

source(file.path(script_dir, "00_setup.R"), local = TRUE)
source(file.path(script_dir, "01_data_import.R"), local = TRUE)
source(file.path(script_dir, "02_data_cleaning.R"), local = TRUE)
source(file.path(script_dir, "03_analysis_main.R"), local = TRUE)
source(file.path(script_dir, "04_tables_figures.R"), local = TRUE)
source(file.path(script_dir, "05_checks_sensitivity.R"), local = TRUE)
