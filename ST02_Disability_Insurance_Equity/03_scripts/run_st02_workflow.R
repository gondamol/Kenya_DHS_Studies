# Study: ST02_Disability_Insurance_Equity
# Script: run_st02_workflow.R
# Author: Nichodemus Werre Amollo
# Date: 2026-04-05
# Purpose: Run the initial ST02 PR-based workflow end to end.

study_root <- normalizePath(".", winslash = "/", mustWork = TRUE)

source(file.path(study_root, "03_scripts", "00_setup.R"), local = globalenv())

run_section <- function(script_name, section_name) {
  tryCatch(
    source(file.path(study_root, "03_scripts", script_name), local = new.env(parent = globalenv())),
    error = function(e) {
      source(file.path(study_root, "03_scripts", "00_setup.R"), local = TRUE)
      append_error(section_name, conditionMessage(e))
      stop(e)
    }
  )
}

run_section("01_data_import.R", "SECTION 1")
run_section("02_variable_construction.R", "SECTION 2")
run_section("03_analysis_main.R", "SECTION 3")
run_section("04_tables_figures.R", "SECTION 4")
