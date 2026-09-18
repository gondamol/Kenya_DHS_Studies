#!/usr/bin/env Rscript
# ST12 workflow using packages available in ~/R/library (data.table, sandwich, ...)
# For full survey/haven scripts see run_st12_workflow.R (requires those packages).

user_lib <- Sys.getenv("R_LIBS_USER", unset = path.expand("~/R/library"))
.libPaths(c(user_lib, .libPaths()))
Sys.setenv(ST12_ROOT = "/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD")
scripts <- file.path(Sys.getenv("ST12_ROOT"), "03_scripts")

cat("============================================================\n")
cat("ST12 R-library workflow\n")
cat("R:", as.character(getRversion()), "\n")
cat("lib:", .libPaths()[[1]], "\n")
cat("============================================================\n\n")

source(file.path(scripts, "01_steps_analyze_Rlib.R"), echo = FALSE)
source(file.path(scripts, "02_dhs_analyze_Rlib.R"), echo = FALSE)

# Dual synthesis (data.table only)
source(file.path(scripts, "03_dual_survey_Rlib.R"), echo = FALSE)

cat("\nDone. Compare tables: Table_Compare_*_R_vs_Python.csv\n")
cat("R dual table: Table6_DualSurvey_Publication_R.csv\n")
