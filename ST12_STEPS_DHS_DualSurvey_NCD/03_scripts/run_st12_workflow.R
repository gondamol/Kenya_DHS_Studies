#!/usr/bin/env Rscript
# ST12 full R workflow: STEPS → DHS → dual synthesis
# Requires packages in R_LIBS_USER (survey, haven, dplyr, ...)

Sys.setenv(ST12_ROOT = "/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD")
root <- Sys.getenv("ST12_ROOT")
scripts <- file.path(root, "03_scripts")

cat("ST12 R workflow\n")
cat("R version:", as.character(getRversion()), "\n")
cat("R_LIBS_USER:", Sys.getenv("R_LIBS_USER", unset = path.expand("~/R/library")), "\n\n")

source(file.path(scripts, "01_steps_construct_analyze.R"))
source(file.path(scripts, "02_dhs_construct_analyze.R"))
source(file.path(scripts, "03_dual_survey_synthesis.R"))

cat("\n=== ST12 R workflow complete ===\n")
cat("Tables written with _R suffix in 04_tables/\n")
