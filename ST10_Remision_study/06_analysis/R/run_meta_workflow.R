# Study: ST10_Remision_study
# Script: run_meta_workflow.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Execute the ST10 meta-analysis pipeline end-to-end.
#          Usage:  Rscript 06_analysis/R/run_meta_workflow.R
#          Falls back to synthetic example data until meta_input.csv exists.

find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (all(dir.exists(file.path(d, c("01_protocol", "06_analysis"))))) return(d)
    p <- dirname(d); if (identical(p, d)) stop("ST10 root not found."); d <- p
  }
}
R_dir <- file.path(find_root(), "06_analysis", "R")

source(file.path(R_dir, "00_setup.R"))
source(file.path(R_dir, "01_import.R"))
source(file.path(R_dir, "02_meta_proportion.R"))
source(file.path(R_dir, "03_meta_binary.R"))
source(file.path(R_dir, "04_meta_continuous.R"))
source(file.path(R_dir, "05_heterogeneity_subgroup.R"))
source(file.path(R_dir, "06_sensitivity.R"))
source(file.path(R_dir, "07_metaregression_patient_chars.R"))
source(file.path(R_dir, "08_publication_bias.R"))
source(file.path(R_dir, "09_rob_grade_viz.R"))
source(file.path(R_dir, "10_prisma_flow.R"))

message("\n=== ST10 META-ANALYSIS WORKFLOW COMPLETE ===")
message("Outputs in 06_analysis/results/ (git-ignored).")
