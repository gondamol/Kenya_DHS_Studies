# Study: ST10_Remision_study
# Script: 00_setup.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Packages, paths, and shared helpers for the ST10 meta-analysis pipeline.
#          Implements the synthesis methods pre-specified in the protocol
#          (random-effects REML + Hartung-Knapp-Sidik-Jonkman CIs; Freeman-Tukey /
#          logit pooled proportions; prediction intervals; subgroup, sensitivity,
#          meta-regression, publication-bias, RoB, and PRISMA components).

message("=== ST10 META-ANALYSIS: SETUP ===")

# --- Packages --------------------------------------------------------------
required_pkgs <- c(
  "meta",        # high-level meta-analysis (metabin, metaprop, metacont, metareg)
  "metafor",     # rma models, GLMM, Peto OR, diagnostics
  "metasens",    # sensitivity for small-study effects / publication bias
  "dplyr", "tidyr", "readr", "stringr", "ggplot2"
)
optional_pkgs <- c(
  "robvis",          # RoB 2 traffic-light / weighted-bar plots
  "PRISMA2020"       # PRISMA 2020 interactive flow diagram
)

install_if_missing <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    message("Installing missing packages: ", paste(miss, collapse = ", "))
    install.packages(miss, repos = "https://cloud.r-project.org")
  }
}
# Uncomment on first run to provision the environment:
# install_if_missing(c(required_pkgs, optional_pkgs))

suppressPackageStartupMessages({
  library(meta)
  library(metafor)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

options(scipen = 999)

# Global meta-analysis defaults aligned with the protocol -------------------
settings.meta(
  method.tau = "REML",     # restricted maximum likelihood for between-study variance
  random = TRUE,
  common = FALSE,          # random-effects is the primary model
  prediction = TRUE        # report prediction intervals for primary analyses
)
# Hartung-Knapp CIs are requested per-call via method.random.ci = "HK"
# (kept explicit at each call so reviewers can see it in the code).

# --- Paths -----------------------------------------------------------------
find_study_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    required_dirs <- c("01_protocol", "02_search", "04_extraction", "06_analysis")
    if (all(dir.exists(file.path(current_dir, required_dirs)))) return(current_dir)
    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir))
      stop("Unable to locate the ST10 study root from: ", start_dir)
    current_dir <- parent_dir
  }
}

study_root <- find_study_root()
paths <- list(
  study_root  = study_root,
  analysis    = file.path(study_root, "06_analysis"),
  R           = file.path(study_root, "06_analysis", "R"),
  data_input  = file.path(study_root, "06_analysis", "data_input"),
  results     = file.path(study_root, "06_analysis", "results"),
  rob_csv     = file.path(study_root, "05_risk_of_bias", "rob2_template.csv"),
  prisma_csv  = file.path(study_root, "08_reporting", "prisma_flow_counts.csv")
)
dir.create(paths$results, recursive = TRUE, showWarnings = FALSE)

# --- Helpers ---------------------------------------------------------------
HKSJ <- "HK"  # method.random.ci value for Hartung-Knapp-Sidik-Jonkman

# Load the analysis-ready input; falls back to the bundled example so the
# pipeline runs end-to-end before real data exist.
load_meta_input <- function(file = "meta_input.csv") {
  fp <- file.path(paths$data_input, file)
  if (!file.exists(fp)) {
    fp <- file.path(paths$data_input, "example_meta_input.csv")
    message("NOTE: '", file, "' not found — using example_meta_input.csv (SYNTHETIC).")
  }
  readr::read_csv(fp, show_col_types = FALSE)
}

save_plot <- function(plot_expr, file, width = 9, height = 6, res = 300) {
  out <- file.path(paths$results, file)
  png(out, width = width, height = height, units = "in", res = res)
  on.exit(dev.off(), add = TRUE)
  force(plot_expr)
  message("Saved: ", out)
  invisible(out)
}

heterogeneity_label <- function(i2) dplyr::case_when(
  is.na(i2)   ~ NA_character_,
  i2 < 25     ~ "low",
  i2 < 50     ~ "moderate",
  i2 < 75     ~ "substantial",
  TRUE        ~ "considerable"
)

message("Setup complete. Study root: ", study_root)
message("=== SETUP COMPLETE ===")
