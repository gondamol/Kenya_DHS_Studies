# Study: ST10_Remision_study
# Script: inter_rater_kappa.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Compute Cohen's kappa for inter-reviewer agreement at title/abstract and
#          full-text screening stages, from a Rayyan / paired-decision export.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# ---------------------------------------------------------------------------
# INPUT
# A CSV with one row per double-screened record and two decision columns coded
# as "include" / "exclude" (or 1/0). Adjust the path and column names to match
# your Rayyan export.
# ---------------------------------------------------------------------------
input_csv      <- "pilot_decisions.csv"   # place export in this folder
reviewer_a_col <- "reviewer_NWA"
reviewer_b_col <- "reviewer_JO"

cohen_kappa <- function(a, b) {
  stopifnot(length(a) == length(b))
  keep <- !is.na(a) & !is.na(b)
  a <- as.character(a[keep]); b <- as.character(b[keep])
  tab <- table(a, b)
  n   <- sum(tab)
  po  <- sum(diag(tab)) / n
  pe  <- sum(rowSums(tab) * colSums(tab)) / n^2
  kappa <- (po - pe) / (1 - pe)
  # Standard error and 95% CI (Cohen 1960 / Fleiss approximation)
  se  <- sqrt(po * (1 - po) / (n * (1 - pe)^2))
  list(
    n = n,
    observed_agreement = po,
    expected_agreement = pe,
    kappa = kappa,
    kappa_lci = kappa - 1.96 * se,
    kappa_uci = kappa + 1.96 * se,
    table = tab
  )
}

interpret <- function(k) dplyr::case_when(
  k < 0.20 ~ "poor",
  k < 0.40 ~ "fair",
  k < 0.60 ~ "moderate",
  k < 0.75 ~ "substantial (below 0.75 threshold)",
  TRUE     ~ "substantial/almost perfect (>=0.75 threshold met)"
)

if (file.exists(input_csv)) {
  dat <- readr::read_csv(input_csv, show_col_types = FALSE)
  res <- cohen_kappa(dat[[reviewer_a_col]], dat[[reviewer_b_col]])
  cat("Records double-screened :", res$n, "\n")
  cat("Observed agreement (Po) :", round(res$observed_agreement, 3), "\n")
  cat("Expected agreement (Pe) :", round(res$expected_agreement, 3), "\n")
  cat(sprintf("Cohen's kappa           : %.3f (95%% CI %.3f to %.3f)\n",
              res$kappa, res$kappa_lci, res$kappa_uci))
  cat("Interpretation          :", interpret(res$kappa), "\n\n")
  print(res$table)
} else {
  message("No '", input_csv, "' found. Export paired decisions from Rayyan into this ",
          "folder, then re-run. Demo on synthetic data:")
  set.seed(20260612)
  a <- sample(c("include", "exclude"), 50, TRUE, c(0.3, 0.7))
  b <- ifelse(runif(50) < 0.9, a, ifelse(a == "include", "exclude", "include"))
  res <- cohen_kappa(a, b)
  cat(sprintf("Demo kappa: %.3f (%s)\n", res$kappa, interpret(res$kappa)))
}
