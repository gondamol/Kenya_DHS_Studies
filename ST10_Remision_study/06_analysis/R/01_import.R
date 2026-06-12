# Study: ST10_Remision_study
# Script: 01_import.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Load and validate the analysis-ready meta-analysis input; derive the
#          objects the synthesis scripts consume. Run 00_setup.R first.

if (!exists("paths"))
  stop("Run 00_setup.R first (or use run_meta_workflow.R, which sources scripts in order).")

message("=== 01: IMPORT ===")

dat <- load_meta_input()

# Minimal schema check ------------------------------------------------------
required_cols <- c("study_id", "year", "income_tier", "intervention_category",
                   "rem_events_int", "rem_n_int", "rem_events_comp", "rem_n_comp")
missing_cols <- setdiff(required_cols, names(dat))
if (length(missing_cols))
  stop("meta_input is missing required columns: ", paste(missing_cols, collapse = ", "))

# Light validation / typing -------------------------------------------------
dat <- dat %>%
  mutate(
    income_tier = factor(income_tier,
                          levels = c("low", "lower-middle", "upper-middle")),
    intervention_category = factor(intervention_category),
    across(c(starts_with("rem_"), ends_with("_int"), ends_with("_comp")),
           ~ suppressWarnings(as.numeric(.)))
  )

# Sanity: events <= n
chk <- dat %>%
  filter(rem_events_int > rem_n_int | rem_events_comp > rem_n_comp)
if (nrow(chk)) warning("Studies with events > n (check extraction): ",
                       paste(chk$study_id, collapse = ", "))

message(sprintf("Loaded %d studies (%d with non-missing remission counts).",
                nrow(dat), sum(!is.na(dat$rem_events_int))))
message("Income-tier distribution:")
print(table(dat$income_tier, useNA = "ifany"))

message("=== 01 COMPLETE ===")
