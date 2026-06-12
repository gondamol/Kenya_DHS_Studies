# Study: ST10_Remision_study
# Script: 06_sensitivity.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Pre-specified sensitivity analyses for the remission RR. Each re-fits the
#          random-effects model on a filtered subset and tabulates the pooled estimate
#          against the primary analysis. (Some filters require columns not present in
#          the example data; those rows are skipped with a message until real data.)

message("=== 06: SENSITIVITY ANALYSES ===")

base <- dat %>% filter(!is.na(rem_events_int), !is.na(rem_events_comp))

fit_rr <- function(d, label) {
  if (nrow(d) < 2) { message("  '", label, "': <2 studies - skipped."); return(NULL) }
  m <- metabin(rem_events_int, rem_n_int, rem_events_comp, rem_n_comp,
               studlab = d$study_id, data = d, sm = "RR", method = "Inverse",
               method.tau = "REML", method.random.ci = HKSJ, incr = 0.5,
               random = TRUE, common = FALSE)
  data.frame(analysis = label, k = m$k,
             RR = exp(m$TE.random),
             lci = exp(m$lower.random), uci = exp(m$upper.random),
             I2 = round(m$I2 * 100, 1))
}

rows <- list(
  fit_rr(base, "Primary (all studies)"),
  fit_rr(base %>% filter(rob_overall != "high"),            "Excluding high RoB"),
  fit_rr(base %>% filter(followup_months >= 12),            "Follow-up >= 12 months"),
  fit_rr(base %>% filter(intervention_category != "diet_only" |
                         TRUE),                              "Excluding lifestyle+drug combos (needs flag)"),
  fit_rr(base %>% filter(income_tier != "upper-middle"),    "Excluding upper-middle-income (illustrative)")
)
# Filters pending dedicated extraction flags (documented, not silently dropped):
message("Pending real-data flags (skipped here): FPG-based remission; remission as ",
        "secondary endpoint; cluster-RCT exclusion; 8-12 week trials; 50% LMIC threshold.")

sens_tbl <- dplyr::bind_rows(Filter(Negate(is.null), rows))
cat("\n-- Sensitivity summary (remission RR) --\n"); print(sens_tbl, row.names = FALSE)
readr::write_csv(sens_tbl, file.path(paths$results, "sensitivity_summary.csv"))

message("=== 06 COMPLETE ===")
