# Study: ST10_Remision_study
# Script: 03_meta_binary.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Comparative (intervention vs comparator) remission as a RISK RATIO.
#          Primary: random-effects RR, REML + HKSJ, 0.5 continuity correction for
#          zero-event arms. Sensitivity: Peto OR and a GLMM that needs no correction.

message("=== 03: REMISSION RISK RATIO (intervention vs comparator) ===")

bin_dat <- dat %>%
  filter(!is.na(rem_events_int), !is.na(rem_n_int),
         !is.na(rem_events_comp), !is.na(rem_n_comp))

# Primary: random-effects RR ------------------------------------------------
m_rr <- metabin(
  event.e = rem_events_int, n.e = rem_n_int,
  event.c = rem_events_comp, n.c = rem_n_comp,
  studlab = study_id, data = bin_dat,
  sm = "RR", method = "Inverse", method.tau = "REML",
  method.random.ci = HKSJ, incr = 0.5, allincr = FALSE,
  prediction = TRUE, random = TRUE, common = FALSE
)
cat("\n-- Random-effects risk ratio --\n"); print(summary(m_rr))

save_plot(forest(m_rr, label.e = "Lifestyle", label.c = "Comparator",
                 smlab = "Remission risk ratio\n(random effects, REML + HKSJ)"),
          "fig_forest_remission_RR.png", height = 5)

# Sensitivity 1: Peto OR (sparse, approximately balanced data) ---------------
m_peto <- metabin(
  event.e = rem_events_int, n.e = rem_n_int,
  event.c = rem_events_comp, n.c = rem_n_comp,
  studlab = study_id, data = bin_dat,
  sm = "OR", method = "Peto", random = TRUE, common = FALSE
)
cat("\n-- Sensitivity: Peto odds ratio --\n"); print(summary(m_peto))

# Sensitivity 2: binomial-normal GLMM (no continuity correction) -------------
m_glmm <- tryCatch(
  metabin(event.e = rem_events_int, n.e = rem_n_int,
          event.c = rem_events_comp, n.c = rem_n_comp,
          studlab = study_id, data = bin_dat,
          sm = "OR", method = "GLMM", random = TRUE, common = FALSE),
  error = function(e) { message("GLMM did not converge: ", conditionMessage(e)); NULL }
)
if (!is.null(m_glmm)) { cat("\n-- Sensitivity: logistic GLMM --\n"); print(summary(m_glmm)) }

saveRDS(list(rr = m_rr, peto = m_peto, glmm = m_glmm),
        file.path(paths$results, "meta_binary.rds"))

message("=== 03 COMPLETE ===")
