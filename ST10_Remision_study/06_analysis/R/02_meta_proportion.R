# Study: ST10_Remision_study
# Script: 02_meta_proportion.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Pooled single-group remission PROPORTION across intervention arms.
#          Per protocol: random-effects on the logit and Freeman-Tukey double-arcsine
#          scales (retains zero-event studies), REML, with prediction interval.

message("=== 02: POOLED REMISSION PROPORTION ===")

prop_dat <- dat %>% filter(!is.na(rem_events_int), !is.na(rem_n_int))

# Logit-transformed pooled proportion (primary presentation) ----------------
m_prop_logit <- metaprop(
  event = rem_events_int, n = rem_n_int, studlab = study_id, data = prop_dat,
  sm = "PLOGIT", method = "Inverse", method.tau = "REML",
  method.random.ci = HKSJ, prediction = TRUE, random = TRUE, common = FALSE
)

# Freeman-Tukey double-arcsine (sensitivity; stabilises variance near 0/1) ---
m_prop_ft <- metaprop(
  event = rem_events_int, n = rem_n_int, studlab = study_id, data = prop_dat,
  sm = "PFT", method.tau = "REML", method.random.ci = HKSJ,
  prediction = TRUE, random = TRUE, common = FALSE
)

cat("\n-- Logit-scale pooled remission proportion --\n");        print(summary(m_prop_logit))
cat("\n-- Freeman-Tukey pooled remission proportion --\n");      print(summary(m_prop_ft))

save_plot(forest(m_prop_logit, xlim = c(0, 1),
                 leftcols = c("studlab", "event", "n"),
                 smlab = "Remission proportion\n(random effects, logit)"),
          "fig_forest_remission_proportion.png", height = 5)

# Persist objects for downstream scripts / reporting
saveRDS(list(logit = m_prop_logit, ft = m_prop_ft),
        file.path(paths$results, "meta_proportion.rds"))

message("=== 02 COMPLETE ===")
