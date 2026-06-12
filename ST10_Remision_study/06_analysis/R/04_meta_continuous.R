# Study: ST10_Remision_study
# Script: 04_meta_continuous.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Continuous outcomes - change in HbA1c (mmol/mol) and change in SBP (mmHg).
#          WMD where scales identical; SMD where they differ. Random-effects, REML+HKSJ.

message("=== 04: CONTINUOUS OUTCOMES (HbA1c, SBP change) ===")

run_metacont <- function(d, m_int, sd_int, n_int, m_comp, sd_comp, n_comp,
                         label, sm = "MD", file) {
  d2 <- d %>% filter(!is.na(.data[[m_int]]), !is.na(.data[[sd_int]]),
                     !is.na(.data[[m_comp]]), !is.na(.data[[sd_comp]]))
  if (nrow(d2) < 2) { message("Too few studies for ", label, " - skipped."); return(NULL) }
  m <- metacont(
    n.e = d2[[n_int]],  mean.e = d2[[m_int]],  sd.e = d2[[sd_int]],
    n.c = d2[[n_comp]], mean.c = d2[[m_comp]], sd.c = d2[[sd_comp]],
    studlab = d2$study_id, data = d2,
    sm = sm, method.tau = "REML", method.random.ci = HKSJ,
    prediction = TRUE, random = TRUE, common = FALSE
  )
  cat("\n-- ", label, " --\n", sep = ""); print(summary(m))
  save_plot(forest(m, label.e = "Lifestyle", label.c = "Comparator",
                   smlab = label), file, height = 5)
  m
}

# Use per-arm sample size = remission denominators as a proxy when arm n columns
# are not separately extracted; replace with dedicated n columns if available.
d <- dat %>% mutate(n_int_c = rem_n_int, n_comp_c = rem_n_comp)

m_hba1c <- run_metacont(
  d, "hba1c_change_mean_int", "hba1c_change_sd_int", "n_int_c",
     "hba1c_change_mean_comp", "hba1c_change_sd_comp", "n_comp_c",
  label = "Change in HbA1c (mmol/mol), MD", sm = "MD",
  file = "fig_forest_hba1c_change.png")

m_sbp <- run_metacont(
  d, "sbp_change_mean_int", "sbp_change_sd_int", "n_int_c",
     "sbp_change_mean_comp", "sbp_change_sd_comp", "n_comp_c",
  label = "Change in SBP (mmHg), MD", sm = "MD",
  file = "fig_forest_sbp_change.png")

saveRDS(list(hba1c = m_hba1c, sbp = m_sbp),
        file.path(paths$results, "meta_continuous.rds"))

message("=== 04 COMPLETE ===")
