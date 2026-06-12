# Study: ST10_Remision_study
# Script: 07_metaregression_patient_chars.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Objective 3. (a) DESCRIPTIVE comparison of baseline participant
#          characteristics by income setting (NOT meta-regressed - aggregate baseline
#          descriptors are not effect sizes). (b) EXPLORATORY random-effects
#          meta-regression of the trial-level remission rate on income group, run ONLY
#          if >=10 studies and >=10 obs per covariate (protocol guard).

message("=== 07: PATIENT CHARACTERISTICS & META-REGRESSION ===")

# (a) Descriptive baseline comparison by income tier ------------------------
char_summary <- dat %>%
  group_by(income_tier) %>%
  summarise(
    k = n(),
    age_mean   = round(mean(mean_age, na.rm = TRUE), 1),
    bmi_mean   = round(mean(mean_bmi, na.rm = TRUE), 1),
    dur_mean   = round(mean(disease_duration_years, na.rm = TRUE), 1),
    hba1c_mean = round(mean(baseline_hba1c, na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\n-- Baseline characteristics by income tier (descriptive) --\n")
print(char_summary, row.names = FALSE)
readr::write_csv(char_summary, file.path(paths$results, "patient_characteristics_by_income.csv"))
message("NOTE: Compare these descriptively against the HIC reference set ",
        "(Sherifali 2025). Do NOT meta-regress baseline descriptors themselves.")

# (b) Exploratory meta-regression: remission rate ~ income group ------------
mr_dat <- dat %>% filter(!is.na(rem_events_int), !is.na(rem_n_int))
k <- nrow(mr_dat)
n_levels <- length(unique(na.omit(mr_dat$income_tier)))

if (k >= 10 && k >= 10 * (n_levels - 1)) {
  m_prop <- metaprop(rem_events_int, rem_n_int, studlab = study_id, data = mr_dat,
                     sm = "PLOGIT", method.tau = "REML", random = TRUE, common = FALSE)
  mr <- metareg(m_prop, ~ income_tier)
  cat("\n-- Exploratory meta-regression: logit(remission) ~ income_tier --\n")
  print(mr)
  saveRDS(mr, file.path(paths$results, "metaregression_income.rds"))
} else {
  message(sprintf(paste0("Meta-regression NOT run: only %d studies (need >=10, and ",
                         ">=10 per covariate). Reporting income-tier differences ",
                         "descriptively only."), k))
}

message("=== 07 COMPLETE ===")
