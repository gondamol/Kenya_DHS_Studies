#!/usr/bin/env Rscript
# ST12 DHS analysis with survey package (from slim CSV; no haven)

user_lib <- Sys.getenv("R_LIBS_USER", unset = path.expand("~/R/library"))
.libPaths(c(user_lib, .libPaths()))
options(survey.lonely.psu = "adjust", stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(survey)
  library(dplyr)
  library(data.table)
})

root <- "/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD"
tab <- file.path(root, "04_tables")
slim <- file.path(root, "07_derived_data", "st12_dhs_analytic_slim.csv")
d <- as.data.frame(fread(slim))
cat("survey", as.character(packageVersion("survey")), " DHS n=", nrow(d), "\n")

des <- svydesign(ids = ~psu, strata = ~stratum, weights = ~weight, data = d, nest = TRUE)

prop_row <- function(design, var, label, domain) {
  fml <- as.formula(paste0("~", var))
  est <- svymean(fml, design, na.rm = TRUE)
  mu <- as.numeric(est[1]); se <- as.numeric(SE(est)[1])
  ci <- tryCatch(confint(est, df = degf(design)),
                 error = function(e) cbind(mu - 1.96 * se, mu + 1.96 * se))
  lo <- max(0, as.numeric(ci[1, 1])); hi <- min(1, as.numeric(ci[1, 2]))
  n <- sum(!is.na(design$variables[[var]]))
  cat(sprintf("  %s: %.1f (%.1f–%.1f) n=%s\n", label, 100*mu, 100*lo, 100*hi, n))
  data.frame(label = label, estimate = mu, se = se, ci_low = lo, ci_high = hi,
             pct = 100*mu, pct_low = 100*lo, pct_high = 100*hi, n = n,
             domain = domain, stringsAsFactors = FALSE)
}

rows <- list(
  prop_row(des, "htn_dx", "Self-reported HTN diagnosis prevalence", "dhs_all"),
  prop_row(des, "dm_dx", "Self-reported DM diagnosis prevalence", "dhs_all"),
  prop_row(des, "any_dx", "Self-reported HTN or DM diagnosis", "dhs_all")
)
htn <- subset(des, htn_dx == 1)
dm <- subset(des, dm_dx == 1)
anyd <- subset(des, any_dx == 1)
rows <- c(rows, list(
  prop_row(htn, "htn_treated_if_dx", "Treated among diagnosed HTN", "dhs_htn_dx"),
  prop_row(dm, "dm_treated_if_dx", "Treated among diagnosed DM", "dhs_dm_dx"),
  prop_row(anyd, "any_treated_if_dx", "Treated among any diagnosed", "dhs_any_dx"),
  prop_row(htn, "insured_any", "Insured among diagnosed HTN", "dhs_htn_dx"),
  prop_row(dm, "insured_any", "Insured among diagnosed DM", "dhs_dm_dx"),
  prop_row(anyd, "insured_any", "Insured among any diagnosed", "dhs_any_dx"),
  prop_row(subset(des, htn_dx == 1 & insured_any == 1), "htn_treated_if_dx",
           "HTN tx among insured diagnosed", "dhs_htn_ins"),
  prop_row(subset(des, htn_dx == 1 & insured_any == 0), "htn_treated_if_dx",
           "HTN tx among uninsured diagnosed", "dhs_htn_ins")
))

key <- bind_rows(rows)
fwrite(as.data.table(key), file.path(tab, "Table5_DHS_KeyEstimates_survey.csv"))

py <- fread(file.path(tab, "Table5_DHS_KeyEstimates.csv"))
cmp <- merge(
  as.data.table(key)[, .(label, pct_survey = pct, n_survey = n)],
  py[, .(label, pct_Py = pct, n_Py = n)],
  by = "label", all = TRUE
)
cmp[, diff := pct_survey - pct_Py]
fwrite(cmp, file.path(tab, "Table_Compare_DHS_survey_pkg.csv"))
cat("\n=== survey package vs Python (DHS) ===\n")
print(cmp[!is.na(pct_survey) & !is.na(pct_Py),
          .(label, pct_survey = round(pct_survey, 2), pct_Py = round(pct_Py, 2),
            diff = round(diff, 4))])
cat("Max |diff| pp:", max(abs(cmp$diff), na.rm = TRUE), "\n")
cat("DHS survey-package analysis complete.\n")
