#!/usr/bin/env Rscript
# ST12 STEPS analysis with the official survey package (svydesign / svymean)
# Loads CSV (no haven required). Uses dplyr + survey from R_LIBS_USER.

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
der <- file.path(root, "07_derived_data")
dir.create(tab, showWarnings = FALSE, recursive = TRUE)

cat("survey", as.character(packageVersion("survey")),
    "| dplyr", as.character(packageVersion("dplyr")), "\n")

# Prefer pre-built analytic slim; else construct from raw CSV
slim <- file.path(der, "st12_steps_analytic_slim.csv")
if (file.exists(slim)) {
  d <- as.data.frame(fread(slim))
  cat("Loaded slim n=", nrow(d), "\n")
} else {
  stop("Run Python construct first or provide slim CSV at ", slim)
}

if (!"sex_f" %in% names(d) && "sex" %in% names(d)) d$sex_f <- as.character(d$sex)
if (!"htn_pop_controlled" %in% names(d)) {
  d$htn_pop_controlled <- ifelse(is.na(d$htn_measured), NA,
    ifelse(d$htn_measured == 1, d$htn_controlled, 0))
}
if (!"htn_ctrl_among_tx" %in% names(d) && "htn_controlled_among_treated" %in% names(d)) {
  d$htn_ctrl_among_tx <- d$htn_controlled_among_treated
}

s2 <- d %>% filter(!is.na(wstep2), wstep2 > 0)
s3 <- d %>% filter(!is.na(wstep3), wstep3 > 0)

des2 <- svydesign(ids = ~psu, strata = ~stratum, weights = ~wstep2, data = s2, nest = TRUE)
des3 <- svydesign(ids = ~psu, strata = ~stratum, weights = ~wstep3, data = s3, nest = TRUE)

prop_row <- function(design, var, label, domain) {
  fml <- as.formula(paste0("~", var))
  est <- svymean(fml, design, na.rm = TRUE)
  mu <- as.numeric(est[1])
  se <- as.numeric(SE(est)[1])
  ci <- tryCatch(confint(est, df = degf(design)),
                 error = function(e) cbind(mu - 1.96 * se, mu + 1.96 * se))
  lo <- max(0, as.numeric(ci[1, 1]))
  hi <- min(1, as.numeric(ci[1, 2]))
  n <- sum(!is.na(design$variables[[var]]))
  cat(sprintf("  %s: %.1f (%.1f–%.1f) n=%s\n", label, 100*mu, 100*lo, 100*hi, n))
  data.frame(
    label = label, estimate = mu, se = se, ci_low = lo, ci_high = hi,
    pct = 100 * mu, pct_low = 100 * lo, pct_high = 100 * hi,
    n = n, domain = domain, stringsAsFactors = FALSE
  )
}

rows <- list()
rows[[1]] <- prop_row(des2, "htn_measured", "Measured hypertension prevalence", "step2_all")
rows[[2]] <- prop_row(des2, "overweight_obese", "Overweight or obese (BMI>=25)", "step2_all")
rows[[3]] <- prop_row(des2, "obese", "Obesity (BMI>=30)", "step2_all")
rows[[4]] <- prop_row(des2, "bp_ever_measured", "Ever had blood pressure measured", "step2_all")
rows[[5]] <- prop_row(des2, "htn_pop_controlled", "Population on controlled HTN treatment", "step2_all")

htn_des <- subset(des2, htn_measured == 1)
rows[[6]] <- prop_row(htn_des, "htn_aware", "HTN: aware (told or on meds)", "step2_htn")
rows[[7]] <- prop_row(htn_des, "htn_treated", "HTN: on medication", "step2_htn")
rows[[8]] <- prop_row(htn_des, "htn_controlled", "HTN: controlled (among all HTN)", "step2_htn")
tx_des <- subset(des2, htn_measured == 1 & htn_treated == 1)
ctrl_var <- if ("htn_ctrl_among_tx" %in% names(s2)) "htn_ctrl_among_tx" else "htn_controlled"
if ("htn_controlled_among_treated" %in% names(s2)) {
  # ensure design has column
  des2 <- update(des2, htn_ctrl_among_tx = htn_controlled_among_treated)
  tx_des <- subset(des2, htn_measured == 1 & htn_treated == 1)
  ctrl_var <- "htn_ctrl_among_tx"
}
rows[[9]] <- prop_row(tx_des, ctrl_var, "HTN: controlled among treated", "step2_htn")

rows[[10]] <- prop_row(des3, "dm_measured", "Measured diabetes prevalence", "step3_all")
rows[[11]] <- prop_row(des3, "gluc_ever_measured", "Ever had blood glucose measured", "step3_all")
rows[[12]] <- prop_row(des3, "chol_raised", "Raised total cholesterol (>=5.0 mmol/L)", "step3_all")
dm_des <- subset(des3, dm_measured == 1)
rows[[13]] <- prop_row(dm_des, "dm_aware", "DM: aware", "step3_dm")
rows[[14]] <- prop_row(dm_des, "dm_treated", "DM: on medication", "step3_dm")
rows[[15]] <- prop_row(dm_des, "dm_controlled", "DM: controlled", "step3_dm")

key <- bind_rows(rows)
fwrite(as.data.table(key), file.path(tab, "Table1_STEPS_KeyEstimates_survey.csv"))

# Compare to Python and Rlib
py <- fread(file.path(tab, "Table1_STEPS_KeyEstimates.csv"))
cmp <- merge(
  as.data.table(key)[, .(label, pct_survey = pct, n_survey = n)],
  py[, .(label, pct_Py = pct, n_Py = n)],
  by = "label", all = TRUE
)
if (file.exists(file.path(tab, "Table1_STEPS_KeyEstimates_R.csv"))) {
  rlib <- fread(file.path(tab, "Table1_STEPS_KeyEstimates_R.csv"))
  cmp <- merge(cmp, rlib[, .(label, pct_Rlib = pct)], by = "label", all = TRUE)
  cmp[, diff_survey_py := pct_survey - pct_Py]
  cmp[, diff_survey_rlib := pct_survey - pct_Rlib]
}
fwrite(cmp, file.path(tab, "Table_Compare_survey_pkg.csv"))
cat("\n=== survey package vs Python ===\n")
print(cmp[!is.na(pct_survey) & !is.na(pct_Py),
          .(label, pct_survey = round(pct_survey, 2), pct_Py = round(pct_Py, 2),
            diff = round(pct_survey - pct_Py, 4))])
cat("Max |survey-Python| pp:",
    max(abs(cmp$pct_survey - cmp$pct_Py), na.rm = TRUE), "\n")
cat("\nSTEPS survey-package analysis complete.\n")
