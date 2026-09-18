#!/usr/bin/env Rscript
# Dual-survey synthesis using data.table only (no survey/haven/dplyr)

user_lib <- Sys.getenv("R_LIBS_USER", unset = path.expand("~/R/library"))
.libPaths(c(user_lib, .libPaths()))
suppressPackageStartupMessages(library(data.table))

root <- "/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD"
tab_dir <- file.path(root, "04_tables")
log_dir <- file.path(root, "08_logs")

pick <- function(dt, pattern) {
  row <- dt[grepl(pattern, label, ignore.case = TRUE)][1]
  if (nrow(row) == 0 || is.na(row$estimate[1])) stop("No match: ", pattern)
  row
}
fmt <- function(r) sprintf("%.1f (%.1f–%.1f)", r$pct, r$pct_low, r$pct_high)

steps_path <- file.path(tab_dir, "Table1_STEPS_KeyEstimates_R.csv")
if (!file.exists(steps_path)) steps_path <- file.path(tab_dir, "Table1_STEPS_KeyEstimates.csv")
dhs_path <- file.path(tab_dir, "Table5_DHS_KeyEstimates_R.csv")
if (!file.exists(dhs_path)) dhs_path <- file.path(tab_dir, "Table5_DHS_KeyEstimates.csv")

steps <- fread(steps_path)
dhs <- fread(dhs_path)
cat("STEPS:", basename(steps_path), " DHS:", basename(dhs_path), "\n")

htn_meas <- pick(steps, "Measured hypertension")
dm_meas <- pick(steps, "Measured diabetes")
htn_aware <- pick(steps, "HTN: aware")
htn_tx <- pick(steps, "HTN: on medication")
htn_ctrl <- pick(steps, "HTN: controlled \\(among all")
dm_aware <- pick(steps, "DM: aware")
dm_tx <- pick(steps, "DM: on medication")
dm_ctrl <- pick(steps, "DM: controlled")
dhs_htn <- pick(dhs, "Self-reported HTN")
dhs_dm <- pick(dhs, "Self-reported DM diagnosis")
dhs_htn_tx <- pick(dhs, "Treated among diagnosed HTN")
dhs_dm_tx <- pick(dhs, "Treated among diagnosed DM")
dhs_htn_ins <- pick(dhs, "Insured among diagnosed HTN")
dhs_dm_ins <- pick(dhs, "Insured among diagnosed DM")

pub <- data.table(
  Condition = c("Hypertension", "Diabetes"),
  `Measured prevalence STEPS % (95% CI)` = c(fmt(htn_meas), fmt(dm_meas)),
  `Self-reported diagnosis KDHS % (95% CI)` = c(fmt(dhs_htn), fmt(dhs_dm)),
  `Detection ratio` = c(
    sprintf("%.2f", dhs_htn$estimate / htn_meas$estimate),
    sprintf("%.2f", dhs_dm$estimate / dm_meas$estimate)
  ),
  `Aware among measured STEPS %` = c(fmt(htn_aware), fmt(dm_aware)),
  `Treated among measured STEPS %` = c(fmt(htn_tx), fmt(dm_tx)),
  `Controlled among measured STEPS %` = c(fmt(htn_ctrl), fmt(dm_ctrl)),
  `Treated among diagnosed KDHS %` = c(fmt(dhs_htn_tx), fmt(dhs_dm_tx)),
  `Insured among diagnosed KDHS %` = c(fmt(dhs_htn_ins), fmt(dhs_dm_ins)),
  `Hybrid treated among measured %` = c(
    sprintf("%.1f", 100 * htn_aware$estimate * dhs_htn_tx$estimate),
    sprintf("%.1f", 100 * dm_aware$estimate * dhs_dm_tx$estimate)
  ),
  `Population unmet need (not controlled) STEPS %` = c(
    sprintf("%.1f", 100 * htn_meas$estimate * (1 - htn_ctrl$estimate)),
    sprintf("%.1f", 100 * dm_meas$estimate * (1 - dm_ctrl$estimate))
  )
)
fwrite(pub, file.path(tab_dir, "Table6_DualSurvey_Publication_R.csv"))

a0 <- htn_aware$estimate; t0 <- dhs_htn_tx$estimate; pm0 <- htn_meas$estimate
grid <- CJ(a_mult = c(0.8, 1.0, 1.2), t_mult = c(0.8, 1.0, 1.2))
sens <- grid[, {
  a <- pmin(1, a0 * a_mult); t <- pmin(1, t0 * t_mult); h <- a * t
  .(awareness = a, tx_among_dx = t, treated_among_measured = h,
    population_tx_coverage = pm0 * h,
    population_untreated_disease = pm0 * (1 - h))
}, by = .(a_mult, t_mult)]
fwrite(sens, file.path(tab_dir, "Table7_Hybrid_Sensitivity_R.csv"))

summary <- c(
  engine = "R data.table + ultimate-cluster SE (+ sandwich check)",
  htn_detection_ratio = dhs_htn$estimate / htn_meas$estimate,
  dm_detection_ratio = dhs_dm$estimate / dm_meas$estimate,
  htn_hybrid_pct = 100 * a0 * t0,
  htn_steps_tx_pct = 100 * htn_tx$estimate,
  htn_pop_unmet_pct = 100 * pm0 * (1 - htn_ctrl$estimate)
)
writeLines(paste(names(summary), summary, sep = " = "),
           file.path(log_dir, "st12_dual_summary_R.txt"))
cat("\n=== Dual-survey (R) ===\n")
print(summary)
cat("\nWrote Table6_DualSurvey_Publication_R.csv\n")
