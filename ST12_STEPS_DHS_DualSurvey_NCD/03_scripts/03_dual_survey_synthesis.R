# ST12 — Dual-survey synthesis from R survey outputs (with Python fallback)

message("=== ST12 R: Dual-survey synthesis ===")
source("/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD/03_scripts/00_setup.R")

p <- paths_st12()
ensure_dirs(p)

pick <- function(df, pattern) {
  row <- df %>% filter(grepl(pattern, label, ignore.case = TRUE)) %>% slice(1)
  if (nrow(row) == 0) stop("No row matching: ", pattern)
  row
}

# Prefer R tables; fall back to Python
steps_path <- file.path(p$tables, "Table1_STEPS_KeyEstimates_R.csv")
if (!file.exists(steps_path)) steps_path <- file.path(p$tables, "Table1_STEPS_KeyEstimates.csv")
dhs_path <- file.path(p$tables, "Table5_DHS_KeyEstimates_R.csv")
if (!file.exists(dhs_path)) dhs_path <- file.path(p$tables, "Table5_DHS_KeyEstimates.csv")

steps <- readr::read_csv(steps_path, show_col_types = FALSE)
dhs <- readr::read_csv(dhs_path, show_col_types = FALSE)
message("Using STEPS table: ", basename(steps_path))
message("Using DHS table:   ", basename(dhs_path))

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

fmt <- function(r) sprintf("%.1f (%.1f–%.1f)", r$pct, r$pct_low, r$pct_high)

pub <- tibble(
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

readr::write_csv(pub, file.path(p$tables, "Table6_DualSurvey_Publication_R.csv"))

# Sensitivity grid HTN
a0 <- htn_aware$estimate
t0 <- dhs_htn_tx$estimate
pm0 <- htn_meas$estimate
grid <- expand.grid(a_mult = c(0.8, 1.0, 1.2), t_mult = c(0.8, 1.0, 1.2))
sens <- grid %>%
  mutate(
    awareness = pmin(1, a0 * a_mult),
    tx_among_dx = pmin(1, t0 * t_mult),
    treated_among_measured = awareness * tx_among_dx,
    population_tx_coverage = pm0 * treated_among_measured,
    population_untreated_disease = pm0 * (1 - treated_among_measured)
  )
readr::write_csv(sens, file.path(p$tables, "Table7_Hybrid_Sensitivity_R.csv"))

summary <- list(
  engine = "R survey package",
  steps_table = basename(steps_path),
  dhs_table = basename(dhs_path),
  htn_detection_ratio = as.numeric(dhs_htn$estimate / htn_meas$estimate),
  dm_detection_ratio = as.numeric(dhs_dm$estimate / dm_meas$estimate),
  htn_hybrid_treated_among_measured_pct = 100 * a0 * t0,
  htn_steps_treated_among_measured_pct = 100 * htn_tx$estimate,
  htn_pop_unmet_not_controlled_pct = 100 * pm0 * (1 - htn_ctrl$estimate)
)
jsonlite_ok <- requireNamespace("jsonlite", quietly = TRUE)
if (jsonlite_ok) {
  jsonlite::write_json(summary, file.path(p$logs, "st12_dual_summary_R.json"),
                       auto_unbox = TRUE, pretty = TRUE)
} else {
  writeLines(capture.output(str(summary)), file.path(p$logs, "st12_dual_summary_R.txt"))
}

message("\n=== Dual-survey summary (R) ===")
print(summary)
message("Dual-survey synthesis complete.")
