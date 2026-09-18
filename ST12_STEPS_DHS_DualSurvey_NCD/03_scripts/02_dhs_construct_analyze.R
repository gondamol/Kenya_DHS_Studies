# ST12 — KDHS 2022 NCD module (R survey package)

message("=== ST12 R: DHS construct + analyse ===")
source("/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD/03_scripts/00_setup.R")

p <- paths_st12()
ensure_dirs(p)

message("Reading IR/MR/PR …")
ir <- haven::read_dta(p$ir, col_select = any_of(c(
  "v001", "v002", "v003", "v005", "v012", "v021", "v022", "v024", "v025",
  "v106", "v190", "chd01", "chd02", "chd05", "chd06", "chd07", "chd10"
)))
mr <- haven::read_dta(p$mr, col_select = any_of(c(
  "mv001", "mv002", "mv003", "mv005", "mv012", "mv021", "mv022", "mv024", "mv025",
  "mv106", "mv190", "mchd01", "mchd02", "mchd05", "mchd06", "mchd07", "mchd10"
)))
pr <- haven::read_dta(p$pr, col_select = any_of(c(
  "hv001", "hv002", "hvidx", "hv005", "hv021", "hv022", "hv102", "hv103",
  "hv104", "hv105", "hv118", "hv028", "sh27", "sh28a"
)))
message("  IR=", nrow(ir), " MR=", nrow(mr), " PR=", nrow(pr))

pr_c <- pr %>%
  transmute(
    cluster = as.numeric(hv001),
    household = as.numeric(hv002),
    line = as.numeric(hvidx),
    insured_any = yn_dhs(sh27),
    insured_nhif = yn_dhs(sh28a),
    sex_code = as.numeric(hv104),
    age_pr = as.numeric(hv105),
    hh_weight = as.numeric(hv005) / 1e6,
    male_eligible = yn_dhs(hv118),
    strata = as.numeric(hv022)
  )

# Male subsample correction factors
men_pr <- pr_c %>%
  filter(sex_code == 1, age_pr >= 15, age_pr <= 54, !is.na(hh_weight), !is.na(strata))
factors <- men_pr %>%
  group_by(strata) %>%
  summarise(
    total_w = sum(hh_weight, na.rm = TRUE),
    sub_w = sum(hh_weight[male_eligible == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(male_pool_factor = ifelse(sub_w > 0, total_w / sub_w, 1))

women <- ir %>%
  transmute(
    cluster = as.numeric(v001),
    household = as.numeric(v002),
    line = as.numeric(v003),
    sex = "Women",
    age = as.numeric(v012),
    weight = as.numeric(v005) / 1e6,
    psu = as.numeric(v021),
    stratum = as.numeric(v022),
    residence = as.character(haven::as_factor(v025)),
    htn_dx = yn_dhs(chd02),
    htn_med = yn_dhs(chd05),
    dm_dx = yn_dhs(chd07),
    dm_med = yn_dhs(chd10)
  )

men <- mr %>%
  transmute(
    cluster = as.numeric(mv001),
    household = as.numeric(mv002),
    line = as.numeric(mv003),
    sex = "Men",
    age = as.numeric(mv012),
    weight_raw = as.numeric(mv005) / 1e6,
    psu = as.numeric(mv021),
    stratum = as.numeric(mv022),
    residence = as.character(haven::as_factor(mv025)),
    htn_dx = yn_dhs(mchd02),
    htn_med = yn_dhs(mchd05),
    dm_dx = yn_dhs(mchd07),
    dm_med = yn_dhs(mchd10)
  ) %>%
  left_join(factors %>% select(strata, male_pool_factor), by = c("stratum" = "strata")) %>%
  mutate(
    male_pool_factor = ifelse(is.na(male_pool_factor), 1, male_pool_factor),
    weight = weight_raw * male_pool_factor
  )

pr_keep <- pr_c %>% select(cluster, household, line, insured_any, insured_nhif)

adults <- bind_rows(
  women,
  men %>% select(-weight_raw, -male_pool_factor)
) %>%
  left_join(pr_keep, by = c("cluster", "household", "line")) %>%
  filter(age >= 18, age <= 69) %>%
  mutate(
    htn_dx = ifelse(is.na(htn_dx), 0, htn_dx),
    dm_dx = ifelse(is.na(dm_dx), 0, dm_dx),
    any_dx = as.numeric(htn_dx == 1 | dm_dx == 1),
    htn_treated_if_dx = ifelse(htn_dx == 1, as.numeric(htn_med == 1), NA_real_),
    dm_treated_if_dx = ifelse(dm_dx == 1, as.numeric(dm_med == 1), NA_real_),
    any_treated_if_dx = ifelse(
      any_dx == 1,
      as.numeric((htn_dx == 1 & htn_med == 1) | (dm_dx == 1 & dm_med == 1)),
      NA_real_
    )
  )

message("  Analytic adults 18-69 n=", nrow(adults),
        " women=", sum(adults$sex == "Women"),
        " men=", sum(adults$sex == "Men"))

saveRDS(adults, file.path(p$derived, "st12_dhs_analytic_R.rds"))

des <- svydesign(
  ids = ~psu, strata = ~stratum, weights = ~weight,
  data = adults, nest = TRUE
)

rows <- list()
add <- function(des, formula, label, domain) {
  r <- svy_prop_row(des, formula, label)
  r$domain <- domain
  rows[[length(rows) + 1]] <<- r
  message(sprintf("  %s: %s n=%s", label, fmt_pct_ci(r$estimate, r$ci_low, r$ci_high), r$n))
}

add(des, ~htn_dx, "Self-reported HTN diagnosis prevalence", "dhs_all")
add(des, ~dm_dx, "Self-reported DM diagnosis prevalence", "dhs_all")
add(des, ~any_dx, "Self-reported HTN or DM diagnosis", "dhs_all")

htn_dx <- subset(des, htn_dx == 1)
dm_dx <- subset(des, dm_dx == 1)
any_dx <- subset(des, any_dx == 1)
add(htn_dx, ~htn_treated_if_dx, "Treated among diagnosed HTN", "dhs_htn_dx")
add(dm_dx, ~dm_treated_if_dx, "Treated among diagnosed DM", "dhs_dm_dx")
add(any_dx, ~any_treated_if_dx, "Treated among any diagnosed", "dhs_any_dx")
add(htn_dx, ~insured_any, "Insured among diagnosed HTN", "dhs_htn_dx")
add(dm_dx, ~insured_any, "Insured among diagnosed DM", "dhs_dm_dx")
add(any_dx, ~insured_any, "Insured among any diagnosed", "dhs_any_dx")

# Insurance-stratified treatment among diagnosed HTN
for (ins in c(0, 1)) {
  lab <- ifelse(ins == 1, "insured", "uninsured")
  sub <- subset(des, htn_dx == 1 & insured_any == ins)
  add(sub, ~htn_treated_if_dx, paste0("HTN tx among ", lab, " diagnosed"), "dhs_htn_ins")
}

key <- bind_rows(rows)
readr::write_csv(key, file.path(p$tables, "Table5_DHS_KeyEstimates_R.csv"))

# R vs Python
py_path <- file.path(p$tables, "Table5_DHS_KeyEstimates.csv")
if (file.exists(py_path)) {
  py <- readr::read_csv(py_path, show_col_types = FALSE)
  cmp <- key %>%
    select(label, estimate_R = estimate, pct_R = pct, n_R = n) %>%
    left_join(
      py %>% select(label, estimate_Py = estimate, pct_Py = pct, n_Py = n),
      by = "label"
    ) %>%
    mutate(diff_pp = pct_R - pct_Py)
  readr::write_csv(cmp, file.path(p$tables, "Table_Compare_DHS_R_vs_Python.csv"))
  message("\n=== R vs Python DHS ===")
  print(cmp %>% select(label, pct_R, pct_Py, diff_pp, n_R, n_Py))
}

message("DHS R analysis complete.")
invisible(key)
