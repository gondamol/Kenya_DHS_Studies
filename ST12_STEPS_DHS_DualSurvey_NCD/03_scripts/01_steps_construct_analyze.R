# ST12 — STEPS 2015 construction + survey-weighted cascade (R survey package)
# Definitions align with Python pipeline and WHO STEPS practice.

message("=== ST12 R: STEPS construct + analyse ===")
source("/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD/03_scripts/00_setup.R")

p <- paths_st12()
ensure_dirs(p)

# Prefer Stata file for labels; fall back to CSV
if (file.exists(p$steps_dta)) {
  message("Reading STEPS .dta …")
  raw <- haven::read_dta(p$steps_dta)
} else {
  message("Reading STEPS .csv …")
  raw <- readr::read_csv(p$steps_csv, show_col_types = FALSE)
}

# Lowercase names for consistency
names(raw) <- tolower(names(raw))
message("  n=", nrow(raw), " ncol=", ncol(raw))

d <- raw %>%
  mutate(
    sbp2 = num_clean(m5a),
    sbp3 = num_clean(m6a),
    dbp2 = num_clean(m5b),
    dbp3 = num_clean(m6b),
    sbp1 = num_clean(m4a),
    dbp1 = num_clean(m4b),
    sbp = (sbp2 + sbp3) / 2,
    dbp = (dbp2 + dbp3) / 2,
    height_cm = num_clean(m11),
    weight_kg = num_clean(m12),
    bmi = weight_kg / ((height_cm / 100)^2),
    glucose = num_clean(b5),
    cholesterol = num_clean(b8),
    bp_ever_measured = yes1(h1),
    told_high_bp = yes1(h2a),
    on_bp_meds = yes1(h3),
    gluc_ever_measured = yes1(h6),
    told_high_gluc = yes1(h7a),
    on_dm_meds = yes1(h8),
    wstep1 = as.numeric(wstep1),
    wstep2 = as.numeric(wstep2),
    wstep3 = as.numeric(wstep3),
    psu = as.numeric(psu),
    stratum = as.numeric(stratum),
    age = num_clean(age)
  )

# BP fallbacks
d$sbp[is.na(d$sbp) & !is.na(d$sbp2) & is.na(d$sbp3)] <- d$sbp2[is.na(d$sbp) & !is.na(d$sbp2) & is.na(d$sbp3)]
d$sbp[is.na(d$sbp) & is.na(d$sbp2) & !is.na(d$sbp3)] <- d$sbp3[is.na(d$sbp) & is.na(d$sbp2) & !is.na(d$sbp3)]
d$sbp[is.na(d$sbp)] <- d$sbp1[is.na(d$sbp)]
d$dbp[is.na(d$dbp) & !is.na(d$dbp2) & is.na(d$dbp3)] <- d$dbp2[is.na(d$dbp) & !is.na(d$dbp2) & is.na(d$dbp3)]
d$dbp[is.na(d$dbp) & is.na(d$dbp2) & !is.na(d$dbp3)] <- d$dbp3[is.na(d$dbp) & is.na(d$dbp2) & !is.na(d$dbp3)]
d$dbp[is.na(d$dbp)] <- d$dbp1[is.na(d$dbp)]
d$bmi[!is.finite(d$bmi) | d$bmi < 10 | d$bmi > 80] <- NA_real_

# Population on-meds: if never measured / never told -> 0
d$on_bp_meds_pop <- d$on_bp_meds
d$on_bp_meds_pop[d$bp_ever_measured == 0 | d$told_high_bp == 0] <-
  ifelse(is.na(d$on_bp_meds_pop[d$bp_ever_measured == 0 | d$told_high_bp == 0]),
         0, d$on_bp_meds_pop[d$bp_ever_measured == 0 | d$told_high_bp == 0])
d$on_bp_meds_pop[d$bp_ever_measured == 0 | d$told_high_bp == 0] <- 0

d$on_dm_meds_pop <- d$on_dm_meds
d$on_dm_meds_pop[d$gluc_ever_measured == 0 | d$told_high_gluc == 0] <- 0

d$htn_measured <- as.numeric(
  (!is.na(d$sbp) & d$sbp >= 140) |
    (!is.na(d$dbp) & d$dbp >= 90) |
    (!is.na(d$on_bp_meds_pop) & d$on_bp_meds_pop == 1)
)
d$htn_measured[is.na(d$sbp) & is.na(d$dbp) & (is.na(d$on_bp_meds_pop) | d$on_bp_meds_pop != 1)] <- NA_real_

d$dm_measured <- as.numeric(
  (!is.na(d$glucose) & d$glucose >= 7.0) |
    (!is.na(d$on_dm_meds_pop) & d$on_dm_meds_pop == 1)
)
d$dm_measured[is.na(d$glucose) & (is.na(d$on_dm_meds_pop) | d$on_dm_meds_pop != 1)] <- NA_real_

d$htn_aware <- ifelse(d$htn_measured == 1,
                      as.numeric(d$told_high_bp == 1 | d$on_bp_meds_pop == 1), NA_real_)
d$htn_treated <- ifelse(d$htn_measured == 1,
                        as.numeric(d$on_bp_meds_pop == 1), NA_real_)
d$htn_controlled <- ifelse(
  d$htn_measured == 1,
  as.numeric(d$on_bp_meds_pop == 1 & !is.na(d$sbp) & !is.na(d$dbp) & d$sbp < 140 & d$dbp < 90),
  NA_real_
)
d$htn_ctrl_among_tx <- ifelse(
  d$htn_measured == 1 & d$htn_treated == 1,
  as.numeric(d$sbp < 140 & d$dbp < 90),
  NA_real_
)

d$dm_aware <- ifelse(d$dm_measured == 1,
                     as.numeric(d$told_high_gluc == 1 | d$on_dm_meds_pop == 1), NA_real_)
d$dm_treated <- ifelse(d$dm_measured == 1,
                       as.numeric(d$on_dm_meds_pop == 1), NA_real_)
d$dm_controlled <- ifelse(
  d$dm_measured == 1,
  as.numeric(d$on_dm_meds_pop == 1 & !is.na(d$glucose) & d$glucose < 7.0),
  NA_real_
)

d$overweight_obese <- ifelse(!is.na(d$bmi), as.numeric(d$bmi >= 25), NA_real_)
d$obese <- ifelse(!is.na(d$bmi), as.numeric(d$bmi >= 30), NA_real_)
d$chol_raised <- ifelse(!is.na(d$cholesterol), as.numeric(d$cholesterol >= 5.0), NA_real_)

# Demographics
sex_chr <- as.character(haven::as_factor(d$sex, levels = "labels"))
if (all(is.na(sex_chr)) || all(sex_chr == as.character(d$sex))) {
  sex_chr <- as.character(d$sex)
}
d$sex_f <- dplyr::case_when(
  grepl("Women|Female|2", sex_chr, ignore.case = TRUE) ~ "Women",
  grepl("Men|Male|1", sex_chr, ignore.case = TRUE) ~ "Men",
  TRUE ~ sex_chr
)
ur <- as.numeric(as.character(d$urbrur))
d$residence <- ifelse(ur == 1, "Urban", ifelse(ur == 2, "Rural", NA_character_))

# Education rank from c5 labels
c5lab <- as.character(haven::as_factor(d$c5, levels = "labels"))
if (all(is.na(c5lab))) c5lab <- as.character(d$c5)
d$educ_label <- c5lab
d$educ_rank <- dplyr::case_when(
  grepl("no formal|less than primary", c5lab, ignore.case = TRUE) ~ 1,
  grepl("primary.*incomplet", c5lab, ignore.case = TRUE) ~ 2,
  grepl("primary.*complet", c5lab, ignore.case = TRUE) ~ 3,
  grepl("secondary.*incomplet|a-level incomplet", c5lab, ignore.case = TRUE) ~ 4,
  grepl("secondary.*complet|a-level complet", c5lab, ignore.case = TRUE) ~ 5,
  grepl("college|university", c5lab, ignore.case = TRUE) ~ 6,
  grepl("post", c5lab, ignore.case = TRUE) ~ 7,
  TRUE ~ NA_real_
)
d$educ <- dplyr::case_when(
  d$educ_rank <= 1 ~ "None/less than primary",
  d$educ_rank <= 3 ~ "Primary",
  d$educ_rank <= 5 ~ "Secondary",
  d$educ_rank >= 6 ~ "Tertiary",
  TRUE ~ NA_character_
)

d$htn_pop_controlled <- ifelse(is.na(d$htn_measured), NA_real_,
                               ifelse(d$htn_measured == 1, d$htn_controlled, 0))

saveRDS(d, file.path(p$derived, "st12_steps_analytic_R.rds"))
message("  Saved analytic RDS")

# --- Survey designs ---
s2 <- d %>% filter(!is.na(wstep2), wstep2 > 0)
s3 <- d %>% filter(!is.na(wstep3), wstep3 > 0)
s1 <- d %>% filter(!is.na(wstep1), wstep1 > 0)

des2 <- svydesign(ids = ~psu, strata = ~stratum, weights = ~wstep2, data = s2, nest = TRUE)
des3 <- svydesign(ids = ~psu, strata = ~stratum, weights = ~wstep3, data = s3, nest = TRUE)
des1 <- svydesign(ids = ~psu, strata = ~stratum, weights = ~wstep1, data = s1, nest = TRUE)

rows <- list()
add <- function(des, formula, label, domain) {
  r <- svy_prop_row(des, formula, label)
  r$domain <- domain
  rows[[length(rows) + 1]] <<- r
  message(sprintf("  %s: %s n=%s", label, fmt_pct_ci(r$estimate, r$ci_low, r$ci_high), r$n))
}

add(des2, ~htn_measured, "Measured hypertension prevalence", "step2_all")
add(des2, ~overweight_obese, "Overweight or obese (BMI>=25)", "step2_all")
add(des2, ~obese, "Obesity (BMI>=30)", "step2_all")
add(des2, ~bp_ever_measured, "Ever had blood pressure measured", "step2_all")
add(des2, ~htn_pop_controlled, "Population on controlled HTN treatment", "step2_all")

htn_des <- subset(des2, htn_measured == 1)
add(htn_des, ~htn_aware, "HTN: aware (told or on meds)", "step2_htn")
add(htn_des, ~htn_treated, "HTN: on medication", "step2_htn")
add(htn_des, ~htn_controlled, "HTN: controlled (among all HTN)", "step2_htn")
tx_des <- subset(des2, htn_measured == 1 & htn_treated == 1)
add(tx_des, ~htn_ctrl_among_tx, "HTN: controlled among treated", "step2_htn")

add(des3, ~dm_measured, "Measured diabetes prevalence", "step3_all")
add(des3, ~gluc_ever_measured, "Ever had blood glucose measured", "step3_all")
add(des3, ~chol_raised, "Raised total cholesterol (>=5.0 mmol/L)", "step3_all")
dm_des <- subset(des3, dm_measured == 1)
add(dm_des, ~dm_aware, "DM: aware", "step3_dm")
add(dm_des, ~dm_treated, "DM: on medication", "step3_dm")
add(dm_des, ~dm_controlled, "DM: controlled", "step3_dm")

key <- bind_rows(rows)
readr::write_csv(key, file.path(p$tables, "Table1_STEPS_KeyEstimates_R.csv"))

# Cascade table
cascade <- key %>%
  filter(label %in% c(
    "HTN: aware (told or on meds)", "HTN: on medication", "HTN: controlled (among all HTN)",
    "DM: aware", "DM: on medication", "DM: controlled"
  )) %>%
  mutate(
    condition = ifelse(grepl("^HTN|^DM: controlled \\(among", label) | grepl("^HTN", label),
                       ifelse(grepl("^DM", label), "Diabetes", "Hypertension"),
                       ifelse(grepl("^DM", label), "Diabetes", "Hypertension")),
    stage = dplyr::case_when(
      grepl("aware", label, ignore.case = TRUE) ~ "Aware",
      grepl("on medication|on medication", label) ~ "Treated",
      grepl("controlled", label, ignore.case = TRUE) ~ "Controlled",
      TRUE ~ label
    ),
    condition = ifelse(grepl("^DM", label), "Diabetes", "Hypertension"),
    fmt = fmt_pct_ci(estimate, ci_low, ci_high)
  ) %>%
  select(condition, stage, pct, pct_low = pct_low, pct_high = pct_high, n, fmt)
# fix condition properly
cascade$condition <- ifelse(grepl("^DM", key$label[match(
  paste(cascade$stage, cascade$n),
  paste(
    dplyr::case_when(
      grepl("aware", key$label, ignore.case = TRUE) ~ "Aware",
      grepl("medication", key$label) ~ "Treated",
      grepl("controlled \\(among all|DM: controlled", key$label) ~ "Controlled",
      TRUE ~ ""
    ),
    key$n
  )
)]), "Diabetes", "Hypertension")

cascade <- bind_rows(
  key %>% filter(label == "HTN: aware (told or on meds)") %>%
    transmute(condition = "Hypertension", stage = "Aware", pct, pct_low, pct_high, n,
              fmt = fmt_pct_ci(estimate, ci_low, ci_high)),
  key %>% filter(label == "HTN: on medication") %>%
    transmute(condition = "Hypertension", stage = "Treated", pct, pct_low, pct_high, n,
              fmt = fmt_pct_ci(estimate, ci_low, ci_high)),
  key %>% filter(label == "HTN: controlled (among all HTN)") %>%
    transmute(condition = "Hypertension", stage = "Controlled", pct, pct_low, pct_high, n,
              fmt = fmt_pct_ci(estimate, ci_low, ci_high)),
  key %>% filter(label == "DM: aware") %>%
    transmute(condition = "Diabetes", stage = "Aware", pct, pct_low, pct_high, n,
              fmt = fmt_pct_ci(estimate, ci_low, ci_high)),
  key %>% filter(label == "DM: on medication") %>%
    transmute(condition = "Diabetes", stage = "Treated", pct, pct_low, pct_high, n,
              fmt = fmt_pct_ci(estimate, ci_low, ci_high)),
  key %>% filter(label == "DM: controlled") %>%
    transmute(condition = "Diabetes", stage = "Controlled", pct, pct_low, pct_high, n,
              fmt = fmt_pct_ci(estimate, ci_low, ci_high))
)
readr::write_csv(cascade, file.path(p$tables, "Table2_STEPS_Cascade_R.csv"))

# Stratified by sex
strat_rows <- list()
for (sx in c("Men", "Women")) {
  des_s <- subset(des2, sex_f == sx)
  htn_s <- subset(des2, sex_f == sx & htn_measured == 1)
  for (pair in list(
    list(des_s, ~htn_measured, "Prevalence"),
    list(htn_s, ~htn_aware, "Aware"),
    list(htn_s, ~htn_treated, "Treated"),
    list(htn_s, ~htn_controlled, "Controlled")
  )) {
    r <- svy_prop_row(pair[[1]], pair[[2]], pair[[3]])
    strat_rows[[length(strat_rows) + 1]] <- r %>%
      mutate(condition = "Hypertension", stratifier = "Sex", category = sx, stage = pair[[3]],
             fmt = fmt_pct_ci(estimate, ci_low, ci_high))
  }
}
for (sx in c("Men", "Women")) {
  des_s <- subset(des3, sex_f == sx)
  dm_s <- subset(des3, sex_f == sx & dm_measured == 1)
  for (pair in list(
    list(des_s, ~dm_measured, "Prevalence"),
    list(dm_s, ~dm_aware, "Aware"),
    list(dm_s, ~dm_treated, "Treated"),
    list(dm_s, ~dm_controlled, "Controlled")
  )) {
    r <- svy_prop_row(pair[[1]], pair[[2]], pair[[3]])
    strat_rows[[length(strat_rows) + 1]] <- r %>%
      mutate(condition = "Diabetes", stratifier = "Sex", category = sx, stage = pair[[3]],
             fmt = fmt_pct_ci(estimate, ci_low, ci_high))
  }
}
strat <- bind_rows(strat_rows) %>%
  select(condition, stratifier, category, stage, pct, pct_low, pct_high, n, fmt)
readr::write_csv(strat, file.path(p$tables, "Table4_STEPS_Cascade_BySex_R.csv"))

# Erreygers CI via survey package approach: rank education, compute concentration
# Simplified: use svyglm / manual Erreygers on weighted data
erreygers_one <- function(data, y, rank_col, w) {
  dd <- data[!is.na(data[[y]]) & !is.na(data[[rank_col]]) & data[[w]] > 0, ]
  if (nrow(dd) < 30) return(c(erreygers = NA, mu = NA, n = nrow(dd)))
  dd <- dd[order(dd[[rank_col]]), ]
  ww <- dd[[w]]
  yy <- dd[[y]]
  W <- sum(ww)
  cum <- cumsum(ww)
  rank <- (cum - ww / 2) / W
  mu <- sum(ww * yy) / W
  cov_yr <- sum(ww * (yy - mu) * (rank - 0.5)) / W
  e <- 8 * cov_yr
  c(erreygers = e, mu = mu, n = nrow(dd))
}

eq <- bind_rows(
  {
    e <- erreygers_one(s2, "htn_measured", "educ_rank", "wstep2")
    tibble(outcome = "HTN prevalence", y = "htn_measured", erreygers = e["erreygers"],
           mu = e["mu"], n = e["n"])
  },
  {
    htn <- s2 %>% filter(htn_measured == 1)
    e <- erreygers_one(htn, "htn_aware", "educ_rank", "wstep2")
    tibble(outcome = "HTN awareness among HTN", y = "htn_aware", erreygers = e["erreygers"],
           mu = e["mu"], n = e["n"])
  },
  {
    htn <- s2 %>% filter(htn_measured == 1)
    e <- erreygers_one(htn, "htn_treated", "educ_rank", "wstep2")
    tibble(outcome = "HTN treatment among HTN", y = "htn_treated", erreygers = e["erreygers"],
           mu = e["mu"], n = e["n"])
  },
  {
    htn <- s2 %>% filter(htn_measured == 1)
    e <- erreygers_one(htn, "htn_controlled", "educ_rank", "wstep2")
    tibble(outcome = "HTN control among HTN", y = "htn_controlled", erreygers = e["erreygers"],
           mu = e["mu"], n = e["n"])
  }
)
readr::write_csv(eq, file.path(p$tables, "Table3_STEPS_Erreygers_R.csv"))

# Comparison with Python tables if present
py_path <- file.path(p$tables, "Table1_STEPS_KeyEstimates.csv")
if (file.exists(py_path)) {
  py <- readr::read_csv(py_path, show_col_types = FALSE)
  cmp <- key %>%
    select(label, estimate_R = estimate, pct_R = pct, n_R = n) %>%
    left_join(
      py %>% select(label, estimate_Py = estimate, pct_Py = pct, n_Py = n),
      by = "label"
    ) %>%
    mutate(
      diff_pp = pct_R - pct_Py,
      abs_diff_pp = abs(diff_pp)
    )
  readr::write_csv(cmp, file.path(p$tables, "Table_Compare_STEPS_R_vs_Python.csv"))
  message("\n=== R vs Python (percentage-point difference) ===")
  print(cmp %>% select(label, pct_R, pct_Py, diff_pp, n_R, n_Py))
}

message("STEPS R analysis complete.")
invisible(key)
