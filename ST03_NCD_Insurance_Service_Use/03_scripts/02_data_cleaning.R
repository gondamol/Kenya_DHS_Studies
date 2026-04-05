# Study: ST03_NCD_Insurance_Service_Use
# Script: 02_data_cleaning.R
# Author: Nichodemus Werre Amollo
# Date: 2026-03-29
# Purpose: Merge KDHS 2022 files, construct the diagnosed adult analytic samples, and save derived datasets.

message("=== SECTION 2: Data Cleaning ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

import_object <- readRDS(file.path(paths$derived_dir, "st03_import_raw.rds"))
pr <- import_object$pr
ir <- import_object$ir
mr <- import_object$mr

append_log("Creating harmonised ST03 analytic datasets.", also_message = TRUE)

pr_clean <- pr %>%
  dplyr::transmute(
    cluster = as.numeric(hv001),
    household = as.numeric(hv002),
    line = as.numeric(hvidx),
    usual_resident = yn_flag(hv102),
    slept_last_night = yn_flag(hv103),
    sex = sex_label(hv104),
    age = as.numeric(hv105),
    male_eligible = eligible_flag(hv118),
    hh_head_sex = clean_label(hv219),
    hh_weight = as.numeric(hv005) / 1e6,
    hh_male_weight = as.numeric(hv028) / 1e6,
    psu = as.numeric(hv021),
    strata = as.numeric(hv022),
    county = clean_label(hv024),
    residence = clean_label(hv025),
    wealth = clean_label(hv270),
    insured_any = yn_flag(sh27),
    insured_nhif_raw = yn_flag(sh28a),
    insured_private_raw = yn_flag(sh28b),
    insured_community_raw = yn_flag(sh28c),
    outpatient_last4w = yn_flag(sh31),
    paid_outpatient = yn_flag(sh32),
    disability = clean_label(hdis9)
  ) %>%
  dplyr::mutate(
    insured_nhif = dplyr::case_when(
      insured_any == 0 ~ 0L,
      insured_any == 1 & insured_nhif_raw == 1 ~ 1L,
      insured_any == 1 & insured_nhif_raw == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    insured_private = dplyr::case_when(
      insured_any == 0 ~ 0L,
      insured_any == 1 & insured_private_raw == 1 ~ 1L,
      insured_any == 1 & insured_private_raw == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    insured_community = dplyr::case_when(
      insured_any == 0 ~ 0L,
      insured_any == 1 & insured_community_raw == 1 ~ 1L,
      insured_any == 1 & insured_community_raw == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

ir_clean <- ir %>%
  dplyr::transmute(
    cluster = as.numeric(v001),
    household = as.numeric(v002),
    line = as.numeric(v003),
    sex = "Women",
    age = as.numeric(v012),
    weight = as.numeric(v005) / 1e6,
    pooled_weight = as.numeric(v005) / 1e6,
    psu = as.numeric(v021),
    strata = as.numeric(v022),
    county = clean_label(v024),
    province = collapse_province(v024),
    education = collapse_education(v106),
    employment = collapse_employment(v717),
    long_questionnaire = to_chr(sshort) == "long questionnaire",
    barrier_permission = problem_flag(v467b),
    barrier_money = problem_flag(v467c),
    barrier_distance = problem_flag(v467d),
    htn_dx = yn_flag(chd02),
    htn_on_med = yn_flag(chd05),
    dm_dx = yn_flag(chd07),
    dm_on_med = yn_flag(chd10)
  )

mr_clean <- mr %>%
  dplyr::transmute(
    cluster = as.numeric(mv001),
    household = as.numeric(mv002),
    line = as.numeric(mv003),
    sex = "Men",
    age = as.numeric(mv012),
    weight = as.numeric(mv005) / 1e6,
    pooled_weight = as.numeric(mv005) / 1e6,
    psu = as.numeric(mv021),
    strata = as.numeric(mv022),
    county = clean_label(mv024),
    province = collapse_province(mv024),
    education = collapse_education(mv106),
    employment = collapse_employment(mv717),
    htn_dx = yn_flag(mchd02),
    htn_on_med = yn_flag(mchd05),
    dm_dx = yn_flag(mchd07),
    dm_on_med = yn_flag(mchd10)
  )

men_factor <- pr_clean %>%
  dplyr::filter(
    sex == "Men",
    age >= 15,
    age <= 54,
    (usual_resident == 1 | slept_last_night == 1),
    !is.na(hh_weight),
    !is.na(strata)
  ) %>%
  dplyr::group_by(strata) %>%
  dplyr::summarise(
    total_weight = sum(hh_weight, na.rm = TRUE),
    subsample_weight = sum(hh_weight[male_eligible == 1], na.rm = TRUE),
    male_pool_factor = dplyr::if_else(subsample_weight > 0, total_weight / subsample_weight, NA_real_),
    .groups = "drop"
  )

pr_keep <- pr_clean %>%
  dplyr::select(
    cluster, household, line, residence, wealth, county, insured_any, insured_nhif,
    insured_private, insured_community, outpatient_last4w, paid_outpatient,
    disability, hh_head_sex
  )

women <- ir_clean %>%
  dplyr::left_join(pr_keep, by = c("cluster", "household", "line")) %>%
  dplyr::mutate(
    diagnosed_any = dplyr::if_else(htn_dx == 1 | dm_dx == 1, 1L, 0L, missing = 0L),
    dx_group = dplyr::case_when(
      htn_dx == 1 & dm_dx == 1 ~ "Both HTN and DM",
      htn_dx == 1 & dm_dx != 1 ~ "HTN only",
      dm_dx == 1 & htn_dx != 1 ~ "DM only",
      TRUE ~ "Neither"
    ),
    treated_any = dplyr::case_when(
      diagnosed_any != 1 ~ NA_real_,
      (htn_dx == 1 & htn_on_med == 1) | (dm_dx == 1 & dm_on_med == 1) ~ 1,
      (htn_dx != 1 | htn_on_med == 0) & (dm_dx != 1 | dm_on_med == 0) ~ 0,
      TRUE ~ NA_real_
    ),
    treated_all = dplyr::case_when(
      diagnosed_any != 1 ~ NA_real_,
      (htn_dx != 1 | htn_on_med == 1) & (dm_dx != 1 | dm_on_med == 1) ~ 1,
      (htn_dx == 1 & htn_on_med == 0) | (dm_dx == 1 & dm_on_med == 0) ~ 0,
      TRUE ~ NA_real_
    ),
    treatment_gap = dplyr::case_when(
      diagnosed_any != 1 ~ NA_integer_,
      treated_all == 1 ~ 0L,
      treated_all == 0 ~ 1L,
      TRUE ~ NA_integer_
    ),
    treatment_gap_none = dplyr::case_when(
      diagnosed_any != 1 ~ NA_integer_,
      treated_any == 1 ~ 0L,
      treated_any == 0 ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  dplyr::filter(age >= 18, age <= 49, long_questionnaire)

men <- mr_clean %>%
  dplyr::left_join(pr_keep, by = c("cluster", "household", "line")) %>%
  dplyr::left_join(men_factor, by = "strata") %>%
  dplyr::mutate(
    pooled_weight = pooled_weight * male_pool_factor,
    diagnosed_any = dplyr::if_else(htn_dx == 1 | dm_dx == 1, 1L, 0L, missing = 0L),
    dx_group = dplyr::case_when(
      htn_dx == 1 & dm_dx == 1 ~ "Both HTN and DM",
      htn_dx == 1 & dm_dx != 1 ~ "HTN only",
      dm_dx == 1 & htn_dx != 1 ~ "DM only",
      TRUE ~ "Neither"
    ),
    treated_any = dplyr::case_when(
      diagnosed_any != 1 ~ NA_real_,
      (htn_dx == 1 & htn_on_med == 1) | (dm_dx == 1 & dm_on_med == 1) ~ 1,
      (htn_dx != 1 | htn_on_med == 0) & (dm_dx != 1 | dm_on_med == 0) ~ 0,
      TRUE ~ NA_real_
    ),
    treated_all = dplyr::case_when(
      diagnosed_any != 1 ~ NA_real_,
      (htn_dx != 1 | htn_on_med == 1) & (dm_dx != 1 | dm_on_med == 1) ~ 1,
      (htn_dx == 1 & htn_on_med == 0) | (dm_dx == 1 & dm_on_med == 0) ~ 0,
      TRUE ~ NA_real_
    ),
    treatment_gap = dplyr::case_when(
      diagnosed_any != 1 ~ NA_integer_,
      treated_all == 1 ~ 0L,
      treated_all == 0 ~ 1L,
      TRUE ~ NA_integer_
    ),
    treatment_gap_none = dplyr::case_when(
      diagnosed_any != 1 ~ NA_integer_,
      treated_any == 1 ~ 0L,
      treated_any == 0 ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  dplyr::filter(age >= 18, age <= 54)

analytic_women <- women %>%
  dplyr::filter(diagnosed_any == 1, !is.na(insured_any), !is.na(treatment_gap), !is.na(treatment_gap_none)) %>%
  dplyr::mutate(
    wealth_rank = dplyr::case_when(
      wealth == "Poorest" ~ 1,
      wealth == "Poorer" ~ 2,
      wealth == "Middle" ~ 3,
      wealth == "Richer" ~ 4,
      wealth == "Richest" ~ 5,
      TRUE ~ NA_real_
    )
  )

analytic_men <- men %>%
  dplyr::filter(diagnosed_any == 1, !is.na(insured_any), !is.na(treatment_gap), !is.na(treatment_gap_none)) %>%
  dplyr::mutate(
    wealth_rank = dplyr::case_when(
      wealth == "Poorest" ~ 1,
      wealth == "Poorer" ~ 2,
      wealth == "Middle" ~ 3,
      wealth == "Richer" ~ 4,
      wealth == "Richest" ~ 5,
      TRUE ~ NA_real_
    )
  )

analytic_pooled <- dplyr::bind_rows(analytic_women, analytic_men)

prepared_object <- list(
  pr = pr_clean,
  women = women,
  men = men,
  analytic_women = analytic_women,
  analytic_men = analytic_men,
  analytic_pooled = analytic_pooled,
  men_factor = men_factor
)

save_rds_output(prepared_object, "st03_prepared.rds")

sample_flow_lines <- c(
  "ST03 sample flow",
  sprintf("Women IR total: %d", nrow(ir)),
  sprintf("Women long questionnaire: %d", sum(ir_clean$long_questionnaire, na.rm = TRUE)),
  sprintf("Women age 18-49 in long questionnaire: %d", nrow(women)),
  sprintf("Women diagnosed HTN and/or DM with non-missing insurance and treatment variables: %d", nrow(analytic_women)),
  sprintf("Men MR total: %d", nrow(mr)),
  sprintf("Men age 18-54: %d", nrow(men)),
  sprintf("Men diagnosed HTN and/or DM with non-missing insurance and treatment variables: %d", nrow(analytic_men)),
  sprintf("Pooled diagnosed analytic sample: %d", nrow(analytic_pooled)),
  "",
  sprintf(
    "Men pooled-weight strata factors: min=%.3f max=%.3f median=%.3f",
    min(men_factor$male_pool_factor, na.rm = TRUE),
    max(men_factor$male_pool_factor, na.rm = TRUE),
    median(men_factor$male_pool_factor, na.rm = TRUE)
  )
)

readr::write_lines(sample_flow_lines, file.path(paths$logs_dir, "st03_sample_flow.txt"))
append_log("Prepared analytic datasets saved and sample flow refreshed.")

message("=== SECTION 2 COMPLETE ===")
