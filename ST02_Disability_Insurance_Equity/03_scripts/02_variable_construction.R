# Study: ST02_Disability_Insurance_Equity
# Script: 02_variable_construction.R
# Author: Nichodemus Werre Amollo
# Date: 2026-04-05
# Purpose: Construct the adult PR analytic dataset and core disability variables for ST02.

message("=== SECTION 2: Variable Construction ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

import_object <- readRDS(file.path(paths$derived_dir, "st02_import_raw.rds"))
pr <- import_object$pr

append_log("Constructing the adult PR analytic dataset for ST02.", also_message = TRUE)

analytic_adults <- pr %>%
  dplyr::transmute(
    cluster = as.numeric(hv001),
    household = as.numeric(hv002),
    line = as.numeric(hvidx),
    usual_resident = yn_flag(hv102),
    slept_last_night = yn_flag(hv103),
    sex = sex_label(hv104),
    age = as.numeric(hv105),
    weight = as.numeric(hv005) / 1e6,
    psu = as.numeric(hv021),
    strata = as.numeric(hv022),
    county = clean_label(hv024),
    residence = clean_label(hv025),
    education = education_label(hv106),
    wealth = clean_label(hv270),
    questionnaire_form = clean_label(shshort),
    insured_any = yn_flag(sh27),
    insured_nhif_raw = yn_flag(sh28a),
    insured_private_raw = yn_flag(sh28b),
    insured_community_raw = yn_flag(sh28c),
    inpatient_last12m = yn_flag(sh29),
    outpatient_last4w = yn_flag(sh31),
    paid_outpatient = yn_flag(sh32),
    uses_glasses_contacts = yn_flag(hdis1),
    uses_hearing_aid = yn_flag(hdis3),
    wg_seeing = difficulty_level(hdis2),
    wg_hearing = difficulty_level(hdis4),
    wg_communication = difficulty_level(hdis5),
    wg_memory = difficulty_level(hdis6),
    wg_walking = difficulty_level(hdis7),
    wg_selfcare = difficulty_level(hdis8),
    wg_summary = difficulty_level(hdis9)
  ) %>%
  dplyr::filter(
    usual_resident == 1,
    age >= 18,
    !is.na(weight),
    !is.na(psu),
    !is.na(strata)
  ) %>%
  dplyr::mutate(
    long_questionnaire = questionnaire_form == "Long Questionnaire",
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
    ),
    any_functional_difficulty = dplyr::case_when(
      dplyr::if_any(c(wg_seeing, wg_hearing, wg_communication, wg_memory, wg_walking, wg_selfcare), is.na) ~ NA_integer_,
      dplyr::if_any(
        c(wg_seeing, wg_hearing, wg_communication, wg_memory, wg_walking, wg_selfcare),
        ~ .x %in% c("Some difficulty", "A lot of difficulty", "Cannot do at all")
      ) ~ 1L,
      TRUE ~ 0L
    ),
    wg_disability = dplyr::case_when(
      dplyr::if_any(c(wg_seeing, wg_hearing, wg_communication, wg_memory, wg_walking, wg_selfcare), is.na) ~ NA_integer_,
      dplyr::if_any(
        c(wg_seeing, wg_hearing, wg_communication, wg_memory, wg_walking, wg_selfcare),
        ~ .x %in% c("A lot of difficulty", "Cannot do at all")
      ) ~ 1L,
      TRUE ~ 0L
    ),
    wg_disability_from_summary = dplyr::case_when(
      wg_summary %in% c("A lot of difficulty", "Cannot do at all") ~ 1L,
      wg_summary %in% c("No difficulty", "Some difficulty") ~ 0L,
      TRUE ~ NA_integer_
    ),
    wg_severity = dplyr::case_when(
      wg_summary == "No difficulty" ~ "No functional difficulty",
      wg_summary == "Some difficulty" ~ "Mild functional difficulty",
      wg_summary == "A lot of difficulty" ~ "Moderate functional difficulty",
      wg_summary == "Cannot do at all" ~ "Severe functional difficulty",
      TRUE ~ NA_character_
    ),
    domain_seeing_threshold = dplyr::if_else(wg_seeing %in% c("A lot of difficulty", "Cannot do at all"), 1L, 0L, missing = NA_integer_),
    domain_hearing_threshold = dplyr::if_else(wg_hearing %in% c("A lot of difficulty", "Cannot do at all"), 1L, 0L, missing = NA_integer_),
    domain_communication_threshold = dplyr::if_else(wg_communication %in% c("A lot of difficulty", "Cannot do at all"), 1L, 0L, missing = NA_integer_),
    domain_memory_threshold = dplyr::if_else(wg_memory %in% c("A lot of difficulty", "Cannot do at all"), 1L, 0L, missing = NA_integer_),
    domain_walking_threshold = dplyr::if_else(wg_walking %in% c("A lot of difficulty", "Cannot do at all"), 1L, 0L, missing = NA_integer_),
    domain_selfcare_threshold = dplyr::if_else(wg_selfcare %in% c("A lot of difficulty", "Cannot do at all"), 1L, 0L, missing = NA_integer_),
    domain_threshold_count = rowSums(
      dplyr::across(
        c(
          domain_seeing_threshold,
          domain_hearing_threshold,
          domain_communication_threshold,
          domain_memory_threshold,
          domain_walking_threshold,
          domain_selfcare_threshold
        )
      ),
      na.rm = TRUE
    ),
    domain_threshold_count = dplyr::if_else(
      dplyr::if_any(
        c(
          domain_seeing_threshold,
          domain_hearing_threshold,
          domain_communication_threshold,
          domain_memory_threshold,
          domain_walking_threshold,
          domain_selfcare_threshold
        ),
        is.na
      ),
      NA_real_,
      as.numeric(domain_threshold_count)
    ),
    uninsured = dplyr::case_when(
      insured_any == 1 ~ 0L,
      insured_any == 0 ~ 1L,
      TRUE ~ NA_integer_
    ),
    paid_outpatient_recent = dplyr::case_when(
      outpatient_last4w == 1 ~ paid_outpatient,
      outpatient_last4w == 0 ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    age_group = dplyr::case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 44 ~ "30-44",
      age >= 45 & age <= 59 ~ "45-59",
      age >= 60 ~ "60+",
      TRUE ~ NA_character_
    )
  )

consistency_check <- analytic_adults %>%
  dplyr::filter(!is.na(wg_disability), !is.na(wg_disability_from_summary)) %>%
  dplyr::summarise(
    adults_checked = dplyr::n(),
    mismatched_cases = sum(wg_disability != wg_disability_from_summary, na.rm = TRUE),
    mismatch_share = mismatched_cases / adults_checked
  )

sample_flow <- tibble::tibble(
  step = c(
    "Raw PR records",
    "Adult usual residents with design variables",
    "Adult usual residents in long questionnaire sample",
    "Adults with non-missing WG summary item",
    "Adults with complete six-domain difficulty data",
    "Adults retained in ST02 analytic dataset"
  ),
  n = c(
    nrow(pr),
    analytic_adults %>% dplyr::filter(!is.na(age)) %>% nrow(),
    analytic_adults %>% dplyr::filter(long_questionnaire) %>% nrow(),
    analytic_adults %>% dplyr::filter(!is.na(wg_summary)) %>% nrow(),
    analytic_adults %>%
      dplyr::filter(!dplyr::if_any(c(wg_seeing, wg_hearing, wg_communication, wg_memory, wg_walking, wg_selfcare), is.na)) %>%
      nrow(),
    nrow(analytic_adults)
  )
)

questionnaire_coverage <- analytic_adults %>%
  dplyr::group_by(questionnaire_form) %>%
  dplyr::summarise(
    adult_residents = dplyr::n(),
    nonmissing_hdis9 = sum(!is.na(wg_summary)),
    nonmissing_sh27 = sum(!is.na(insured_any)),
    nonmissing_sh29 = sum(!is.na(inpatient_last12m)),
    nonmissing_sh31 = sum(!is.na(outpatient_last4w)),
    .groups = "drop"
  )

variable_map <- tibble::tribble(
  ~constructed_variable, ~source_variable, ~definition,
  "wg_seeing", "hdis2", "WG seeing difficulty item",
  "wg_hearing", "hdis4", "WG hearing difficulty item",
  "wg_communication", "hdis5", "WG communication difficulty item",
  "wg_memory", "hdis6", "WG remembering/concentrating difficulty item",
  "wg_walking", "hdis7", "WG walking/climbing difficulty item",
  "wg_selfcare", "hdis8", "WG washing/dressing difficulty item",
  "wg_summary", "hdis9", "Highest difficulty across the six functional-difficulty items",
  "wg_disability", "hdis2/hdis4/hdis5/hdis6/hdis7/hdis8", "WG threshold met in at least one domain: a lot of difficulty or cannot do at all",
  "any_functional_difficulty", "hdis2/hdis4/hdis5/hdis6/hdis7/hdis8", "Any difficulty in at least one domain: some, a lot, or cannot do at all",
  "insured_any", "sh27", "Any health insurance coverage",
  "insured_nhif", "sh28a", "NHIF coverage among those with any insurance",
  "outpatient_last4w", "sh31", "Outpatient medical care in the previous 4 weeks",
  "inpatient_last12m", "sh29", "Overnight stay in a medical facility in the previous 12 months",
  "paid_outpatient_recent", "sh32", "Paid money for last outpatient care, among recent outpatient users"
)

save_rds_output(analytic_adults, "st02_analytic_pr_adults.rds")
readr::write_csv(sample_flow, file.path(paths$logs_dir, "st02_sample_flow.csv"))
readr::write_csv(questionnaire_coverage, file.path(paths$logs_dir, "st02_questionnaire_coverage.csv"))
readr::write_csv(consistency_check, file.path(paths$logs_dir, "st02_wg_summary_consistency.csv"))
readr::write_csv(variable_map, file.path(paths$data_notes_dir, "st02_variable_map.csv"))

append_log("Adult analytic dataset, sample flow, and variable map saved for ST02.")

message("=== SECTION 2 COMPLETE ===")
