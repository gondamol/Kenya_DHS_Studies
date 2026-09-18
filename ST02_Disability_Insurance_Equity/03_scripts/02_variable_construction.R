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
    insurance_response = to_chr(sh27),
    outpatient_module_eligible = as.numeric(sh33),
    cost_outpatient_total = dhs_amount(sh304),
    cost_met_cash = dhs_amount(sh305a),
    cost_met_nhif = dhs_amount(sh305b),
    cost_met_private = dhs_amount(sh305c),
    cost_met_inkind = dhs_amount(sh305d),
    cost_met_other = dhs_amount(sh305e),
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
    # Sensitivity coding. The main analysis treats a don't-know response as
    # missing; the KDHS report convention groups it with "no". The two differ by
    # 293 adults, and the sensitivity analysis reports whether that matters.
    uninsured_incl_dk = dplyr::case_when(
      insured_any == 1 ~ 0L,
      insured_any == 0 ~ 1L,
      insurance_response == "don't know" ~ 1L,
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
    ),
    # Multi-domain alternative to the hdis9 maximum-severity summary. hdis9
    # records the worst difficulty in any single domain, so one severe domain and
    # substantial difficulty in four domains are collapsed differently; the count
    # of domains at or above the WG threshold separates them.
    domain_burden = dplyr::case_when(
      is.na(domain_threshold_count) ~ NA_character_,
      domain_threshold_count == 0 ~ "No domain at threshold",
      domain_threshold_count == 1 ~ "One domain at threshold",
      domain_threshold_count >= 2 ~ "Two or more domains at threshold"
    ),
    # Amount and payer of the last outpatient payment. These are defined only for
    # respondents who used outpatient care and reported paying, which is the
    # eligibility rule the questionnaire itself applies.
    cost_components_total = dplyr::if_else(
      dplyr::if_all(c(cost_met_cash, cost_met_nhif, cost_met_private, cost_met_inkind, cost_met_other), is.na),
      NA_real_,
      rowSums(dplyr::across(c(cost_met_cash, cost_met_nhif, cost_met_private, cost_met_inkind, cost_met_other)), na.rm = TRUE)
    ),
    nhif_met_any = dplyr::case_when(
      is.na(cost_met_nhif) ~ NA_integer_,
      cost_met_nhif > 0 ~ 1L,
      TRUE ~ 0L
    ),
    private_met_any = dplyr::case_when(
      is.na(cost_met_private) ~ NA_integer_,
      cost_met_private > 0 ~ 1L,
      TRUE ~ 0L
    ),
    insurer_met_any = dplyr::case_when(
      is.na(cost_met_nhif) & is.na(cost_met_private) ~ NA_integer_,
      dplyr::coalesce(cost_met_nhif, 0) > 0 | dplyr::coalesce(cost_met_private, 0) > 0 ~ 1L,
      TRUE ~ 0L
    ),
    cash_share_of_cost = dplyr::if_else(
      !is.na(cost_components_total) & cost_components_total > 0,
      dplyr::coalesce(cost_met_cash, 0) / cost_components_total,
      NA_real_
    )
  )

consistency_check <- analytic_adults %>%
  dplyr::filter(!is.na(wg_disability), !is.na(wg_disability_from_summary)) %>%
  dplyr::summarise(
    adults_checked = dplyr::n(),
    mismatched_cases = sum(wg_disability != wg_disability_from_summary, na.rm = TRUE),
    mismatch_share = mismatched_cases / adults_checked
  )

# Participant flow. The saved analytic object deliberately keeps short-form
# records so that questionnaire coverage can be tabulated, but the analytic base
# for every estimate in the manuscript is the long-form subsample with complete
# WG-SS classification. An earlier version of this table ended with the row count
# of the saved object, which reported 77,909 and described no analysis.
wg_items <- c("wg_seeing", "wg_hearing", "wg_communication", "wg_memory", "wg_walking", "wg_selfcare")

long_form_adults <- analytic_adults %>% dplyr::filter(long_questionnaire)
analysis_base_flow <- long_form_adults %>%
  dplyr::filter(!dplyr::if_any(dplyr::all_of(wg_items), is.na))

sample_flow <- tibble::tibble(
  step = c(
    "Raw PR records",
    "Adult usual residents aged 18+ with complete design variables",
    "  of whom in short-questionnaire households (modules not administered)",
    "Adult usual residents in long-questionnaire households",
    "Adults with a non-missing WG summary item (hdis9)",
    "ST02 analytic base: adults with complete six-domain WG-SS data",
    "  with non-missing insurance status (sh27)",
    "  with non-missing outpatient use (sh31)",
    "  with non-missing hospitalisation (sh29)",
    "  outpatient users in the previous four weeks",
    "  outpatient users with a non-missing payment response (sh32)",
    "  outpatient users who paid, with a usable cost amount (sh304)"
  ),
  n = c(
    nrow(pr),
    nrow(analytic_adults),
    analytic_adults %>% dplyr::filter(!long_questionnaire) %>% nrow(),
    nrow(long_form_adults),
    long_form_adults %>% dplyr::filter(!is.na(wg_summary)) %>% nrow(),
    nrow(analysis_base_flow),
    analysis_base_flow %>% dplyr::filter(!is.na(insured_any)) %>% nrow(),
    analysis_base_flow %>% dplyr::filter(!is.na(outpatient_last4w)) %>% nrow(),
    analysis_base_flow %>% dplyr::filter(!is.na(inpatient_last12m)) %>% nrow(),
    analysis_base_flow %>% dplyr::filter(outpatient_last4w == 1) %>% nrow(),
    analysis_base_flow %>% dplyr::filter(outpatient_last4w == 1, !is.na(paid_outpatient)) %>% nrow(),
    analysis_base_flow %>% dplyr::filter(outpatient_last4w == 1, paid_outpatient == 1, !is.na(cost_outpatient_total)) %>% nrow()
  )
)

# Missingness by variable within the analytic base, for the STROBE missing-data item.
missingness_summary <- analysis_base_flow %>%
  dplyr::transmute(
    `Insurance status (sh27)` = as.character(insured_any),
    `Outpatient use (sh31)` = as.character(outpatient_last4w),
    `Hospitalisation (sh29)` = as.character(inpatient_last12m),
    `Sex` = as.character(sex),
    `Age group` = as.character(age_group),
    `Wealth quintile` = as.character(wealth),
    `Place of residence` = as.character(residence),
    `Educational attainment` = as.character(education),
    `WG severity` = as.character(wg_severity)
  ) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    denominator = dplyr::n(),
    missing_n = sum(is.na(value)),
    missing_pct = 100 * missing_n / denominator,
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(missing_n))

payment_missingness <- analysis_base_flow %>%
  dplyr::filter(outpatient_last4w == 1) %>%
  dplyr::summarise(
    variable = "Payment at last outpatient visit (sh32), among outpatient users",
    denominator = dplyr::n(),
    missing_n = sum(is.na(paid_outpatient)),
    missing_pct = 100 * missing_n / denominator
  )

missingness_summary <- dplyr::bind_rows(missingness_summary, payment_missingness)

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
  "paid_outpatient_recent", "sh32", "Paid money for last outpatient care, among recent outpatient users",
  "cost_outpatient_total", "sh304", "Total cost of the last outpatient visit in Kenyan shillings, among those who paid",
  "cost_met_cash", "sh305a", "Amount of that cost met in cash",
  "cost_met_nhif", "sh305b", "Amount of that cost met by NHIF",
  "cost_met_private", "sh305c", "Amount of that cost met by private insurance",
  "cost_met_inkind", "sh305d", "Amount of that cost met in kind",
  "cost_met_other", "sh305e", "Amount of that cost met by other means",
  "nhif_met_any", "sh305b", "NHIF met any part of the cost of the last outpatient visit",
  "insurer_met_any", "sh305b/sh305c", "NHIF or private insurance met any part of that cost",
  "cash_share_of_cost", "sh305a-sh305e", "Share of the reported amounts met in cash",
  "domain_burden", "hdis2/hdis4/hdis5/hdis6/hdis7/hdis8", "Count of WG domains at or above threshold: none, one, two or more",
  "uninsured_incl_dk", "sh27", "Uninsured with don't-know responses grouped as uninsured (sensitivity coding)"
)

save_rds_output(analytic_adults, "st02_analytic_pr_adults.rds")
readr::write_csv(sample_flow, file.path(paths$logs_dir, "st02_sample_flow.csv"))
readr::write_csv(questionnaire_coverage, file.path(paths$logs_dir, "st02_questionnaire_coverage.csv"))
readr::write_csv(consistency_check, file.path(paths$logs_dir, "st02_wg_summary_consistency.csv"))
readr::write_csv(missingness_summary, file.path(paths$logs_dir, "st02_missingness.csv"))
readr::write_csv(variable_map, file.path(paths$data_notes_dir, "st02_variable_map.csv"))

append_log("Adult analytic dataset, sample flow, and variable map saved for ST02.")

message("=== SECTION 2 COMPLETE ===")
