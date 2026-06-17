# Study: ST11_Mental_Health_Treatment_Gap
# Script: 02_variable_construction.R
# Purpose: Harmonise women's (IR) and men's (MR) records into one adult analytic
#          file; construct the diagnosed-but-untreated mental-health outcome, the
#          NCD-comorbidity count, and analysis covariates.

message("=== SECTION 2: Variable Construction ===")
source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

import_object <- readRDS(file.path(paths$derived_dir, "st11_import_raw.rds"))
ir <- import_object$ir
mr <- import_object$mr
pr_insurance <- import_object$pr_insurance

append_log("Harmonising IR and MR into the ST11 adult analytic file.", also_message = TRUE)

# Common builder applied to each sex after column names are harmonised.
build_block <- function(df, sex_label) {
  df %>%
    dplyr::transmute(
      cluster = as.numeric(cluster),
      household = as.numeric(household),
      line = as.numeric(line),
      sex = sex_label,
      age = as.numeric(age),
      weight = as.numeric(weight) / 1e6,
      psu = as.numeric(psu),
      strata = as.numeric(strata),
      region = clean_label(region),
      residence = clean_label(residence),
      education = education_label(education),
      wealth = wealth_label(wealth),
      dx_hypertension = yn_flag(chd02),
      dx_diabetes = yn_flag(chd07),
      dx_heart = yn_flag(chd11),
      dx_lung = yn_flag(chd13),
      dx_arthritis = yn_flag(chd20),
      dx_depression = yn_flag(chd17),
      dx_anxiety = yn_flag(chd18),
      treat_dep_anx = yn_flag(chd19)
    )
}

ir_std <- ir %>%
  dplyr::rename(cluster = v001, household = v002, line = v003, weight = v005, age = v012,
                psu = v021, strata = v022, region = v024, residence = v025,
                education = v149, wealth = v190,
                chd02 = chd02, chd07 = chd07, chd11 = chd11, chd13 = chd13, chd20 = chd20,
                chd17 = chd17, chd18 = chd18, chd19 = chd19) %>%
  build_block("Women")

mr_std <- mr %>%
  dplyr::rename(cluster = mv001, household = mv002, line = mv003, weight = mv005, age = mv012,
                psu = mv021, strata = mv022, region = mv024, residence = mv025,
                education = mv149, wealth = mv190,
                chd02 = mchd02, chd07 = mchd07, chd11 = mchd11, chd13 = mchd13, chd20 = mchd20,
                chd17 = mchd17, chd18 = mchd18, chd19 = mchd19) %>%
  build_block("Men")

analytic <- dplyr::bind_rows(ir_std, mr_std)

if (!is.null(pr_insurance)) {
  analytic <- analytic %>%
    dplyr::left_join(pr_insurance, by = c("cluster", "household", "line"))
} else {
  analytic$insured_any <- NA_integer_
}

analytic <- analytic %>%
  dplyr::mutate(
    # NCD comorbidity count across the five conditions asked of both sexes
    ncd_count = rowSums(dplyr::across(c(dx_hypertension, dx_diabetes, dx_heart, dx_lung, dx_arthritis)), na.rm = TRUE),
    ncd_count = dplyr::if_else(
      dplyr::if_all(c(dx_hypertension, dx_diabetes, dx_heart, dx_lung, dx_arthritis), is.na),
      NA_real_, as.numeric(ncd_count)
    ),
    ncd_group = dplyr::case_when(
      is.na(ncd_count) ~ NA_character_,
      ncd_count == 0 ~ "0",
      ncd_count == 1 ~ "1",
      ncd_count >= 2 ~ "2+"
    ),
    any_ncd = dplyr::case_when(is.na(ncd_count) ~ NA_integer_, ncd_count >= 1 ~ 1L, TRUE ~ 0L),
    # Mental-health outcomes
    dx_dep_or_anx = dplyr::case_when(
      is.na(dx_depression) & is.na(dx_anxiety) ~ NA_integer_,
      dx_depression == 1 | dx_anxiety == 1 ~ 1L,
      TRUE ~ 0L
    ),
    # Treatment gap: among those diagnosed with depression OR anxiety, not in treatment
    untreated = dplyr::case_when(
      dx_dep_or_anx == 1 & treat_dep_anx == 0 ~ 1L,
      dx_dep_or_anx == 1 & treat_dep_anx == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    age_group = dplyr::case_when(
      age >= 15 & age <= 24 ~ "15-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 49 ~ "35-49",
      age >= 50 ~ "50+",
      TRUE ~ NA_character_
    ),
    wealth_rank = wealth_rank_num(wealth),
    insured_lab = dplyr::case_when(insured_any == 1 ~ "Insured", insured_any == 0 ~ "Uninsured", TRUE ~ NA_character_)
  ) %>%
  dplyr::filter(!is.na(weight), !is.na(psu), !is.na(strata))

sample_flow <- tibble::tibble(
  step = c("IR women records", "MR men records", "Pooled adults with design vars",
           "Adults with any depression/anxiety classification",
           "Diagnosed with depression or anxiety (treatment-gap denominator)",
           "Diagnosed with non-missing treatment status"),
  n = c(nrow(ir), nrow(mr), nrow(analytic),
        sum(!is.na(analytic$dx_dep_or_anx)),
        sum(analytic$dx_dep_or_anx == 1, na.rm = TRUE),
        sum(!is.na(analytic$untreated)))
)

variable_map <- tibble::tribble(
  ~constructed, ~source, ~definition,
  "dx_depression", "chd17/mchd17", "Ever told by a health worker you have depression",
  "dx_anxiety", "chd18/mchd18", "Ever told by a health worker you have anxiety",
  "dx_dep_or_anx", "chd17/18", "Diagnosed with depression OR anxiety",
  "treat_dep_anx", "chd19/mchd19", "Currently receiving treatment for depression/anxiety (among diagnosed)",
  "untreated", "chd17/18/19", "Diagnosed but NOT receiving treatment (the treatment gap)",
  "ncd_count", "chd02/07/11/13/20", "Count of diagnosed physical NCDs (HTN, diabetes, heart, lung, arthritis)",
  "insured_any", "sh27 (PR merge)", "Any health insurance coverage (household member)",
  "wealth_rank", "v190/mv190", "Wealth quintile rank 1 (poorest) to 5 (richest)"
)

save_rds_output(analytic, "st11_analytic_adults.rds")
readr::write_csv(sample_flow, file.path(paths$logs_dir, "st11_sample_flow.csv"))
readr::write_csv(variable_map, file.path(paths$data_notes_dir, "st11_variable_map.csv"))
append_log("ST11 analytic file, sample flow, and variable map saved.")
message("=== SECTION 2 COMPLETE ===")
