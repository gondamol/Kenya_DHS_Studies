# Study: ST03_NCD_Insurance_Service_Use
# Script: 03_analysis_main.R
# Author: Nichodemus Werre Amollo
# Date: 2026-03-30
# Purpose: Build manuscript-facing analysis objects from the prepared ST03 analytic datasets.

message("=== SECTION 3: Analysis Main ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

prepared_object <- readRDS(file.path(paths$derived_dir, "st03_prepared.rds"))

pr <- prepared_object$pr
women <- prepared_object$women
men <- prepared_object$men
analytic_women <- prepared_object$analytic_women
analytic_men <- prepared_object$analytic_men
analytic_pooled <- prepared_object$analytic_pooled

append_log("Building manuscript-facing analysis objects for ST03.", also_message = TRUE)

analytic_women_pub <- analytic_women %>%
  dplyr::mutate(
    education_table1 = dplyr::case_when(
      education %in% c("No education", "Primary") ~ "None or primary",
      education == "Secondary+" ~ "Secondary+",
      TRUE ~ NA_character_
    ),
    employment_pub = dplyr::case_when(
      employment == "Other/unknown" ~ "Not working",
      TRUE ~ employment
    ),
    province_pub = dplyr::case_when(
      province == "North Eastern" ~ "NE",
      TRUE ~ province
    )
  )

analytic_men_pub <- analytic_men %>%
  dplyr::mutate(
    education_table1 = dplyr::case_when(
      education %in% c("No education", "Primary") ~ "None or primary",
      education == "Secondary+" ~ "Secondary+",
      TRUE ~ NA_character_
    ),
    employment_pub = dplyr::case_when(
      employment == "Other/unknown" ~ "Not working",
      TRUE ~ employment
    ),
    province_pub = dplyr::case_when(
      province == "North Eastern" ~ "NE",
      TRUE ~ province
    )
  )

analytic_pooled_pub <- dplyr::bind_rows(analytic_women_pub, analytic_men_pub)

insured_women <- analytic_women_pub %>% dplyr::filter(insured_any == 1)
insured_men <- analytic_men_pub %>% dplyr::filter(insured_any == 1)
insured_pooled <- analytic_pooled_pub %>% dplyr::filter(insured_any == 1)

make_table1_panel <- function(data, weight_var = "weight") {
  dplyr::bind_rows(
    tibble::tibble(order = 1, characteristic = "Sample size, n", n = nrow(data), estimate = ""),
    {
      stat <- weighted_mean(data, "age", weight_var = weight_var)
      tibble::tibble(order = 2, characteristic = "Mean age, years (95% CI)", n = stat$unweighted_n, estimate = stat$formatted)
    },
    tibble::tibble(order = 3, characteristic = "Diagnosis profile", n = NA_integer_, estimate = ""),
    {
      stat <- weighted_level(data, "dx_group", "HTN only", weight_var = weight_var)
      tibble::tibble(order = 4, characteristic = "  HTN only", n = sum(data$dx_group == "HTN only", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "dx_group", "DM only", weight_var = weight_var)
      tibble::tibble(order = 5, characteristic = "  DM only", n = sum(data$dx_group == "DM only", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "dx_group", "Both HTN and DM", weight_var = weight_var)
      tibble::tibble(order = 6, characteristic = "  Both HTN and DM", n = sum(data$dx_group == "Both HTN and DM", na.rm = TRUE), estimate = stat$formatted)
    },
    tibble::tibble(order = 7, characteristic = "Wealth quintile", n = NA_integer_, estimate = ""),
    {
      stat <- weighted_level(data, "wealth", "Poorest", weight_var = weight_var)
      tibble::tibble(order = 8, characteristic = "  Poorest", n = sum(data$wealth == "Poorest", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "wealth", "Poorer", weight_var = weight_var)
      tibble::tibble(order = 9, characteristic = "  Poorer", n = sum(data$wealth == "Poorer", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "wealth", "Middle", weight_var = weight_var)
      tibble::tibble(order = 10, characteristic = "  Middle", n = sum(data$wealth == "Middle", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "wealth", "Richer", weight_var = weight_var)
      tibble::tibble(order = 11, characteristic = "  Richer", n = sum(data$wealth == "Richer", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "wealth", "Richest", weight_var = weight_var)
      tibble::tibble(order = 12, characteristic = "  Richest", n = sum(data$wealth == "Richest", na.rm = TRUE), estimate = stat$formatted)
    },
    tibble::tibble(order = 13, characteristic = "Educational attainment", n = NA_integer_, estimate = ""),
    {
      stat <- weighted_level(data, "education_table1", "None or primary", weight_var = weight_var)
      tibble::tibble(order = 14, characteristic = "  None or primary", n = sum(data$education_table1 == "None or primary", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "education_table1", "Secondary+", weight_var = weight_var)
      tibble::tibble(order = 15, characteristic = "  Secondary+", n = sum(data$education_table1 == "Secondary+", na.rm = TRUE), estimate = stat$formatted)
    },
    tibble::tibble(order = 16, characteristic = "Residence", n = NA_integer_, estimate = ""),
    {
      stat <- weighted_level(data, "residence", "Urban", weight_var = weight_var)
      tibble::tibble(order = 17, characteristic = "  Urban", n = sum(data$residence == "Urban", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "residence", "Rural", weight_var = weight_var)
      tibble::tibble(order = 18, characteristic = "  Rural", n = sum(data$residence == "Rural", na.rm = TRUE), estimate = stat$formatted)
    },
    tibble::tibble(order = 19, characteristic = "Employment", n = NA_integer_, estimate = ""),
    {
      stat <- weighted_level(data, "employment_pub", "Agriculture", weight_var = weight_var)
      tibble::tibble(order = 20, characteristic = "  Agriculture", n = sum(data$employment_pub == "Agriculture", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "employment_pub", "Professional/clerical", weight_var = weight_var)
      tibble::tibble(order = 21, characteristic = "  Professional/clerical", n = sum(data$employment_pub == "Professional/clerical", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "employment_pub", "Sales/services", weight_var = weight_var)
      tibble::tibble(order = 22, characteristic = "  Sales/services", n = sum(data$employment_pub == "Sales/services", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "employment_pub", "Manual", weight_var = weight_var)
      tibble::tibble(order = 23, characteristic = "  Manual", n = sum(data$employment_pub == "Manual", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_level(data, "employment_pub", "Not working", weight_var = weight_var)
      tibble::tibble(order = 24, characteristic = "  Not working", n = sum(data$employment_pub == "Not working", na.rm = TRUE), estimate = stat$formatted)
    },
    {
      stat <- weighted_binary(data, "insured_any", weight_var = weight_var)
      tibble::tibble(order = 25, characteristic = "Any insurance, % (95% CI)", n = stat$unweighted_n, estimate = stat$formatted)
    },
    {
      stat <- weighted_binary(data, "insured_nhif", weight_var = weight_var)
      tibble::tibble(order = 26, characteristic = "NHIF coverage, % (95% CI)", n = stat$unweighted_n, estimate = stat$formatted)
    },
    {
      stat <- weighted_binary(data, "treatment_gap", weight_var = weight_var)
      tibble::tibble(order = 27, characteristic = "Treatment gap, % (95% CI)", n = stat$unweighted_n, estimate = stat$formatted)
    }
  )
}

table1 <- make_table1_panel(analytic_women_pub) %>%
  dplyr::rename(`Women n` = n, Women = estimate) %>%
  dplyr::full_join(
    make_table1_panel(analytic_men_pub) %>% dplyr::rename(`Men n` = n, Men = estimate),
    by = c("order", "characteristic")
  ) %>%
  dplyr::full_join(
    make_table1_panel(analytic_pooled_pub, weight_var = "pooled_weight") %>% dplyr::rename(`Pooled n` = n, Pooled = estimate),
    by = c("order", "characteristic")
  ) %>%
  dplyr::arrange(order) %>%
  dplyr::select(-order)

make_cascade <- function(data, sex_name, weight_var = "weight") {
  diagnosis_levels <- c("HTN only", "DM only", "Both HTN and DM")

  diagnosis_rows <- purrr::map_dfr(diagnosis_levels, function(dx) {
    data_dx <- data %>% dplyr::filter(dx_group == dx)
    insurance <- weighted_binary(data_dx, "insured_any", weight_var = weight_var)
    nhif <- weighted_binary(data_dx, "insured_nhif", weight_var = weight_var)
    gap <- weighted_binary(data_dx, "treatment_gap", weight_var = weight_var)

    tibble::tibble(
      sex = sex_name,
      diagnosis_profile = dx,
      n = nrow(data_dx),
      any_insurance = insurance$formatted,
      nhif = nhif$formatted,
      treatment_gap = gap$formatted
    )
  })

  insurance <- weighted_binary(data, "insured_any", weight_var = weight_var)
  nhif <- weighted_binary(data, "insured_nhif", weight_var = weight_var)
  gap <- weighted_binary(data, "treatment_gap", weight_var = weight_var)

  dplyr::bind_rows(
    diagnosis_rows,
    tibble::tibble(
      sex = sex_name,
      diagnosis_profile = "Any diagnosed NCD",
      n = nrow(data),
      any_insurance = insurance$formatted,
      nhif = nhif$formatted,
      treatment_gap = gap$formatted
    )
  )
}

table2 <- dplyr::bind_rows(
  make_cascade(analytic_women_pub, "Women"),
  make_cascade(analytic_men_pub, "Men"),
  make_cascade(analytic_pooled_pub, "Pooled", weight_var = "pooled_weight")
)

prepare_model_df <- function(data, pooled = FALSE, outcome_var = "treatment_gap") {
  output <- data %>%
    dplyr::mutate(
      insured_any = factor(insured_any, levels = c(0, 1), labels = c("Uninsured", "Insured")),
      dx_group = factor(dx_group, levels = c("Both HTN and DM", "HTN only", "DM only")),
      wealth = factor(wealth, levels = c("Richest", "Richer", "Middle", "Poorer", "Poorest")),
      education = factor(education, levels = c("No education", "Primary", "Secondary+")),
      residence = factor(residence, levels = c("Rural", "Urban")),
      employment_pub = factor(employment_pub, levels = c("Agriculture", "Professional/clerical", "Sales/services", "Manual", "Not working")),
      province_pub = factor(province_pub, levels = c("Central", "Coast", "Eastern", "Nairobi", "NE", "Nyanza", "Rift Valley", "Western")),
      analysis_outcome = .data[[outcome_var]]
    ) %>%
    {
      if (pooled) {
        dplyr::mutate(., sex = factor(sex, levels = c("Men", "Women")))
      } else {
        .
      }
    }

  if (pooled) {
    output %>%
      dplyr::filter(stats::complete.cases(
        insured_any, sex, age, dx_group, wealth, education, residence,
        employment_pub, province_pub, analysis_outcome, pooled_weight
      ))
  } else {
    output %>%
      dplyr::filter(stats::complete.cases(
        insured_any, age, dx_group, wealth, education, residence,
        employment_pub, province_pub, analysis_outcome, weight
      ))
  }
}

model_df_w <- prepare_model_df(analytic_women_pub)
model_df_m <- prepare_model_df(analytic_men_pub)
model_df_p <- prepare_model_df(analytic_pooled_pub, pooled = TRUE)
model_df_w_none <- prepare_model_df(analytic_women_pub, outcome_var = "treatment_gap_none")
model_df_m_none <- prepare_model_df(analytic_men_pub, outcome_var = "treatment_gap_none")
model_df_p_none <- prepare_model_df(analytic_pooled_pub, pooled = TRUE, outcome_var = "treatment_gap_none")

design_w <- make_design(model_df_w)
design_m <- make_design(model_df_m)
design_p <- make_design(model_df_p, weight_var = "pooled_weight")
design_w_none <- make_design(model_df_w_none)
design_m_none <- make_design(model_df_m_none)
design_p_none <- make_design(model_df_p_none, weight_var = "pooled_weight")

form_main <- analysis_outcome ~ insured_any + age + dx_group + wealth + education + residence + employment_pub + province_pub
form_pooled <- analysis_outcome ~ insured_any + sex + age + dx_group + wealth + education + residence + employment_pub + province_pub

mod_w_pr <- survey::svyglm(form_main, design = design_w, family = stats::quasipoisson(link = "log"))
mod_m_pr <- survey::svyglm(form_main, design = design_m, family = stats::quasipoisson(link = "log"))
mod_p_pr <- survey::svyglm(form_pooled, design = design_p, family = stats::quasipoisson(link = "log"))
mod_w_pr_none <- survey::svyglm(form_main, design = design_w_none, family = stats::quasipoisson(link = "log"))
mod_m_pr_none <- survey::svyglm(form_main, design = design_m_none, family = stats::quasipoisson(link = "log"))
mod_p_pr_none <- survey::svyglm(form_pooled, design = design_p_none, family = stats::quasipoisson(link = "log"))

mod_p_insurance_sex_int <- survey::svyglm(
  analysis_outcome ~ insured_any * sex + age + dx_group + wealth + education + residence + employment_pub + province_pub,
  design = design_p,
  family = stats::quasipoisson(link = "log")
)

mod_p_sex_wealth_int <- survey::svyglm(
  analysis_outcome ~ insured_any + sex * wealth + age + dx_group + education + residence + employment_pub + province_pub,
  design = design_p,
  family = stats::quasipoisson(link = "log")
)

mod_w_or <- survey::svyglm(form_main, design = design_w, family = stats::quasibinomial())
mod_m_or <- survey::svyglm(form_main, design = design_m, family = stats::quasibinomial())
mod_p_or <- survey::svyglm(form_pooled, design = design_p, family = stats::quasibinomial())

insurance_sex_interaction <- survey::regTermTest(mod_p_insurance_sex_int, ~ insured_any:sex)
sex_wealth_interaction <- survey::regTermTest(mod_p_sex_wealth_int, ~ sex:wealth)

interaction_tests <- tibble::tibble(
  test = c("Insurance by sex", "Sex by wealth"),
  numerator_df = c(insurance_sex_interaction$df, sex_wealth_interaction$df),
  denominator_df = c(insurance_sex_interaction$ddf, sex_wealth_interaction$ddf),
  f_statistic = c(as.numeric(insurance_sex_interaction$Ftest[1, 1]), as.numeric(sex_wealth_interaction$Ftest[1, 1])),
  p_value = c(as.numeric(insurance_sex_interaction$p[1, 1]), as.numeric(sex_wealth_interaction$p[1, 1]))
) %>%
  dplyr::mutate(
    dplyr::across(c(denominator_df, f_statistic, p_value), ~ round(.x, 4))
  )

table3_models <- dplyr::bind_rows(
  extract_model_table(mod_w_pr, "Women: adjusted prevalence ratio", "APR"),
  extract_model_table(mod_m_pr, "Men: adjusted prevalence ratio", "APR"),
  extract_model_table(mod_p_pr, "Pooled: adjusted prevalence ratio", "APR"),
  extract_model_table(mod_w_or, "Women: odds ratio sensitivity", "OR"),
  extract_model_table(mod_m_or, "Men: odds ratio sensitivity", "OR"),
  extract_model_table(mod_p_or, "Pooled: odds ratio sensitivity", "OR")
) %>%
  dplyr::mutate(dplyr::across(c(estimate, std.error, statistic, p.value, effect, ci_low, ci_high), ~ round(.x, 4)))

model_table_spec <- tibble::tribble(
  ~kind, ~covariate, ~term,
  "header", "Insurance", NA_character_,
  "ref", "  Uninsured (ref=1.00)", NA_character_,
  "term", "  Insured", "insured_anyInsured",
  "term", "Age (per year)", "age",
  "header", "Diagnosis", NA_character_,
  "ref", "  Both HTN+DM (ref=1.00)", NA_character_,
  "term", "  HTN only", "dx_groupHTN only",
  "term", "  DM only", "dx_groupDM only",
  "header", "Wealth", NA_character_,
  "ref", "  Richest (ref=1.00)", NA_character_,
  "term", "  Richer", "wealthRicher",
  "term", "  Middle", "wealthMiddle",
  "term", "  Poorer", "wealthPoorer",
  "term", "  Poorest", "wealthPoorest",
  "header", "Education", NA_character_,
  "ref", "  None (ref=1.00)", NA_character_,
  "term", "  Primary", "educationPrimary",
  "term", "  Secondary+", "educationSecondary+",
  "header", "Residence", NA_character_,
  "ref", "  Rural (ref=1.00)", NA_character_,
  "term", "  Urban", "residenceUrban",
  "header", "Employment", NA_character_,
  "ref", "  Agriculture (ref=1.00)", NA_character_,
  "term", "  Professional/clerical", "employment_pubProfessional/clerical",
  "term", "  Sales/services", "employment_pubSales/services",
  "term", "  Manual", "employment_pubManual",
  "term", "  Not working", "employment_pubNot working",
  "header", "Province", NA_character_,
  "ref", "  Central (ref=1.00)", NA_character_,
  "term", "  Coast", "province_pubCoast",
  "term", "  Eastern", "province_pubEastern",
  "term", "  Nairobi", "province_pubNairobi",
  "term", "  NE", "province_pubNE",
  "term", "  Nyanza", "province_pubNyanza",
  "term", "  Rift Valley", "province_pubRift Valley",
  "term", "  Western", "province_pubWestern"
)

build_model_table <- function(model_table, women_label, men_label, pooled_label, prefix) {
  model_table_spec %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      `Women Value` = dplyr::case_when(
        kind == "header" ~ "",
        kind == "ref" ~ "1.00 (ref)",
        TRUE ~ extract_effect(model_table, women_label, term)
      ),
      `Men Value` = dplyr::case_when(
        kind == "header" ~ "",
        kind == "ref" ~ "1.00 (ref)",
        TRUE ~ extract_effect(model_table, men_label, term)
      ),
      `Pooled Value` = dplyr::case_when(
        kind == "header" ~ "",
        kind == "ref" ~ "1.00 (ref)",
        TRUE ~ extract_effect(model_table, pooled_label, term)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Covariate = covariate,
      !!paste0("Women ", prefix, " (95% CI)") := `Women Value`,
      !!paste0("Men ", prefix, " (95% CI)") := `Men Value`,
      !!paste0("Pooled ", prefix, " (95% CI)") := `Pooled Value`
    )
}

table3 <- build_model_table(
  table3_models,
  "Women: adjusted prevalence ratio",
  "Men: adjusted prevalence ratio",
  "Pooled: adjusted prevalence ratio",
  "APR"
)

table_s1 <- build_model_table(
  table3_models,
  "Women: odds ratio sensitivity",
  "Men: odds ratio sensitivity",
  "Pooled: odds ratio sensitivity",
  "OR"
)

table_s2_models <- dplyr::bind_rows(
  extract_model_table(mod_w_pr_none, "Women: stricter no-treatment sensitivity", "APR"),
  extract_model_table(mod_m_pr_none, "Men: stricter no-treatment sensitivity", "APR"),
  extract_model_table(mod_p_pr_none, "Pooled: stricter no-treatment sensitivity", "APR")
) %>%
  dplyr::mutate(dplyr::across(c(estimate, std.error, statistic, p.value, effect, ci_low, ci_high), ~ round(.x, 4)))

women_barriers_lookup <- purrr::map_dfr(c("insured_any", "insured_nhif"), function(exposure) {
  purrr::map_dfr(c("barrier_money", "barrier_distance", "barrier_permission"), function(barrier) {
    purrr::map_dfr(c(0, 1), function(value) {
      data_sub <- analytic_women_pub %>% dplyr::filter(.data[[exposure]] == value)
      stat <- weighted_binary(data_sub, barrier)

      tibble::tibble(
        exposure = exposure,
        group = ifelse(value == 1, "Yes", "No"),
        barrier = barrier,
        unweighted_n = stat$unweighted_n,
        estimate = stat$formatted
      )
    })
  })
})

table4 <- women_barriers_lookup %>%
  dplyr::mutate(
    `Insurance status` = dplyr::recode(exposure, insured_any = "Any insurance", insured_nhif = "NHIF coverage"),
    Insured = dplyr::recode(group, No = "No", Yes = "Yes"),
    `Barrier type` = dplyr::recode(
      barrier,
      barrier_money = "Money for treatment",
      barrier_distance = "Distance to facility",
      barrier_permission = "Permission to seek care"
    ),
    `Unweighted n` = unweighted_n,
    `Prevalence % (95% CI)` = estimate
  ) %>%
  dplyr::select(`Insurance status`, Insured, `Barrier type`, `Unweighted n`, `Prevalence % (95% CI)`)

extract_ci_metric <- function(ci_table, measure_name, column_name) {
  row <- ci_table %>%
    dplyr::filter(measure == measure_name)

  row[[column_name]][[1]]
}

build_ci_row <- function(data, outcome_name, subgroup_name, weight_var = "weight") {
  ci_table <- weighted_concentration_indices(data, outcome_name, weight_var = weight_var)

  tibble::tibble(
    outcome = outcome_name,
    subgroup = subgroup_name,
    standard_ci = extract_ci_metric(ci_table, "Standard CI", "estimate"),
    standard_ci_low = extract_ci_metric(ci_table, "Standard CI", "ci_low"),
    standard_ci_high = extract_ci_metric(ci_table, "Standard CI", "ci_high"),
    erreygers_ci = extract_ci_metric(ci_table, "Erreygers CI", "estimate"),
    erreygers_ci_low = extract_ci_metric(ci_table, "Erreygers CI", "ci_low"),
    erreygers_ci_high = extract_ci_metric(ci_table, "Erreygers CI", "ci_high")
  )
}

table5 <- dplyr::bind_rows(
  build_ci_row(analytic_pooled_pub, "insured_any", "Pooled", weight_var = "pooled_weight"),
  build_ci_row(analytic_pooled_pub, "insured_nhif", "Pooled", weight_var = "pooled_weight"),
  build_ci_row(analytic_pooled_pub, "treatment_gap", "Pooled", weight_var = "pooled_weight"),
  build_ci_row(analytic_women_pub, "insured_any", "Women"),
  build_ci_row(analytic_men_pub, "insured_any", "Men")
) %>%
  dplyr::mutate(
    dplyr::across(
      c(standard_ci, standard_ci_low, standard_ci_high, erreygers_ci, erreygers_ci_low, erreygers_ci_high),
      ~ round(.x, 4)
    )
  )

pooled_gap_by_wealth <- weighted_by_group(
  analytic_pooled_pub,
  "wealth",
  "treatment_gap",
  weight_var = "pooled_weight"
) %>%
  dplyr::mutate(
    wealth = factor(group, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))
  ) %>%
  dplyr::arrange(wealth)

both_dx_gap_patterns <- analytic_pooled_pub %>%
  dplyr::filter(dx_group == "Both HTN and DM") %>%
  dplyr::mutate(
    gap_pattern = dplyr::case_when(
      htn_on_med == 1 & dm_on_med == 1 ~ "No gap",
      (htn_on_med == 0 & dm_on_med == 1) | (htn_on_med == 1 & dm_on_med == 0) ~ "Gap in one condition",
      htn_on_med == 0 & dm_on_med == 0 ~ "Gap in both conditions",
      TRUE ~ "Missing"
    )
  ) %>%
  dplyr::count(gap_pattern, name = "unweighted_n")

make_fig1_metrics <- function(data, sex_name, weight_var = "weight") {
  purrr::map_dfr(c("HTN only", "DM only", "Both HTN and DM", "Any diagnosed NCD"), function(dx) {
    data_dx <- if (dx == "Any diagnosed NCD") data else data %>% dplyr::filter(dx_group == dx)

    dplyr::bind_rows(
      {
        stat <- weighted_binary(data_dx, "insured_any", weight_var = weight_var)
        tibble::tibble(sex = sex_name, diagnosis_profile = dx, metric = "Any insurance", estimate = stat$est, ci_low = stat$ci_low, ci_high = stat$ci_high)
      },
      {
        stat <- weighted_binary(data_dx, "insured_nhif", weight_var = weight_var)
        tibble::tibble(sex = sex_name, diagnosis_profile = dx, metric = "NHIF coverage", estimate = stat$est, ci_low = stat$ci_low, ci_high = stat$ci_high)
      },
      {
        stat <- weighted_binary(data_dx, "treatment_gap", weight_var = weight_var)
        tibble::tibble(sex = sex_name, diagnosis_profile = dx, metric = "Treatment gap", estimate = stat$est, ci_low = stat$ci_low, ci_high = stat$ci_high)
      }
    )
  })
}

figure1_data <- dplyr::bind_rows(
  make_fig1_metrics(analytic_women_pub, "Women"),
  make_fig1_metrics(analytic_men_pub, "Men")
) %>%
  dplyr::mutate(
    diagnosis_profile = factor(
      diagnosis_profile,
      levels = c("HTN only", "DM only", "Both HTN and DM", "Any diagnosed NCD"),
      labels = c("HTN only", "DM only", "Both", "Overall")
    ),
    metric = factor(metric, levels = c("Any insurance", "NHIF coverage", "Treatment gap"))
  )

figure2_data <- dplyr::bind_rows(
  weighted_by_group(analytic_women_pub, "wealth", "insured_any") %>% dplyr::mutate(sex = "Women", metric = "Any insurance"),
  weighted_by_group(analytic_men_pub, "wealth", "insured_any") %>% dplyr::mutate(sex = "Men", metric = "Any insurance"),
  weighted_by_group(analytic_women_pub, "wealth", "treatment_gap") %>% dplyr::mutate(sex = "Women", metric = "Treatment gap"),
  weighted_by_group(analytic_men_pub, "wealth", "treatment_gap") %>% dplyr::mutate(sex = "Men", metric = "Treatment gap")
) %>%
  dplyr::mutate(
    group = factor(group, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    metric = factor(metric, levels = c("Any insurance", "Treatment gap"))
  )

make_coverage_cascade <- function(data, sex_name, weight_var = "weight") {
  any_insurance <- weighted_binary(data, "insured_any", weight_var = weight_var)
  nhif <- weighted_binary(data, "insured_nhif", weight_var = weight_var)
  treated_all <- weighted_binary(data, "treated_all", weight_var = weight_var)

  dplyr::bind_rows(
    tibble::tibble(sex = sex_name, step = "Diagnosed NCD", estimate = 1, ci_low = 1, ci_high = 1),
    tibble::tibble(sex = sex_name, step = "Any insurance", estimate = any_insurance$est, ci_low = any_insurance$ci_low, ci_high = any_insurance$ci_high),
    tibble::tibble(sex = sex_name, step = "NHIF coverage", estimate = nhif$est, ci_low = nhif$ci_low, ci_high = nhif$ci_high),
    tibble::tibble(sex = sex_name, step = "On medication for all diagnosed conditions", estimate = treated_all$est, ci_low = treated_all$ci_low, ci_high = treated_all$ci_high)
  )
}

figure3_data <- dplyr::bind_rows(
  make_coverage_cascade(analytic_women_pub, "Women"),
  make_coverage_cascade(analytic_men_pub, "Men")
) %>%
  dplyr::mutate(
    step = factor(
      step,
      levels = c(
        "Diagnosed NCD",
        "Any insurance",
        "NHIF coverage",
        "On medication for all diagnosed conditions"
      )
    )
  )

women_gap <- weighted_binary(analytic_women_pub, "treatment_gap")
men_gap <- weighted_binary(analytic_men_pub, "treatment_gap")
pooled_gap <- weighted_binary(analytic_pooled_pub, "treatment_gap", weight_var = "pooled_weight")
women_gap_none <- weighted_binary(analytic_women_pub, "treatment_gap_none")
men_gap_none <- weighted_binary(analytic_men_pub, "treatment_gap_none")
pooled_gap_none <- weighted_binary(analytic_pooled_pub, "treatment_gap_none", weight_var = "pooled_weight")
pooled_insurance <- weighted_binary(analytic_pooled_pub, "insured_any", weight_var = "pooled_weight")
pooled_nhif <- weighted_binary(analytic_pooled_pub, "insured_nhif", weight_var = "pooled_weight")
pooled_nhif_insured <- weighted_binary(insured_pooled, "insured_nhif", weight_var = "pooled_weight")

apr_w <- table3_models %>% dplyr::filter(model == "Women: adjusted prevalence ratio", term == "insured_anyInsured")
apr_m <- table3_models %>% dplyr::filter(model == "Men: adjusted prevalence ratio", term == "insured_anyInsured")
apr_p <- table3_models %>% dplyr::filter(model == "Pooled: adjusted prevalence ratio", term == "insured_anyInsured")
apr_w_none <- table_s2_models %>% dplyr::filter(model == "Women: stricter no-treatment sensitivity", term == "insured_anyInsured")
apr_m_none <- table_s2_models %>% dplyr::filter(model == "Men: stricter no-treatment sensitivity", term == "insured_anyInsured")
apr_p_none <- table_s2_models %>% dplyr::filter(model == "Pooled: stricter no-treatment sensitivity", term == "insured_anyInsured")
gap_pattern_lookup <- setNames(both_dx_gap_patterns$unweighted_n, both_dx_gap_patterns$gap_pattern)

table_s2 <- tibble::tibble(
  `Outcome definition` = c(
    "Main analysis: not on medication for at least one diagnosed condition",
    "Sensitivity analysis: no medication for any diagnosed condition"
  ),
  `Women prevalence % (95% CI)` = c(women_gap$formatted, women_gap_none$formatted),
  `Men prevalence % (95% CI)` = c(men_gap$formatted, men_gap_none$formatted),
  `Pooled prevalence % (95% CI)` = c(pooled_gap$formatted, pooled_gap_none$formatted),
  `Women APR for insured vs uninsured (95% CI)` = c(extract_effect(table3_models, "Women: adjusted prevalence ratio", "insured_anyInsured"), extract_effect(table_s2_models, "Women: stricter no-treatment sensitivity", "insured_anyInsured")),
  `Men APR for insured vs uninsured (95% CI)` = c(extract_effect(table3_models, "Men: adjusted prevalence ratio", "insured_anyInsured"), extract_effect(table_s2_models, "Men: stricter no-treatment sensitivity", "insured_anyInsured")),
  `Pooled APR for insured vs uninsured (95% CI)` = c(extract_effect(table3_models, "Pooled: adjusted prevalence ratio", "insured_anyInsured"), extract_effect(table_s2_models, "Pooled: stricter no-treatment sensitivity", "insured_anyInsured"))
)

software_versions <- list(
  r = as.character(getRversion()),
  survey = as.character(utils::packageVersion("survey")),
  conindex = if (requireNamespace("conindex", quietly = TRUE)) as.character(utils::packageVersion("conindex")) else NA_character_,
  acid = if (requireNamespace("acid", quietly = TRUE)) as.character(utils::packageVersion("acid")) else NA_character_
)

key_results <- list(
  sample_size_women = nrow(analytic_women_pub),
  sample_size_men = nrow(analytic_men_pub),
  sample_size_pooled = nrow(analytic_pooled_pub),
  women_any_insurance = weighted_binary(analytic_women_pub, "insured_any"),
  men_any_insurance = weighted_binary(analytic_men_pub, "insured_any"),
  pooled_any_insurance = pooled_insurance,
  women_nhif = weighted_binary(analytic_women_pub, "insured_nhif"),
  men_nhif = weighted_binary(analytic_men_pub, "insured_nhif"),
  pooled_nhif = pooled_nhif,
  women_nhif_among_insured = weighted_binary(insured_women, "insured_nhif"),
  men_nhif_among_insured = weighted_binary(insured_men, "insured_nhif"),
  pooled_nhif_among_insured = pooled_nhif_insured,
  women_treatment_gap = women_gap,
  men_treatment_gap = men_gap,
  pooled_treatment_gap = pooled_gap,
  women_treatment_gap_none = women_gap_none,
  men_treatment_gap_none = men_gap_none,
  pooled_treatment_gap_none = pooled_gap_none,
  women_apr_insurance = apr_w,
  men_apr_insurance = apr_m,
  pooled_apr_insurance = apr_p,
  women_apr_insurance_none = apr_w_none,
  men_apr_insurance_none = apr_m_none,
  pooled_apr_insurance_none = apr_p_none,
  pooled_gap_by_wealth = pooled_gap_by_wealth,
  interaction_tests = interaction_tests,
  both_dx_gap_one_count = unname(gap_pattern_lookup["Gap in one condition"]),
  both_dx_gap_both_count = unname(gap_pattern_lookup["Gap in both conditions"]),
  both_dx_no_gap_count = unname(gap_pattern_lookup["No gap"]),
  software_versions = software_versions
)

summary_lines <- c(
  "ST03 results summary",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  sprintf("Women diagnosed analytic sample: %d", nrow(analytic_women_pub)),
  sprintf("Men diagnosed analytic sample: %d", nrow(analytic_men_pub)),
  sprintf("Pooled diagnosed analytic sample: %d", nrow(analytic_pooled_pub)),
  sprintf("Pooled any insurance: %s", pooled_insurance$formatted),
  sprintf("Pooled NHIF coverage: %s", pooled_nhif$formatted),
  sprintf("Pooled NHIF among insured: %s", pooled_nhif_insured$formatted),
  sprintf("Women treatment gap: %s", women_gap$formatted),
  sprintf("Men treatment gap: %s", men_gap$formatted),
  sprintf("Pooled treatment gap: %s", pooled_gap$formatted),
  sprintf("Women stricter no-treatment sensitivity: %s", women_gap_none$formatted),
  sprintf("Men stricter no-treatment sensitivity: %s", men_gap_none$formatted),
  sprintf("Pooled stricter no-treatment sensitivity: %s", pooled_gap_none$formatted),
  sprintf("Women APR for any insurance vs uninsured: %.3f (95%% CI %.3f, %.3f)", apr_w$effect, apr_w$ci_low, apr_w$ci_high),
  sprintf("Men APR for any insurance vs uninsured: %.3f (95%% CI %.3f, %.3f)", apr_m$effect, apr_m$ci_low, apr_m$ci_high),
  sprintf("Pooled APR for any insurance vs uninsured: %.3f (95%% CI %.3f, %.3f)", apr_p$effect, apr_p$ci_low, apr_p$ci_high),
  sprintf("Women APR for stricter no-treatment sensitivity: %.3f (95%% CI %.3f, %.3f)", apr_w_none$effect, apr_w_none$ci_low, apr_w_none$ci_high),
  sprintf("Men APR for stricter no-treatment sensitivity: %.3f (95%% CI %.3f, %.3f)", apr_m_none$effect, apr_m_none$ci_low, apr_m_none$ci_high),
  sprintf("Pooled APR for stricter no-treatment sensitivity: %.3f (95%% CI %.3f, %.3f)", apr_p_none$effect, apr_p_none$ci_low, apr_p_none$ci_high),
  sprintf(
    "Pooled standard concentration index for any insurance: %.4f (95%% CI %.4f, %.4f)",
    table5$standard_ci[table5$outcome == "insured_any" & table5$subgroup == "Pooled"],
    table5$standard_ci_low[table5$outcome == "insured_any" & table5$subgroup == "Pooled"],
    table5$standard_ci_high[table5$outcome == "insured_any" & table5$subgroup == "Pooled"]
  ),
  sprintf(
    "Pooled Erreygers-corrected concentration index for any insurance: %.4f (95%% CI %.4f, %.4f)",
    table5$erreygers_ci[table5$outcome == "insured_any" & table5$subgroup == "Pooled"],
    table5$erreygers_ci_low[table5$outcome == "insured_any" & table5$subgroup == "Pooled"],
    table5$erreygers_ci_high[table5$outcome == "insured_any" & table5$subgroup == "Pooled"]
  ),
  sprintf(
    "Pooled standard concentration index for treatment gap: %.4f (95%% CI %.4f, %.4f)",
    table5$standard_ci[table5$outcome == "treatment_gap" & table5$subgroup == "Pooled"],
    table5$standard_ci_low[table5$outcome == "treatment_gap" & table5$subgroup == "Pooled"],
    table5$standard_ci_high[table5$outcome == "treatment_gap" & table5$subgroup == "Pooled"]
  ),
  sprintf(
    "Pooled Erreygers-corrected concentration index for treatment gap: %.4f (95%% CI %.4f, %.4f)",
    table5$erreygers_ci[table5$outcome == "treatment_gap" & table5$subgroup == "Pooled"],
    table5$erreygers_ci_low[table5$outcome == "treatment_gap" & table5$subgroup == "Pooled"],
    table5$erreygers_ci_high[table5$outcome == "treatment_gap" & table5$subgroup == "Pooled"]
  ),
  sprintf(
    "Interaction p-values: insurance by sex = %.4f; sex by wealth = %.4f",
    interaction_tests$p_value[interaction_tests$test == "Insurance by sex"],
    interaction_tests$p_value[interaction_tests$test == "Sex by wealth"]
  ),
  "Interpretation: insurance coverage remains incomplete and the treatment gap remains large even among insured adults, indicating that pre-SHA protection failures reflect both incomplete enrolment and incomplete effective access."
)

analysis_object <- list(
  key_results = key_results,
  table1 = table1,
  table2 = table2,
  table3 = table3,
  table4 = table4,
  table4_lookup = women_barriers_lookup,
  table5 = table5,
  supplementary_table_s1 = table_s1,
  supplementary_table_s2 = table_s2,
  figure1_data = figure1_data,
  figure2_data = figure2_data,
  figure3_data = figure3_data,
  pooled_gap_by_wealth = pooled_gap_by_wealth,
  both_dx_gap_patterns = both_dx_gap_patterns,
  interaction_tests = interaction_tests,
  model_dump = table3_models,
  software_versions = software_versions,
  summary_lines = summary_lines
)

save_rds_output(analysis_object, "st03_analysis_outputs.rds")
write_lines_output(summary_lines, "Results_Summary.txt")

append_log("Analysis objects saved for manuscript-facing tables and figures.")

message("=== SECTION 3 COMPLETE ===")
