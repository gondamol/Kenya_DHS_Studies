# Study: ST02_Disability_Insurance_Equity
# Script: 03b_analysis_whole_sample.R
# Author: Nichodemus Werre Amollo
# Date: 2026-09-18
# Purpose: Whole-sample disability contrasts, formal severity tests, the
#          outpatient cost and payer-source analysis, and the sensitivity
#          analyses. Section 3 estimates associations *within* the disability
#          subpopulation; this section estimates the disability contrast itself,
#          which is the comparison the study is about.

message("=== SECTION 3B: Whole-sample contrasts, costs, and sensitivity analyses ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analytic_adults <- readRDS(file.path(paths$derived_dir, "st02_analytic_pr_adults.rds"))

append_log("Running whole-sample, cost, and sensitivity analyses for ST02.", also_message = TRUE)

# ---------------------------------------------------------------------------
# Analytic base and design
# ---------------------------------------------------------------------------

model_levels <- function(data) {
  data %>%
    dplyr::mutate(
      sex = factor(sex, levels = c("Men", "Women")),
      age_group = factor(age_group, levels = c("18-29", "30-44", "45-59", "60+")),
      wealth = factor(wealth, levels = c("Richest", "Richer", "Middle", "Poorer", "Poorest")),
      residence = factor(residence, levels = c("Urban", "Rural")),
      education = factor(education, levels = c("No education", "Primary", "Secondary", "Higher")),
      wg_severity = factor(
        wg_severity,
        levels = c(
          "No functional difficulty",
          "Mild functional difficulty",
          "Moderate functional difficulty",
          "Severe functional difficulty"
        )
      ),
      domain_burden = factor(
        domain_burden,
        levels = c("No domain at threshold", "One domain at threshold", "Two or more domains at threshold")
      )
    )
}

analysis_base <- analytic_adults %>%
  dplyr::filter(long_questionnaire, !is.na(wg_disability), !is.na(wg_severity)) %>%
  model_levels()

design_base <- make_design(analysis_base)

design_metadata <- tibble::tibble(
  analytic_n = nrow(analysis_base),
  psu_n = dplyr::n_distinct(analysis_base$psu),
  strata_n = dplyr::n_distinct(analysis_base$strata),
  households_n = analysis_base %>% dplyr::distinct(cluster, household) %>% nrow(),
  design_df = survey::degf(design_base)
)

# ---------------------------------------------------------------------------
# Whole-sample disability contrasts
#
# Three specifications are reported for each outcome, and the difference between
# them is the substantive result rather than a robustness footnote.
#
#   crude      - no adjustment; the contrast a descriptive table reports.
#   confounder - sex, age group and place of residence. These precede functional
#                difficulty or are fixed characteristics, so they confound the
#                disability contrast without lying on a pathway from it.
#   full       - additionally wealth quintile and educational attainment. For a
#                population whose functional difficulty largely predates its
#                current economic position, these are plausibly mediators:
#                disability restricts schooling and earnings, which in turn
#                determine the ability to pay a contributory premium. Adjusting
#                for them estimates a direct effect net of that pathway, not a
#                better-confounded total effect.
# ---------------------------------------------------------------------------

confounder_terms <- "sex + age_group + residence"
mediator_terms <- "wealth + education"

fit_specifications <- function(outcome, exposure = "wg_disability", data = analysis_base) {
  needed <- c(outcome, exposure, "sex", "age_group", "residence", "wealth", "education")
  complete_data <- data %>% dplyr::filter(!dplyr::if_any(dplyr::all_of(needed), is.na))
  design_complete <- make_design(complete_data)

  specs <- list(
    crude = paste(outcome, "~", exposure),
    confounder = paste(outcome, "~", exposure, "+", confounder_terms),
    full = paste(outcome, "~", exposure, "+", confounder_terms, "+", mediator_terms)
  )

  purrr::imap_dfr(specs, function(formula_txt, spec_name) {
    model <- survey::svyglm(
      stats::as.formula(formula_txt),
      design = design_complete,
      family = quasipoisson(link = "log")
    )
    estimates <- tidy_apr_design(model, design_complete) %>%
      dplyr::filter(term == exposure)
    standardised <- standardised_contrast(model, design_complete, exposure)

    tibble::tibble(
      outcome = outcome,
      specification = spec_name,
      model_n = nrow(complete_data),
      apr = estimates$apr,
      apr_ci_low = estimates$ci_low,
      apr_ci_high = estimates$ci_high,
      p_value = estimates$p.value,
      standardised_exposed = standardised$prevalence_exposed,
      standardised_unexposed = standardised$prevalence_unexposed,
      standardised_difference = standardised$difference,
      standardised_difference_ci_low = standardised$difference_ci_low,
      standardised_difference_ci_high = standardised$difference_ci_high,
      design_df = estimates$df
    )
  })
}

whole_sample_contrasts <- dplyr::bind_rows(
  fit_specifications("uninsured"),
  fit_specifications("outpatient_last4w"),
  fit_specifications("inpatient_last12m")
)

# Full coefficient table for the fully adjusted insurance model, so that the
# disability term can be read alongside everything it is adjusted for.
uninsured_whole_data <- analysis_base %>%
  dplyr::filter(!dplyr::if_any(
    c(uninsured, wg_disability, sex, age_group, residence, wealth, education), is.na
  ))
design_uninsured_whole <- make_design(uninsured_whole_data)

model_uninsured_whole <- survey::svyglm(
  uninsured ~ wg_disability + sex + age_group + residence + wealth + education,
  design = design_uninsured_whole,
  family = quasipoisson(link = "log")
)
model_uninsured_whole_tidy <- tidy_apr_design(model_uninsured_whole, design_uninsured_whole)

# ---------------------------------------------------------------------------
# Severity: prespecified adjacent contrasts and a design-based global test
#
# The earlier draft read a severity gradient off four sets of overlapping
# confidence intervals. Overlapping intervals are not a test, so the adjacent
# steps are tested directly here and the whole variable is tested jointly.
# ---------------------------------------------------------------------------

severity_contrast_set <- list(
  "Mild vs no difficulty" = c(
    "wg_severityMild functional difficulty" = 1
  ),
  "Moderate vs mild" = c(
    "wg_severityModerate functional difficulty" = 1,
    "wg_severityMild functional difficulty" = -1
  ),
  "Severe vs moderate" = c(
    "wg_severitySevere functional difficulty" = 1,
    "wg_severityModerate functional difficulty" = -1
  ),
  "Severe vs no difficulty" = c(
    "wg_severitySevere functional difficulty" = 1
  )
)

severity_contrasts <- function(model, design, specification) {
  df_design <- survey::degf(design)
  purrr::imap_dfr(severity_contrast_set, function(weights_vec, label) {
    contrast <- survey::svycontrast(model, list(contrast = weights_vec))
    estimate <- as.numeric(coef(contrast))
    standard_error <- as.numeric(survey::SE(contrast))
    tibble::tibble(
      specification = specification,
      contrast = label,
      pr = exp(estimate),
      ci_low = exp(estimate - stats::qt(0.975, df_design) * standard_error),
      ci_high = exp(estimate + stats::qt(0.975, df_design) * standard_error),
      p_value = 2 * stats::pt(-abs(estimate / standard_error), df_design)
    )
  })
}

severity_data <- analysis_base %>%
  dplyr::filter(!dplyr::if_any(
    c(uninsured, wg_severity, sex, age_group, residence, wealth, education), is.na
  ))
design_severity <- make_design(severity_data)

model_severity_crude <- survey::svyglm(
  uninsured ~ wg_severity,
  design = design_severity,
  family = quasipoisson(link = "log")
)

model_severity_confounder <- survey::svyglm(
  uninsured ~ wg_severity + sex + age_group + residence,
  design = design_severity,
  family = quasipoisson(link = "log")
)

model_severity_full <- survey::svyglm(
  uninsured ~ wg_severity + sex + age_group + residence + wealth + education,
  design = design_severity,
  family = quasipoisson(link = "log")
)

severity_contrast_table <- dplyr::bind_rows(
  severity_contrasts(model_severity_crude, design_severity, "crude"),
  severity_contrasts(model_severity_confounder, design_severity, "confounder"),
  severity_contrasts(model_severity_full, design_severity, "full")
)

severity_global_tests <- purrr::imap_dfr(
  list(
    crude = model_severity_crude,
    confounder = model_severity_confounder,
    full = model_severity_full
  ),
  function(model, spec_name) {
    test <- survey::regTermTest(model, ~wg_severity)
    tibble::tibble(
      specification = spec_name,
      statistic = as.numeric(test$Ftest),
      df = as.numeric(test$df),
      ddf = as.numeric(test$ddf),
      p_value = as.numeric(test$p)
    )
  }
)

# Multi-domain sensitivity: the same question asked of the domain count rather
# than the maximum-severity summary.
domain_burden_data <- analysis_base %>%
  dplyr::filter(!dplyr::if_any(
    c(uninsured, domain_burden, sex, age_group, residence, wealth, education), is.na
  ))
design_domain_burden <- make_design(domain_burden_data)

model_domain_burden <- survey::svyglm(
  uninsured ~ domain_burden + sex + age_group + residence + wealth + education,
  design = design_domain_burden,
  family = quasipoisson(link = "log")
)
domain_burden_table <- tidy_apr_design(model_domain_burden, design_domain_burden) %>%
  dplyr::filter(stringr::str_detect(term, "domain_burden"))

domain_burden_global <- survey::regTermTest(model_domain_burden, ~domain_burden)

# ---------------------------------------------------------------------------
# Payment at outpatient contact: incidence, amount, and payer
#
# sh32 records only whether any money changed hands, so on its own it cannot
# support a statement about financial protection. sh304 gives the amount and
# sh305a-e give the payer split. Both are conditional on having paid, which is
# the questionnaire's own skip pattern, so they describe the size and the source
# of a payment among those who made one and are reported that way.
# ---------------------------------------------------------------------------

outpatient_users <- analysis_base %>%
  dplyr::filter(outpatient_last4w == 1, !is.na(paid_outpatient_recent))

payers <- outpatient_users %>%
  dplyr::filter(paid_outpatient_recent == 1, !is.na(cost_outpatient_total), !is.na(insured_any))

design_payers <- make_design(payers)

cost_group_summary <- payers %>%
  dplyr::mutate(
    group = paste0(
      dplyr::if_else(wg_disability == 1, "WG disability", "No WG disability"),
      ", ",
      dplyr::if_else(insured_any == 1, "insured", "uninsured")
    )
  ) %>%
  dplyr::group_by(group) %>%
  dplyr::group_modify(~{
    design_group <- make_design(.x)
    median_estimate <- survey::svyquantile(
      ~cost_outpatient_total, design_group,
      quantiles = 0.5, ci = TRUE, na.rm = TRUE
    )
    median_matrix <- median_estimate$cost_outpatient_total
    mean_estimate <- survey::svymean(~cost_outpatient_total, design_group, na.rm = TRUE)
    mean_ci <- stats::confint(mean_estimate, df = survey::degf(design_group))

    # sh304 is the gross cost of the visit; sh305a is the part of it met in cash,
    # which is the net out-of-pocket payment once any insurer contribution is
    # taken off. Reporting both separates use from protection, following the
    # gross/net distinction drawn in the KDHS 2022 matching analysis.
    cash_data <- .x %>% dplyr::filter(!is.na(cost_met_cash))
    if (nrow(cash_data) > 0) {
      design_cash <- make_design(cash_data)
      cash_median <- survey::svyquantile(
        ~cost_met_cash, design_cash, quantiles = 0.5, ci = TRUE, na.rm = TRUE
      )$cost_met_cash
      cash_mean <- survey::svymean(~cost_met_cash, design_cash, na.rm = TRUE)
      cash_mean_ci <- stats::confint(cash_mean, df = survey::degf(design_cash))
      cash_n <- nrow(cash_data)
      cash_median_value <- as.numeric(cash_median[1, "quantile"])
      cash_median_low <- as.numeric(cash_median[1, "ci.2.5"])
      cash_median_high <- as.numeric(cash_median[1, "ci.97.5"])
      cash_mean_value <- as.numeric(stats::coef(cash_mean)[1])
      cash_mean_low <- cash_mean_ci[1, 1]
      cash_mean_high <- cash_mean_ci[1, 2]
    } else {
      cash_n <- 0L
      cash_median_value <- cash_median_low <- cash_median_high <- NA_real_
      cash_mean_value <- cash_mean_low <- cash_mean_high <- NA_real_
    }

    tibble::tibble(
      n = nrow(.x),
      median = as.numeric(median_matrix[1, "quantile"]),
      median_ci_low = as.numeric(median_matrix[1, "ci.2.5"]),
      median_ci_high = as.numeric(median_matrix[1, "ci.97.5"]),
      mean = as.numeric(stats::coef(mean_estimate)[1]),
      mean_ci_low = mean_ci[1, 1],
      mean_ci_high = mean_ci[1, 2],
      cash_n = cash_n,
      cash_median = cash_median_value,
      cash_median_ci_low = cash_median_low,
      cash_median_ci_high = cash_median_high,
      cash_mean = cash_mean_value,
      cash_mean_ci_low = cash_mean_low,
      cash_mean_ci_high = cash_mean_high
    )
  }) %>%
  dplyr::ungroup()

# Who met the bill. Restricted to payers with a non-missing payer split.
payer_source_base <- payers %>% dplyr::filter(!is.na(nhif_met_any))

payer_source_summary <- payer_source_base %>%
  dplyr::mutate(
    group = paste0(
      dplyr::if_else(wg_disability == 1, "WG disability", "No WG disability"),
      ", ",
      dplyr::if_else(insured_any == 1, "insured", "uninsured")
    )
  ) %>%
  dplyr::group_by(group) %>%
  dplyr::group_modify(~{
    nhif_stat <- weighted_binary(.x, "nhif_met_any")
    insurer_stat <- weighted_binary(.x, "insurer_met_any")
    cash_stat <- weighted_mean_stat(.x, "cash_share_of_cost")
    tibble::tibble(
      n = nrow(.x),
      nhif_met_any = nhif_stat$est,
      nhif_ci_low = nhif_stat$ci_low,
      nhif_ci_high = nhif_stat$ci_high,
      insurer_met_any = insurer_stat$est,
      insurer_ci_low = insurer_stat$ci_low,
      insurer_ci_high = insurer_stat$ci_high,
      cash_share = cash_stat$est,
      cash_share_ci_low = cash_stat$ci_low,
      cash_share_ci_high = cash_stat$ci_high
    )
  }) %>%
  dplyr::ungroup()

# Among insured payers with disability, the single number the exemption debate
# turns on: did the scheme meet any part of the bill.
insured_disabled_payers <- payer_source_base %>%
  dplyr::filter(wg_disability == 1, insured_any == 1)
insured_all_payers <- payer_source_base %>% dplyr::filter(insured_any == 1)

insurer_realisation <- dplyr::bind_rows(
  weighted_binary(insured_all_payers, "insurer_met_any") %>%
    dplyr::mutate(group = "All insured payers"),
  weighted_binary(insured_disabled_payers, "insurer_met_any") %>%
    dplyr::mutate(group = "Insured payers with disability")
) %>%
  dplyr::select(group, dplyr::everything())

# Ratio of amounts paid, disability versus no disability and insured versus
# uninsured, on the log scale among payers. Reported as a ratio of geometric
# means because the amounts are strongly right-skewed.
cost_model_data <- payers %>%
  dplyr::filter(cost_outpatient_total > 0) %>%
  dplyr::filter(!dplyr::if_any(c(sex, age_group, residence, wealth, education), is.na))
design_cost_model <- make_design(cost_model_data)

model_cost <- survey::svyglm(
  log(cost_outpatient_total) ~ wg_disability + insured_any + sex + age_group + residence + wealth + education,
  design = design_cost_model
)
cost_model_table <- broom::tidy(model_cost) %>%
  dplyr::mutate(
    ratio = exp(estimate),
    ci_low = exp(estimate - stats::qt(0.975, survey::degf(design_cost_model)) * std.error),
    ci_high = exp(estimate + stats::qt(0.975, survey::degf(design_cost_model)) * std.error)
  ) %>%
  dplyr::select(term, ratio, ci_low, ci_high, p.value)

# The same model for the cash (net out-of-pocket) amount.
cash_model_data <- cost_model_data %>% dplyr::filter(!is.na(cost_met_cash), cost_met_cash > 0)
design_cash_model <- make_design(cash_model_data)

model_cash <- survey::svyglm(
  log(cost_met_cash) ~ wg_disability + insured_any + sex + age_group + residence + wealth + education,
  design = design_cash_model
)
cash_model_table <- broom::tidy(model_cash) %>%
  dplyr::mutate(
    ratio = exp(estimate),
    ci_low = exp(estimate - stats::qt(0.975, survey::degf(design_cash_model)) * std.error),
    ci_high = exp(estimate + stats::qt(0.975, survey::degf(design_cash_model)) * std.error)
  ) %>%
  dplyr::select(term, ratio, ci_low, ci_high, p.value)

# Any-payment incidence, whole sample, adjusted. Conditioning on having used
# outpatient care is a selection step and is reported as such.
payment_whole_data <- outpatient_users %>%
  dplyr::filter(!dplyr::if_any(
    c(paid_outpatient_recent, wg_disability, insured_any, sex, age_group, residence, wealth, education), is.na
  ))
design_payment_whole <- make_design(payment_whole_data)

model_payment_whole <- survey::svyglm(
  paid_outpatient_recent ~ wg_disability + insured_any + sex + age_group + residence + wealth + education,
  design = design_payment_whole,
  family = quasipoisson(link = "log")
)
model_payment_whole_tidy <- tidy_apr_design(model_payment_whole, design_payment_whole)

payment_outcome_counts <- tibble::tibble(
  model_n = nrow(payment_whole_data),
  paid_n = sum(payment_whole_data$paid_outpatient_recent == 1),
  did_not_pay_n = sum(payment_whole_data$paid_outpatient_recent == 0),
  weighted_paid = as.numeric(survey::svymean(~paid_outpatient_recent, design_payment_whole, na.rm = TRUE)[1])
)

# ---------------------------------------------------------------------------
# Sensitivity analyses
# ---------------------------------------------------------------------------

dk_data <- analysis_base %>%
  dplyr::filter(!dplyr::if_any(
    c(uninsured_incl_dk, wg_disability, sex, age_group, residence, wealth, education), is.na
  ))
design_dk <- make_design(dk_data)

model_dk_confounder <- survey::svyglm(
  uninsured_incl_dk ~ wg_disability + sex + age_group + residence,
  design = design_dk,
  family = quasipoisson(link = "log")
)
model_dk_full <- survey::svyglm(
  uninsured_incl_dk ~ wg_disability + sex + age_group + residence + wealth + education,
  design = design_dk,
  family = quasipoisson(link = "log")
)

sensitivity_dk <- dplyr::bind_rows(
  tidy_apr_design(model_dk_confounder, design_dk) %>%
    dplyr::filter(term == "wg_disability") %>% dplyr::mutate(specification = "confounder"),
  tidy_apr_design(model_dk_full, design_dk) %>%
    dplyr::filter(term == "wg_disability") %>% dplyr::mutate(specification = "full")
) %>%
  dplyr::mutate(model_n = nrow(dk_data), coding = "Don't know grouped with uninsured")

# Comparison of adults included in and excluded from the fully adjusted
# insurance model, for the STROBE missing-data item.
inclusion_comparison <- analysis_base %>%
  dplyr::mutate(
    included = as.integer(!dplyr::if_any(
      c(uninsured, wg_disability, sex, age_group, residence, wealth, education), is.na
    ))
  ) %>%
  dplyr::group_by(included) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_age = mean(age, na.rm = TRUE),
    pct_women = 100 * mean(sex == "Women", na.rm = TRUE),
    pct_rural = 100 * mean(residence == "Rural", na.rm = TRUE),
    pct_wg_disability = 100 * mean(wg_disability == 1, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

extended_outputs <- list(
  design_metadata = design_metadata,
  whole_sample_contrasts = whole_sample_contrasts,
  model_uninsured_whole = model_uninsured_whole_tidy,
  severity_contrasts = severity_contrast_table,
  severity_global_tests = severity_global_tests,
  domain_burden = domain_burden_table,
  domain_burden_global = tibble::tibble(
    statistic = as.numeric(domain_burden_global$Ftest),
    df = as.numeric(domain_burden_global$df),
    ddf = as.numeric(domain_burden_global$ddf),
    p_value = as.numeric(domain_burden_global$p)
  ),
  cost_group_summary = cost_group_summary,
  payer_source_summary = payer_source_summary,
  insurer_realisation = insurer_realisation,
  cost_model = cost_model_table,
  cost_model_n = nrow(cost_model_data),
  cash_model = cash_model_table,
  cash_model_n = nrow(cash_model_data),
  model_payment_whole = model_payment_whole_tidy,
  payment_outcome_counts = payment_outcome_counts,
  sensitivity_dk = sensitivity_dk,
  inclusion_comparison = inclusion_comparison
)

save_rds_output(extended_outputs, "st02_extended_outputs.rds")

readr::write_csv(whole_sample_contrasts, file.path(paths$logs_dir, "st02_whole_sample_contrasts.csv"))
readr::write_csv(model_uninsured_whole_tidy, file.path(paths$logs_dir, "st02_model_uninsured_whole_sample.csv"))
readr::write_csv(severity_contrast_table, file.path(paths$logs_dir, "st02_severity_contrasts.csv"))
readr::write_csv(severity_global_tests, file.path(paths$logs_dir, "st02_severity_global_tests.csv"))
readr::write_csv(domain_burden_table, file.path(paths$logs_dir, "st02_domain_burden_model.csv"))
readr::write_csv(cost_group_summary, file.path(paths$logs_dir, "st02_outpatient_cost_summary.csv"))
readr::write_csv(payer_source_summary, file.path(paths$logs_dir, "st02_outpatient_payer_source.csv"))
readr::write_csv(cost_model_table, file.path(paths$logs_dir, "st02_model_outpatient_cost.csv"))
readr::write_csv(cash_model_table, file.path(paths$logs_dir, "st02_model_outpatient_cash.csv"))
readr::write_csv(model_payment_whole_tidy, file.path(paths$logs_dir, "st02_model_payment_whole_sample.csv"))
readr::write_csv(sensitivity_dk, file.path(paths$logs_dir, "st02_sensitivity_dont_know.csv"))
readr::write_csv(inclusion_comparison, file.path(paths$logs_dir, "st02_inclusion_comparison.csv"))
readr::write_csv(design_metadata, file.path(paths$logs_dir, "st02_design_metadata.csv"))

append_log("Whole-sample contrasts, cost analyses, and sensitivity analyses saved for ST02.")

message("=== SECTION 3B COMPLETE ===")
