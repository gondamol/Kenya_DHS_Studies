# Study: ST02_Disability_Insurance_Equity
# Script: 03b_analysis_whole_sample.R
# Author: Nichodemus Werre Amollo
# Date: 2026-09-19
# Purpose: Whole-sample disability contrasts, formal severity tests, the
#          outpatient amount and payer-source analysis, and the sensitivity
#          analyses. Section 3 estimates associations *within* the disability
#          subpopulation; this section estimates the disability contrast itself.
#
#          Every estimate here is a domain of one parent survey design rather
#          than a design rebuilt on filtered rows, so variances keep the parent
#          PSU and stratum structure.

message("=== SECTION 3B: Whole-sample contrasts, amounts, and sensitivity analyses ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analytic_adults <- readRDS(file.path(paths$derived_dir, "st02_analytic_pr_adults.rds"))

append_log("Running whole-sample, amount, and sensitivity analyses for ST02.", also_message = TRUE)

# ---------------------------------------------------------------------------
# Analytic base and parent design
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

analytic_adults <- model_levels(analytic_adults)
register_parent_design(analytic_adults)

analysis_base <- analytic_adults %>%
  dplyr::filter(long_questionnaire, !is.na(wg_disability), !is.na(wg_severity))

design_base <- domain_design(analysis_base)

design_metadata <- tibble::tibble(
  analytic_n = nrow(analysis_base),
  psu_n = dplyr::n_distinct(analysis_base$psu),
  strata_n = dplyr::n_distinct(analysis_base$strata),
  households_n = analysis_base %>% dplyr::distinct(cluster, household) %>% nrow(),
  design_df = survey::degf(design_base),
  parent_n = nrow(analytic_adults),
  parent_psu_n = dplyr::n_distinct(analytic_adults$psu)
)

# ---------------------------------------------------------------------------
# Whole-sample disability contrasts
#
# Three specifications per outcome. They are named for what they adjust for
# rather than for a causal role, because the survey records no age at disability
# onset and the ordering of education, wealth and functional difficulty cannot be
# established from it. For disability acquired in later life, education precedes
# the exposure and confounds it; for lifelong or early-onset difficulty, schooling
# and earnings are downstream of it. Attenuation between specification 2 and
# specification 3 is consistent with either, and with measurement error in the
# adjustment variables, so it is reported as attenuation and not as mediation.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Questionnaire-half comparison
# ---------------------------------------------------------------------------
#
# The disability, insurance, utilisation and payment modules were administered
# only in long-questionnaire households, so every estimate in this study is
# conditional on that half of the sample. Sex, age group, residence, wealth and
# education are collected in both halves, which makes the two directly
# comparable on the characteristics most plausibly related to coverage and
# care-seeking. A large difference here would mean the half-sample cannot stand
# in for the adult population. A small one does not establish exchangeability on
# the module variables themselves, which are unobserved in the short half, but
# it removes the most obvious route by which the restriction could bias these
# estimates.
questionnaire_half_vars <- c("sex", "age_group", "residence", "wealth", "education")

questionnaire_half_comparison <- purrr::map_dfr(questionnaire_half_vars, function(variable_name) {
  data_use <- analytic_adults %>% dplyr::filter(!is.na(.data[[variable_name]]))
  design_use <- domain_design(data_use)

  means <- survey::svyby(
    stats::as.formula(paste0("~", variable_name)), ~long_questionnaire,
    design_use, survey::svymean, na.rm = TRUE
  )
  # Rao-Scott F test of independence between questionnaire half and the variable.
  p_value <- tryCatch(
    as.numeric(survey::svychisq(
      stats::as.formula(paste0("~", variable_name, " + long_questionnaire")),
      design_use, statistic = "F"
    )$p.value),
    error = function(e) NA_real_
  )

  estimates <- means[, grepl(paste0("^", variable_name), names(means)), drop = FALSE]
  tibble::tibble(
    variable = variable_name,
    level = sub(paste0("^", variable_name), "", names(estimates)),
    long_pct = 100 * as.numeric(estimates[means$long_questionnaire == TRUE, ]),
    short_pct = 100 * as.numeric(estimates[means$long_questionnaire == FALSE, ]),
    p_value = p_value
  ) %>%
    dplyr::mutate(difference_pp = long_pct - short_pct)
})

questionnaire_half_max_difference <- max(abs(questionnaire_half_comparison$difference_pp))

specification_labels <- c(
  crude = "Unadjusted",
  confounder = "Adjusted for sex, age group, residence",
  full = "Additionally adjusted for wealth, education"
)

base_terms <- "sex + age_group + residence"
additional_terms <- "wealth + education"

fit_specifications <- function(outcome, exposure = "wg_disability", data = analysis_base,
                               replicate_reps = 500) {
  needed <- c(outcome, exposure, "sex", "age_group", "residence", "wealth", "education")
  complete_data <- data %>% dplyr::filter(!dplyr::if_any(dplyr::all_of(needed), is.na))
  design_complete <- domain_design(complete_data)

  specs <- list(
    crude = paste(outcome, "~", exposure),
    confounder = paste(outcome, "~", exposure, "+", base_terms),
    full = paste(outcome, "~", exposure, "+", base_terms, "+", additional_terms)
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
    replicated <- standardised_contrast_replicate(
      formula_txt, design_complete, exposure, replicates = replicate_reps
    )

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
      replicate_difference = replicated$difference,
      replicate_difference_ci_low = replicated$difference_ci_low,
      replicate_difference_ci_high = replicated$difference_ci_high,
      design_df = estimates$df
    )
  })
}

# Run the outcomes one at a time and collect afterwards, so that at most one
# outcome's replicate machinery is resident at a time.
whole_sample_contrasts <- purrr::map_dfr(
  c("uninsured", "outpatient_last4w", "inpatient_last12m"),
  function(outcome_name) {
    result <- fit_specifications(outcome_name)
    gc(verbose = FALSE)
    result
  }
)

uninsured_whole_data <- analysis_base %>%
  dplyr::filter(!dplyr::if_any(
    c(uninsured, wg_disability, sex, age_group, residence, wealth, education), is.na
  ))
design_uninsured_whole <- domain_design(uninsured_whole_data)

model_uninsured_whole <- survey::svyglm(
  uninsured ~ wg_disability + sex + age_group + residence + wealth + education,
  design = design_uninsured_whole,
  family = quasipoisson(link = "log")
)
model_uninsured_whole_tidy <- tidy_apr_design(model_uninsured_whole, design_uninsured_whole)

# ---------------------------------------------------------------------------
# Severity: prespecified contrasts and a design-based global test
#
# The contrast family is fixed in advance and comprises the three adjacent steps
# plus the extreme comparison of severe against no difficulty. Inference is
# descriptive and is not adjusted for multiplicity; the four contrasts are
# reported together so that readers can see the whole family.
# ---------------------------------------------------------------------------

severity_contrast_set <- list(
  "Mild vs no difficulty" = c("wg_severityMild functional difficulty" = 1),
  "Moderate vs mild" = c(
    "wg_severityModerate functional difficulty" = 1,
    "wg_severityMild functional difficulty" = -1
  ),
  "Severe vs moderate" = c(
    "wg_severitySevere functional difficulty" = 1,
    "wg_severityModerate functional difficulty" = -1
  ),
  "Severe vs no difficulty (extreme contrast)" = c("wg_severitySevere functional difficulty" = 1)
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
design_severity <- domain_design(severity_data)

model_severity_crude <- survey::svyglm(
  uninsured ~ wg_severity, design = design_severity, family = quasipoisson(link = "log")
)
model_severity_confounder <- survey::svyglm(
  uninsured ~ wg_severity + sex + age_group + residence,
  design = design_severity, family = quasipoisson(link = "log")
)
model_severity_full <- survey::svyglm(
  uninsured ~ wg_severity + sex + age_group + residence + wealth + education,
  design = design_severity, family = quasipoisson(link = "log")
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

domain_burden_data <- analysis_base %>%
  dplyr::filter(!dplyr::if_any(
    c(uninsured, domain_burden, sex, age_group, residence, wealth, education), is.na
  ))
design_domain_burden <- domain_design(domain_burden_data)

model_domain_burden <- survey::svyglm(
  uninsured ~ domain_burden + sex + age_group + residence + wealth + education,
  design = design_domain_burden,
  family = quasipoisson(link = "log")
)
domain_burden_table <- tidy_apr_design(model_domain_burden, design_domain_burden) %>%
  dplyr::filter(stringr::str_detect(term, "domain_burden"))
domain_burden_global <- survey::regTermTest(model_domain_burden, ~domain_burden)

# ---------------------------------------------------------------------------
# Payment: incidence, amount, and payer
#
# Three nested populations, kept distinct throughout because they answer
# different questions and have different denominators:
#
#   users   - used outpatient care in the previous four weeks, with a payment
#             response. Supports statements about payment incidence.
#   payers  - of those, reported paying and have a usable total amount. Supports
#             statements about how much was paid.
#   split   - of those, have the payer breakdown. Supports statements about who
#             met the bill.
#
# The amount and payer items are skipped for anyone who reported no payment, so
# nothing here describes an encounter at which nothing was paid, including one an
# insurer may have covered in full. Statements about insurer contribution are
# therefore bounded below and above at the end of this section rather than
# generalised to all insured users.
# ---------------------------------------------------------------------------

outpatient_users <- analysis_base %>%
  dplyr::filter(outpatient_last4w == 1, !is.na(paid_outpatient_recent), !is.na(insured_any))

payers <- outpatient_users %>%
  dplyr::filter(paid_outpatient_recent == 1, !is.na(cost_outpatient_total))

payer_split_base <- payers %>% dplyr::filter(!is.na(nhif_met_any))

group_label <- function(data) {
  data %>%
    dplyr::mutate(
      group = paste0(
        dplyr::if_else(wg_disability == 1, "WG disability", "No WG disability"),
        ", ",
        dplyr::if_else(insured_any == 1, "insured", "uninsured")
      )
    )
}

survey_median <- function(data, var) {
  usable <- data %>% dplyr::filter(!is.na(.data[[var]]))
  if (nrow(usable) == 0) {
    return(tibble::tibble(n = 0L, median = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  design_group <- domain_design(usable)
  quantile_estimate <- survey::svyquantile(
    stats::as.formula(paste0("~", var)), design_group,
    quantiles = 0.5, ci = TRUE, na.rm = TRUE
  )[[1]]
  tibble::tibble(
    n = nrow(usable),
    median = as.numeric(quantile_estimate[1, "quantile"]),
    ci_low = as.numeric(quantile_estimate[1, "ci.2.5"]),
    ci_high = as.numeric(quantile_estimate[1, "ci.97.5"])
  )
}

survey_mean_amount <- function(data, var) {
  usable <- data %>% dplyr::filter(!is.na(.data[[var]]))
  if (nrow(usable) == 0) {
    return(tibble::tibble(n = 0L, mean = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  design_group <- domain_design(usable)
  estimate <- survey::svymean(stats::as.formula(paste0("~", var)), design_group, na.rm = TRUE)
  ci <- stats::confint(estimate, df = survey::degf(design_group))
  tibble::tibble(
    n = nrow(usable),
    mean = as.numeric(stats::coef(estimate)[1]),
    ci_low = ci[1, 1],
    ci_high = ci[1, 2]
  )
}

cost_group_summary <- group_label(payers) %>%
  dplyr::group_by(group) %>%
  dplyr::group_modify(~{
    gross_median <- survey_median(.x, "cost_outpatient_total")
    gross_mean <- survey_mean_amount(.x, "cost_outpatient_total")
    # Named so they cannot collide with the columns built below: inside tibble()
    # a later expression sees the columns already defined in the same call.
    cash_median_stat <- survey_median(.x, "cost_met_cash")
    cash_mean_stat <- survey_mean_amount(.x, "cost_met_cash")
    # The mean cash payment including the zeros is the quantity that answers
    # "what did this group pay out of pocket on average", and the zeros are where
    # insurance shows up, so it is reported beside the conditional median.
    tibble::tibble(
      n = nrow(.x),
      median = gross_median$median,
      median_ci_low = gross_median$ci_low,
      median_ci_high = gross_median$ci_high,
      mean = gross_mean$mean,
      mean_ci_low = gross_mean$ci_low,
      mean_ci_high = gross_mean$ci_high,
      cash_n = cash_median_stat$n,
      cash_median = cash_median_stat$median,
      cash_median_ci_low = cash_median_stat$ci_low,
      cash_median_ci_high = cash_median_stat$ci_high,
      cash_mean = cash_mean_stat$mean,
      cash_mean_ci_low = cash_mean_stat$ci_low,
      cash_mean_ci_high = cash_mean_stat$ci_high
    )
  }) %>%
  dplyr::ungroup()

# Zero cash is not a nuisance category: it is one of the two ways insurance can
# show up in these data, and it is concentrated among the insured.
zero_cash_summary <- group_label(payers) %>%
  dplyr::filter(!is.na(cost_met_cash)) %>%
  dplyr::group_by(group) %>%
  dplyr::group_modify(~{
    zero_stat <- weighted_binary(.x %>% dplyr::mutate(zero_cash = as.integer(cost_met_cash == 0)), "zero_cash")
    tibble::tibble(
      n = nrow(.x),
      zero_cash_n = sum(.x$cost_met_cash == 0),
      zero_cash_pct = zero_stat$est,
      zero_cash_ci_low = zero_stat$ci_low,
      zero_cash_ci_high = zero_stat$ci_high
    )
  }) %>%
  dplyr::ungroup()

payer_source_summary <- group_label(payer_split_base) %>%
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

insurer_realisation <- dplyr::bind_rows(
  weighted_binary(payer_split_base %>% dplyr::filter(insured_any == 1), "insurer_met_any") %>%
    dplyr::mutate(group = "All insured payers with payer data"),
  weighted_binary(payer_split_base %>% dplyr::filter(insured_any == 1, wg_disability == 1), "insurer_met_any") %>%
    dplyr::mutate(group = "Insured payers with disability and payer data")
) %>%
  dplyr::select(group, dplyr::everything())

# Bounds on insurer contribution among *all* insured outpatient users.
#
# The recorded proportion is conditional on having paid and on having a payer
# split. Encounters at which nothing was paid are unobserved for payer source and
# could in principle all have been met in full by an insurer, or none of them.
# Those two assumptions bound the quantity the policy question actually asks
# about. Nothing in between is identified by these data.
# The bounds are population proportions, so they are estimated with the DHS
# person weights on the parent design, not from raw counts. Each endpoint is an
# ordinary survey proportion of the same domain (insured outpatient users): the
# lower endpoint counts only encounters with a recorded insurer contribution,
# the upper endpoint additionally counts every encounter whose payer source is
# unobserved. Unweighted counts are retained alongside them to describe the
# sample, not to estimate the population.
#
# The design-based limits below are confidence limits for each endpoint
# separately. They are not a confidence interval for the partially identified
# parameter, and the manuscript says so where they are reported.
insurer_bounds <- purrr::map_dfr(list(all = NULL, disability = 1), function(restriction) {
  users_group <- outpatient_users %>% dplyr::filter(insured_any == 1)
  if (!is.null(restriction)) users_group <- users_group %>% dplyr::filter(wg_disability == 1)

  users_group <- users_group %>%
    dplyr::mutate(
      .has_split = paid_outpatient_recent == 1 & !is.na(nhif_met_any),
      .insurer_recorded = dplyr::coalesce(
        as.integer(.has_split & insurer_met_any == 1), 0L
      ),
      # Unobserved payer source: nothing was paid, or the split is missing.
      .bound_lower = .insurer_recorded,
      .bound_upper = as.integer(.insurer_recorded == 1 | !.has_split)
    )

  users_n <- nrow(users_group)
  no_payment_n <- sum(users_group$paid_outpatient_recent == 0)
  with_split_n <- sum(users_group$.has_split)
  insurer_n <- sum(users_group$.insurer_recorded)
  unknown_n <- users_n - no_payment_n - with_split_n

  lower <- weighted_binary(users_group, ".bound_lower")
  upper <- weighted_binary(users_group, ".bound_upper")

  tibble::tibble(
    population = if (is.null(restriction)) "All insured outpatient users" else "Insured outpatient users with disability",
    users_n = users_n,
    no_payment_n = no_payment_n,
    payers_with_split_n = with_split_n,
    payer_split_missing_n = unknown_n,
    insurer_recorded_n = insurer_n,
    lower_bound_unweighted = insurer_n / users_n,
    upper_bound_unweighted = (insurer_n + no_payment_n + unknown_n) / users_n,
    lower_bound = lower$est,
    lower_bound_ci_low = lower$ci_low,
    lower_bound_ci_high = lower$ci_high,
    upper_bound = upper$est,
    upper_bound_ci_low = upper$ci_low,
    upper_bound_ci_high = upper$ci_high
  )
})

# Two-part analysis of the cash amount.
#
# Part one is the probability of paying anything in cash, which is where the
# zeros live. Part two is the amount among those who did. An earlier version
# fitted only part two and described it as the amount paid in cash, which
# excluded 403 zero-cash records concentrated among insured patients and so
# removed the observations most likely to be consistent with an insurance contribution.
cash_model_covariates <- c("sex", "age_group", "residence", "wealth", "education")

cash_part_data <- payers %>%
  dplyr::filter(!is.na(cost_met_cash), !dplyr::if_any(dplyr::all_of(cash_model_covariates), is.na)) %>%
  dplyr::mutate(any_cash = as.integer(cost_met_cash > 0))
design_cash_part1 <- domain_design(cash_part_data)

model_cash_part1 <- survey::svyglm(
  any_cash ~ wg_disability + insured_any + sex + age_group + residence + wealth + education,
  design = design_cash_part1,
  family = quasipoisson(link = "log")
)
cash_part1_table <- tidy_apr_design(model_cash_part1, design_cash_part1)

cash_part2_data <- cash_part_data %>% dplyr::filter(cost_met_cash > 0)
design_cash_part2 <- domain_design(cash_part2_data)
model_cash_part2 <- survey::svyglm(
  log(cost_met_cash) ~ wg_disability + insured_any + sex + age_group + residence + wealth + education,
  design = design_cash_part2
)
cash_part2_table <- broom::tidy(model_cash_part2) %>%
  dplyr::mutate(
    ratio = exp(estimate),
    ci_low = exp(estimate - stats::qt(0.975, survey::degf(design_cash_part2)) * std.error),
    ci_high = exp(estimate + stats::qt(0.975, survey::degf(design_cash_part2)) * std.error)
  ) %>%
  dplyr::select(term, ratio, ci_low, ci_high, p.value)

# Marginal mean cash payment, zeros retained, so that the two parts can be read
# together on the scale the policy question uses.
cash_marginal <- group_label(cash_part_data) %>%
  dplyr::group_by(group) %>%
  dplyr::group_modify(~ survey_mean_amount(.x, "cost_met_cash")) %>%
  dplyr::ungroup()

# Gross amount model, unchanged in structure but reported as conditional on a
# payment having been made.
cost_model_data <- payers %>%
  dplyr::filter(cost_outpatient_total > 0,
                !dplyr::if_any(dplyr::all_of(cash_model_covariates), is.na))
design_cost_model <- domain_design(cost_model_data)
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

# Sensitivity: drop the records whose amounts are internally inconsistent.
cost_model_consistent_data <- cost_model_data %>%
  dplyr::filter(dplyr::coalesce(amount_inconsistent, 0L) == 0L)
design_cost_consistent <- domain_design(cost_model_consistent_data)
model_cost_consistent <- survey::svyglm(
  log(cost_outpatient_total) ~ wg_disability + insured_any + sex + age_group + residence + wealth + education,
  design = design_cost_consistent
)
cost_model_consistent_table <- broom::tidy(model_cost_consistent) %>%
  dplyr::mutate(
    ratio = exp(estimate),
    ci_low = exp(estimate - stats::qt(0.975, survey::degf(design_cost_consistent)) * std.error),
    ci_high = exp(estimate + stats::qt(0.975, survey::degf(design_cost_consistent)) * std.error)
  ) %>%
  dplyr::select(term, ratio, ci_low, ci_high, p.value) %>%
  dplyr::filter(term %in% c("wg_disability", "insured_any"))

# Payment incidence among users, whole sample.
payment_whole_data <- outpatient_users %>%
  dplyr::filter(!dplyr::if_any(
    c(paid_outpatient_recent, wg_disability, insured_any, sex, age_group, residence, wealth, education), is.na
  ))
design_payment_whole <- domain_design(payment_whole_data)

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
design_dk <- domain_design(dk_data)

model_dk_confounder <- survey::svyglm(
  uninsured_incl_dk ~ wg_disability + sex + age_group + residence,
  design = design_dk, family = quasipoisson(link = "log")
)
model_dk_full <- survey::svyglm(
  uninsured_incl_dk ~ wg_disability + sex + age_group + residence + wealth + education,
  design = design_dk, family = quasipoisson(link = "log")
)

sensitivity_dk <- dplyr::bind_rows(
  tidy_apr_design(model_dk_confounder, design_dk) %>%
    dplyr::filter(term == "wg_disability") %>% dplyr::mutate(specification = "confounder"),
  tidy_apr_design(model_dk_full, design_dk) %>%
    dplyr::filter(term == "wg_disability") %>% dplyr::mutate(specification = "full")
) %>%
  dplyr::mutate(model_n = nrow(dk_data), coding = "Don't know grouped with uninsured")

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
  zero_cash_summary = zero_cash_summary,
  payer_source_summary = payer_source_summary,
  insurer_realisation = insurer_realisation,
  insurer_bounds = insurer_bounds,
  cash_marginal = cash_marginal,
  cash_part1 = cash_part1_table,
  cash_part1_n = nrow(cash_part_data),
  cash_part2 = cash_part2_table,
  cash_part2_n = nrow(cash_part2_data),
  cost_model = cost_model_table,
  cost_model_n = nrow(cost_model_data),
  cost_model_consistent = cost_model_consistent_table,
  cost_model_consistent_n = nrow(cost_model_consistent_data),
  model_payment_whole = model_payment_whole_tidy,
  payment_outcome_counts = payment_outcome_counts,
  sensitivity_dk = sensitivity_dk,
  inclusion_comparison = inclusion_comparison,
  questionnaire_half_comparison = questionnaire_half_comparison,
  questionnaire_half_max_difference = questionnaire_half_max_difference,
  population_counts = tibble::tibble(
    outpatient_users = nrow(outpatient_users),
    payers_with_amount = nrow(payers),
    payers_with_split = nrow(payer_split_base)
  )
)

save_rds_output(extended_outputs, "st02_extended_outputs.rds")

readr::write_csv(whole_sample_contrasts, file.path(paths$logs_dir, "st02_whole_sample_contrasts.csv"))
readr::write_csv(model_uninsured_whole_tidy, file.path(paths$logs_dir, "st02_model_uninsured_whole_sample.csv"))
readr::write_csv(severity_contrast_table, file.path(paths$logs_dir, "st02_severity_contrasts.csv"))
readr::write_csv(severity_global_tests, file.path(paths$logs_dir, "st02_severity_global_tests.csv"))
readr::write_csv(domain_burden_table, file.path(paths$logs_dir, "st02_domain_burden_model.csv"))
readr::write_csv(cost_group_summary, file.path(paths$logs_dir, "st02_outpatient_cost_summary.csv"))
readr::write_csv(zero_cash_summary, file.path(paths$logs_dir, "st02_zero_cash_summary.csv"))
readr::write_csv(payer_source_summary, file.path(paths$logs_dir, "st02_outpatient_payer_source.csv"))
readr::write_csv(insurer_bounds, file.path(paths$logs_dir, "st02_insurer_bounds.csv"))
readr::write_csv(cash_part1_table, file.path(paths$logs_dir, "st02_model_cash_part1.csv"))
readr::write_csv(cash_part2_table, file.path(paths$logs_dir, "st02_model_cash_part2.csv"))
readr::write_csv(cost_model_table, file.path(paths$logs_dir, "st02_model_outpatient_cost.csv"))
readr::write_csv(cost_model_consistent_table, file.path(paths$logs_dir, "st02_model_outpatient_cost_consistent.csv"))
readr::write_csv(model_payment_whole_tidy, file.path(paths$logs_dir, "st02_model_payment_whole_sample.csv"))
readr::write_csv(sensitivity_dk, file.path(paths$logs_dir, "st02_sensitivity_dont_know.csv"))
readr::write_csv(inclusion_comparison, file.path(paths$logs_dir, "st02_inclusion_comparison.csv"))
readr::write_csv(design_metadata, file.path(paths$logs_dir, "st02_design_metadata.csv"))

append_log("Whole-sample contrasts, amount analyses, bounds, and sensitivity analyses saved for ST02.")

message("=== SECTION 3B COMPLETE ===")
