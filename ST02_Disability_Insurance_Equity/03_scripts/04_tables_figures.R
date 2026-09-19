# Study: ST02_Disability_Insurance_Equity
# Script: 04_tables_figures.R
# Author: Nichodemus Werre Amollo
# Date: 2026-04-05
# Purpose: Create manuscript-facing tables, figures, and inline-ready summary outputs for ST02.

message("=== SECTION 4: Tables and Figures ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analytic_adults <- readRDS(file.path(paths$derived_dir, "st02_analytic_pr_adults.rds"))
model_uninsured <- readr::read_csv(file.path(paths$logs_dir, "st02_model_uninsured_disabled.csv"), show_col_types = FALSE)
model_payment <- readr::read_csv(file.path(paths$logs_dir, "st02_model_paid_outpatient_disabled.csv"), show_col_types = FALSE)
extended <- readRDS(file.path(paths$derived_dir, "st02_extended_outputs.rds"))
sample_flow <- readr::read_csv(file.path(paths$logs_dir, "st02_sample_flow.csv"), show_col_types = FALSE)
missingness <- readr::read_csv(file.path(paths$logs_dir, "st02_missingness.csv"), show_col_types = FALSE)

append_log("Building ST02 manuscript-facing tables, figures, and summary outputs.", also_message = TRUE)

# Every estimate below is a domain of one parent design, so that variances keep
# the parent PSU and stratum structure rather than being computed from a design
# rebuilt on filtered rows.
register_parent_design(analytic_adults)

analysis_base <- analytic_adults %>%
  dplyr::filter(long_questionnaire, !is.na(wg_disability), !is.na(wg_severity))

insurance_base <- analysis_base %>%
  dplyr::filter(!is.na(insured_any), !is.na(insured_nhif))

outpatient_base <- analysis_base %>%
  dplyr::filter(!is.na(outpatient_last4w))

inpatient_base <- analysis_base %>%
  dplyr::filter(!is.na(inpatient_last12m))

payment_base <- analysis_base %>%
  dplyr::filter(outpatient_last4w == 1, !is.na(paid_outpatient_recent))

payment_disabled_base <- payment_base %>%
  dplyr::filter(wg_disability == 1, !is.na(insured_any), !is.na(wg_severity))

table_groups <- list(
  Overall = analysis_base,
  NoWG = analysis_base %>% dplyr::filter(wg_disability == 0),
  WG = analysis_base %>% dplyr::filter(wg_disability == 1)
)

model_covariates <- c("sex", "age_group", "wealth", "residence", "education", "wg_severity")

uninsured_model_n_value <- analysis_base %>%
  dplyr::filter(wg_disability == 1, !is.na(uninsured),
                !dplyr::if_any(dplyr::all_of(model_covariates), is.na)) %>%
  nrow()

payment_model_n_value <- payment_disabled_base %>%
  dplyr::filter(!is.na(insured_any), !dplyr::if_any(dplyr::all_of(model_covariates), is.na)) %>%
  nrow()

fmt_pvalue <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}

fmt_pct_point <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f"), 100 * x)
}

fmt_apr_ci_p <- function(apr, ci_low, ci_high, p.value) {
  p_text <- ifelse(p.value < 0.001, "p < 0.001", paste0("p = ", sprintf("%.3f", p.value)))
  paste0(fmt_apr_ci(apr, ci_low, ci_high), "; ", p_text)
}

get_binary_stat_string <- function(data, var) {
  stat <- weighted_binary(data, var)
  fmt_pct_ci(stat$est, stat$ci_low, stat$ci_high)
}

get_mean_stat_string <- function(data, var, digits = 1) {
  stat <- weighted_mean(data, var)
  fmt_mean_ci(stat$est, stat$ci_low, stat$ci_high, digits = digits)
}

make_sample_row <- function(label, var = NULL, level = NULL, type = c("level", "binary", "mean", "count")) {
  type <- match.arg(type)
  res <- purrr::map(table_groups, function(df) {
    if (type == "count") {
      return(list(n = format(nrow(df), big.mark = ","), stat = ""))
    }

    if (type == "mean") {
      return(list(
        n = format(sum(!is.na(df[[var]])), big.mark = ","),
        stat = get_mean_stat_string(df, var)
      ))
    }

    if (type == "binary") {
      return(list(
        n = format(sum(!is.na(df[[var]])), big.mark = ","),
        stat = get_binary_stat_string(df, var)
      ))
    }

    # For a category row, n is the unweighted count of people in that category,
    # not the denominator. Reporting the denominator on every row repeated the
    # same sample size down the table and invited it to be read as a count.
    list(
      n = format(sum(df[[var]] == level, na.rm = TRUE), big.mark = ","),
      stat = get_binary_stat_string(
        df %>% dplyr::mutate(.indicator = dplyr::if_else(.data[[var]] == level, 1L, 0L, missing = NA_integer_)),
        ".indicator"
      )
    )
  })

  tibble::tibble(
    Characteristic = label,
    Overall_n = res$Overall$n,
    Overall_stat = res$Overall$stat,
    NoWG_n = res$NoWG$n,
    NoWG_stat = res$NoWG$stat,
    WG_n = res$WG$n,
    WG_stat = res$WG$stat
  )
}

add_section_row <- function(label, table_type = c("table1", "table2", "table3", "table4")) {
  table_type <- match.arg(table_type)

  if (table_type == "table1") {
    return(tibble::tibble(
      Characteristic = label,
      Overall_n = "",
      Overall_stat = "",
      NoWG_n = "",
      NoWG_stat = "",
      WG_n = "",
      WG_stat = ""
    ))
  }

  if (table_type == "table2") {
    return(tibble::tibble(
      Characteristic = label,
      n = "",
      AnyInsurance = "",
      NHIF = ""
    ))
  }

  if (table_type == "table3") {
    return(tibble::tibble(
      AnalysisGroup = label,
      OutpatientN = "",
      Outpatient = "",
      HospitalisationN = "",
      Hospitalisation = "",
      PaymentN = "",
      Payment = ""
    ))
  }

  tibble::tibble(
    Characteristic = label,
    UninsuredAPR = "",
    PaymentAPR = ""
  )
}

table1 <- dplyr::bind_rows(
  make_sample_row("Sample size, n", type = "count"),
  make_sample_row("Mean age, years (95% CI)", var = "age", type = "mean"),
  add_section_row("Sex", "table1"),
  make_sample_row("  Women", var = "sex", level = "Women"),
  add_section_row("Age group", "table1"),
  make_sample_row("  18-29", var = "age_group", level = "18-29"),
  make_sample_row("  30-44", var = "age_group", level = "30-44"),
  make_sample_row("  45-59", var = "age_group", level = "45-59"),
  make_sample_row("  60+", var = "age_group", level = "60+"),
  add_section_row("Place of residence", "table1"),
  make_sample_row("  Rural", var = "residence", level = "Rural"),
  add_section_row("Wealth quintile", "table1"),
  make_sample_row("  Poorest", var = "wealth", level = "Poorest"),
  make_sample_row("  Poorer", var = "wealth", level = "Poorer"),
  make_sample_row("  Middle", var = "wealth", level = "Middle"),
  make_sample_row("  Richer", var = "wealth", level = "Richer"),
  make_sample_row("  Richest", var = "wealth", level = "Richest"),
  add_section_row("Educational attainment", "table1"),
  make_sample_row("  No education", var = "education", level = "No education"),
  make_sample_row("  Primary", var = "education", level = "Primary"),
  make_sample_row("  Secondary", var = "education", level = "Secondary"),
  make_sample_row("  Higher", var = "education", level = "Higher")
)

severity_specs <- tibble::tribble(
  ~label, ~severity,
  "  No functional difficulty", "No functional difficulty",
  "  Mild functional difficulty", "Mild functional difficulty",
  "  Moderate functional difficulty", "Moderate functional difficulty",
  "  Severe functional difficulty", "Severe functional difficulty"
)

domain_specs <- tibble::tribble(
  ~label, ~var_name,
  "  Seeing", "domain_seeing_threshold",
  "  Hearing", "domain_hearing_threshold",
  "  Communication", "domain_communication_threshold",
  "  Memory", "domain_memory_threshold",
  "  Walking", "domain_walking_threshold",
  "  Self-care", "domain_selfcare_threshold"
)

table2_severity <- purrr::map_dfr(seq_len(nrow(severity_specs)), function(i) {
  data_use <- insurance_base %>%
    dplyr::filter(wg_severity == severity_specs$severity[[i]])

  tibble::tibble(
    Characteristic = severity_specs$label[[i]],
    n = format(nrow(data_use), big.mark = ","),
    AnyInsurance = get_binary_stat_string(data_use, "insured_any"),
    NHIF = get_binary_stat_string(data_use, "insured_nhif")
  )
})

domain_coverage_raw <- purrr::map_dfr(seq_len(nrow(domain_specs)), function(i) {
  data_use <- insurance_base %>%
    dplyr::filter(wg_disability == 1, .data[[domain_specs$var_name[[i]]]] == 1)

  any_insurance <- weighted_binary(data_use, "insured_any")
  nhif_coverage <- weighted_binary(data_use, "insured_nhif")

  tibble::tibble(
    Characteristic = domain_specs$label[[i]],
    domain = stringr::str_squish(domain_specs$label[[i]]),
    n = nrow(data_use),
    any_insurance_est = any_insurance$est,
    any_insurance_ci_low = any_insurance$ci_low,
    any_insurance_ci_high = any_insurance$ci_high,
    nhif_est = nhif_coverage$est,
    nhif_ci_low = nhif_coverage$ci_low,
    nhif_ci_high = nhif_coverage$ci_high
  )
})

table2_domains <- domain_coverage_raw %>%
  dplyr::transmute(
    Characteristic,
    n = format(n, big.mark = ","),
    AnyInsurance = fmt_pct_ci(any_insurance_est, any_insurance_ci_low, any_insurance_ci_high),
    NHIF = fmt_pct_ci(nhif_est, nhif_ci_low, nhif_ci_high)
  )

table2 <- dplyr::bind_rows(
  add_section_row("Disability severity", "table2"),
  table2_severity,
  add_section_row("WG-threshold domains among adults with disability", "table2"),
  table2_domains
)

# Each column of Table 3 has its own denominator: utilisation is estimated among
# all adults in the group, payment only among those who used outpatient care in
# the previous four weeks. A single n column implied one denominator for all
# three and understated how small the payment cells are.
make_use_row <- function(label, data_group, payment_group) {
  outpatient_data <- data_group %>% dplyr::filter(!is.na(outpatient_last4w))
  inpatient_data <- data_group %>% dplyr::filter(!is.na(inpatient_last12m))

  tibble::tibble(
    AnalysisGroup = label,
    OutpatientN = format(nrow(outpatient_data), big.mark = ","),
    Outpatient = get_binary_stat_string(outpatient_data, "outpatient_last4w"),
    HospitalisationN = format(nrow(inpatient_data), big.mark = ","),
    Hospitalisation = get_binary_stat_string(inpatient_data, "inpatient_last12m"),
    PaymentN = format(nrow(payment_group), big.mark = ","),
    Payment = get_binary_stat_string(payment_group, "paid_outpatient_recent")
  )
}

payment_disabled_by_insurance <- purrr::map_dfr(c(1, 0), function(insured_value) {
  data_use <- payment_disabled_base %>%
    dplyr::filter(insured_any == insured_value)

  tibble::tibble(
    AnalysisGroup = ifelse(insured_value == 1, "  Insured", "  Uninsured"),
    OutpatientN = "",
    Outpatient = "",
    HospitalisationN = "",
    Hospitalisation = "",
    PaymentN = format(nrow(data_use), big.mark = ","),
    Payment = get_binary_stat_string(data_use, "paid_outpatient_recent")
  )
})

table_use <- dplyr::bind_rows(
  add_section_row("Disability status", "table3"),
  make_use_row(
    "  No WG disability",
    table_groups$NoWG,
    payment_base %>% dplyr::filter(wg_disability == 0)
  ),
  make_use_row(
    "  WG disability threshold",
    table_groups$WG,
    payment_base %>% dplyr::filter(wg_disability == 1)
  ),
  add_section_row("Insurance status, outpatient users with disability", "table3"),
  payment_disabled_by_insurance
)

within_term_order <- c(
  "insured_any",
  "sexWomen",
  "age_group30-44",
  "age_group45-59",
  "age_group60+",
  "wealthRicher",
  "wealthMiddle",
  "wealthPoorer",
  "wealthPoorest",
  "residenceRural",
  "educationPrimary",
  "educationSecondary",
  "educationHigher",
  "wg_severitySevere functional difficulty"
)

within_labels <- tibble::tribble(
  ~term, ~label,
  "insured_any", "  Any insurance (ref: uninsured)",
  "sexWomen", "  Women (ref: men)",
  "age_group30-44", "  30-44",
  "age_group45-59", "  45-59",
  "age_group60+", "  60+",
  "wealthRicher", "  Richer",
  "wealthMiddle", "  Middle",
  "wealthPoorer", "  Poorer",
  "wealthPoorest", "  Poorest",
  "residenceRural", "  Rural (ref: urban)",
  "educationPrimary", "  Primary",
  "educationSecondary", "  Secondary",
  "educationHigher", "  Higher",
  "wg_severitySevere functional difficulty", "  Severe functional difficulty (ref: moderate)"
)

model_uninsured_display <- model_uninsured %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    UninsuredAPR = fmt_apr_ci_p(apr, ci_low, ci_high, p.value)
  ) %>%
  dplyr::select(term, UninsuredAPR)

model_payment_display <- model_payment %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    PaymentAPR = fmt_apr_ci_p(apr, ci_low, ci_high, p.value)
  ) %>%
  dplyr::select(term, PaymentAPR)

within_body <- within_labels %>%
  dplyr::left_join(model_uninsured_display, by = "term") %>%
  dplyr::left_join(model_payment_display, by = "term") %>%
  dplyr::mutate(
    UninsuredAPR = dplyr::coalesce(UninsuredAPR, ""),
    PaymentAPR = dplyr::coalesce(PaymentAPR, "")
  ) %>%
  dplyr::select(Characteristic = label, UninsuredAPR, PaymentAPR)

table_within_build <- dplyr::bind_rows(
  add_section_row("Insurance status", "table4"),
  within_body %>% dplyr::slice(1),
  add_section_row("Sex", "table4"),
  within_body %>% dplyr::slice(2),
  add_section_row("Age group (ref: 18-29)", "table4"),
  within_body %>% dplyr::slice(3:5),
  add_section_row("Wealth quintile (ref: richest)", "table4"),
  within_body %>% dplyr::slice(6:9),
  add_section_row("Place of residence", "table4"),
  within_body %>% dplyr::slice(10),
  add_section_row("Educational attainment (ref: no education)", "table4"),
  within_body %>% dplyr::slice(11:13),
  add_section_row("Disability severity", "table4"),
  within_body %>% dplyr::slice(14)
)

save_table_bundle(
  table1,
  file.path(paths$tables_dir, "Table1_Sample_Characteristics.csv"),
  file.path(paths$tables_dir, "Table1_Sample_Characteristics.docx"),
  "Table 1. Weighted sociodemographic characteristics of adults aged 18 years and above in the KDHS 2022 long-questionnaire subsample, by Washington Group Short Set disability status.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates.",
    "Table 1 is restricted to adult usual residents in the PR long-questionnaire subsample with complete WG-SS classification."
  ),
  header_labels = list(
    Characteristic = "Characteristic",
    Overall_n = "n",
    Overall_stat = "Estimate (95% CI)",
    NoWG_n = "n",
    NoWG_stat = "Estimate (95% CI)",
    WG_n = "n",
    WG_stat = "Estimate (95% CI)"
  ),
  spanner_values = c("", "Overall", "No WG disability", "WG disability threshold"),
  spanner_widths = c(1, 2, 2, 2)
)

save_table_bundle(
  table2,
  file.path(paths$tables_dir, "Table2_Insurance_By_Severity_Domain.csv"),
  file.path(paths$tables_dir, "Table2_Insurance_By_Severity_Domain.docx"),
  "Table 2. Weighted insurance coverage by disability severity and WG-threshold functional domain among adults aged 18 years and above, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates.",
    "Domain rows are non-mutually exclusive and are restricted to adults meeting the WG disability threshold."
  ),
  header_labels = list(
    Characteristic = "Characteristic",
    n = "n",
    AnyInsurance = "Any insurance % (95% CI)",
    NHIF = "NHIF coverage % (95% CI)"
  )
)

table_use_header_labels <- list(
  AnalysisGroup = "Analysis group",
  OutpatientN = "n",
  Outpatient = "Outpatient use % (95% CI)",
  HospitalisationN = "n",
  Hospitalisation = "Hospitalisation % (95% CI)",
  PaymentN = "n",
  Payment = "Paid at last visit % (95% CI)"
)

save_table_bundle(
  table_use,
  file.path(paths$tables_dir, "Table4_Service_Use_And_Payment.csv"),
  file.path(paths$tables_dir, "Table4_Service_Use_And_Payment.docx"),
  "Table 4. Weighted service utilisation and payment at the last outpatient visit by disability status and insurance coverage, adults aged 18 years and above, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates; n is the unweighted denominator for the adjacent column.",
    "Utilisation is estimated among all adults in the row group. Payment is estimated only among adults in that group who used outpatient care in the previous four weeks, so its denominator is smaller.",
    "Payment records whether any money was paid, not how much; amounts and payers are in Table 5."
  ),
  header_labels = table_use_header_labels
)

table_within <- table_within_build
table_within_header_labels <- list(
  Characteristic = "Characteristic",
  UninsuredAPR = "Uninsured APR (95% CI); p",
  PaymentAPR = "Any payment APR (95% CI); p"
)

save_table_bundle(
  table_within,
  file.path(paths$tables_dir, "Table6_Within_Disability_Models.csv"),
  file.path(paths$tables_dir, "Table6_Within_Disability_Models.docx"),
  "Table 6. Survey-weighted adjusted prevalence ratios for uninsured status and for any payment at the last outpatient visit, within the population of adults meeting the Washington Group Short Set disability threshold, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted quasi-Poisson models with a log link; confidence limits use the survey degrees of freedom.",
    paste0("Uninsured model: adults meeting the WG disability threshold (n = ", format(uninsured_model_n_value, big.mark = ","), "). Payment model: adults meeting the threshold who used outpatient care in the previous four weeks (n = ", format(payment_model_n_value, big.mark = ","), ")."),
    "These models describe variation within the disability subpopulation. The disability contrast itself is in Table 3.",
    "The payment model conditions on having used outpatient care, which is affected by disability, insurance and wealth; estimates are descriptive associations among users and not effects of insurance."
  ),
  header_labels = table_within_header_labels
)

# --------------------------------------------------------------------------
# Table 4. The disability contrast itself, estimated on the whole adult sample.
#
# The previous Table 4 reported only within-disability variation, so the study's
# main comparison was supported by crude percentages alone. This table reports it
# under three specifications and on both the ratio and the difference scale.
# --------------------------------------------------------------------------

spec_labels <- c(
  crude = "  Crude",
  confounder = "  Adjusted for sex, age group, residence",
  full = "  Additionally adjusted for wealth, education"
)

outcome_labels <- c(
  uninsured = "Uninsured",
  outpatient_last4w = "Outpatient use in previous 4 weeks",
  inpatient_last12m = "Hospitalisation in previous 12 months"
)

fmt_pp_ci <- function(est, lo, hi) {
  sprintf("%+.1f (%+.1f, %+.1f)", 100 * est, 100 * lo, 100 * hi)
}

table_contrasts <- purrr::imap_dfr(outcome_labels, function(outcome_label, outcome_name) {
  rows <- extended$whole_sample_contrasts %>%
    dplyr::filter(outcome == outcome_name) %>%
    dplyr::mutate(specification = factor(specification, levels = names(spec_labels))) %>%
    dplyr::arrange(specification) %>%
    dplyr::transmute(
      Characteristic = spec_labels[as.character(specification)],
      APR = fmt_apr_ci_p(apr, apr_ci_low, apr_ci_high, p_value),
      # Absolute standardised prevalences, so the percentage-point contrast can be
      # read without going back to the descriptive tables.
      StandardisedPrevalence = sprintf(
        "%.1f vs %.1f",
        100 * standardised_exposed, 100 * standardised_unexposed
      ),
      StandardisedDifference = fmt_pp_ci(
        standardised_difference, standardised_difference_ci_low, standardised_difference_ci_high
      )
    )

  dplyr::bind_rows(
    tibble::tibble(
      Characteristic = paste0(outcome_label, " (n = ",
                              format(dplyr::first(extended$whole_sample_contrasts$model_n[extended$whole_sample_contrasts$outcome == outcome_name]), big.mark = ","),
                              ")"),
      APR = "",
      StandardisedPrevalence = "",
      StandardisedDifference = ""
    ),
    rows
  )
})

save_table_bundle(
  table_contrasts,
  file.path(paths$tables_dir, "Table3_Disability_Contrasts_Whole_Sample.csv"),
  file.path(paths$tables_dir, "Table3_Disability_Contrasts_Whole_Sample.docx"),
  "Table 3. Prevalence ratios and standardised prevalence differences for adults meeting the Washington Group Short Set disability threshold compared with adults below it, whole adult sample, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted quasi-Poisson models with a log link; confidence limits use the survey degrees of freedom.",
    "Standardised prevalences and differences are marginal (g-computation) estimates over the covariate distribution of the analytic sample, at the WG threshold versus below it, with delta-method confidence limits. Bootstrap replicate-weight limits are in Table S6 and agree closely.",
    "Specifications are named for what they adjust for. KDHS records no age at disability onset, so whether education and wealth precede or follow functional difficulty cannot be established here; the change between the second and third row of each block is reported as attenuation, not as a mediated effect."
  ),
  header_labels = list(
    Characteristic = "Outcome and specification",
    APR = "Prevalence ratio (95% CI); p",
    StandardisedPrevalence = "Standardised prevalence, % (disability vs no disability)",
    StandardisedDifference = "Standardised difference, percentage points (95% CI)"
  )
)

# --------------------------------------------------------------------------
# Table 5. What was paid and who paid it.
# --------------------------------------------------------------------------

zero_safe_pct <- function(est, lo, hi) {
  # svyciprop returns a vanishingly small positive value rather than an exact
  # zero when no respondent in a domain has the outcome. Reported as zero, with
  # no interval, because there is no information for one.
  ifelse(est < 1e-6, "0 (not estimable)", fmt_pct_ci(est, lo, hi))
}

fmt_money <- function(x) format(round(x), big.mark = ",", trim = TRUE)

fmt_money_ci <- function(est, lo, hi) {
  sprintf("%s (%s, %s)", fmt_money(est), fmt_money(lo), fmt_money(hi))
}

cost_group_order <- c(
  "No WG disability, uninsured",
  "No WG disability, insured",
  "WG disability, uninsured",
  "WG disability, insured"
)

# Table 5 carries three different denominators and now says so in the table
# itself: the amount columns come from respondents who reported paying and have a
# usable total, the cash columns from those with a cash amount, and the payer
# column from those with the full payer breakdown.
table_cost <- extended$cost_group_summary %>%
  dplyr::left_join(
    extended$zero_cash_summary %>%
      dplyr::select(group, zero_cash_pct, zero_cash_ci_low, zero_cash_ci_high),
    by = "group"
  ) %>%
  dplyr::left_join(
    extended$cash_marginal %>% dplyr::select(group, cash_mean_all = mean,
                                             cash_mean_all_low = ci_low,
                                             cash_mean_all_high = ci_high),
    by = "group"
  ) %>%
  dplyr::left_join(
    extended$payer_source_summary %>%
      dplyr::select(group, payer_n = n, insurer_met_any, insurer_ci_low, insurer_ci_high),
    by = "group"
  ) %>%
  dplyr::mutate(group = factor(group, levels = cost_group_order)) %>%
  dplyr::arrange(group) %>%
  dplyr::transmute(
    Group = as.character(group),
    AmountN = format(n, big.mark = ",", trim = TRUE),
    MedianCost = fmt_money_ci(median, median_ci_low, median_ci_high),
    ZeroCash = fmt_pct_ci(zero_cash_pct, zero_cash_ci_low, zero_cash_ci_high),
    MeanCash = fmt_money_ci(cash_mean_all, cash_mean_all_low, cash_mean_all_high),
    PayerN = format(payer_n, big.mark = ",", trim = TRUE),
    InsurerMet = zero_safe_pct(insurer_met_any, insurer_ci_low, insurer_ci_high)
  )

table_cost_header_labels <- list(
  Group = "Group",
  AmountN = "n with amount",
  MedianCost = "Median total cost, KSh (95% CI)",
  ZeroCash = "Paid nothing in cash, % (95% CI)",
  MeanCash = "Mean cash paid, KSh (95% CI)",
  PayerN = "n with payer split",
  InsurerMet = "Insurer met any part, % (95% CI)"
)

save_table_bundle(
  table_cost,
  file.path(paths$tables_dir, "Table5_Outpatient_Cost_And_Payer.csv"),
  file.path(paths$tables_dir, "Table5_Outpatient_Cost_And_Payer.docx"),
  "Table 5. Amount paid at the last outpatient visit and the source of that payment, among adults who used outpatient care in the previous four weeks and reported paying, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates; amounts are Kenyan shillings at the time of the survey. The two n columns are the unweighted denominators for the columns to their right.",
    "Restricted to respondents who reported paying, which is the questionnaire's own skip pattern for these items: no amount or payer is recorded for a visit at which nothing was paid. These estimates therefore describe payments that were made, and do not describe all outpatient contacts.",
    "Total cost is sh304. Cash is sh305a, the part met out of pocket; the mean cash figure retains respondents who paid nothing in cash, and the preceding column gives their share. Insurer contribution is any non-zero amount met by NHIF or private insurance (sh305b, sh305c).",
    "KDHS does not collect household consumption, so catastrophic health expenditure cannot be constructed from these data."
  ),
  header_labels = table_cost_header_labels
)

# --------------------------------------------------------------------------
# Supplementary tables: participant flow, missingness, severity contrasts.
# --------------------------------------------------------------------------

# Each retention percentage is taken against the denominator carried alongside
# the row in sample_flow. It is deliberately not recovered from the step label:
# the CSV round trip strips the leading spaces that mark nesting.
table_s1 <- sample_flow %>%
  dplyr::transmute(
    Step = paste0(strrep("  ", dplyr::coalesce(indent, 0L)), step),
    N = dplyr::if_else(is.na(n), "", format(n, big.mark = ",", trim = TRUE)),
    Retained = dplyr::if_else(
      !is.na(n) & !is.na(reference_n) & reference_n > 0,
      sprintf("%.1f%%", 100 * n / reference_n),
      ""
    )
  )

save_table_bundle(
  table_s1,
  file.path(paths$tables_dir, "TableS1_Participant_Flow.csv"),
  file.path(paths$tables_dir, "TableS1_Participant_Flow.docx"),
  "Table S1. Participant flow from the KDHS 2022 person recode file to the ST02 analytic base and to each outcome-specific denominator.",
  footer_lines = c(
    "Source: Kenya DHS 2022, person recode file. Unweighted counts.",
    "Indented rows are subsets of the row above them, and the percentage is of that row.",
    "The item-level rows count the survey questions as asked. The payment analysis populations additionally exclude records with missing insurance status, because insurance is a term in each of those models, so they are smaller than the corresponding item-level rows."
  ),
  header_labels = list(Step = "Step", N = "n", Retained = "% of preceding population")
)

table_s2 <- missingness %>%
  dplyr::transmute(
    Variable = variable,
    Denominator = format(denominator, big.mark = ",", trim = TRUE),
    Missing = format(missing_n, big.mark = ",", trim = TRUE),
    Percent = sprintf("%.2f", missing_pct)
  )

save_table_bundle(
  table_s2,
  file.path(paths$tables_dir, "TableS2_Missing_Data.csv"),
  file.path(paths$tables_dir, "TableS2_Missing_Data.docx"),
  "Table S2. Missing values for each outcome and adjustment variable within the ST02 analytic base, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Unweighted counts within the analytic base of adults in long-questionnaire households with complete WG-SS classification.",
    "Analyses are complete-case within each model; the excluded and included groups are compared in the text."
  ),
  header_labels = list(
    Variable = "Variable",
    Denominator = "Denominator",
    Missing = "Missing n",
    Percent = "Missing %"
  )
)

severity_spec_labels <- c(
  crude = "Crude",
  confounder = "Adjusted for sex, age group, residence",
  full = "Additionally adjusted for wealth, education"
)

table_s3 <- extended$severity_contrasts %>%
  dplyr::mutate(specification = factor(specification, levels = names(severity_spec_labels))) %>%
  dplyr::arrange(specification) %>%
  dplyr::transmute(
    Specification = severity_spec_labels[as.character(specification)],
    Contrast = contrast,
    PR = fmt_apr_ci_p(pr, ci_low, ci_high, p_value)
  )

save_table_bundle(
  table_s3,
  file.path(paths$tables_dir, "TableS3_Severity_Contrasts.csv"),
  file.path(paths$tables_dir, "TableS3_Severity_Contrasts.docx"),
  "Table S3. Prespecified contrasts between adjacent Washington Group severity categories for uninsured status, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted quasi-Poisson models with a log link; contrasts estimated on the linear predictor and exponentiated.",
    paste0(
      "Design-based global test of severity: crude F = ",
      sprintf("%.2f", extended$severity_global_tests$statistic[extended$severity_global_tests$specification == "crude"]),
      " (p ", fmt_pvalue(extended$severity_global_tests$p_value[extended$severity_global_tests$specification == "crude"]),
      "); confounder-adjusted F = ",
      sprintf("%.2f", extended$severity_global_tests$statistic[extended$severity_global_tests$specification == "confounder"]),
      " (p ", fmt_pvalue(extended$severity_global_tests$p_value[extended$severity_global_tests$specification == "confounder"]),
      "); fully adjusted F = ",
      sprintf("%.2f", extended$severity_global_tests$statistic[extended$severity_global_tests$specification == "full"]),
      " (p = ", sprintf("%.2f", extended$severity_global_tests$p_value[extended$severity_global_tests$specification == "full"]),
      ")."
    )
  ),
  header_labels = list(
    Specification = "Specification",
    Contrast = "Contrast",
    PR = "Prevalence ratio (95% CI); p"
  )
)

# --------------------------------------------------------------------------
# Table S4. Bounds on insurer contribution among all insured outpatient users.
#
# The recorded proportion is conditional on having paid and on having a payer
# split. Encounters at which nothing was paid carry no payer information and
# could in principle all have been met by an insurer, or none of them. Those two
# assumptions bound the quantity the policy question asks about; nothing between
# them is identified by these data.
# --------------------------------------------------------------------------

table_s4 <- extended$insurer_bounds %>%
  dplyr::transmute(
    Population = population,
    Users = format(users_n, big.mark = ",", trim = TRUE),
    NoPayment = format(no_payment_n, big.mark = ",", trim = TRUE),
    SplitMissing = format(payer_split_missing_n, big.mark = ",", trim = TRUE),
    Recorded = sprintf("%s/%s (%.1f%%)",
                       format(insurer_recorded_n, big.mark = ",", trim = TRUE),
                       format(payers_with_split_n, big.mark = ",", trim = TRUE),
                       100 * insurer_recorded_n / payers_with_split_n),
    Bounds = sprintf("%.1f%% to %.1f%%", 100 * lower_bound, 100 * upper_bound),
    BoundsCI = sprintf("%.1f%% to %.1f%%; %.1f%% to %.1f%%",
                       100 * lower_bound_ci_low, 100 * lower_bound_ci_high,
                       100 * upper_bound_ci_low, 100 * upper_bound_ci_high)
  )

save_table_bundle(
  table_s4,
  file.path(paths$tables_dir, "TableS4_Insurer_Contribution_Bounds.csv"),
  file.path(paths$tables_dir, "TableS4_Insurer_Contribution_Bounds.docx"),
  "Table S4. Bounds on the proportion of insured outpatient contacts at which an insurer met any part of the cost, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Counts are unweighted; both bounds are survey-weighted proportions estimated on the parent design with the DHS person weights.",
    "Lower bound: only contacts with a recorded insurer contribution are counted, so every contact at which nothing was paid and every contact with a missing payer split is treated as having no insurer contribution.",
    "Upper bound: every contact at which nothing was paid and every contact with a missing payer split is treated as having been met by an insurer.",
    "The payer items are not asked of respondents who reported no payment, so no estimate between these bounds is identified by these data.",
    "The final column gives design-based 95% limits for the lower and upper endpoint separately. They describe sampling uncertainty in each endpoint and are not a confidence interval for the partially identified quantity."
  ),
  header_labels = list(
    Population = "Population",
    Users = "Outpatient users",
    NoPayment = "Reported no payment",
    SplitMissing = "Payer split missing",
    Recorded = "Insurer contribution recorded",
    Bounds = "Bounds on insurer contribution (survey weighted)",
    BoundsCI = "95% limits for each endpoint"
  )
)

# --------------------------------------------------------------------------
# Table S5. Two-part analysis of the cash payment.
# --------------------------------------------------------------------------

two_part_terms <- c(wg_disability = "WG disability threshold", insured_any = "Any insurance")

table_s5 <- dplyr::bind_rows(
  extended$cash_part1 %>%
    dplyr::filter(term %in% names(two_part_terms)) %>%
    dplyr::transmute(
      Part = paste0("Any cash paid (n = ", format(extended$cash_part1_n, big.mark = ","), ")"),
      Term = two_part_terms[term],
      Estimate = fmt_apr_ci_p(apr, ci_low, ci_high, p.value),
      Scale = "Prevalence ratio"
    ),
  extended$cash_part2 %>%
    dplyr::filter(term %in% names(two_part_terms)) %>%
    dplyr::transmute(
      Part = paste0("Amount, given cash paid (n = ", format(extended$cash_part2_n, big.mark = ","), ")"),
      Term = two_part_terms[term],
      Estimate = fmt_apr_ci_p(ratio, ci_low, ci_high, p.value),
      Scale = "Ratio of geometric means"
    )
)

save_table_bundle(
  table_s5,
  file.path(paths$tables_dir, "TableS5_Two_Part_Cash_Model.csv"),
  file.path(paths$tables_dir, "TableS5_Two_Part_Cash_Model.docx"),
  "Table S5. Two-part analysis of the cash payment at the last outpatient visit, among adults who reported paying, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted models adjusted for sex, age group, residence, wealth quintile and educational attainment; confidence limits use the survey degrees of freedom.",
    "Part one is the probability of paying anything in cash, which is where insurance appears as a zero. Part two is the amount among those who paid something in cash, and is conditional on that.",
    "Reporting part two alone would exclude the zero-cash records, which are concentrated among insured respondents."
  ),
  header_labels = list(
    Part = "Part",
    Term = "Term",
    Estimate = "Estimate (95% CI); p",
    Scale = "Scale"
  )
)

# --------------------------------------------------------------------------
# Table S6. Delta-method and replicate-weight limits for the standardised
# differences, so the model-based interval can be checked against one that also
# carries the uncertainty in the covariate distribution.
# --------------------------------------------------------------------------

table_s6 <- extended$whole_sample_contrasts %>%
  dplyr::mutate(
    outcome_label = dplyr::recode(
      outcome,
      uninsured = "Uninsured",
      outpatient_last4w = "Outpatient use",
      inpatient_last12m = "Hospitalisation"
    ),
    specification_label = spec_labels[specification]
  ) %>%
  dplyr::transmute(
    Outcome = outcome_label,
    Specification = stringr::str_squish(specification_label),
    Delta = fmt_pp_ci(standardised_difference, standardised_difference_ci_low, standardised_difference_ci_high),
    Replicate = fmt_pp_ci(replicate_difference, replicate_difference_ci_low, replicate_difference_ci_high)
  )

# --------------------------------------------------------------------------
# Table S7. Long- against short-questionnaire adults on the characteristics
# collected in both halves. This is the check on whether the half-sample the
# modules were administered in can stand in for the adult population.
# --------------------------------------------------------------------------

variable_labels_s7 <- c(
  sex = "Sex", age_group = "Age group", residence = "Residence",
  wealth = "Wealth quintile", education = "Education"
)

table_s7 <- extended$questionnaire_half_comparison %>%
  dplyr::mutate(
    Characteristic = paste0("  ", level),
    Long = sprintf("%.1f", long_pct),
    Short = sprintf("%.1f", short_pct),
    Difference = sprintf("%+.1f", difference_pp),
    P = fmt_pvalue(p_value),
    .group = unname(variable_labels_s7[variable])
  ) %>%
  dplyr::group_by(.group) %>%
  dplyr::group_modify(~ dplyr::bind_rows(
    tibble::tibble(Characteristic = .y$.group, Long = "", Short = "", Difference = "", P = .x$P[1]),
    .x %>% dplyr::select(Characteristic, Long, Short, Difference) %>% dplyr::mutate(P = "")
  )) %>%
  dplyr::ungroup() %>%
  dplyr::select(Characteristic, Long, Short, Difference, P)

save_table_bundle(
  table_s7,
  file.path(paths$tables_dir, "TableS7_Questionnaire_Half_Comparison.csv"),
  file.path(paths$tables_dir, "TableS7_Questionnaire_Half_Comparison.docx"),
  "Table S7. Survey-weighted composition of long-questionnaire and short-questionnaire adults on the characteristics recorded in both halves, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted column percentages estimated on the parent design; n is unweighted.",
    "The disability, insurance, utilisation and payment modules were administered only in long-questionnaire households, so every estimate in this study is conditional on that half. These five characteristics are the ones recorded in both halves.",
    "P values are design-based Rao-Scott F tests of independence between questionnaire half and the characteristic, reported once per characteristic.",
    "Agreement here does not establish that the two halves would agree on the module variables, which are unobserved in the short half."
  ),
  header_labels = list(
    Characteristic = "Characteristic",
    Long = "Long questionnaire, %",
    Short = "Short questionnaire, %",
    Difference = "Difference, pp",
    P = "p"
  )
)

save_table_bundle(
  table_s6,
  file.path(paths$tables_dir, "TableS6_Standardisation_Method_Comparison.csv"),
  file.path(paths$tables_dir, "TableS6_Standardisation_Method_Comparison.docx"),
  "Table S6. Standardised prevalence differences in percentage points under delta-method and bootstrap replicate-weight variance estimation, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Point estimates are identical by construction; only the variance estimator differs.",
    "Delta-method limits propagate uncertainty in the fitted coefficients and treat the weighted covariate distribution as fixed. Replicate-weight limits refit the model in 500 subbootstrap replicates constructed from the parent design, with the analytic domain applied after the replicate weights are formed, and carry both components."
  ),
  header_labels = list(
    Outcome = "Outcome",
    Specification = "Specification",
    Delta = "Delta-method difference (95% CI)",
    Replicate = "Replicate-weight difference (95% CI)"
  )
)

figure1_data <- insurance_base %>%
  dplyr::filter(wg_severity %in% c(
    "No functional difficulty",
    "Mild functional difficulty",
    "Moderate functional difficulty",
    "Severe functional difficulty"
  )) %>%
  dplyr::select(wg_severity, insured_any, insured_nhif, psu, strata, weight) %>%
  tidyr::pivot_longer(
    cols = c(insured_any, insured_nhif),
    names_to = "metric",
    values_to = "value"
  ) %>%
  dplyr::group_by(metric, wg_severity) %>%
  dplyr::group_modify(~{
    stat <- weighted_binary(.x, "value")
    tibble::tibble(
      n = stat$unweighted_n,
      est = stat$est,
      ci_low = stat$ci_low,
      ci_high = stat$ci_high
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    metric = dplyr::recode(metric, insured_any = "Any insurance", insured_nhif = "NHIF coverage"),
    wg_severity = factor(
      wg_severity,
      levels = c(
        "No functional difficulty",
        "Mild functional difficulty",
        "Moderate functional difficulty",
        "Severe functional difficulty"
      )
    )
  )

figure1 <- ggplot2::ggplot(
  figure1_data,
  ggplot2::aes(x = wg_severity, y = 100 * est, fill = metric)
) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.76), width = 0.66) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = 100 * ci_low, ymax = 100 * ci_high),
    position = ggplot2::position_dodge(width = 0.76),
    width = 0.16
  ) +
  ggplot2::scale_fill_manual(values = c("Any insurance" = "#0f6e8c", "NHIF coverage" = "#5c7cfa")) +
  ggplot2::scale_y_continuous(limits = c(0, 40), expand = ggplot2::expansion(mult = c(0, 0.04))) +
  ggplot2::geom_text(
    ggplot2::aes(y = 100 * ci_high, label = paste0("n=", format(n, big.mark = ","))),
    position = ggplot2::position_dodge(width = 0.72),
    vjust = -0.6, size = 2.5, colour = "grey30"
  ) +
  ggplot2::labs(
    title = "Insurance coverage by disability severity, Kenyan adults aged 18 and above",
    subtitle = "Kenya DHS 2022, survey-weighted prevalence with 95% confidence intervals",
    x = NULL,
    y = "Weighted prevalence (%)",
    fill = NULL,
    caption = paste(
      "Disability measured with the Washington Group Short Set. Unadjusted prevalences; the",
      "severity steps are tested formally in
Table S3, where the step from mild to moderate",
      "difficulty is the one that reaches significance before adjustment for wealth and",
      "education
and is null after it. NHIF tracks any insurance at every severity level, so other",
      "schemes did not offset the gradient.
Intervals are logit-transformed; n is the unweighted denominator."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "top",
    axis.text.x = ggplot2::element_text(angle = 12, hjust = 1),
    plot.title = ggplot2::element_text(face = "bold", size = 11.5),
    plot.subtitle = ggplot2::element_text(size = 9),
    plot.caption = ggplot2::element_text(size = 7.5, colour = "grey30", hjust = 0)
  )

ggplot2::ggsave(
  file.path(paths$figures_dir, "Insurance_By_Disability_Severity.png"),
  figure1,
  width = 9.5,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(paths$figures_dir, "Insurance_By_Disability_Severity.tiff"),
  figure1,
  width = 9.5,
  height = 5,
  dpi = 300,
  compression = "lzw"
)

# The severe-and-insured cell has 19 respondents. It is kept because it is the
# only direct observation of the group a severe-disability exemption would cover,
# but it is marked on the figure so that it cannot be read as a stable estimate.
unstable_cell_threshold <- 30

figure_payment_data <- payment_disabled_base %>%
  dplyr::filter(wg_severity %in% c("Moderate functional difficulty", "Severe functional difficulty")) %>%
  dplyr::group_by(wg_severity, insured_any) %>%
  dplyr::group_modify(~{
    stat <- weighted_binary(.x, "paid_outpatient_recent")
    tibble::tibble(
      n = stat$unweighted_n,
      est = stat$est,
      ci_low = stat$ci_low,
      ci_high = stat$ci_high
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    insurance = dplyr::if_else(insured_any == 1, "Insured", "Uninsured"),
    unstable = n < unstable_cell_threshold,
    label = dplyr::if_else(unstable, paste0("n=", n, " (unstable)"), paste0("n=", n)),
    wg_severity = factor(
      wg_severity,
      levels = c("Moderate functional difficulty", "Severe functional difficulty")
    )
  )

figure_payment <- ggplot2::ggplot(
  figure_payment_data,
  ggplot2::aes(x = wg_severity, y = 100 * est, fill = insurance)
) +
  ggplot2::geom_col(
    ggplot2::aes(alpha = unstable, group = insurance),
    position = ggplot2::position_dodge(width = 0.72), width = 0.62
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = 100 * ci_low, ymax = 100 * ci_high, group = insurance),
    position = ggplot2::position_dodge(width = 0.72),
    width = 0.14
  ) +
  ggplot2::scale_fill_manual(values = c("Insured" = "#0f6e8c", "Uninsured" = "#cf5c36")) +
  ggplot2::scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.45), guide = "none") +
  ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::geom_text(
    ggplot2::aes(y = 100 * ci_high, label = label, group = insurance),
    position = ggplot2::position_dodge(width = 0.72),
    vjust = -0.6, size = 2.5, colour = "grey30"
  ) +
  ggplot2::labs(
    title = "Payment at the last outpatient visit, by disability severity and insurance",
    subtitle = "Kenya DHS 2022, adults with disability who used outpatient care in the previous four weeks",
    x = NULL,
    y = "Paid at last outpatient visit (%)",
    fill = NULL,
    caption = paste(
      "Unadjusted proportions paying any amount. The severe-and-insured cell is shaded because it",
      "rests on fewer than 30
respondents and cannot support a conclusion on its own. Whether any",
      "money was paid is a coarse measure of financial protection;
the amount paid and who met it",
      "are in Table 5. Intervals are logit-transformed and bounded at 100%; n is unweighted."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold", size = 11.5),
    plot.subtitle = ggplot2::element_text(size = 9),
    plot.caption = ggplot2::element_text(size = 7.5, colour = "grey30", hjust = 0)
  )

ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure2_Outpatient_Payment_By_Severity_Insurance.png"),
  figure_payment,
  width = 9,
  height = 5.6,
  dpi = 300
)
ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure2_Outpatient_Payment_By_Severity_Insurance.tiff"),
  figure_payment,
  width = 9,
  height = 5.6,
  dpi = 300,
  compression = "lzw"
)

# Figure 1. What happens to the disability contrast as the adjustment set grows.
# The point of the figure is that the insurance contrast and the utilisation
# contrasts behave differently: utilisation survives every specification, while
# the coverage contrast is absorbed once wealth and education enter.
figure_forest_data <- extended$whole_sample_contrasts %>%
  dplyr::mutate(
    outcome_label = dplyr::recode(
      outcome,
      uninsured = "Uninsured",
      outpatient_last4w = "Outpatient use, 4 weeks",
      inpatient_last12m = "Hospitalisation, 12 months"
    ),
    outcome_label = factor(
      outcome_label,
      levels = c("Uninsured", "Outpatient use, 4 weeks", "Hospitalisation, 12 months")
    ),
    specification_label = dplyr::recode(
      specification,
      crude = "Crude",
      confounder = "+ sex, age, residence",
      full = "+ wealth, education"
    ),
    specification_label = factor(
      specification_label,
      levels = rev(c("Crude", "+ sex, age, residence", "+ wealth, education"))
    )
  )

figure_forest <- ggplot2::ggplot(
  figure_forest_data,
  ggplot2::aes(x = apr, y = specification_label, colour = outcome_label)
) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = apr_ci_low, xmax = apr_ci_high),
    height = 0.16, linewidth = 0.6
  ) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f (%.2f, %.2f)", apr, apr_ci_low, apr_ci_high)),
    vjust = -1.1, size = 2.6, show.legend = FALSE
  ) +
  ggplot2::facet_wrap(~outcome_label, ncol = 1, scales = "free_x") +
  ggplot2::scale_colour_manual(
    values = c(
      "Uninsured" = "#0f6e8c",
      "Outpatient use, 4 weeks" = "#1b7f5f",
      "Hospitalisation, 12 months" = "#8c4a0f"
    ),
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Disability contrast by adjustment set, Kenyan adults aged 18 and above",
    subtitle = "Kenya DHS 2022. Prevalence ratios for adults at the WG threshold versus adults below it",
    x = "Prevalence ratio (log scale)",
    y = NULL,
    caption = paste(
      "Wealth and education are plausibly consequences of lifelong functional difficulty rather than",
      "confounders of it, so the
bottom row of each panel estimates an association net of that",
      "pathway. The coverage contrast is absorbed by it; the two
utilisation contrasts are not."
    )
  ) +
  ggplot2::scale_x_log10() +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 9.5),
    plot.title = ggplot2::element_text(face = "bold", size = 11.5),
    plot.subtitle = ggplot2::element_text(size = 9),
    plot.caption = ggplot2::element_text(size = 7.5, colour = "grey30", hjust = 0),
    panel.spacing = ggplot2::unit(0.9, "lines")
  )

ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure1_Disability_Contrast_By_Adjustment.png"),
  figure_forest,
  width = 8.4,
  height = 6.4,
  dpi = 300
)
ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure1_Disability_Contrast_By_Adjustment.tiff"),
  figure_forest,
  width = 8.4,
  height = 6.4,
  dpi = 300,
  compression = "lzw"
)

disabled_insurance <- insurance_base %>%
  dplyr::filter(wg_disability == 1)

nondisabled_insurance <- insurance_base %>%
  dplyr::filter(wg_disability == 0)

disabled_outpatient <- outpatient_base %>%
  dplyr::filter(wg_disability == 1)

nondisabled_outpatient <- outpatient_base %>%
  dplyr::filter(wg_disability == 0)

disabled_payment_insured <- payment_disabled_base %>%
  dplyr::filter(insured_any == 1)

disabled_payment_uninsured <- payment_disabled_base %>%
  dplyr::filter(insured_any == 0)

domain_threshold_prevalence <- purrr::map_dfr(seq_len(nrow(domain_specs)), function(i) {
  data_use <- analysis_base %>%
    dplyr::filter(!is.na(.data[[domain_specs$var_name[[i]]]]))
  stat <- weighted_binary(data_use, domain_specs$var_name[[i]])
  tibble::tibble(
    domain = stringr::str_squish(domain_specs$label[[i]]),
    est = stat$est,
    ci_low = stat$ci_low,
    ci_high = stat$ci_high
  )
})

key_results <- list(
  sample_long_questionnaire = nrow(analysis_base),
  sample_households_long_questionnaire = analysis_base %>% dplyr::distinct(cluster, household) %>% nrow(),
  any_functional_difficulty = weighted_binary(analysis_base, "any_functional_difficulty"),
  wg_disability = weighted_binary(analysis_base, "wg_disability"),
  no_disability_insurance = weighted_binary(nondisabled_insurance, "insured_any"),
  disability_insurance = weighted_binary(disabled_insurance, "insured_any"),
  no_disability_nhif = weighted_binary(nondisabled_insurance, "insured_nhif"),
  disability_nhif = weighted_binary(disabled_insurance, "insured_nhif"),
  no_disability_outpatient = weighted_binary(nondisabled_outpatient, "outpatient_last4w"),
  disability_outpatient = weighted_binary(disabled_outpatient, "outpatient_last4w"),
  no_disability_inpatient = weighted_binary(inpatient_base %>% dplyr::filter(wg_disability == 0), "inpatient_last12m"),
  disability_inpatient = weighted_binary(inpatient_base %>% dplyr::filter(wg_disability == 1), "inpatient_last12m"),
  no_disability_payment = weighted_binary(payment_base %>% dplyr::filter(wg_disability == 0), "paid_outpatient_recent"),
  disability_payment = weighted_binary(payment_base %>% dplyr::filter(wg_disability == 1), "paid_outpatient_recent"),
  disability_payment_insured = weighted_binary(disabled_payment_insured, "paid_outpatient_recent"),
  disability_payment_uninsured = weighted_binary(disabled_payment_uninsured, "paid_outpatient_recent"),
  payment_gap_disabled = weighted_binary(disabled_payment_uninsured, "paid_outpatient_recent")$est -
    weighted_binary(disabled_payment_insured, "paid_outpatient_recent")$est,
  severity_insurance = figure1_data,
  severity_coverage_raw = table2_severity,
  domain_coverage_raw = domain_coverage_raw,
  domain_insurance = table2_domains,
  domain_threshold_prevalence = domain_threshold_prevalence,
  uninsured_model_n = analysis_base %>%
    dplyr::filter(
      wg_disability == 1,
      !is.na(uninsured),
      !is.na(sex),
      !is.na(age_group),
      !is.na(wealth),
      !is.na(residence),
      !is.na(education),
      !is.na(wg_severity)
    ) %>%
    nrow(),
  payment_model_n = payment_disabled_base %>%
    dplyr::filter(
      !is.na(insured_any),
      !is.na(sex),
      !is.na(age_group),
      !is.na(wealth),
      !is.na(residence),
      !is.na(education),
      !is.na(wg_severity)
    ) %>%
    nrow(),
  model_uninsured = model_uninsured,
  model_payment = model_payment,

  # Whole-sample disability contrasts, the comparison the study is named for.
  contrast = function(outcome_name, spec_name) {
    extended$whole_sample_contrasts %>%
      dplyr::filter(outcome == outcome_name, specification == spec_name)
  },
  whole_sample_contrasts = extended$whole_sample_contrasts,
  severity_contrasts = extended$severity_contrasts,
  severity_global_tests = extended$severity_global_tests,
  domain_burden = extended$domain_burden,
  domain_burden_global = extended$domain_burden_global,
  design_metadata = extended$design_metadata,
  cost_group_summary = extended$cost_group_summary,
  payer_source_summary = extended$payer_source_summary,
  insurer_realisation = extended$insurer_realisation,
  cost_model = extended$cost_model,
  cost_model_n = extended$cost_model_n,
  cash_model = extended$cash_model,
  cash_model_n = extended$cash_model_n,
  model_payment_whole = extended$model_payment_whole,
  payment_outcome_counts = extended$payment_outcome_counts,
  sensitivity_dk = extended$sensitivity_dk,
  inclusion_comparison = extended$inclusion_comparison,
  insurer_bounds = extended$insurer_bounds,
  zero_cash_summary = extended$zero_cash_summary,
  cash_marginal = extended$cash_marginal,
  cash_part1 = extended$cash_part1,
  cash_part1_n = extended$cash_part1_n,
  cash_part2 = extended$cash_part2,
  cash_part2_n = extended$cash_part2_n,
  cost_model_consistent = extended$cost_model_consistent,
  population_counts = extended$population_counts,
  questionnaire_half_comparison = extended$questionnaire_half_comparison,
  questionnaire_half_max_difference = extended$questionnaire_half_max_difference,
  amount_quality = readr::read_csv(file.path(paths$logs_dir, "st02_amount_data_quality.csv"), show_col_types = FALSE),
  dk_n = sum(analysis_base$insurance_response == "don't know", na.rm = TRUE),
  sample_flow = sample_flow,
  missingness = missingness,
  cost_payers_n = sum(extended$cost_group_summary$n),
  payer_source_n = sum(extended$payer_source_summary$n)
)

# Numbers follow the order in which the tables appear in the manuscript.
analysis_object <- list(
  table1 = table1,
  table2 = table2,
  table3 = table_contrasts,
  table4 = table_use,
  table5 = table_cost,
  table6 = table_within,
  table_s1 = table_s1,
  table_s7 = table_s7,
  table_s2 = table_s2,
  table_s3 = table_s3,
  table_s4 = table_s4,
  table_s5 = table_s5,
  table_s6 = table_s6,
  figure1_data = figure1_data,
  figure2_data = figure_forest_data,
  figure3_data = figure_payment_data,
  key_results = key_results,
  table_specs = list(
    table1 = list(
      header_labels = list(
        Characteristic = "Characteristic",
        Overall_n = "n",
        Overall_stat = "Estimate (95% CI)",
        NoWG_n = "n",
        NoWG_stat = "Estimate (95% CI)",
        WG_n = "n",
        WG_stat = "Estimate (95% CI)"
      ),
      spanner_values = c("", "Overall", "No WG disability", "WG disability threshold"),
      spanner_widths = c(1, 2, 2, 2)
    ),
    table2 = list(
      header_labels = list(
        Characteristic = "Characteristic",
        n = "n",
        AnyInsurance = "Any insurance % (95% CI)",
        NHIF = "NHIF coverage % (95% CI)"
      )
    ),
    table3 = list(
      header_labels = list(
        Characteristic = "Outcome and specification",
        APR = "Prevalence ratio (95% CI); p",
        StandardisedPrevalence = "Standardised prevalence, % (disability vs no disability)",
        StandardisedDifference = "Standardised difference, percentage points (95% CI)"
      )
    ),
    table4 = list(header_labels = table_use_header_labels),
    table6 = list(header_labels = table_within_header_labels),
    table5 = list(header_labels = table_cost_header_labels),
    table_s1 = list(header_labels = list(Step = "Step", N = "n", Retained = "% of preceding population")),
    table_s7 = list(header_labels = list(Characteristic = "Characteristic", Long = "Long questionnaire, %", Short = "Short questionnaire, %", Difference = "Difference, pp", P = "p")),
    table_s2 = list(
      header_labels = list(
        Variable = "Variable",
        Denominator = "Denominator",
        Missing = "Missing n",
        Percent = "Missing %"
      )
    ),
    table_s3 = list(
      header_labels = list(
        Specification = "Specification",
        Contrast = "Contrast",
        PR = "Prevalence ratio (95% CI); p"
      )
    ),
    table_s4 = list(
      header_labels = list(
        Population = "Population",
        Users = "Outpatient users",
        NoPayment = "Reported no payment",
        SplitMissing = "Payer split missing",
        Recorded = "Insurer contribution recorded",
        Bounds = "Bounds on insurer contribution"
      )
    ),
    table_s5 = list(
      header_labels = list(
        Part = "Part",
        Term = "Term",
        Estimate = "Estimate (95% CI); p",
        Scale = "Scale"
      )
    ),
    table_s6 = list(
      header_labels = list(
        Outcome = "Outcome",
        Specification = "Specification",
        Delta = "Delta-method difference (95% CI)",
        Replicate = "Replicate-weight difference (95% CI)"
      )
    )
  )
)

save_rds_output(analysis_object, "st02_analysis_outputs.rds")

append_log("ST02 manuscript-facing tables, figures, and summary outputs saved.")

message("=== SECTION 4 COMPLETE ===")
