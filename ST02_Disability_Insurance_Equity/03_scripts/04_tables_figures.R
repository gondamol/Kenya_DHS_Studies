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
    "Standardised differences are marginal (g-computation) contrasts in percentage points over the covariate distribution of the analytic sample, with delta-method confidence limits.",
    "Wealth quintile and educational attainment are plausibly downstream of lifelong functional difficulty. The third row of each block therefore estimates an association net of that pathway rather than a more completely confounder-adjusted one."
  ),
  header_labels = list(
    Characteristic = "Outcome and specification",
    APR = "Prevalence ratio (95% CI); p",
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

cost_group_order <- c(
  "No WG disability, uninsured",
  "No WG disability, insured",
  "WG disability, uninsured",
  "WG disability, insured"
)

table_cost <- extended$cost_group_summary %>%
  dplyr::left_join(
    extended$payer_source_summary %>%
      dplyr::select(group, payer_n = n, insurer_met_any, insurer_ci_low, insurer_ci_high,
                    cash_share, cash_share_ci_low, cash_share_ci_high),
    by = "group"
  ) %>%
  dplyr::mutate(group = factor(group, levels = cost_group_order)) %>%
  dplyr::arrange(group) %>%
  dplyr::transmute(
    Group = as.character(group),
    N = format(n, big.mark = ",", trim = TRUE),
    MedianCost = sprintf("%s (%s, %s)",
                         format(round(median), big.mark = ",", trim = TRUE),
                         format(round(median_ci_low), big.mark = ",", trim = TRUE),
                         format(round(median_ci_high), big.mark = ",", trim = TRUE)),
    MedianCash = sprintf("%s (%s, %s)",
                         format(round(cash_median), big.mark = ",", trim = TRUE),
                         format(round(cash_median_ci_low), big.mark = ",", trim = TRUE),
                         format(round(cash_median_ci_high), big.mark = ",", trim = TRUE)),
    MeanCost = sprintf("%s (%s, %s)",
                       format(round(mean), big.mark = ",", trim = TRUE),
                       format(round(mean_ci_low), big.mark = ",", trim = TRUE),
                       format(round(mean_ci_high), big.mark = ",", trim = TRUE)),
    InsurerMet = zero_safe_pct(insurer_met_any, insurer_ci_low, insurer_ci_high),
    CashShare = fmt_pct_ci(cash_share, cash_share_ci_low, cash_share_ci_high)
  )

save_table_bundle(
  table_cost,
  file.path(paths$tables_dir, "Table5_Outpatient_Cost_And_Payer.csv"),
  file.path(paths$tables_dir, "Table5_Outpatient_Cost_And_Payer.docx"),
  "Table 5. Amount paid at the last outpatient visit and the source of that payment, among adults who used outpatient care in the previous four weeks and reported paying, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates; amounts are Kenyan shillings at the time of the survey.",
    "Restricted to respondents who reported paying, which is the questionnaire's own skip pattern for these items: KDHS does not record an amount for a visit at which nothing was paid. Estimates therefore describe the size and source of a payment among those who made one.",
    "The amount is the gross cost of the visit (sh304); the cash figure is the part of it met out of pocket (sh305a), which is the net payment after any insurer contribution.",
    "Insurer contribution is any non-zero amount met by NHIF or private insurance. Cash share is the mean proportion of the reported amounts met in cash.",
    "KDHS does not collect household consumption, so catastrophic health expenditure cannot be constructed from these data."
  ),
  header_labels = list(
    Group = "Group",
    N = "n",
    MedianCost = "Median amount, KSh (95% CI)",
    MedianCash = "Median paid in cash, KSh (95% CI)",
    MeanCost = "Mean amount, KSh (95% CI)",
    InsurerMet = "Insurer met any part % (95% CI)",
    CashShare = "Mean cash share of amount % (95% CI)"
  )
)

# --------------------------------------------------------------------------
# Supplementary tables: participant flow, missingness, severity contrasts.
# --------------------------------------------------------------------------

table_s1 <- sample_flow %>%
  dplyr::transmute(Step = step, N = format(n, big.mark = ",", trim = TRUE))

save_table_bundle(
  table_s1,
  file.path(paths$tables_dir, "TableS1_Participant_Flow.csv"),
  file.path(paths$tables_dir, "TableS1_Participant_Flow.docx"),
  "Table S1. Participant flow from the KDHS 2022 person recode file to the ST02 analytic base and to each outcome-specific denominator.",
  footer_lines = c(
    "Source: Kenya DHS 2022, person recode file. Unweighted counts.",
    "Indented rows are subsets of the row above them."
  ),
  header_labels = list(Step = "Step", N = "n")
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
  file.path(paths$figures_dir, "Figure1_Insurance_By_Disability_Severity.png"),
  figure1,
  width = 9.5,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure1_Insurance_By_Disability_Severity.tiff"),
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
  file.path(paths$figures_dir, "Figure3_Outpatient_Payment_By_Severity_Insurance.png"),
  figure_payment,
  width = 9,
  height = 5.6,
  dpi = 300
)
ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure3_Outpatient_Payment_By_Severity_Insurance.tiff"),
  figure_payment,
  width = 9,
  height = 5.6,
  dpi = 300,
  compression = "lzw"
)

# Figure 3. What happens to the disability contrast as the adjustment set grows.
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
  file.path(paths$figures_dir, "Figure2_Disability_Contrast_By_Adjustment.png"),
  figure_forest,
  width = 8.4,
  height = 6.4,
  dpi = 300
)
ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure2_Disability_Contrast_By_Adjustment.tiff"),
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
  table_s2 = table_s2,
  table_s3 = table_s3,
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
        StandardisedDifference = "Standardised difference, percentage points (95% CI)"
      )
    ),
    table4 = list(header_labels = table_use_header_labels),
    table6 = list(header_labels = table_within_header_labels),
    table5 = list(
      header_labels = list(
        Group = "Group",
        N = "n",
        MedianCost = "Median amount, KSh (95% CI)",
        MedianCash = "Median paid in cash, KSh (95% CI)",
        MeanCost = "Mean amount, KSh (95% CI)",
        InsurerMet = "Insurer met any part % (95% CI)",
        CashShare = "Mean cash share of amount % (95% CI)"
      )
    ),
    table_s1 = list(header_labels = list(Step = "Step", N = "n")),
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
    )
  )
)

save_rds_output(analysis_object, "st02_analysis_outputs.rds")

append_log("ST02 manuscript-facing tables, figures, and summary outputs saved.")

message("=== SECTION 4 COMPLETE ===")
