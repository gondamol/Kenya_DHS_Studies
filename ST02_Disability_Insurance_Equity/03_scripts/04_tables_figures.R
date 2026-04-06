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

    list(
      n = format(sum(!is.na(df[[var]])), big.mark = ","),
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
      n = "",
      Outpatient = "",
      Hospitalisation = "",
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

payment_disabled_by_insurance <- purrr::map_dfr(c(1, 0), function(insured_value) {
  data_use <- payment_disabled_base %>%
    dplyr::filter(insured_any == insured_value)

  tibble::tibble(
    AnalysisGroup = ifelse(insured_value == 1, "  Insured", "  Uninsured"),
    n = format(nrow(data_use), big.mark = ","),
    Outpatient = "",
    Hospitalisation = "",
    Payment = get_binary_stat_string(data_use, "paid_outpatient_recent")
  )
})

table3 <- dplyr::bind_rows(
  add_section_row("Disability status", "table3"),
  tibble::tibble(
    AnalysisGroup = "  No WG disability",
    n = format(nrow(table_groups$NoWG), big.mark = ","),
    Outpatient = get_binary_stat_string(table_groups$NoWG %>% dplyr::filter(!is.na(outpatient_last4w)), "outpatient_last4w"),
    Hospitalisation = get_binary_stat_string(table_groups$NoWG %>% dplyr::filter(!is.na(inpatient_last12m)), "inpatient_last12m"),
    Payment = get_binary_stat_string(payment_base %>% dplyr::filter(wg_disability == 0), "paid_outpatient_recent")
  ),
  tibble::tibble(
    AnalysisGroup = "  WG disability threshold",
    n = format(nrow(table_groups$WG), big.mark = ","),
    Outpatient = get_binary_stat_string(table_groups$WG %>% dplyr::filter(!is.na(outpatient_last4w)), "outpatient_last4w"),
    Hospitalisation = get_binary_stat_string(table_groups$WG %>% dplyr::filter(!is.na(inpatient_last12m)), "inpatient_last12m"),
    Payment = get_binary_stat_string(payment_base %>% dplyr::filter(wg_disability == 1), "paid_outpatient_recent")
  ),
  add_section_row("Insurance status among adults with disability who used outpatient care", "table3"),
  payment_disabled_by_insurance
)

table4_term_order <- c(
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

table4_labels <- tibble::tribble(
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

table4_body <- table4_labels %>%
  dplyr::left_join(model_uninsured_display, by = "term") %>%
  dplyr::left_join(model_payment_display, by = "term") %>%
  dplyr::mutate(
    UninsuredAPR = dplyr::coalesce(UninsuredAPR, ""),
    PaymentAPR = dplyr::coalesce(PaymentAPR, "")
  ) %>%
  dplyr::select(Characteristic = label, UninsuredAPR, PaymentAPR)

table4 <- dplyr::bind_rows(
  add_section_row("Insurance status", "table4"),
  table4_body %>% dplyr::slice(1),
  add_section_row("Sex", "table4"),
  table4_body %>% dplyr::slice(2),
  add_section_row("Age group (ref: 18-29)", "table4"),
  table4_body %>% dplyr::slice(3:5),
  add_section_row("Wealth quintile (ref: richest)", "table4"),
  table4_body %>% dplyr::slice(6:9),
  add_section_row("Place of residence", "table4"),
  table4_body %>% dplyr::slice(10),
  add_section_row("Educational attainment (ref: no education)", "table4"),
  table4_body %>% dplyr::slice(11:13),
  add_section_row("Disability severity", "table4"),
  table4_body %>% dplyr::slice(14)
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

save_table_bundle(
  table3,
  file.path(paths$tables_dir, "Table3_Service_Use_And_Payment.csv"),
  file.path(paths$tables_dir, "Table3_Service_Use_And_Payment.docx"),
  "Table 3. Weighted service utilisation and outpatient payment by disability status and insurance coverage, adults aged 18 years and above, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates.",
    "Payment estimates are restricted to recent outpatient users within each row group."
  ),
  header_labels = list(
    AnalysisGroup = "Analysis group",
    n = "n",
    Outpatient = "Outpatient use % (95% CI)",
    Hospitalisation = "Hospitalisation % (95% CI)",
    Payment = "Paid at last outpatient visit % (95% CI)"
  )
)

save_table_bundle(
  table4,
  file.path(paths$tables_dir, "Table4_Adjusted_Prevalence_Ratios.csv"),
  file.path(paths$tables_dir, "Table4_Adjusted_Prevalence_Ratios.docx"),
  "Table 4. Survey-weighted adjusted prevalence ratios for uninsured status and outpatient payment among adults meeting the Washington Group Short Set disability threshold, KDHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates.",
    "Uninsured model: adults meeting the WG disability threshold. Payment model: adults meeting the WG disability threshold who used outpatient care in the previous four weeks. Both models used quasi-Poisson regression with log link."
  ),
  header_labels = list(
    Characteristic = "Characteristic",
    UninsuredAPR = "Uninsured APR (95% CI); p",
    PaymentAPR = "Outpatient payment APR (95% CI); p"
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
  ggplot2::labs(
    title = "Any insurance and NHIF coverage by disability severity among adults",
    subtitle = "Source: Kenya DHS 2022. Survey-weighted estimates.",
    x = NULL,
    y = "Weighted prevalence (%)",
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "top",
    axis.text.x = ggplot2::element_text(angle = 12, hjust = 1),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9)
  )

ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure1_Insurance_By_Disability_Severity.png"),
  figure1,
  width = 9,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure1_Insurance_By_Disability_Severity.tiff"),
  figure1,
  width = 9,
  height = 5,
  dpi = 300,
  compression = "lzw"
)

figure2_data <- payment_disabled_base %>%
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
    wg_severity = factor(
      wg_severity,
      levels = c("Moderate functional difficulty", "Severe functional difficulty")
    )
  )

figure2 <- ggplot2::ggplot(
  figure2_data,
  ggplot2::aes(x = wg_severity, y = 100 * est, fill = insurance)
) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = 100 * ci_low, ymax = 100 * ci_high),
    position = ggplot2::position_dodge(width = 0.72),
    width = 0.14
  ) +
  ggplot2::scale_fill_manual(values = c("Insured" = "#0f6e8c", "Uninsured" = "#cf5c36")) +
  ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(
    title = "Payment at last outpatient visit among adults with disability by severity and insurance status",
    subtitle = "Source: Kenya DHS 2022. Survey-weighted estimates.",
    x = NULL,
    y = "Paid at last outpatient visit (%)",
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9)
  )

ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure2_Outpatient_Payment_By_Severity_Insurance.png"),
  figure2,
  width = 8,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(paths$figures_dir, "Figure2_Outpatient_Payment_By_Severity_Insurance.tiff"),
  figure2,
  width = 8,
  height = 5,
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
  model_payment = model_payment
)

analysis_object <- list(
  table1 = table1,
  table2 = table2,
  table3 = table3,
  table4 = table4,
  figure1_data = figure1_data,
  figure2_data = figure2_data,
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
        AnalysisGroup = "Analysis group",
        n = "n",
        Outpatient = "Outpatient use % (95% CI)",
        Hospitalisation = "Hospitalisation % (95% CI)",
        Payment = "Paid at last outpatient visit % (95% CI)"
      )
    ),
    table4 = list(
      header_labels = list(
        Characteristic = "Characteristic",
        UninsuredAPR = "Uninsured APR (95% CI); p",
        PaymentAPR = "Outpatient payment APR (95% CI); p"
      )
    )
  )
)

save_rds_output(analysis_object, "st02_analysis_outputs.rds")

append_log("ST02 manuscript-facing tables, figures, and summary outputs saved.")

message("=== SECTION 4 COMPLETE ===")
