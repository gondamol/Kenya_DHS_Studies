# Study: ST11_Mental_Health_Treatment_Gap
# Script: 04_tables_figures.R
# Purpose: Build five manuscript tables, two figures, and inline-ready results.

message("=== SECTION 4: Tables and Figures ===")
source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analytic <- readRDS(file.path(paths$derived_dir, "st11_analytic_adults.rds"))
m <- readRDS(file.path(paths$derived_dir, "st11_analysis_main.rds"))
append_log("Building ST11 tables and figures.", also_message = TRUE)

module <- analytic %>% dplyr::filter(!is.na(dx_dep_or_anx))
diagnosed <- analytic %>% dplyr::filter(dx_dep_or_anx == 1, !is.na(untreated))

pct <- function(est, lo, hi) ifelse(is.na(est), "-", fmt_pct_ci(est, lo, hi))

# ---- Table 1: characteristics by sex (module sample) -------------------------
wpct_level <- function(df, var, level) {
  d <- df %>% dplyr::mutate(.ind = dplyr::if_else(.data[[var]] == level, 1L, 0L, missing = NA_integer_))
  s <- weighted_binary(d, ".ind"); pct(s$est, s$ci_low, s$ci_high)
}
wmean_age <- function(df) {
  d <- df %>% dplyr::filter(!is.na(age), !is.na(weight), !is.na(psu), !is.na(strata))
  est <- survey::svymean(~age, make_design(d)); ci <- suppressWarnings(stats::confint(est))
  sprintf("%.1f (%.1f, %.1f)", as.numeric(est)[1], ci[1, 1], ci[1, 2])
}
grp <- list(Overall = module, Women = dplyr::filter(module, sex == "Women"), Men = dplyr::filter(module, sex == "Men"))

row3 <- function(label, fun) tibble::tibble(Characteristic = label,
  Overall = fun(grp$Overall), Women = fun(grp$Women), Men = fun(grp$Men))
sec <- function(label) tibble::tibble(Characteristic = label, Overall = "", Women = "", Men = "")
lvl_row <- function(label, var, level) row3(label, function(df) wpct_level(df, var, level))

table1 <- dplyr::bind_rows(
  row3("Unweighted n", function(df) format(nrow(df), big.mark = ",")),
  row3("Mean age, years (95% CI)", wmean_age),
  sec("Age group, % (95% CI)"),
  lvl_row("  15-24", "age_group", "15-24"), lvl_row("  25-34", "age_group", "25-34"),
  lvl_row("  35-49", "age_group", "35-49"), lvl_row("  50+", "age_group", "50+"),
  sec("Residence, %"),
  lvl_row("  Urban", "residence", "Urban"), lvl_row("  Rural", "residence", "Rural"),
  sec("Education, %"),
  lvl_row("  No education", "education", "No education"), lvl_row("  Primary", "education", "Primary"),
  lvl_row("  Secondary", "education", "Secondary"), lvl_row("  Higher", "education", "Higher"),
  sec("Wealth quintile, %"),
  lvl_row("  Poorest", "wealth", "Poorest"), lvl_row("  Poorer", "wealth", "Poorer"),
  lvl_row("  Middle", "wealth", "Middle"), lvl_row("  Richer", "wealth", "Richer"),
  lvl_row("  Richest", "wealth", "Richest"),
  sec("Health insurance, %"),
  lvl_row("  Insured", "insured_lab", "Insured"), lvl_row("  Uninsured", "insured_lab", "Uninsured"),
  sec("Physical NCD count, %"),
  lvl_row("  0", "ncd_group", "0"), lvl_row("  1", "ncd_group", "1"), lvl_row("  2+", "ncd_group", "2+"),
  sec("Diagnosed depression or anxiety, %"),
  row3("  Yes", function(df) { s <- weighted_binary(df, "dx_dep_or_anx"); pct(s$est, s$ci_low, s$ci_high) })
)

# ---- Table 2: diagnosis prevalence by subgroup -------------------------------
ov <- m$diagnosis_overall
overall_row <- tibble::tibble(
  Section = "Overall", Subgroup = "All adults",
  n = format(ov$unweighted_n[ov$metric == "Diagnosed depression or anxiety"], big.mark = ","),
  `Depression % (95% CI)` = pct(ov$est[ov$metric == "Diagnosed depression"], ov$ci_low[ov$metric == "Diagnosed depression"], ov$ci_high[ov$metric == "Diagnosed depression"]),
  `Anxiety % (95% CI)` = pct(ov$est[ov$metric == "Diagnosed anxiety"], ov$ci_low[ov$metric == "Diagnosed anxiety"], ov$ci_high[ov$metric == "Diagnosed anxiety"]),
  `Either % (95% CI)` = pct(ov$est[ov$metric == "Diagnosed depression or anxiety"], ov$ci_low[ov$metric == "Diagnosed depression or anxiety"], ov$ci_high[ov$metric == "Diagnosed depression or anxiety"])
)
table2 <- m$prevalence_by_subgroup %>%
  dplyr::transmute(Section = section, Subgroup = level, n = format(n, big.mark = ","),
                   `Depression % (95% CI)` = pct(dep_est, dep_lo, dep_hi),
                   `Anxiety % (95% CI)` = pct(anx_est, anx_lo, anx_hi),
                   `Either % (95% CI)` = pct(either_est, either_lo, either_hi))
table2 <- dplyr::bind_rows(overall_row, table2)

# ---- Table 3: treatment gap by subgroup --------------------------------------
table3 <- m$treatment_gap %>%
  dplyr::transmute(Section = section, Subgroup = level, n = format(unweighted_n, big.mark = ","),
                   `Untreated % (95% CI)` = pct(est, ci_low, ci_high))

# ---- Table 4: adjusted APRs (diagnosis vs treatment gap) ---------------------
term_labels <- tibble::tribble(
  ~term, ~order, ~Characteristic,
  "sexWomen", 1, "Women (ref: men)",
  "age_group25-34", 2, "Age 25-34 (ref: 15-24)",
  "age_group35-49", 3, "Age 35-49",
  "age_group50+", 4, "Age 50+",
  "wealthRicher", 5, "Richer (ref: richest)",
  "wealthMiddle", 6, "Middle",
  "wealthPoorer", 7, "Poorer",
  "wealthPoorest", 8, "Poorest",
  "residenceRural", 9, "Rural (ref: urban)",
  "educationSecondary", 10, "Secondary (ref: higher)",
  "educationPrimary", 11, "Primary",
  "educationNo education", 12, "No education",
  "ncd_group1", 13, "1 physical NCD (ref: 0)",
  "ncd_group2+", 14, "2+ physical NCDs",
  "insured_labUninsured", 15, "Uninsured (ref: insured)"
)
apr_cell <- function(df) df %>% dplyr::mutate(cell = ifelse(is.na(apr), "-",
  paste0(fmt_apr_ci(apr, ci_low, ci_high), ifelse(p.value < 0.05, "*", "")))) %>% dplyr::select(term, cell)
table4 <- term_labels %>%
  dplyr::left_join(apr_cell(m$model_diagnosis) %>% dplyr::rename(`Diagnosis APR (95% CI)` = cell), by = "term") %>%
  dplyr::left_join(apr_cell(m$model_untreated) %>% dplyr::rename(`Untreated APR (95% CI)` = cell), by = "term") %>%
  dplyr::arrange(order) %>%
  dplyr::transmute(Characteristic,
                   `Diagnosis APR (95% CI)` = dplyr::coalesce(`Diagnosis APR (95% CI)`, "-"),
                   `Untreated APR (95% CI)` = dplyr::coalesce(`Untreated APR (95% CI)`, "-"))

# ---- Table 5: concentration indices ------------------------------------------
table5 <- m$ci_table %>%
  dplyr::transmute(Outcome = outcome, Stratum = stratum, n = format(n, big.mark = ","),
                   `Concentration index (95% CI)` = ifelse(is.na(CI), "-", sprintf("%.3f (%.3f, %.3f)", CI, CI_low, CI_high)),
                   `Erreygers index (95% CI)` = ifelse(is.na(Erreygers), "-", sprintf("%.3f (%.3f, %.3f)", Erreygers, E_low, E_high)))

# ---- Figure 1: diagnosis (pro-rich) vs treatment gap (flat) by wealth --------
wl <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
fig1_df <- dplyr::bind_rows(
  m$diag_by_wealth %>% dplyr::mutate(panel = "A. Diagnosed with depression/anxiety"),
  m$gap_by_wealth %>% dplyr::mutate(panel = "B. Untreated among the diagnosed")
) %>% dplyr::filter(wealth %in% wl) %>% dplyr::mutate(wealth = factor(wealth, levels = wl))

figure1 <- ggplot2::ggplot(fig1_df, ggplot2::aes(x = wealth, y = 100 * est, group = 1)) +
  ggplot2::geom_col(fill = "#3b6ea5", width = 0.66) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = 100 * lo, ymax = 100 * hi), width = 0.18) +
  ggplot2::facet_wrap(~panel, scales = "free_y") +
  ggplot2::labs(title = "A modest wealth gradient in diagnosis, but a socially uniform treatment gap",
                subtitle = "Source: Kenya DHS 2022. Survey-weighted estimates with 95% CIs.",
                x = "Wealth quintile", y = "Weighted prevalence (%)") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                 plot.subtitle = ggplot2::element_text(size = 9),
                 strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                 axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
ggplot2::ggsave(file.path(paths$figures_dir, "Figure1_Diagnosis_vs_Gap_By_Wealth.png"), figure1, width = 9, height = 4.6, dpi = 300)
ggplot2::ggsave(file.path(paths$figures_dir, "Figure1_Diagnosis_vs_Gap_By_Wealth.tiff"), figure1, width = 9, height = 4.6, dpi = 300, compression = "lzw")

# ---- Figure 2: forest plot of adjusted predictors of being untreated ---------
forest_df <- term_labels %>%
  dplyr::left_join(m$model_untreated, by = "term") %>%
  dplyr::filter(!is.na(apr)) %>%
  dplyr::arrange(dplyr::desc(order)) %>%
  dplyr::mutate(Characteristic = factor(Characteristic, levels = Characteristic))
figure2 <- ggplot2::ggplot(forest_df, ggplot2::aes(x = apr, y = Characteristic)) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(size = 2.4, colour = "#b5462f") +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_low, xmax = ci_high), height = 0.25, colour = "#b5462f") +
  ggplot2::scale_x_log10() +
  ggplot2::labs(title = "Adjusted predictors of being untreated among diagnosed adults",
                subtitle = "Source: Kenya DHS 2022. Survey-weighted quasi-Poisson APRs (log scale) with 95% CIs.",
                x = "Adjusted prevalence ratio (untreated)", y = NULL) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), plot.subtitle = ggplot2::element_text(size = 9))
ggplot2::ggsave(file.path(paths$figures_dir, "Figure2_Untreated_Forest.png"), figure2, width = 8, height = 5.2, dpi = 300)
ggplot2::ggsave(file.path(paths$figures_dir, "Figure2_Untreated_Forest.tiff"), figure2, width = 8, height = 5.2, dpi = 300, compression = "lzw")

# ---- inline results ----------------------------------------------------------
gv <- function(metric, field) ov[[field]][ov$metric == metric]
ci_row <- function(out, strat) m$ci_table %>% dplyr::filter(outcome == out, stratum == strat)
key_results <- list(
  n_adults = nrow(analytic), n_module = m$n_module, n_diagnosed = m$n_diagnosed,
  n_diag_model = m$n_diag_model, n_untreated_model = m$n_untreated_model,
  prev_depression = m$diagnosis_overall %>% dplyr::filter(metric == "Diagnosed depression"),
  prev_anxiety = m$diagnosis_overall %>% dplyr::filter(metric == "Diagnosed anxiety"),
  prev_either = m$diagnosis_overall %>% dplyr::filter(metric == "Diagnosed depression or anxiety"),
  gap_overall = m$treatment_gap %>% dplyr::filter(section == "Overall"),
  gap_insured = m$treatment_gap %>% dplyr::filter(section == "Health insurance", level == "Insured"),
  gap_uninsured = m$treatment_gap %>% dplyr::filter(section == "Health insurance", level == "Uninsured"),
  gap_richest = m$treatment_gap %>% dplyr::filter(section == "Wealth quintile", level == "Richest"),
  gap_poorest = m$treatment_gap %>% dplyr::filter(section == "Wealth quintile", level == "Poorest"),
  untreated_dep = m$untreated_dep, untreated_anx = m$untreated_anx,
  ci_diag = ci_row("Diagnosis (depression/anxiety)", "Overall"),
  ci_gap = ci_row("Treatment gap (untreated)", "Overall")
)

save_table_bundle(table1, file.path(paths$tables_dir, "Table1_Characteristics.csv"), file.path(paths$tables_dir, "Table1_Characteristics.docx"),
  "Table 1. Weighted characteristics of adults asked the KDHS 2022 mental-health module, by sex.")
save_table_bundle(table2, file.path(paths$tables_dir, "Table2_Diagnosis_Prevalence.csv"), file.path(paths$tables_dir, "Table2_Diagnosis_Prevalence.docx"),
  "Table 2. Weighted prevalence of self-reported diagnosed depression and anxiety by subgroup, KDHS 2022.")
save_table_bundle(table3, file.path(paths$tables_dir, "Table3_Treatment_Gap.csv"), file.path(paths$tables_dir, "Table3_Treatment_Gap.docx"),
  "Table 3. Weighted depression/anxiety treatment gap (diagnosed but untreated) by subgroup, KDHS 2022.")
save_table_bundle(table4, file.path(paths$tables_dir, "Table4_Adjusted_Models.csv"), file.path(paths$tables_dir, "Table4_Adjusted_Models.docx"),
  "Table 4. Survey-weighted adjusted prevalence ratios for being diagnosed with depression/anxiety and, among the diagnosed, for being untreated, KDHS 2022.")
save_table_bundle(table5, file.path(paths$tables_dir, "Table5_Concentration_Indices.csv"), file.path(paths$tables_dir, "Table5_Concentration_Indices.docx"),
  "Table 5. Wealth-related concentration indices for diagnosis and for the treatment gap, KDHS 2022.")

save_rds_output(list(table1 = table1, table2 = table2, table3 = table3, table4 = table4, table5 = table5,
                     fig1_df = fig1_df, key_results = key_results), "st11_analysis_outputs.rds")
append_log("ST11 tables, figures, and inline results saved.")
message("=== SECTION 4 COMPLETE ===")
