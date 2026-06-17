# Study: ST11_Mental_Health_Treatment_Gap
# Script: 04_tables_figures.R
# Purpose: Build manuscript-facing tables, a figure, and inline-ready results.

message("=== SECTION 4: Tables and Figures ===")
source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analytic <- readRDS(file.path(paths$derived_dir, "st11_analytic_adults.rds"))
main <- readRDS(file.path(paths$derived_dir, "st11_analysis_main.rds"))
append_log("Building ST11 tables and figures.", also_message = TRUE)

fmt_row <- function(df_row) fmt_pct_ci(df_row$est, df_row$ci_low, df_row$ci_high)

# ---- Table 1: diagnosis prevalence (whole sample) ----------------------------
table1 <- main$diagnosis_prevalence %>%
  dplyr::transmute(
    Indicator = metric,
    n = format(unweighted_n, big.mark = ","),
    `Weighted % (95% CI)` = fmt_pct_ci(est, ci_low, ci_high)
  )

# ---- Table 2: treatment gap by subgroup --------------------------------------
group_labels <- c(Overall = "Overall", sex = "Sex", wealth = "Wealth quintile",
                  insured_lab = "Insurance", residence = "Residence",
                  age_group = "Age group", ncd_group = "Physical NCD count")
table2 <- main$treatment_gap %>%
  dplyr::mutate(
    Section = dplyr::recode(group, !!!group_labels),
    Subgroup = level,
    n = format(unweighted_n, big.mark = ","),
    `Untreated % (95% CI)` = ifelse(is.na(est), "-", fmt_pct_ci(est, ci_low, ci_high))
  ) %>%
  dplyr::select(Section, Subgroup, n, `Untreated % (95% CI)`)

# ---- Table 3: adjusted predictors of being untreated -------------------------
table3 <- main$model_untreated %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::transmute(
    Term = term,
    `APR (95% CI)` = fmt_apr_ci(apr, ci_low, ci_high),
    `p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )

# ---- Figure 1: untreated % by wealth quintile --------------------------------
diagnosed <- analytic %>% dplyr::filter(dx_dep_or_anx == 1, !is.na(untreated), !is.na(wealth))
fig1_df <- diagnosed %>%
  dplyr::group_by(wealth) %>%
  dplyr::group_modify(~{ s <- weighted_binary(.x, "untreated"); tibble::tibble(est = s$est, lo = s$ci_low, hi = s$ci_high, n = s$unweighted_n) }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(wealth = factor(wealth, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))) %>%
  dplyr::filter(!is.na(wealth))

figure1 <- ggplot2::ggplot(fig1_df, ggplot2::aes(x = wealth, y = 100 * est)) +
  ggplot2::geom_col(fill = "#3b6ea5", width = 0.65) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = 100 * lo, ymax = 100 * hi), width = 0.18) +
  ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(
    title = "Depression/anxiety treatment gap by wealth quintile, adults in Kenya",
    subtitle = "Source: Kenya DHS 2022. Untreated among those reporting a diagnosis. Survey-weighted.",
    x = NULL, y = "Diagnosed but untreated (%)"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), plot.subtitle = ggplot2::element_text(size = 9))

ggplot2::ggsave(file.path(paths$figures_dir, "Figure1_TreatmentGap_By_Wealth.png"), figure1, width = 8, height = 5, dpi = 300)

# ---- inline results ----------------------------------------------------------
get_metric <- function(m) main$diagnosis_prevalence %>% dplyr::filter(metric == m)
key_results <- list(
  n_adults = nrow(analytic),
  n_diagnosed = main$n_diagnosed,
  prev_depression = get_metric("Diagnosed depression"),
  prev_anxiety = get_metric("Diagnosed anxiety"),
  prev_dep_or_anx = get_metric("Diagnosed depression or anxiety"),
  gap_overall = main$treatment_gap %>% dplyr::filter(group == "Overall"),
  ci_untreated = main$ci_untreated
)

save_table_bundle(table1,
  file.path(paths$tables_dir, "Table1_Diagnosis_Prevalence.csv"),
  file.path(paths$tables_dir, "Table1_Diagnosis_Prevalence.docx"),
  "Table 1. Weighted prevalence of self-reported diagnosed depression and anxiety, adults, KDHS 2022.")
save_table_bundle(table2,
  file.path(paths$tables_dir, "Table2_Treatment_Gap_By_Subgroup.csv"),
  file.path(paths$tables_dir, "Table2_Treatment_Gap_By_Subgroup.docx"),
  "Table 2. Weighted depression/anxiety treatment gap (diagnosed but untreated) by subgroup, KDHS 2022.")
save_table_bundle(table3,
  file.path(paths$tables_dir, "Table3_Adjusted_Untreated.csv"),
  file.path(paths$tables_dir, "Table3_Adjusted_Untreated.docx"),
  "Table 3. Survey-weighted adjusted prevalence ratios for being untreated among adults with diagnosed depression or anxiety, KDHS 2022.")

save_rds_output(list(table1 = table1, table2 = table2, table3 = table3,
                     fig1_df = fig1_df, key_results = key_results),
                "st11_analysis_outputs.rds")
append_log("ST11 tables, figure, and inline results saved.")
message("=== SECTION 4 COMPLETE ===")
