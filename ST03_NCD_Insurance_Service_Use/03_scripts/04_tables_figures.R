# Study: ST03_NCD_Insurance_Service_Use
# Script: 04_tables_figures.R
# Author: Nichodemus Werre Amollo
# Date: 2026-03-30
# Purpose: Write manuscript-facing ST03 tables, supplementary files, figures, and summary text from the harmonised analysis object.

message("=== SECTION 4: Tables and Figures ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analysis_object <- readRDS(file.path(paths$derived_dir, "st03_analysis_outputs.rds"))

table1 <- analysis_object$table1
table2 <- analysis_object$table2
table3 <- analysis_object$table3
table4 <- analysis_object$table4
table5 <- analysis_object$table5
table_s1 <- analysis_object$supplementary_table_s1
table_s2 <- analysis_object$supplementary_table_s2
figure1_data <- analysis_object$figure1_data
figure2_data <- analysis_object$figure2_data
figure3_data <- analysis_object$figure3_data
summary_lines <- analysis_object$summary_lines

append_log("Writing harmonised manuscript-facing tables and figures.", also_message = TRUE)

fmt_pct_string <- function(est) {
  vals <- as.numeric(unlist(regmatches(est, gregexpr("[0-9]+\\.?[0-9]*", est))))
  sprintf("%.1f%% (95%% CI %.1f%% to %.1f%%)", vals[1], vals[2], vals[3])
}

table4_display <- table4 %>%
  dplyr::mutate(`Prevalence % (95% CI)` = vapply(`Prevalence % (95% CI)`, fmt_pct_string, character(1)))

fmt_ci_string <- function(est, ci_low, ci_high) {
  stringr::str_replace_all(
    sprintf("%.4f (%.4f, %.4f)", est, ci_low, ci_high),
    "-",
    "\u2212"
  )
}

table5_display <- table5 %>%
  dplyr::transmute(
    Outcome = dplyr::recode(
      outcome,
      insured_any = "Any insurance",
      insured_nhif = "NHIF coverage",
      treatment_gap = "Treatment gap"
    ),
    Subgroup = subgroup,
    `Standard CI (95% CI)` = purrr::pmap_chr(
      list(standard_ci, standard_ci_low, standard_ci_high),
      fmt_ci_string
    ),
    `Erreygers-corrected CI (95% CI)` = purrr::pmap_chr(
      list(erreygers_ci, erreygers_ci_low, erreygers_ci_high),
      fmt_ci_string
    )
  )

save_table_bundle(
  table1,
  file.path(paths$tables_dir, "Table1_Sample_Characteristics.csv"),
  file.path(paths$tables_dir, "Table1_Sample_Characteristics.docx"),
  "Table 1. Sample characteristics among diagnosed adults with hypertension and/or diabetes, Kenya DHS 2022.",
  header_labels = st03_table_display_specs$table1$header_labels,
  spanner_values = st03_table_display_specs$table1$spanner_values,
  spanner_widths = st03_table_display_specs$table1$spanner_widths
)

table2_footer <- c(
  "Source: Kenya DHS 2022. Survey-weighted estimates.",
  "Estimates for 'Both HTN and DM' should be interpreted with caution owing to small unweighted sample sizes (women n=41, men n=49). Wide confidence intervals reflect sampling imprecision."
)

save_table_bundle(
  table2,
  file.path(paths$tables_dir, "Table2_Treatment_Cascade.csv"),
  file.path(paths$tables_dir, "Table2_Treatment_Cascade.docx"),
  "Table 2. Insurance coverage and treatment gaps by diagnosis profile and sex, Kenya DHS 2022.",
  footer_lines = table2_footer,
  header_labels = st03_table_display_specs$table2$header_labels
)

table3_footer <- c(
  "Source: Kenya DHS 2022. Survey-weighted estimates.",
  "APR from survey-weighted quasi-Poisson GLM with log link. Reference categories are shown in parentheses. The pooled model includes a sex term with male-subsample correction. NE = North Eastern province."
)

save_table_bundle(
  table3,
  file.path(paths$tables_dir, "Table3_Adjusted_Prevalence_Ratios.csv"),
  file.path(paths$tables_dir, "Table3_Adjusted_Prevalence_Ratios.docx"),
  "Table 3. Adjusted prevalence ratios for the treatment gap among diagnosed adults, Kenya DHS 2022.",
  footer_lines = table3_footer,
  header_labels = st03_table_display_specs$table3$header_labels
)

table_s1_footer <- c(
  "Additional file 1. Odds-ratio sensitivity analyses for the treatment gap.",
  "Source: Kenya DHS 2022. Survey-weighted estimates. Directional conclusions were unchanged from the adjusted prevalence-ratio models."
)

save_table_bundle(
  table_s1,
  file.path(paths$tables_dir, "Supplementary_Table_S1_OR_Sensitivity.csv"),
  file.path(paths$tables_dir, "Supplementary_Table_S1_OR_Sensitivity.docx"),
  "Supplementary Table S1. Odds-ratio sensitivity analyses for the treatment gap among diagnosed adults, Kenya DHS 2022.",
  footer_lines = table_s1_footer
)
file.copy(file.path(paths$tables_dir, "Supplementary_Table_S1_OR_Sensitivity.docx"), file.path(paths$manuscript_dir, "Additional_file_1_Supplementary_Table_S1.docx"), overwrite = TRUE)

table_s2_footer <- c(
  "Additional file 3. Alternative treatment-gap sensitivity analysis.",
  "The main analysis defined the treatment gap as not currently taking medication for at least one diagnosed condition. The stricter sensitivity definition classified respondents as untreated only if they reported no current medication for any diagnosed condition."
)

save_table_bundle(
  table_s2,
  file.path(paths$tables_dir, "Supplementary_Table_S2_Alternative_Gap_Definition.csv"),
  file.path(paths$tables_dir, "Supplementary_Table_S2_Alternative_Gap_Definition.docx"),
  "Supplementary Table S2. Sensitivity analysis using a stricter no-medication-for-any-diagnosed-condition definition, Kenya DHS 2022.",
  footer_lines = table_s2_footer,
  header_labels = st03_table_display_specs$table_s2$header_labels
)
file.copy(file.path(paths$tables_dir, "Supplementary_Table_S2_Alternative_Gap_Definition.docx"), file.path(paths$manuscript_dir, "Additional_file_3_Supplementary_Table_S2.docx"), overwrite = TRUE)

save_table_bundle(
  table4_display,
  file.path(paths$tables_dir, "Table4_Women_Barriers.csv"),
  file.path(paths$tables_dir, "Table4_Women_Barriers.docx"),
  "Table 4. Women-specific access barriers by insurance status among diagnosed adults, Kenya DHS 2022.",
  header_labels = st03_table_display_specs$table4$header_labels
)

save_table_bundle(
  table5_display,
  file.path(paths$tables_dir, "Table5_Concentration_Indices.csv"),
  file.path(paths$tables_dir, "Table5_Concentration_Indices.docx"),
  "Table 5. Standard and Erreygers-corrected concentration indices for insurance coverage and treatment gaps among diagnosed adults, Kenya DHS 2022.",
  footer_lines = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates.",
    "Standard concentration indices are shown for comparability with prior Kenya inequality studies. Erreygers-corrected indices are included as sensitivity analyses for binary outcomes."
  ),
  header_labels = st03_table_display_specs$table5$header_labels
)

figure1 <- ggplot2::ggplot(figure1_data, ggplot2::aes(x = diagnosis_profile, y = 100 * estimate, fill = metric)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = 100 * ci_low, ymax = 100 * ci_high),
    position = ggplot2::position_dodge(width = 0.8),
    width = 0.2
  ) +
  ggplot2::facet_wrap(~sex) +
  ggplot2::scale_fill_manual(values = c("Any insurance" = "#0f6e8c", "NHIF coverage" = "#5c7cfa", "Treatment gap" = "#cf5c36")) +
  ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.06))) +
  ggplot2::labs(
    title = "Treatment cascade by sex and diagnosis profile among diagnosed adults",
    subtitle = "Source: Kenya DHS 2022. Survey-weighted estimates.",
    x = NULL,
    y = "Weighted prevalence (%)",
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9)
  )

ggplot2::ggsave(file.path(paths$figures_dir, "Figure1_Treatment_Cascade_by_Sex_Diagnosis.png"), figure1, width = 10, height = 6, dpi = 300)
ggplot2::ggsave(file.path(paths$figures_dir, "Figure1_Treatment_Cascade_by_Sex_Diagnosis.tiff"), figure1, width = 10, height = 6, dpi = 300, compression = "lzw")

figure2 <- ggplot2::ggplot(figure2_data, ggplot2::aes(x = group, y = 100 * estimate, color = sex, group = sex)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = 100 * ci_low, ymax = 100 * ci_high), width = 0.1) +
  ggplot2::facet_wrap(~metric, ncol = 1) +
  ggplot2::scale_color_manual(values = c("Women" = "#0f6e8c", "Men" = "#2f855a")) +
  ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.08))) +
  ggplot2::labs(
    title = "Wealth gradients in insurance coverage and treatment gaps by sex",
    subtitle = "Source: Kenya DHS 2022. Survey-weighted estimates.",
    x = "Wealth quintile",
    y = "Weighted prevalence (%)",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9)
  )

ggplot2::ggsave(file.path(paths$figures_dir, "Figure2_Insurance_by_Wealth_Sex.png"), figure2, width = 8, height = 5, dpi = 300)
ggplot2::ggsave(file.path(paths$figures_dir, "Figure2_Insurance_by_Wealth_Sex.tiff"), figure2, width = 8, height = 5, dpi = 300, compression = "lzw")

figure3 <- ggplot2::ggplot(figure3_data, ggplot2::aes(x = step, y = 100 * estimate, fill = step)) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = 100 * ci_low, ymax = 100 * ci_high),
    width = 0.16
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f", 100 * estimate)),
    vjust = -0.4,
    size = 3
  ) +
  ggplot2::facet_wrap(~sex) +
  ggplot2::scale_fill_manual(
    values = c(
      "Diagnosed NCD" = "#0f6e8c",
      "Any insurance" = "#4c956c",
      "NHIF coverage" = "#7e57c2",
      "On medication for all diagnosed conditions" = "#cf5c36"
    )
  ) +
  ggplot2::scale_x_discrete(
    labels = c(
      "Diagnosed NCD" = "Diagnosed\nNCD",
      "Any insurance" = "Any\ninsurance",
      "NHIF coverage" = "NHIF\ncoverage",
      "On medication for all diagnosed conditions" = "On medication\nfor all diagnosed\nconditions"
    )
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 105), expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::labs(
    title = "Diagnosis-to-treatment coverage cascade by sex among diagnosed adults",
    subtitle = "Source: Kenya DHS 2022. Survey-weighted estimates.",
    x = NULL,
    y = "Weighted prevalence (%)"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(lineheight = 0.9),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9)
  )

ggplot2::ggsave(file.path(paths$figures_dir, "Figure3_Effective_Coverage_Cascade_by_Sex.png"), figure3, width = 9, height = 5.5, dpi = 300)
ggplot2::ggsave(file.path(paths$figures_dir, "Figure3_Effective_Coverage_Cascade_by_Sex.tiff"), figure3, width = 9, height = 5.5, dpi = 300, compression = "lzw")

write_lines_output(summary_lines, "Results_Summary.txt")

append_log("Harmonised tables, supplementary files, figures, and summary text refreshed.")

message("=== SECTION 4 COMPLETE ===")
