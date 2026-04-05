# Study: ST03_NCD_Insurance_Service_Use
# Script: 05_checks_sensitivity.R
# Author: Nichodemus Werre Amollo
# Date: 2026-03-30
# Purpose: Verify the harmonised ST03 workflow outputs and document submission-readiness.

message("=== SECTION 5: Checks and Sensitivity ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analysis_object <- readRDS(file.path(paths$derived_dir, "st03_analysis_outputs.rds"))
table5 <- analysis_object$table5
key_results <- analysis_object$key_results
interaction_tests <- analysis_object$interaction_tests
sensitivity_table_s2 <- analysis_object$supplementary_table_s2

expected_checks <- tibble::tribble(
  ~metric, ~expected,
  "sample_size_women", 1384,
  "sample_size_men", 548,
  "sample_size_pooled", 1932,
  "pooled_any_insurance", 47.7,
  "pooled_nhif", 43.4,
  "pooled_treatment_gap", 63.8,
  "women_treatment_gap", 67.1,
  "men_treatment_gap", 59.8,
  "women_apr_insurance", 0.912,
  "men_apr_insurance", 0.931,
  "pooled_apr_insurance", 0.919,
  "pooled_standard_ci_any_insurance", 0.2889,
  "pooled_erreygers_ci_any_insurance", 0.5507,
  "pooled_standard_ci_treatment_gap", -0.0347,
  "pooled_erreygers_ci_treatment_gap", -0.0887
) %>%
  dplyr::mutate(
    observed = c(
      key_results$sample_size_women,
      key_results$sample_size_men,
      key_results$sample_size_pooled,
      round(100 * key_results$pooled_any_insurance$est, 1),
      round(100 * key_results$pooled_nhif$est, 1),
      round(100 * key_results$pooled_treatment_gap$est, 1),
      round(100 * key_results$women_treatment_gap$est, 1),
      round(100 * key_results$men_treatment_gap$est, 1),
      round(key_results$women_apr_insurance$effect, 3),
      round(key_results$men_apr_insurance$effect, 3),
      round(key_results$pooled_apr_insurance$effect, 3),
      table5$standard_ci[table5$outcome == "insured_any" & table5$subgroup == "Pooled"],
      table5$erreygers_ci[table5$outcome == "insured_any" & table5$subgroup == "Pooled"],
      table5$standard_ci[table5$outcome == "treatment_gap" & table5$subgroup == "Pooled"],
      table5$erreygers_ci[table5$outcome == "treatment_gap" & table5$subgroup == "Pooled"]
    ),
    absolute_difference = abs(observed - expected),
    within_tolerance = dplyr::case_when(
      stringr::str_detect(metric, "sample_size") ~ absolute_difference == 0,
      TRUE ~ absolute_difference <= 0.05
    )
  )

required_outputs <- c(
  file.path(paths$derived_dir, "Results_Summary.txt"),
  file.path(paths$tables_dir, "Table1_Sample_Characteristics.csv"),
  file.path(paths$tables_dir, "Table1_Sample_Characteristics.docx"),
  file.path(paths$tables_dir, "Table2_Treatment_Cascade.csv"),
  file.path(paths$tables_dir, "Table2_Treatment_Cascade.docx"),
  file.path(paths$tables_dir, "Table3_Adjusted_Prevalence_Ratios.csv"),
  file.path(paths$tables_dir, "Table3_Adjusted_Prevalence_Ratios.docx"),
  file.path(paths$tables_dir, "Supplementary_Table_S1_OR_Sensitivity.docx"),
  file.path(paths$tables_dir, "Supplementary_Table_S2_Alternative_Gap_Definition.docx"),
  file.path(paths$tables_dir, "Table4_Women_Barriers.csv"),
  file.path(paths$tables_dir, "Table5_Concentration_Indices.csv"),
  file.path(paths$figures_dir, "Figure1_Treatment_Cascade_by_Sex_Diagnosis.png"),
  file.path(paths$figures_dir, "Figure1_Treatment_Cascade_by_Sex_Diagnosis.tiff"),
  file.path(paths$figures_dir, "Figure2_Insurance_by_Wealth_Sex.png"),
  file.path(paths$figures_dir, "Figure2_Insurance_by_Wealth_Sex.tiff"),
  file.path(paths$figures_dir, "Figure3_Effective_Coverage_Cascade_by_Sex.png"),
  file.path(paths$figures_dir, "Figure3_Effective_Coverage_Cascade_by_Sex.tiff"),
  file.path(paths$manuscript_dir, "Additional_file_1_Supplementary_Table_S1.docx"),
  file.path(paths$manuscript_dir, "Additional_file_3_Supplementary_Table_S2.docx")
)

output_check <- tibble::tibble(
  file_path = required_outputs,
  exists = file.exists(required_outputs)
)

checks_object <- list(
  key_estimate_checks = expected_checks,
  output_check = output_check
)

save_rds_output(checks_object, "st03_checks.rds")
readr::write_csv(expected_checks, file.path(paths$logs_dir, "st03_key_estimate_checks.csv"))
readr::write_csv(output_check, file.path(paths$logs_dir, "st03_output_file_check.csv"))
readr::write_csv(interaction_tests, file.path(paths$logs_dir, "st03_interaction_tests.csv"))
readr::write_csv(sensitivity_table_s2, file.path(paths$logs_dir, "st03_alt_gap_sensitivity.csv"))

progress_note <- c(
  "# ST03 progress note",
  "",
  sprintf("Date: %s", format(Sys.Date(), "%Y-%m-%d")),
  "",
  "## Workflow actions completed",
  "- Harmonised the modular ST03 workflow to the same manuscript-facing analysis objects used by the final manuscript path.",
  "- Consolidated derived data in 07_derived_data and validation logs in 08_logs.",
  "- Regenerated publication tables, Supplementary Table S1, and standalone 300 DPI submission figures from the modular runner.",
  "- Added standard and Erreygers-corrected concentration-index outputs, interaction-test logs, an overall diagnosis-to-treatment coverage cascade figure, and a stricter alternative treatment-gap sensitivity table.",
  "- Reconfirmed the canonical submission versions of Tables 1, 3, 4, and 5.",
  "",
  "## Validation summary",
  sprintf("- Key estimate checks passed: %s of %s", sum(expected_checks$within_tolerance), nrow(expected_checks)),
  sprintf("- Required output files present: %s of %s", sum(output_check$exists), nrow(output_check)),
  "",
  "## Submission status",
  "- Corresponding-author email and DHS authorization reference are populated in the manuscript.",
  "- Final manual edits and author review can proceed before BMC submission."
)

progress_note_path <- file.path(paths$logs_dir, "ST03_progress_note.md")
readr::write_lines(progress_note, progress_note_path)
write_session_info("st03_session_info.txt")

append_log("Checks complete and submission-readiness note written.")

message("=== SECTION 5 COMPLETE ===")
