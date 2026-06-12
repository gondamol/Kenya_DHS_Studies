# Study: ST10_Remision_study
# Script: 09_rob_grade_viz.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Risk-of-bias visualisation (robvis traffic-light + weighted-bar) from the
#          completed RoB 2 CSV (../../05_risk_of_bias/rob2_template.csv).

message("=== 09: ROB VISUALISATION ===")

if (!requireNamespace("robvis", quietly = TRUE)) {
  message("Package 'robvis' not installed - run install.packages('robvis'). Skipping.")
} else if (!file.exists(paths$rob_csv)) {
  message("No RoB CSV at ", paths$rob_csv, " - skipping.")
} else {
  rob <- readr::read_csv(paths$rob_csv, show_col_types = FALSE)
  if (nrow(rob) == 0) {
    message("RoB CSV is empty (template only) - complete assessments first. Skipping.")
  } else {
    # robvis expects columns: Study, D1..D5, Overall, Weight (optional)
    rob_in <- rob %>%
      transmute(Study = study_id,
                D1 = d1_randomisation, D2 = d2_deviations, D3 = d3_missing_data,
                D4 = d4_measurement,   D5 = d5_selection_reporting,
                Overall = overall_rob) %>%
      mutate(across(everything(), ~ recode(as.character(.),
              low = "Low", some_concerns = "Some concerns", high = "High",
              .default = as.character(.))))
    tryCatch(
      save_plot(print(robvis::rob_traffic_light(data = rob_in, tool = "ROB2")),
                "fig_rob2_traffic_light.png", height = max(4, 0.4 * nrow(rob_in))),
      error = function(e) message("traffic-light plot skipped: ", conditionMessage(e)))
    tryCatch(
      save_plot(print(robvis::rob_summary(data = rob_in, tool = "ROB2", weighted = FALSE)),
                "fig_rob2_summary_bar.png", height = 3),
      error = function(e) message("summary-bar plot skipped: ", conditionMessage(e)))
  }
}

message("=== 09 COMPLETE ===")
