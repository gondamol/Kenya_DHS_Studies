# Study: ST11_Mental_Health_Treatment_Gap
# Script: 03_analysis_main.R
# Purpose: Estimate diagnosis prevalence, the depression/anxiety treatment gap,
#          its socioeconomic/insurance gradient, and adjusted predictors.

message("=== SECTION 3: Analysis ===")
source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analytic <- readRDS(file.path(paths$derived_dir, "st11_analytic_adults.rds"))
append_log("Running ST11 main analysis.", also_message = TRUE)

# ---- 1. Diagnosis prevalence (whole adult sample) ----------------------------
diagnosis_prevalence <- dplyr::bind_rows(
  weighted_binary(analytic, "dx_depression") %>% dplyr::mutate(metric = "Diagnosed depression"),
  weighted_binary(analytic, "dx_anxiety") %>% dplyr::mutate(metric = "Diagnosed anxiety"),
  weighted_binary(analytic, "dx_dep_or_anx") %>% dplyr::mutate(metric = "Diagnosed depression or anxiety")
) %>% dplyr::select(metric, dplyr::everything())

# ---- 2. Treatment gap (untreated among the diagnosed) ------------------------
diagnosed <- analytic %>% dplyr::filter(dx_dep_or_anx == 1, !is.na(untreated))

gap_overall <- weighted_binary(diagnosed, "untreated") %>% dplyr::mutate(group = "Overall", level = "All diagnosed")

gap_by <- function(var) {
  diagnosed %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    dplyr::group_split(.data[[var]]) %>%
    purrr::map_dfr(function(g) {
      lvl <- as.character(g[[var]][1])
      weighted_binary(g, "untreated") %>% dplyr::mutate(group = var, level = lvl)
    })
}

treatment_gap <- dplyr::bind_rows(
  gap_overall,
  gap_by("sex"), gap_by("wealth"), gap_by("insured_lab"),
  gap_by("residence"), gap_by("age_group"), gap_by("ncd_group")
) %>% dplyr::select(group, level, dplyr::everything())

# ---- 3. Adjusted predictors of being untreated -------------------------------
model_df <- diagnosed %>%
  dplyr::filter(!is.na(sex), !is.na(age_group), !is.na(wealth), !is.na(residence), !is.na(education)) %>%
  dplyr::mutate(
    sex = factor(sex, levels = c("Men", "Women")),
    age_group = factor(age_group, levels = c("15-24", "25-34", "35-49", "50+")),
    wealth = factor(wealth, levels = c("Richest", "Richer", "Middle", "Poorer", "Poorest")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    education = factor(education, levels = c("Higher", "Secondary", "Primary", "No education")),
    ncd_group = factor(dplyr::coalesce(ncd_group, "0"), levels = c("0", "1", "2+"))
  )

model_untreated <- tryCatch({
  des <- make_design(model_df)
  survey::svyglm(untreated ~ sex + age_group + wealth + residence + education + ncd_group,
                 design = des, family = quasipoisson(link = "log"))
}, error = function(e) { append_error("SECTION 3 model", conditionMessage(e)); NULL })

tidy_apr <- function(model) {
  if (is.null(model)) return(tibble::tibble(term = character(), apr = numeric(), ci_low = numeric(), ci_high = numeric(), p.value = numeric()))
  broom::tidy(model) %>%
    dplyr::mutate(apr = exp(estimate), ci_low = exp(estimate - 1.96 * std.error), ci_high = exp(estimate + 1.96 * std.error)) %>%
    dplyr::select(term, apr, ci_low, ci_high, p.value)
}
model_untreated_tidy <- tidy_apr(model_untreated)

# ---- 4. Concentration index of the treatment gap by wealth -------------------
ci_untreated <- concentration_index(diagnosed, "untreated")

# ---- save --------------------------------------------------------------------
readr::write_csv(diagnosis_prevalence, file.path(paths$logs_dir, "st11_diagnosis_prevalence.csv"))
readr::write_csv(treatment_gap, file.path(paths$logs_dir, "st11_treatment_gap.csv"))
readr::write_csv(model_untreated_tidy, file.path(paths$logs_dir, "st11_model_untreated.csv"))
readr::write_csv(ci_untreated, file.path(paths$logs_dir, "st11_concentration_index.csv"))

save_rds_output(list(
  diagnosis_prevalence = diagnosis_prevalence,
  treatment_gap = treatment_gap,
  model_untreated = model_untreated_tidy,
  ci_untreated = ci_untreated,
  n_diagnosed = nrow(diagnosed)
), "st11_analysis_main.rds")

append_log(sprintf("ST11 analysis done. Diagnosed-with-treatment-status n = %s.", format(nrow(diagnosed), big.mark = ",")))
message("=== SECTION 3 COMPLETE ===")
