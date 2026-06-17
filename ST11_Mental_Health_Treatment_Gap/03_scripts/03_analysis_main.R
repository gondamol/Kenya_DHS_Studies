# Study: ST11_Mental_Health_Treatment_Gap
# Script: 03_analysis_main.R
# Purpose: Estimate (1) diagnosis prevalence and its social patterning,
#          (2) the depression/anxiety treatment gap and its social patterning,
#          (3) adjusted predictors of both, and (4) wealth-related inequality
#          (concentration indices) for diagnosis vs the treatment gap.

message("=== SECTION 3: Analysis ===")
source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analytic <- readRDS(file.path(paths$derived_dir, "st11_analytic_adults.rds"))
append_log("Running ST11 main analysis.", also_message = TRUE)

# Module sample = adults asked the mental-health module (non-missing classification)
module <- analytic %>% dplyr::filter(!is.na(dx_dep_or_anx))
# Diagnosed subgroup = treatment-gap denominator
diagnosed <- analytic %>% dplyr::filter(dx_dep_or_anx == 1, !is.na(untreated))

# ---- helpers -----------------------------------------------------------------
subgroup_levels <- list(
  sex = c("Women", "Men"),
  age_group = c("15-24", "25-34", "35-49", "50+"),
  residence = c("Urban", "Rural"),
  wealth = c("Poorest", "Poorer", "Middle", "Richer", "Richest"),
  education = c("No education", "Primary", "Secondary", "Higher"),
  insured_lab = c("Insured", "Uninsured"),
  ncd_group = c("0", "1", "2+")
)
section_label <- c(sex = "Sex", age_group = "Age group", residence = "Residence",
                   wealth = "Wealth quintile", education = "Education",
                   insured_lab = "Health insurance", ncd_group = "Physical NCD count")

# prevalence of one outcome across the levels of one grouping variable
prev_by <- function(data, outcome, var) {
  purrr::map_dfr(subgroup_levels[[var]], function(lvl) {
    g <- data %>% dplyr::filter(.data[[var]] == lvl)
    s <- weighted_binary(g, outcome)
    s %>% dplyr::mutate(section = section_label[[var]], level = lvl)
  })
}
prev_all <- function(data, outcome) {
  purrr::map_dfr(names(subgroup_levels), ~prev_by(data, outcome, .x)) %>%
    dplyr::select(section, level, dplyr::everything())
}

# ---- 1. Diagnosis prevalence -------------------------------------------------
diagnosis_overall <- dplyr::bind_rows(
  weighted_binary(module, "dx_depression") %>% dplyr::mutate(metric = "Diagnosed depression"),
  weighted_binary(module, "dx_anxiety") %>% dplyr::mutate(metric = "Diagnosed anxiety"),
  weighted_binary(module, "dx_dep_or_anx") %>% dplyr::mutate(metric = "Diagnosed depression or anxiety")
) %>% dplyr::select(metric, dplyr::everything())

prev_dep <- prev_all(module, "dx_depression")
prev_anx <- prev_all(module, "dx_anxiety")
prev_either <- prev_all(module, "dx_dep_or_anx")

prevalence_by_subgroup <- prev_either %>%
  dplyr::transmute(section, level,
                   n = unweighted_n,
                   either_est = est, either_lo = ci_low, either_hi = ci_high) %>%
  dplyr::left_join(prev_dep %>% dplyr::transmute(section, level, dep_est = est, dep_lo = ci_low, dep_hi = ci_high),
                   by = c("section", "level")) %>%
  dplyr::left_join(prev_anx %>% dplyr::transmute(section, level, anx_est = est, anx_lo = ci_low, anx_hi = ci_high),
                   by = c("section", "level"))

# ---- 2. Treatment gap (untreated among diagnosed) ----------------------------
gap_overall <- weighted_binary(diagnosed, "untreated") %>% dplyr::mutate(section = "Overall", level = "All diagnosed")
treatment_gap <- dplyr::bind_rows(
  gap_overall,
  purrr::map_dfr(names(subgroup_levels), function(v) {
    purrr::map_dfr(subgroup_levels[[v]], function(lvl) {
      g <- diagnosed %>% dplyr::filter(.data[[v]] == lvl)
      weighted_binary(g, "untreated") %>% dplyr::mutate(section = section_label[[v]], level = lvl)
    })
  })
) %>% dplyr::select(section, level, dplyr::everything())

# sensitivity: depression-only and anxiety-only untreated
untreated_dep <- analytic %>% dplyr::filter(dx_depression == 1, !is.na(treat_dep_anx)) %>%
  dplyr::mutate(u = dplyr::if_else(treat_dep_anx == 0, 1L, 0L)) %>% weighted_binary("u")
untreated_anx <- analytic %>% dplyr::filter(dx_anxiety == 1, !is.na(treat_dep_anx)) %>%
  dplyr::mutate(u = dplyr::if_else(treat_dep_anx == 0, 1L, 0L)) %>% weighted_binary("u")

# ---- 3. Adjusted models (diagnosis and treatment gap) ------------------------
prep_factors <- function(df) {
  df %>% dplyr::mutate(
    sex = factor(sex, levels = c("Men", "Women")),
    age_group = factor(age_group, levels = c("15-24", "25-34", "35-49", "50+")),
    wealth = factor(wealth, levels = c("Richest", "Richer", "Middle", "Poorer", "Poorest")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    education = factor(education, levels = c("Higher", "Secondary", "Primary", "No education")),
    ncd_group = factor(dplyr::coalesce(ncd_group, "0"), levels = c("0", "1", "2+")),
    insured_lab = factor(insured_lab, levels = c("Insured", "Uninsured"))
  )
}
covars <- "sex + age_group + wealth + residence + education + ncd_group + insured_lab"

fit_qp <- function(df, outcome, section) {
  d <- df %>% dplyr::filter(!is.na(.data[[outcome]]), !is.na(sex), !is.na(age_group), !is.na(wealth),
                            !is.na(residence), !is.na(education), !is.na(insured_lab)) %>% prep_factors()
  m <- tryCatch(survey::svyglm(stats::as.formula(paste(outcome, "~", covars)),
                               design = make_design(d), family = quasipoisson(link = "log")),
                error = function(e) { append_error(section, conditionMessage(e)); NULL })
  list(model = m, n = nrow(d))
}

tidy_apr <- function(fit) {
  if (is.null(fit$model)) return(tibble::tibble(term = character(), apr = numeric(), ci_low = numeric(), ci_high = numeric(), p.value = numeric()))
  broom::tidy(fit$model) %>%
    dplyr::mutate(apr = exp(estimate), ci_low = exp(estimate - 1.96 * std.error), ci_high = exp(estimate + 1.96 * std.error)) %>%
    dplyr::select(term, apr, ci_low, ci_high, p.value)
}

fit_diag <- fit_qp(module, "dx_dep_or_anx", "SECTION 3 diagnosis model")
fit_untreated <- fit_qp(diagnosed, "untreated", "SECTION 3 untreated model")
model_diagnosis <- tidy_apr(fit_diag)
model_untreated <- tidy_apr(fit_untreated)

# ---- 4. Concentration indices (diagnosis vs treatment gap) --------------------
ci_table <- dplyr::bind_rows(
  concentration_index(module, "dx_dep_or_anx") %>% dplyr::mutate(outcome = "Diagnosis (depression/anxiety)", stratum = "Overall"),
  concentration_index(module %>% dplyr::filter(sex == "Women"), "dx_dep_or_anx") %>% dplyr::mutate(outcome = "Diagnosis (depression/anxiety)", stratum = "Women"),
  concentration_index(module %>% dplyr::filter(sex == "Men"), "dx_dep_or_anx") %>% dplyr::mutate(outcome = "Diagnosis (depression/anxiety)", stratum = "Men"),
  concentration_index(diagnosed, "untreated") %>% dplyr::mutate(outcome = "Treatment gap (untreated)", stratum = "Overall"),
  concentration_index(diagnosed %>% dplyr::filter(sex == "Women"), "untreated") %>% dplyr::mutate(outcome = "Treatment gap (untreated)", stratum = "Women"),
  concentration_index(diagnosed %>% dplyr::filter(sex == "Men"), "untreated") %>% dplyr::mutate(outcome = "Treatment gap (untreated)", stratum = "Men")
) %>% dplyr::select(outcome, stratum, dplyr::everything())

# diagnosis prevalence by wealth (for the contrast figure)
diag_by_wealth <- prev_by(module, "dx_dep_or_anx", "wealth") %>%
  dplyr::transmute(wealth = level, est, lo = ci_low, hi = ci_high, n = unweighted_n)
gap_by_wealth <- treatment_gap %>% dplyr::filter(section == "Wealth quintile") %>%
  dplyr::transmute(wealth = level, est, lo = ci_low, hi = ci_high, n = unweighted_n)
gap_by_insurance <- treatment_gap %>% dplyr::filter(section == "Health insurance") %>%
  dplyr::transmute(insured_lab = level, est, lo = ci_low, hi = ci_high, n = unweighted_n)

# ---- save --------------------------------------------------------------------
readr::write_csv(diagnosis_overall, file.path(paths$logs_dir, "st11_diagnosis_overall.csv"))
readr::write_csv(prevalence_by_subgroup, file.path(paths$logs_dir, "st11_prevalence_by_subgroup.csv"))
readr::write_csv(treatment_gap, file.path(paths$logs_dir, "st11_treatment_gap.csv"))
readr::write_csv(model_diagnosis, file.path(paths$logs_dir, "st11_model_diagnosis.csv"))
readr::write_csv(model_untreated, file.path(paths$logs_dir, "st11_model_untreated.csv"))
readr::write_csv(ci_table, file.path(paths$logs_dir, "st11_concentration_indices.csv"))

save_rds_output(list(
  diagnosis_overall = diagnosis_overall,
  prevalence_by_subgroup = prevalence_by_subgroup,
  treatment_gap = treatment_gap,
  untreated_dep = untreated_dep, untreated_anx = untreated_anx,
  model_diagnosis = model_diagnosis, model_untreated = model_untreated,
  n_diag_model = fit_diag$n, n_untreated_model = fit_untreated$n,
  ci_table = ci_table,
  diag_by_wealth = diag_by_wealth, gap_by_wealth = gap_by_wealth, gap_by_insurance = gap_by_insurance,
  n_module = nrow(module), n_diagnosed = nrow(diagnosed)
), "st11_analysis_main.rds")

append_log(sprintf("ST11 analysis done. Module n = %s; diagnosed n = %s.",
                   format(nrow(module), big.mark = ","), format(nrow(diagnosed), big.mark = ",")))
message("=== SECTION 3 COMPLETE ===")
