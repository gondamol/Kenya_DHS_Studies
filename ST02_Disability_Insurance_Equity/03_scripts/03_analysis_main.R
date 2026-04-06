# Study: ST02_Disability_Insurance_Equity
# Script: 03_analysis_main.R
# Author: Nichodemus Werre Amollo
# Date: 2026-04-05
# Purpose: Produce initial descriptive estimates and core adjusted models for ST02.

message("=== SECTION 3: Initial Analysis ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

analytic_adults <- readRDS(file.path(paths$derived_dir, "st02_analytic_pr_adults.rds"))

append_log("Running initial descriptive analysis for ST02.", also_message = TRUE)

overall_descriptives <- dplyr::bind_rows(
  weighted_binary(analytic_adults, "any_functional_difficulty") %>% dplyr::mutate(metric = "Any functional difficulty"),
  weighted_binary(analytic_adults, "wg_disability") %>% dplyr::mutate(metric = "WG disability threshold"),
  weighted_binary(analytic_adults, "insured_any") %>% dplyr::mutate(metric = "Any insurance"),
  weighted_binary(analytic_adults, "insured_nhif") %>% dplyr::mutate(metric = "NHIF coverage"),
  weighted_binary(analytic_adults, "outpatient_last4w") %>% dplyr::mutate(metric = "Outpatient use in last 4 weeks"),
  weighted_binary(analytic_adults, "inpatient_last12m") %>% dplyr::mutate(metric = "Hospitalisation in last 12 months")
) %>%
  dplyr::select(metric, dplyr::everything())

design_all <- make_design(analytic_adults)

insured_by_disability <- survey::svyby(
  ~insured_any,
  ~wg_disability,
  design_all,
  survey::svymean,
  vartype = c("ci"),
  na.rm = TRUE,
  keep.names = FALSE
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(group = dplyr::if_else(wg_disability == 1, "WG disability threshold", "No WG disability")) %>%
  dplyr::select(group, est = insured_any, ci_low = ci_l, ci_high = ci_u)

outpatient_by_disability <- survey::svyby(
  ~outpatient_last4w,
  ~wg_disability,
  design_all,
  survey::svymean,
  vartype = c("ci"),
  na.rm = TRUE,
  keep.names = FALSE
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(group = dplyr::if_else(wg_disability == 1, "WG disability threshold", "No WG disability")) %>%
  dplyr::select(group, est = outpatient_last4w, ci_low = ci_l, ci_high = ci_u)

payment_by_disability <- analytic_adults %>%
  dplyr::filter(outpatient_last4w == 1) %>%
  make_design() %>%
  survey::svyby(
    ~paid_outpatient_recent,
    ~wg_disability,
    .,
    survey::svymean,
    vartype = c("ci"),
    na.rm = TRUE,
    keep.names = FALSE
  ) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(group = dplyr::if_else(wg_disability == 1, "WG disability threshold", "No WG disability")) %>%
  dplyr::select(group, est = paid_outpatient_recent, ci_low = ci_l, ci_high = ci_u)

severity_coverage <- analytic_adults %>%
  dplyr::filter(!is.na(wg_severity)) %>%
  make_design() %>%
  survey::svyby(
    ~insured_any,
    ~wg_severity,
    .,
    survey::svymean,
    vartype = c("ci"),
    na.rm = TRUE,
    keep.names = FALSE
  ) %>%
  tibble::as_tibble() %>%
  dplyr::rename(est = insured_any, ci_low = ci_l, ci_high = ci_u)

disabled_domain_data <- analytic_adults %>%
  dplyr::filter(wg_disability == 1)

domain_specs <- tibble::tribble(
  ~domain, ~var_name,
  "Seeing", "domain_seeing_threshold",
  "Hearing", "domain_hearing_threshold",
  "Communication", "domain_communication_threshold",
  "Memory", "domain_memory_threshold",
  "Walking", "domain_walking_threshold",
  "Self-care", "domain_selfcare_threshold"
)

domain_coverage <- purrr::map_dfr(seq_len(nrow(domain_specs)), function(i) {
  domain_name <- domain_specs$domain[[i]]
  var_name <- domain_specs$var_name[[i]]
  data_use <- disabled_domain_data %>%
    dplyr::filter(.data[[var_name]] == 1, !is.na(insured_any))

  if (nrow(data_use) == 0) {
    return(tibble::tibble(
      domain = domain_name,
      unweighted_n = 0L,
      est = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }

  estimate <- weighted_binary(data_use, "insured_any")

  tibble::tibble(
    domain = domain_name,
    unweighted_n = nrow(data_use),
    est = estimate$est,
    ci_low = estimate$ci_low,
    ci_high = estimate$ci_high
  )
})

uninsured_model_data <- analytic_adults %>%
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
  dplyr::mutate(
    sex = factor(sex),
    age_group = factor(age_group, levels = c("18-29", "30-44", "45-59", "60+")),
    wealth = factor(wealth, levels = c("Richest", "Richer", "Middle", "Poorer", "Poorest")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    education = factor(education, levels = c("No education", "Primary", "Secondary", "Higher")),
    wg_severity = factor(
      wg_severity,
      levels = c("Moderate functional difficulty", "Severe functional difficulty")
    )
  )

payment_model_data <- analytic_adults %>%
  dplyr::filter(
    wg_disability == 1,
    outpatient_last4w == 1,
    !is.na(paid_outpatient_recent),
    !is.na(insured_any),
    !is.na(sex),
    !is.na(age_group),
    !is.na(wealth),
    !is.na(residence),
    !is.na(education),
    !is.na(wg_severity)
  ) %>%
  dplyr::mutate(
    sex = factor(sex),
    age_group = factor(age_group, levels = c("18-29", "30-44", "45-59", "60+")),
    wealth = factor(wealth, levels = c("Richest", "Richer", "Middle", "Poorer", "Poorest")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    education = factor(education, levels = c("No education", "Primary", "Secondary", "Higher")),
    wg_severity = factor(
      wg_severity,
      levels = c("Moderate functional difficulty", "Severe functional difficulty")
    )
  )

design_uninsured <- make_design(uninsured_model_data)
design_payment <- make_design(payment_model_data)

model_uninsured <- survey::svyglm(
  uninsured ~ sex + age_group + wealth + residence + education + wg_severity,
  design = design_uninsured,
  family = quasipoisson(link = "log")
)

model_payment <- survey::svyglm(
  paid_outpatient_recent ~ insured_any + sex + age_group + wealth + residence + education + wg_severity,
  design = design_payment,
  family = quasipoisson(link = "log")
)

tidy_apr <- function(model) {
  broom::tidy(model) %>%
    dplyr::mutate(
      apr = exp(estimate),
      ci_low = exp(estimate - 1.96 * std.error),
      ci_high = exp(estimate + 1.96 * std.error)
    ) %>%
    dplyr::select(term, apr, ci_low, ci_high, p.value)
}

readr::write_csv(overall_descriptives, file.path(paths$logs_dir, "st02_overall_descriptives.csv"))
readr::write_csv(insured_by_disability, file.path(paths$logs_dir, "st02_insurance_by_disability.csv"))
readr::write_csv(outpatient_by_disability, file.path(paths$logs_dir, "st02_outpatient_by_disability.csv"))
readr::write_csv(payment_by_disability, file.path(paths$logs_dir, "st02_payment_by_disability.csv"))
readr::write_csv(severity_coverage, file.path(paths$logs_dir, "st02_insurance_by_severity.csv"))
readr::write_csv(domain_coverage, file.path(paths$logs_dir, "st02_insurance_by_domain.csv"))
readr::write_csv(tidy_apr(model_uninsured), file.path(paths$logs_dir, "st02_model_uninsured_disabled.csv"))
readr::write_csv(tidy_apr(model_payment), file.path(paths$logs_dir, "st02_model_paid_outpatient_disabled.csv"))

append_log("Initial descriptive outputs and adjusted models saved for ST02.")

message("=== SECTION 3 COMPLETE ===")
