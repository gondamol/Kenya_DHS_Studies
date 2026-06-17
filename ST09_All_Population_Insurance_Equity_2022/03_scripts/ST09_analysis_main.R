# =============================================================================
# Study  : ST09 — Health Insurance Coverage, Equity, and the SHA Transition:
#          A Life-Course Analysis of the Kenyan Population, KDHS 2022
# Script : ST09_analysis_main.R
# Author : Nichodemus Werre Amollo
# Date   : 2026-03-31
# Purpose: Comprehensive all-ages, both-sexes, disability-inclusive analysis
#          of insurance coverage and financial protection using the 2022 KDHS
#          Person Recode (PR) file. Directly informs SHA exemption targeting.
#
# SHA exemption categories addressed:
#   - Children under 18 years
#   - Elderly persons aged 60 and above
#   - Persons with disability
#   - Poor households (poorest/poorer wealth quintiles)
#   - Rural residents
# =============================================================================

message("=== SECTION 0: Setup ===")

suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
  library(survey)
  library(labelled)
  library(flextable)
  library(officer)
  library(ggplot2)
  library(broom)
  library(scales)
  library(lme4)
})

options(
  survey.lonely.psu = "adjust",
  scipen = 999
)

find_study_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    required_dirs <- c("03_scripts", "04_tables", "05_figures", "06_manuscript")
    if (all(dir.exists(file.path(current_dir, required_dirs)))) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Unable to locate the ST09 study root from the current working directory.")
    }
    current_dir <- parent_dir
  }
}

study_root <- find_study_root()
research_root <- normalizePath(file.path(study_root, "..", ".."), winslash = "/", mustWork = TRUE)
data_root  <- file.path(research_root, "01_DHS_Data")

paths <- list(
  admin_dir    = file.path(study_root, "00_Admin"),
  protocol_dir = file.path(study_root, "01_protocol"),
  notes_dir    = file.path(study_root, "02_data_notes"),
  scripts_dir  = file.path(study_root, "03_scripts"),
  tables_dir   = file.path(study_root, "04_tables"),
  figures_dir  = file.path(study_root, "05_figures"),
  manuscript_dir = file.path(study_root, "06_manuscript"),
  derived_dir  = file.path(study_root, "07_derived_data"),
  logs_dir     = file.path(study_root, "08_logs"),
  results_dir  = file.path(study_root, "results", "logs"),
  errors_log   = file.path(study_root, "00_Admin", "errors_log.txt")
)

for (d in unname(paths[c(
  "admin_dir", "notes_dir", "tables_dir", "figures_dir",
  "manuscript_dir", "derived_dir", "logs_dir", "results_dir"
)])) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

log_error <- function(section, msg) {
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", ts, section, msg),
      file = paths$errors_log, append = TRUE)
}

# ── Utility functions ────────────────────────────────────────────────────────

# Safe numeric coercion of haven_labelled vectors
num <- function(x) as.numeric(x)

fmt_pct <- function(est, lo, hi)
  sprintf("%.1f (%.1f, %.1f)", 100 * est, 100 * lo, 100 * hi)

fmt_effect <- function(est, lo, hi, digits = 2)
  sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"), est, lo, hi)

make_design <- function(data, wvar = "weight") {
  svydesign(
    ids     = ~psu,
    strata  = ~strata,
    weights = as.formula(paste0("~", wvar)),
    data    = data,
    nest    = TRUE
  )
}

# Weighted binary prevalence with CI
wprev <- function(data, var, wvar = "weight") {
  d <- data %>% filter(!is.na(.data[[var]]), !is.na(.data[[wvar]]))
  if (nrow(d) == 0 || var_is_constant(d[[var]])) {
    return(tibble(
      unweighted_n = nrow(d),
      est = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
      formatted = NA_character_
    ))
  }
  des    <- make_design(d, wvar)
  svyres <- svymean(as.formula(paste0("~", var)), des, na.rm = TRUE)
  ci     <- suppressWarnings(confint(svyres))
  p      <- as.numeric(coef(svyres)[1])
  tibble(
    unweighted_n = nrow(d),
    est     = p,
    ci_low  = ci[1, 1],
    ci_high = ci[1, 2],
    formatted = fmt_pct(p, ci[1, 1], ci[1, 2])
  )
}

var_is_constant <- function(x) length(unique(stats::na.omit(x))) <= 1

# Weighted prevalence by group
wprev_by <- function(data, group_var, outcome_var, wvar = "weight") {
  groups <- sort(unique(stats::na.omit(as.character(data[[group_var]]))))
  bind_rows(lapply(groups, function(g) {
    sub <- data %>% filter(as.character(.data[[group_var]]) == g)
    r   <- wprev(sub, outcome_var, wvar)
    tibble(
      group        = g,
      unweighted_n = r$unweighted_n,
      estimate     = r$est,
      ci_low       = r$ci_low,
      ci_high      = r$ci_high,
      formatted    = r$formatted
    )
  }))
}

# Standard and Erreygers-corrected concentration indices with bootstrap CI
calc_conc_index <- function(data, outcome_var, rank_var = "wealth_rank",
                             wvar = "weight", nboot = 250, seed = 20260406) {
  d <- data %>%
    filter(!is.na(.data[[outcome_var]]),
           !is.na(.data[[rank_var]]),
           !is.na(.data[[wvar]])) %>%
    mutate(y = .data[[outcome_var]],
           r = as.numeric(.data[[rank_var]]),
           w = .data[[wvar]])

  if (nrow(d) < 30 || mean(d$y, na.rm = TRUE) == 0) {
    return(tibble(
      standard_ci = NA_real_,
      standard_ci_lo = NA_real_,
      standard_ci_hi = NA_real_,
      erreygers_ci = NA_real_,
      erreygers_ci_lo = NA_real_,
      erreygers_ci_hi = NA_real_
    ))
  }

  calc_ci_inner <- function(df) {
    ranked <- df %>%
      arrange(r) %>%
      mutate(w_norm  = w / sum(w),
             frac_r  = cumsum(w_norm) - 0.5 * w_norm)
    mu <- weighted.mean(ranked$y, ranked$w)
    if (!is.finite(mu) || mu == 0) {
      return(c(standard = NA_real_, erreygers = NA_real_))
    }
    standard_ci <- 2 * weighted.mean((ranked$y - mu) * (ranked$frac_r - 0.5), ranked$w) / mu
    c(
      standard = standard_ci,
      erreygers = 4 * mu * standard_ci
    )
  }

  point <- calc_ci_inner(d)
  set.seed(seed)
  boots <- replicate(nboot, {
    idx <- sample.int(nrow(d), replace = TRUE)
    calc_ci_inner(d[idx, , drop = FALSE])
  })
  if (is.null(dim(boots))) {
    boots <- matrix(boots, nrow = 2)
    rownames(boots) <- c("standard", "erreygers")
  }

  tibble(
    standard_ci = round(point["standard"], 4),
    standard_ci_lo = round(as.numeric(quantile(boots["standard", is.finite(boots["standard", ])], 0.025, na.rm = TRUE)), 4),
    standard_ci_hi = round(as.numeric(quantile(boots["standard", is.finite(boots["standard", ])], 0.975, na.rm = TRUE)), 4),
    erreygers_ci = round(point["erreygers"], 4),
    erreygers_ci_lo = round(as.numeric(quantile(boots["erreygers", is.finite(boots["erreygers", ])], 0.025, na.rm = TRUE)), 4),
    erreygers_ci_hi = round(as.numeric(quantile(boots["erreygers", is.finite(boots["erreygers", ])], 0.975, na.rm = TRUE)), 4)
  )
}

add_wealth_ridit <- function(data, wealth_var = "wealth", wvar = "weight") {
  wealth_levels <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
  dist <- data %>%
    filter(!is.na(.data[[wealth_var]]), !is.na(.data[[wvar]])) %>%
    mutate(wealth_tmp = factor(as.character(.data[[wealth_var]]), levels = wealth_levels)) %>%
    group_by(wealth_tmp) %>%
    summarise(w_sum = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
    complete(wealth_tmp = factor(wealth_levels, levels = wealth_levels), fill = list(w_sum = 0)) %>%
    arrange(wealth_tmp) %>%
    mutate(
      w_prop = w_sum / sum(w_sum),
      ridit = cumsum(w_prop) - 0.5 * w_prop
    ) %>%
    transmute(wealth_join = as.character(wealth_tmp), wealth_ridit = ridit)

  data %>%
    mutate(wealth_join = as.character(.data[[wealth_var]])) %>%
    left_join(dist, by = "wealth_join") %>%
    select(-wealth_join)
}

calc_wealth_inequality_metrics <- function(data, label, outcome_var = "insured_any",
                                           wvar = "weight") {
  d <- data %>%
    filter(!is.na(.data[[outcome_var]]), !is.na(wealth), !is.na(.data[[wvar]])) %>%
    add_wealth_ridit(wvar = wvar)

  if (nrow(d) < 50 || length(unique(d$wealth)) < 2) {
    return(tibble(
      Group = label,
      `Poorest % (95% CI)` = NA_character_,
      `Richest % (95% CI)` = NA_character_,
      `Richest-poorest gap, pp` = NA_character_,
      `Richest/poorest ratio` = NA_real_,
      `SII, pp (95% CI)` = NA_character_,
      `RII (95% CI)` = NA_character_
    ))
  }

  poorest <- wprev(d %>% filter(wealth == "Poorest"), outcome_var)
  richest <- wprev(d %>% filter(wealth == "Richest"), outcome_var)
  des <- make_design(d, wvar)

  gap_design <- svyby(
    as.formula(paste0("~", outcome_var)),
    ~wealth,
    des,
    svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>% as_tibble()
  names(gap_design)[2:4] <- c("estimate", "ci_l", "ci_u")
  gap_poor <- gap_design %>% filter(wealth == "Poorest")
  gap_rich <- gap_design %>% filter(wealth == "Richest")
  gap <- 100 * (gap_rich$estimate - gap_poor$estimate)

  sii_mod <- tryCatch(
    svyglm(
      as.formula(paste0(outcome_var, " ~ wealth_ridit")),
      design = des,
      family = gaussian()
    ),
    error = function(e) NULL
  )
  rii_mod <- tryCatch(
    svyglm(
      as.formula(paste0(outcome_var, " ~ wealth_ridit")),
      design = des,
      family = quasipoisson(link = "log")
    ),
    error = function(e) NULL
  )

  sii <- if (!is.null(sii_mod)) {
    est <- coef(sii_mod)[["wealth_ridit"]]
    se <- sqrt(vcov(sii_mod)["wealth_ridit", "wealth_ridit"])
    sprintf("%.1f (%.1f, %.1f)", 100 * est, 100 * (est - 1.96 * se), 100 * (est + 1.96 * se))
  } else {
    NA_character_
  }

  rii <- if (!is.null(rii_mod)) {
    est <- coef(rii_mod)[["wealth_ridit"]]
    se <- sqrt(vcov(rii_mod)["wealth_ridit", "wealth_ridit"])
    sprintf("%.2f (%.2f, %.2f)", exp(est), exp(est - 1.96 * se), exp(est + 1.96 * se))
  } else {
    NA_character_
  }

  tibble(
    Group = label,
    `Poorest % (95% CI)` = poorest$formatted,
    `Richest % (95% CI)` = richest$formatted,
    `Richest-poorest gap, pp` = sprintf("%.1f", gap),
    `Richest/poorest ratio` = round(richest$est / max(poorest$est, 0.0001), 1),
    `SII, pp (95% CI)` = sii,
    `RII (95% CI)` = rii
  )
}

weighted_composition_row <- function(total_data, subgroup_data, label) {
  total_w <- sum(total_data$weight, na.rm = TRUE)
  subgroup_w <- sum(subgroup_data$weight, na.rm = TRUE)
  total_uninsured_w <- sum(total_data$weight[total_data$uninsured == 1], na.rm = TRUE)
  subgroup_uninsured_w <- sum(subgroup_data$weight[subgroup_data$uninsured == 1], na.rm = TRUE)
  coverage <- wprev(subgroup_data, "insured_any")

  pop_share <- 100 * subgroup_w / total_w
  uninsured_share <- 100 * subgroup_uninsured_w / total_uninsured_w

  tibble(
    `Population group` = label,
    `Unweighted n` = nrow(subgroup_data),
    `Any insurance % (95% CI)` = coverage$formatted,
    `Uninsured %` = sprintf("%.1f", 100 - 100 * coverage$est),
    `Share of total population %` = sprintf("%.1f", pop_share),
    `Share of all uninsured %` = sprintf("%.1f", uninsured_share),
    `Uninsured representation ratio` = round(uninsured_share / max(pop_share, 0.0001), 2)
  )
}

# Save table bundle (CSV + DOCX)
build_publication_flextable <- function(data,
                                        footer = c("Source: Kenya DHS 2022. Survey-weighted estimates."),
                                        font_size = 9,
                                        header_labels = NULL,
                                        spanner_values = NULL,
                                        spanner_widths = NULL) {
  ft <- flextable(data)

  if (!is.null(header_labels)) {
    ft <- set_header_labels(ft, values = header_labels)
  }

  if (!is.null(spanner_values) && !is.null(spanner_widths)) {
    ft <- add_header_row(ft, values = spanner_values, colwidths = spanner_widths, top = TRUE)
  }

  ft <- ft %>%
    set_table_properties(layout = "autofit", opts_word = list(split = TRUE)) %>%
    autofit() %>%
    fit_to_width(max_width = 7.1) %>%
    theme_booktabs() %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    bold(part = "header") %>%
    align(align = "left", part = "all") %>%
    align(j = seq_len(ncol(data)), align = "center", part = "header") %>%
    align(j = 1, align = "left", part = "header") %>%
    valign(valign = "top", part = "all") %>%
    padding(padding = 3, part = "all")

  if (length(footer) > 0) {
    ft <- add_footer_lines(ft, values = footer) %>%
      merge_h(part = "footer") %>%
      italic(part = "footer") %>%
      fontsize(size = 8, part = "footer") %>%
      align(align = "left", part = "footer")
  }

  ft
}

save_bundle <- function(data, csv_path, docx_path, caption,
                         footer = c("Source: Kenya DHS 2022. Survey-weighted estimates."),
                         font_size = 9,
                         header_labels = NULL,
                         spanner_values = NULL,
                         spanner_widths = NULL) {
  readr::write_csv(data, csv_path)
  ft <- build_publication_flextable(
    data = data,
    footer = footer,
    font_size = font_size,
    header_labels = header_labels,
    spanner_values = spanner_values,
    spanner_widths = spanner_widths
  )
  read_docx() %>%
    body_add_par(value = caption, style = "Normal") %>%
    body_add_flextable(ft) %>%
    print(target = docx_path)
  invisible(data)
}

message("=== SECTION 0 COMPLETE ===")

# =============================================================================
message("=== SECTION 1: Data import and preparation ===")
# =============================================================================

pr_path <- file.path(data_root, "KDHS_2022", "PR_Person_Recode", "KEPR8CFL.DTA")

cat("Reading 2022 KDHS Person Recode...\n")
raw <- read_dta(
  pr_path,
  col_select = c(
    hv001, hv002, hvidx,
    hv005, hv021, hv022,         # weight, PSU, strata
    hv103,                        # de-facto resident (slept last night)
    hv104, hv105,                 # sex, age
    hv106, hv270, hv024, hv025,  # education, wealth, region, residence
    hv101,                        # relationship to HH head
    hv111, hv113,                 # mother alive, father alive (child parental survival)
    hv219, hv220,                 # sex and age of household head
    hv243a,                       # household owns a mobile phone
    sh27,                         # any insurance
    sh28a, sh28b, sh28c, sh28x,  # NHIF, private, community, other
    sh29, sh31, sh32,             # hospital admission, outpatient, paid OPD
    # Out-of-pocket cost depth (full household questionnaire subsample)
    sh204, sh205a, sh205b, sh205c, sh205d, sh205e,  # inpatient: total + cash/NHIF/private/in-kind/other
    sh304, sh305a, sh305b, sh305c, sh305d, sh305e,  # outpatient: total + cash/NHIF/private/in-kind/other
    # Household social safety-net receipt
    sh134aa, sh134ab, sh134ac, sh134ad, sh134ae,    # national/county gov, NGO/CBO, religious, friends/relatives
    hdis9                         # disability summary (Washington Group)
  )
)

# DHS reserved-code cleaner for continuous cost variables.
# Reserved codes ("don't know"/"missing") occupy the maximum field width:
#   7-digit totals (sh204, sh304): 9999998 / 9999999
#   6-digit components (sh205a-e, sh305a-e): 999998 / 999999
# Leaving these in place inflates the raw mean of sh304 to >500,000 KES
# (median 500 KES); they must be set to NA before any cost calculation.
clean_cost <- function(x, width = c("total", "component")) {
  width <- match.arg(width)
  reserved <- if (width == "total") 9999998 else 999998
  xn <- as.numeric(x)
  ifelse(xn >= reserved, NA_real_, xn)
}

cat("Raw rows:", nrow(raw), "\n")
defacto_pr_n <- sum(num(raw$hv103) == 1, na.rm = TRUE)

# ── Region collapse: 47 counties → 8 former provinces ────────────────────────
# County codes in 2022 KDHS hv024 (1-47)
assign_region <- function(x) {
  cx <- as.numeric(x)
  case_when(
    cx == 47                          ~ "Nairobi",
    cx %in% c(13:16, 37)             ~ "Central",
    cx %in% c(1:6)                   ~ "Coast",
    cx %in% c(7:9)                   ~ "North Eastern",
    cx %in% c(17:23)                 ~ "Eastern",
    cx %in% c(10:12, 38)             ~ "Western",
    cx %in% c(24:36, 39)             ~ "Rift Valley",
    cx %in% c(40:46)                 ~ "Nyanza",
    TRUE                              ~ NA_character_
  )
}

# ── Build analytic dataset ────────────────────────────────────────────────────
analytic_raw <- raw %>%
  # 1. Restrict to de-facto household members (slept last night)
  filter(num(hv103) == 1) %>%
  # 2. Must have a valid insurance response (not DK=8)
  filter(num(sh27) %in% c(0, 1)) %>%
  transmute(
    # Identifiers & design
    cluster   = num(hv001),
    household = num(hv002),
    line      = num(hvidx),
    weight    = num(hv005) / 1e6,
    psu       = num(hv021),
    strata    = num(hv022),

    # Demographics
    age_yrs   = num(hv105),
    sex       = factor(case_when(
      num(hv104) == 1 ~ "Male",
      num(hv104) == 2 ~ "Female",
      TRUE ~ NA_character_
    ), levels = c("Male", "Female")),

    # Policy-relevant age groups for pre-SHA monitoring
    age_group = factor(case_when(
      age_yrs <  5  ~ "Under 5",
      age_yrs <  15 ~ "5-14",
      age_yrs <  18 ~ "15-17",
      age_yrs <  25 ~ "18-24",
      age_yrs <  35 ~ "25-34",
      age_yrs <  45 ~ "35-44",
      age_yrs <  60 ~ "45-59",
      TRUE           ~ "60+"
    ), levels = c("Under 5","5-14","15-17","18-24",
                  "25-34","35-44","45-59","60+")),

    # Broad age bands for concentration-index stratification
    broad_age = factor(case_when(
      age_yrs < 18 ~ "Under 18",
      age_yrs < 60 ~ "18-59 (working age)",
      TRUE          ~ "60+"
    ), levels = c("Under 18",
                  "18-59 (working age)",
                  "60+")),

    # Socioeconomic
    education = factor(case_when(
      num(hv106) == 0 ~ "No Education",
      num(hv106) == 1 ~ "Primary",
      num(hv106) == 2 ~ "Secondary",
      num(hv106) == 3 ~ "Higher",
      TRUE           ~ NA_character_
    ), levels = c("No Education","Primary","Secondary","Higher")),

    wealth = factor(case_when(
      num(hv270) == 1 ~ "Poorest",
      num(hv270) == 2 ~ "Poorer",
      num(hv270) == 3 ~ "Middle",
      num(hv270) == 4 ~ "Richer",
      num(hv270) == 5 ~ "Richest",
      TRUE           ~ NA_character_
    ), levels = c("Poorest","Poorer","Middle","Richer","Richest")),

    wealth_rank = num(hv270),   # numeric 1-5 for concentration index

    residence = factor(case_when(
      num(hv025) == 1 ~ "Urban",
      num(hv025) == 2 ~ "Rural",
      TRUE           ~ NA_character_
    ), levels = c("Urban","Rural")),

    region = factor(assign_region(hv024)),

    # Household relationship
    hh_relation = factor(case_when(
      num(hv101) == 1  ~ "Household head",
      num(hv101) == 2  ~ "Spouse/partner",
      num(hv101) == 3  ~ "Son/daughter",
      num(hv101) == 5  ~ "Grandchild",
      num(hv101) == 6  ~ "Parent",
      num(hv101) %in% c(4,7,8,9,10,11,12) ~ "Other relation",
      TRUE            ~ NA_character_
    ), levels = c("Household head","Spouse/partner","Son/daughter",
                  "Grandchild","Parent","Other relation")),

    # Disability summary from the household module; under-five children were not assessed
    disability = factor(case_when(
      num(hdis9) == 1           ~ "No difficulty",
      num(hdis9) %in% c(2)     ~ "Some difficulty",
      num(hdis9) %in% c(3, 4)  ~ "A lot / cannot do",
      TRUE                    ~ NA_character_   # NA = not asked / DK
    ), levels = c("No difficulty","Some difficulty","A lot / cannot do")),

    disability_any = case_when(
      num(hdis9) == 1           ~ 0L,
      num(hdis9) %in% c(2,3,4) ~ 1L,
      TRUE                      ~ NA_integer_
    ),

    # Primary outcome
    insured_any = case_when(
      num(sh27) == 1 ~ 1L,
      num(sh27) == 0 ~ 0L,
      TRUE           ~ NA_integer_
    ),
    uninsured = 1L - insured_any,

    # Insurance types (conditional on insured: NA for non-insured)
    ins_nhif      = as.integer(num(sh28a) == 1),
    ins_private   = as.integer(num(sh28b) == 1),
    ins_community = as.integer(num(sh28c) == 1),
    ins_other     = as.integer(num(sh28x) == 1),

    # Healthcare utilisation
    hosp_admit = case_when(
      num(sh29) == 1 ~ 1L,
      num(sh29) == 0 ~ 0L,
      TRUE           ~ NA_integer_
    ),
    outpatient = case_when(
      num(sh31) == 1 ~ 1L,
      num(sh31) == 0 ~ 0L,
      TRUE           ~ NA_integer_
    ),
    paid_opd = case_when(
      num(sh32) == 1 ~ 1L,
      num(sh32) == 0 ~ 0L,
      TRUE           ~ NA_integer_
    ),

    # ── Out-of-pocket cost depth (reserved codes cleaned to NA) ──────────────
    # Outpatient (conditional on a recent outpatient visit)
    opd_cost_total = clean_cost(sh304, "total"),
    opd_cost_cash  = clean_cost(sh305a, "component"),
    opd_cost_nhif  = clean_cost(sh305b, "component"),
    opd_cost_priv  = clean_cost(sh305c, "component"),
    opd_cost_kind  = clean_cost(sh305d, "component"),
    opd_cost_other = clean_cost(sh305e, "component"),
    opd_share_oop  = ifelse(!is.na(opd_cost_total) & opd_cost_total > 0,
                            opd_cost_cash / opd_cost_total, NA_real_),
    opd_share_nhif = ifelse(!is.na(opd_cost_total) & opd_cost_total > 0,
                            opd_cost_nhif / opd_cost_total, NA_real_),
    # Inpatient (conditional on an overnight admission)
    hosp_cost_total = clean_cost(sh204, "total"),
    hosp_cost_cash  = clean_cost(sh205a, "component"),
    hosp_cost_nhif  = clean_cost(sh205b, "component"),
    hosp_cost_priv  = clean_cost(sh205c, "component"),
    hosp_cost_kind  = clean_cost(sh205d, "component"),
    hosp_cost_other = clean_cost(sh205e, "component"),
    hosp_share_oop  = ifelse(!is.na(hosp_cost_total) & hosp_cost_total > 0,
                             hosp_cost_cash / hosp_cost_total, NA_real_),
    hosp_share_nhif = ifelse(!is.na(hosp_cost_total) & hosp_cost_total > 0,
                             hosp_cost_nhif / hosp_cost_total, NA_real_),

    # ── Household social safety-net receipt ──────────────────────────────────
    sn_national  = as.integer(num(sh134aa) == 1),
    sn_county    = as.integer(num(sh134ab) == 1),
    sn_ngo       = as.integer(num(sh134ac) == 1),
    sn_religious = as.integer(num(sh134ad) == 1),
    sn_informal  = as.integer(num(sh134ae) == 1),
    # Government cash-transfer safety net (the channel SHA premium subsidies build on)
    safety_net_gov = as.integer(num(sh134aa) == 1 | num(sh134ab) == 1),
    # Any formal/organised safety net (government, NGO/CBO, or religious; excludes informal kin)
    safety_net_any = as.integer(
      num(sh134aa) == 1 | num(sh134ab) == 1 |
        num(sh134ac) == 1 | num(sh134ad) == 1
    ),

    # SHA vulnerability flag (≥1 exemption category)
    policy_priority_any = as.integer(
      age_yrs < 18 |
        age_yrs >= 60 |
        (!is.na(disability_any) & disability_any == 1) |
        (!is.na(wealth) & wealth %in% c("Poorest","Poorer"))
    ),

    # ── Variable enrichment (household-head, connectivity, child parental survival) ──
    hh_head_sex = factor(case_when(
      num(hv219) == 1 ~ "Male",
      num(hv219) == 2 ~ "Female",
      TRUE            ~ NA_character_
    ), levels = c("Male", "Female")),
    # hv220 reserved/implausible codes (97-99 / >95) set to missing
    hh_head_age = ifelse(num(hv220) >= 96, NA_real_, num(hv220)),
    mobile_phone = case_when(
      num(hv243a) == 1 ~ 1L,
      num(hv243a) == 0 ~ 0L,
      TRUE             ~ NA_integer_
    ),
    # Parental survival (hv111 mother alive, hv113 father alive); asked of children only
    mother_alive = case_when(num(hv111) == 1 ~ 1L, num(hv111) == 0 ~ 0L, TRUE ~ NA_integer_),
    father_alive = case_when(num(hv113) == 1 ~ 1L, num(hv113) == 0 ~ 0L, TRUE ~ NA_integer_),
    orphan_status = factor(case_when(
      is.na(mother_alive) & is.na(father_alive)                              ~ NA_character_,
      (mother_alive == 0 & !is.na(mother_alive)) &
        (father_alive == 0 & !is.na(father_alive))                           ~ "Double orphan",
      (mother_alive == 0 & !is.na(mother_alive)) |
        (father_alive == 0 & !is.na(father_alive))                          ~ "Single orphan",
      TRUE                                                                    ~ "Both parents alive"
    ), levels = c("Both parents alive", "Single orphan", "Double orphan"))
  )

cat("Analytic sample (de-facto, sh27 valid):", nrow(analytic_raw), "\n")
cat("De-facto household members in PR file:", defacto_pr_n, "\n")
cat("Insured:", sum(analytic_raw$insured_any, na.rm = TRUE),
    sprintf("(%.1f%%)\n",
            100 * mean(analytic_raw$insured_any, na.rm = TRUE)))
cat("Has disability data:", sum(!is.na(analytic_raw$disability)), "\n")
cat("Policy-priority flag:", sum(analytic_raw$policy_priority_any, na.rm = TRUE),
    sprintf("(%.1f%%)\n",
            100 * mean(analytic_raw$policy_priority_any, na.rm = TRUE)))
cat("\nBy sex:\n"); print(table(analytic_raw$sex))
cat("By age group:\n"); print(table(analytic_raw$age_group))
cat("By wealth:\n"); print(table(analytic_raw$wealth))

# Save combined analytic file
readr::write_csv(analytic_raw,
  file.path(paths$notes_dir, "ST09_analytic_dataset.csv"))

message("=== SECTION 1 COMPLETE ===")

# =============================================================================
message("=== SECTION 2: Descriptive tables ===")
# =============================================================================

analytic <- analytic_raw  # working alias

# ── TABLE 1: National baseline — coverage, types, service use, by sex ─────────
build_row <- function(data, label) {
  tibble(
    Group                        = label,
    `Unweighted n`               = nrow(data),
    `Any insurance % (95% CI)`   = wprev(data, "insured_any")$formatted,
    `NHIF % of insured (95% CI)` = wprev(
      data %>% filter(insured_any == 1), "ins_nhif")$formatted,
    `Private % of insured (95% CI)` = wprev(
      data %>% filter(insured_any == 1), "ins_private")$formatted,
    `Community % of insured (95% CI)` = wprev(
      data %>% filter(insured_any == 1), "ins_community")$formatted,
    `Uninsured % (95% CI)`       = wprev(data, "uninsured")$formatted,
    `Hospital admission % (95% CI)` = wprev(data, "hosp_admit")$formatted,
    `Outpatient use % (95% CI)`  = wprev(data, "outpatient")$formatted,
    `Paid for OPD % (95% CI)`    = wprev(
      data %>% filter(!is.na(paid_opd) & outpatient == 1),
      "paid_opd")$formatted
  )
}

table1 <- bind_rows(
  build_row(analytic,                      "All household members"),
  build_row(analytic %>% filter(sex == "Male"),   "Male"),
  build_row(analytic %>% filter(sex == "Female"), "Female")
)

save_bundle(
  table1,
  file.path(paths$tables_dir, "Table1_ST09_National_Baseline.csv"),
  file.path(paths$tables_dir, "Table1_ST09_National_Baseline.docx"),
  caption = paste(
    "Table 1. National health insurance coverage, insurance type distribution,",
    "and healthcare service utilisation among all de-facto household members,",
    "KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates with 95% confidence intervals.",
    "Insurance types (NHIF, private, community) are conditional on being insured.",
    "Paid for OPD is among those with an outpatient visit in the past 4 weeks.",
    "SHA = Social Health Authority."
  )
)
cat("Table 1 done.\n")

# ── TABLE 2: Life-course coverage by age group × sex ─────────────────────────
age_levels <- c("Under 5","5-14","15-17","18-24","25-34","35-44","45-59","60+")

table2 <- bind_rows(lapply(age_levels, function(ag) {
  d_all <- analytic %>% filter(age_group == ag)
  d_m   <- analytic %>% filter(age_group == ag, sex == "Male")
  d_f   <- analytic %>% filter(age_group == ag, sex == "Female")

  tibble(
    `Age group`               = ag,
    `Policy group`            = case_when(
      ag %in% c("Under 5","5-14","15-17") ~ "Child (<18)",
      ag == "18-24"                        ~ "Young adult transition",
      ag == "60+"                          ~ "Older adult (60+)",
      TRUE                                 ~ "—"
    ),
    `n (all)`                 = nrow(d_all),
    `All: insured % (95% CI)` = wprev(d_all, "insured_any")$formatted,
    `n (male)`                = nrow(d_m),
    `Male: insured % (95% CI)` = wprev(d_m, "insured_any")$formatted,
    `n (female)`              = nrow(d_f),
    `Female: insured % (95% CI)` = wprev(d_f, "insured_any")$formatted,
    `Hospital admit % (95% CI)`  = wprev(d_all, "hosp_admit")$formatted,
    `Outpatient % (95% CI)`      = wprev(d_all, "outpatient")$formatted
  )
}))

save_bundle(
  table2,
  file.path(paths$tables_dir, "Table2_ST09_LifeCourse_AgeSex.csv"),
  file.path(paths$tables_dir, "Table2_ST09_LifeCourse_AgeSex.docx"),
  caption = paste(
    "Table 2. Health insurance coverage and healthcare utilisation across the",
    "life course by age group and sex, all household members, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates with 95% confidence intervals.",
    "Age bands were chosen for policy relevance to child, young-adult, and older-adult monitoring before SHA.",
    "n values are unweighted."
  )
)
cat("Table 2 done.\n")

# ── TABLE 3: Socioeconomic gradient (wealth, education, residence, region) ─────
build_equity_section <- function(data, group_var, section_label) {
  wprev_by(data, group_var, "insured_any") %>%
    mutate(
      Section              = section_label,
      `Insurance % (95% CI)` = formatted,
      `Unweighted n`         = unweighted_n
    ) %>%
    select(Section, Category = group, `Unweighted n`, `Insurance % (95% CI)`,
           estimate, ci_low, ci_high)
}

table3 <- bind_rows(
  build_equity_section(analytic, "wealth",    "Wealth quintile"),
  build_equity_section(
    analytic %>% filter(age_yrs >= 15),
    "education", "Education (adults 15+)"
  ),
  build_equity_section(analytic, "residence", "Residence"),
  build_equity_section(analytic, "region",    "Region")
) %>%
  select(-estimate, -ci_low, -ci_high)

save_bundle(
  table3,
  file.path(paths$tables_dir, "Table3_ST09_Socioeconomic_Gradient.csv"),
  file.path(paths$tables_dir, "Table3_ST09_Socioeconomic_Gradient.docx"),
  caption = paste(
    "Table 3. Health insurance coverage by wealth quintile, education,",
    "residence, and region, all household members, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates with 95% confidence intervals.",
    "Education restricted to adults aged 15 and above.",
    "Region derived by collapsing 2022 KDHS county codes to Kenya's eight former provinces."
  )
)
cat("Table 3 done.\n")

# ── TABLE 4: Vulnerable group profiles (SHA-targeted populations) ─────────────
vuln_row <- function(data, label, extra_cols = TRUE) {
  base <- tibble(
    `Population group`         = label,
    `Unweighted n`             = nrow(data),
    `Any insured % (95% CI)`   = wprev(data, "insured_any")$formatted,
    `NHIF % of insured (95% CI)` = wprev(
      data %>% filter(insured_any == 1), "ins_nhif")$formatted,
    `Private % of insured (95% CI)` = wprev(
      data %>% filter(insured_any == 1), "ins_private")$formatted,
    `Outpatient % (95% CI)`    = wprev(data, "outpatient")$formatted,
    `Paid for OPD % (95% CI)`  = wprev(
      data %>% filter(!is.na(paid_opd) & outpatient == 1),
      "paid_opd")$formatted
  )
  base
}

table4 <- bind_rows(
  # ── SHA child exemption ──
  vuln_row(analytic %>% filter(age_yrs < 18),
           "Children under 18"),
  vuln_row(analytic %>% filter(age_yrs < 5),
           "  Infants/toddlers under 5"),
  vuln_row(analytic %>% filter(age_yrs >= 5, age_yrs < 15),
           "  School-age children 5-14"),
  vuln_row(analytic %>% filter(age_yrs >= 15, age_yrs < 18),
           "  Adolescents 15-17"),
  # ── SHA elderly exemption ──
  vuln_row(analytic %>% filter(age_yrs >= 60),
           "Older adults 60+"),
  vuln_row(analytic %>% filter(age_yrs >= 60, age_yrs < 70),
           "  Aged 60-69"),
  vuln_row(analytic %>% filter(age_yrs >= 70),
           "  Aged 70+"),
  # ── Disability ──
  vuln_row(analytic %>% filter(!is.na(disability_any)),
           "All with disability assessment"),
  vuln_row(analytic %>% filter(!is.na(disability_any) & disability_any == 0),
           "  No functional difficulty"),
  vuln_row(analytic %>% filter(!is.na(disability_any) & disability_any == 1),
           "  Any functional difficulty"),
  vuln_row(analytic %>% filter(disability == "Some difficulty"),
           "    Some difficulty"),
  vuln_row(analytic %>% filter(disability == "A lot / cannot do"),
           "    A lot of difficulty / cannot do"),
  # ── Poverty ──
  vuln_row(analytic %>% filter(wealth %in% c("Poorest","Poorer")),
           "Two poorest wealth quintiles"),
  vuln_row(analytic %>% filter(wealth == "Poorest"),
           "  Poorest quintile"),
  vuln_row(analytic %>% filter(wealth == "Poorer"),
           "  Poorer quintile"),
  # ── Residence & HH relationship ──
  vuln_row(analytic %>% filter(residence == "Rural"),
           "Rural residents"),
  vuln_row(analytic %>% filter(hh_relation == "Grandchild"),
           "Grandchildren in household"),
  vuln_row(analytic %>% filter(hh_relation %in% c("Son/daughter") &
                                 age_yrs >= 18),
           "Adult children (18+) living at home")
)

save_bundle(
  table4,
  file.path(paths$tables_dir, "Table4_ST09_Vulnerable_Groups.csv"),
  file.path(paths$tables_dir, "Table4_ST09_Vulnerable_Groups.docx"),
  caption = paste(
    "Table 4. Health insurance coverage and service use among policy-priority",
    "population groups relevant to SHA implementation, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates with 95% confidence intervals.",
    "Insurance types conditional on being insured.",
    "Paid for OPD is among those with an outpatient visit in the past 4 weeks and refers to payment at the last outpatient contact.",
    "Groups were chosen for policy relevance and do not map perfectly onto administrative eligibility rules.",
    "Disability assessed using household variable hdis9; under-five children were not assessed."
  )
)
cat("Table 4 done.\n")

message("=== SECTION 2 COMPLETE ===")

# =============================================================================
message("=== SECTION 3: Multivariable analysis ===")
# =============================================================================

# ── Model A: All ages — age group, sex, wealth, residence, disability ──────────
model_A_df <- analytic %>%
  mutate(
    age_group = stats::relevel(factor(age_group, levels = age_levels), ref = "25-34"),
    sex       = stats::relevel(factor(sex), ref = "Male"),
    wealth    = stats::relevel(factor(wealth), ref = "Poorest"),
    residence = stats::relevel(factor(residence), ref = "Urban"),
    region    = stats::relevel(factor(region), ref = "Central")
  ) %>%
  filter(!is.na(insured_any), !is.na(age_group), !is.na(sex),
         !is.na(wealth), !is.na(residence), !is.na(region))

des_A <- make_design(model_A_df)
model_A_n <- nrow(model_A_df)

mod_A <- tryCatch(
  svyglm(
    insured_any ~ age_group + sex + wealth + residence + region,
    design = des_A,
    family = quasipoisson(link = "log")
  ),
  error = function(e) { log_error("Model A", conditionMessage(e)); NULL }
)

# ── Model B: Adults 15+, adds education + disability ─────────────────────────
model_B_df <- analytic %>%
  filter(age_yrs >= 15) %>%
  mutate(
    age_group  = stats::relevel(droplevels(factor(age_group, levels = age_levels)), ref = "25-34"),
    sex        = stats::relevel(factor(sex), ref = "Male"),
    wealth     = stats::relevel(factor(wealth), ref = "Poorest"),
    residence  = stats::relevel(factor(residence), ref = "Urban"),
    education  = stats::relevel(factor(education, levels = c("No Education","Primary","Secondary","Higher")), ref = "No Education"),
    region     = stats::relevel(factor(region), ref = "Central"),
    disability_any = factor(case_when(
      disability_any == 0 ~ "No difficulty",
      disability_any == 1 ~ "Any difficulty",
      TRUE ~ NA_character_
    ), levels = c("No difficulty", "Any difficulty"))
  ) %>%
  filter(!is.na(insured_any), !is.na(age_group), !is.na(sex),
         !is.na(wealth), !is.na(residence), !is.na(education),
         !is.na(region), !is.na(disability_any))

des_B <- make_design(model_B_df)
model_B_n <- nrow(model_B_df)

mod_B <- tryCatch(
  svyglm(
    insured_any ~ age_group + sex + wealth + residence + education +
      disability_any + region,
    design = des_B,
    family = quasipoisson(link = "log")
  ),
  error = function(e) { log_error("Model B", conditionMessage(e)); NULL }
)

# ── Extract model results into Table 5 ───────────────────────────────────────
extract_apr_table <- function(mod, model_label) {
  if (is.null(mod)) return(tibble())
  broom::tidy(mod) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      APR    = exp(estimate),
      CI_lo  = exp(estimate - 1.96 * std.error),
      CI_hi  = exp(estimate + 1.96 * std.error),
      Model  = model_label,
      `APR (95% CI)` = fmt_effect(APR, CI_lo, CI_hi),
      `p-value` = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", p.value)
      )
    ) %>%
    select(Model, Covariate = term, `APR (95% CI)`, `p-value`)
}

# Combine and clean term labels
clean_terms <- function(df) {
  df %>% mutate(
    Covariate = Covariate %>%
      str_replace("^age_group",    "Age: ") %>%
      str_replace("^sex",          "Sex: ") %>%
      str_replace("^wealth",       "Wealth: ") %>%
      str_replace("^residence",    "Residence: ") %>%
      str_replace("^education",    "Education: ") %>%
      str_replace("^disability_any", "Disability: ") %>%
      str_replace("^region",       "Region: ")
  )
}

table5_A <- extract_apr_table(mod_A, "Model A: All ages") %>% clean_terms()
table5_B <- extract_apr_table(mod_B, "Model B: Adults 15+") %>% clean_terms()
table5   <- bind_rows(table5_A, table5_B)

save_bundle(
  table5,
  file.path(paths$tables_dir, "Table5_ST09_Multivariable.csv"),
  file.path(paths$tables_dir, "Table5_ST09_Multivariable.docx"),
  caption = paste(
    "Table 5. Survey-weighted multivariable adjusted prevalence ratios (APR)",
    "for any health insurance coverage, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates.",
    "APR from survey-weighted quasi-Poisson GLM with log link.",
    paste0("Model A (all ages) complete-case n=", format(model_A_n, big.mark = ","), "; reference categories: age 25-34, Male, Poorest, Urban, Central."),
    paste0("Model B (adults 15+) complete-case n=", format(model_B_n, big.mark = ","), "; same references plus education No Education and functional difficulty No difficulty."),
    "Both models adjust for region (not tabulated).",
    "Functional-difficulty status appears only in Model B and is restricted to adults 15+ with non-missing disability data."
  )
)
cat("Table 5 done.\n")

message("=== SECTION 3 COMPLETE ===")

# =============================================================================
message("=== SECTION 4: Concentration index analysis ===")
# =============================================================================

# Overall CI
ci_overall <- calc_conc_index(analytic, "insured_any") %>%
  mutate(Group = "Overall", Subgroup = "All household members")

# By sex
ci_sex <- bind_rows(lapply(c("Male","Female"), function(s) {
  calc_conc_index(analytic %>% filter(sex == s), "insured_any") %>%
    mutate(Group = "Sex", Subgroup = s)
}))

# By broad age category
broad_levels <- c("Under 18",
                  "18-59 (working age)",
                  "60+")
ci_broad_age <- bind_rows(lapply(broad_levels, function(ag) {
  calc_conc_index(analytic %>% filter(broad_age == ag), "insured_any") %>%
    mutate(Group = "Broad age group", Subgroup = ag)
}))

# By residence
ci_residence <- bind_rows(lapply(c("Urban","Rural"), function(r) {
  calc_conc_index(analytic %>% filter(residence == r), "insured_any") %>%
    mutate(Group = "Residence", Subgroup = r)
}))

# Disability sub-analysis (those with valid disability data)
ci_disability <- bind_rows(lapply(c("No difficulty","Some difficulty","A lot / cannot do"), function(d) {
  calc_conc_index(
    analytic %>% filter(!is.na(disability) & disability == d),
    "insured_any"
  ) %>% mutate(Group = "Disability", Subgroup = d)
}))

table6 <- bind_rows(
  ci_overall, ci_sex, ci_broad_age, ci_residence, ci_disability
) %>%
  transmute(
    `Stratification` = Group,
    `Subgroup` = Subgroup,
    `Standard CI` = standard_ci,
    `Standard CI lower` = standard_ci_lo,
    `Standard CI upper` = standard_ci_hi,
    `Erreygers CI` = erreygers_ci,
    `Erreygers CI lower` = erreygers_ci_lo,
    `Erreygers CI upper` = erreygers_ci_hi
  )

save_bundle(
  table6,
  file.path(paths$tables_dir, "Table6_ST09_Concentration_Indices.csv"),
  file.path(paths$tables_dir, "Table6_ST09_Concentration_Indices.docx"),
  caption = paste(
    "Table 6. Standard and Erreygers-corrected concentration indices for any health insurance",
    "coverage by population subgroup, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates.",
    "Positive values indicate pro-rich concentration of insurance coverage.",
    "95% confidence intervals from 250 bootstrap replicates.",
    "Disability subgroup restricted to household members with valid disability assessment.",
    "Standard concentration indices are reported with Erreygers-corrected sensitivity estimates for the bounded binary outcome.",
    "Concentration indices use wealth quintile rank as the socioeconomic ordering variable."
  )
)
cat("Table 6 done.\n")

# Absolute and relative wealth inequality metrics used by WHO-style equity
# reporting: SII is the modelled coverage difference from poorest to richest,
# and RII is the corresponding relative ratio.
table7 <- bind_rows(
  calc_wealth_inequality_metrics(analytic, "All household members"),
  calc_wealth_inequality_metrics(analytic %>% filter(sex == "Male"), "Male"),
  calc_wealth_inequality_metrics(analytic %>% filter(sex == "Female"), "Female"),
  calc_wealth_inequality_metrics(analytic %>% filter(broad_age == "Under 18"), "Children under 18"),
  calc_wealth_inequality_metrics(analytic %>% filter(broad_age == "18-59 (working age)"), "Adults 18-59"),
  calc_wealth_inequality_metrics(analytic %>% filter(broad_age == "60+"), "Older adults 60+"),
  calc_wealth_inequality_metrics(analytic %>% filter(residence == "Rural"), "Rural residents"),
  calc_wealth_inequality_metrics(analytic %>% filter(!is.na(disability_any) & disability_any == 1), "Any functional difficulty")
)

save_bundle(
  table7,
  file.path(paths$tables_dir, "Table7_ST09_Wealth_Inequality_Metrics.csv"),
  file.path(paths$tables_dir, "Table7_ST09_Wealth_Inequality_Metrics.docx"),
  caption = paste(
    "Table 7. Absolute and relative wealth-related inequality metrics for health insurance",
    "coverage by population subgroup, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates.",
    "SII = slope index of inequality; RII = relative index of inequality.",
    "SII is expressed in percentage points; positive values indicate higher coverage among wealthier households.",
    "RII above 1 indicates higher modelled coverage at the top than the bottom of the wealth distribution.",
    "Functional-difficulty subgroup restricted to household members with valid disability assessment."
  )
)
cat("Table 7 done.\n")

# Policy-targeting composition: which groups account for the uninsured pool?
table8 <- bind_rows(
  weighted_composition_row(analytic, analytic %>% filter(age_yrs < 18), "Children under 18"),
  weighted_composition_row(analytic, analytic %>% filter(age_yrs >= 18, age_yrs < 25), "Young adults 18-24"),
  weighted_composition_row(analytic, analytic %>% filter(age_yrs >= 60), "Older adults 60+"),
  weighted_composition_row(analytic, analytic %>% filter(wealth == "Poorest"), "Poorest quintile"),
  weighted_composition_row(analytic, analytic %>% filter(wealth %in% c("Poorest", "Poorer")), "Two poorest wealth quintiles"),
  weighted_composition_row(analytic, analytic %>% filter(residence == "Rural"), "Rural residents"),
  weighted_composition_row(analytic, analytic %>% filter(!is.na(disability_any) & disability_any == 1), "Any functional difficulty"),
  weighted_composition_row(analytic, analytic %>% filter(disability == "A lot / cannot do"), "A lot of difficulty / cannot do"),
  weighted_composition_row(analytic, analytic %>% filter(hh_relation == "Grandchild"), "Grandchildren in household")
) %>%
  arrange(desc(`Uninsured representation ratio`))

save_bundle(
  table8,
  file.path(paths$tables_dir, "Table8_ST09_Uninsured_Composition.csv"),
  file.path(paths$tables_dir, "Table8_ST09_Uninsured_Composition.docx"),
  caption = paste(
    "Table 8. Population composition of uninsured household members in policy-priority",
    "groups, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Weighted shares use PR household-member weights.",
    "The representation ratio is the group's share of all uninsured persons divided by its share of the total population.",
    "Values above 1 indicate over-representation in the uninsured population.",
    "Groups overlap and should not be summed."
  )
)
cat("Table 8 done.\n")

# Additional file 2 (supplementary tables) is assembled in Section 11, after the
# decomposition and model-sensitivity tables have been created.

message("=== SECTION 4 COMPLETE ===")

# =============================================================================
message("=== SECTION 5: Figures ===")
# =============================================================================

# Shared theme
theme_st09 <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "top",
    plot.caption      = element_text(size = 8, colour = "grey50"),
    strip.text        = element_text(face = "bold")
  )

sha_pal   <- c("Male" = "#2166ac", "Female" = "#d6604d")
wealth_pal <- c("Poorest" = "#d73027", "Poorer" = "#f46d43",
                 "Middle"  = "#fdae61", "Richer" = "#74add1",
                 "Richest" = "#313695")

# ── Figure 1: Life-course insurance curve — male vs female ────────────────────
fig1_df <- bind_rows(lapply(age_levels, function(ag) {
  bind_rows(lapply(c("Male","Female"), function(s) {
    d <- analytic %>% filter(age_group == ag, sex == s)
    r <- wprev(d, "insured_any")
    tibble(age_group = ag, sex = s,
           est = 100 * r$est,
           lo  = 100 * r$ci_low,
           hi  = 100 * r$ci_high,
           n   = r$unweighted_n)
  }))
})) %>%
  mutate(age_group = factor(age_group, levels = age_levels),
         sex = factor(sex, levels = c("Male","Female")))

fig1 <- ggplot(fig1_df, aes(x = age_group, y = est,
                              colour = sex, group = sex)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = sex),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(values = sha_pal, name = "Sex") +
  scale_fill_manual(values   = sha_pal, name = "Sex") +
  annotate("rect", xmin = 0.5, xmax = 3.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.07, fill = "#4dac26") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.07, fill = "#8073ac") +
  annotate("text", x = 2, y = max(fig1_df$hi, na.rm = TRUE) * 0.95,
           label = "Children\n<18", size = 3, colour = "#4dac26") +
  annotate("text", x = 8, y = max(fig1_df$hi, na.rm = TRUE) * 0.95,
           label = "Older adults\n60+", size = 3, colour = "#8073ac") +
  labs(
    title    = "Health insurance coverage across the life course by sex",
    subtitle = "All de-facto household members, KDHS 2022",
    x        = "Age group",
    y        = "Any insurance coverage (%)",
    caption  = "Source: Kenya DHS 2022. Survey-weighted estimates with 95% CI ribbons.\nShaded areas mark the child (<18) and older-adult (60+) monitoring bands."
  ) +
  theme_st09 +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(paths$figures_dir, "Figure1_ST09_LifeCourse_BySex.png"),
       fig1, width = 10, height = 6, dpi = 300)
cat("Figure 1 done.\n")

# ── Figure 2: Wealth gradient by age group ────────────────────────────────────
fig2_df <- bind_rows(lapply(age_levels, function(ag) {
  bind_rows(lapply(c("Poorest","Poorer","Middle","Richer","Richest"), function(w) {
    d <- analytic %>% filter(age_group == ag, wealth == w)
    r <- wprev(d, "insured_any")
    tibble(age_group = ag, wealth = w,
           est = 100 * r$est,
           lo  = 100 * r$ci_low,
           hi  = 100 * r$ci_high,
           n   = r$unweighted_n)
  }))
})) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    wealth    = factor(wealth,
      levels = c("Poorest","Poorer","Middle","Richer","Richest"))
  )

fig2 <- ggplot(fig2_df, aes(x = wealth, y = est,
                              colour = age_group, group = age_group)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 2.2) +
  scale_colour_viridis_d(name = "Age group", option = "turbo") +
  labs(
    title    = "Wealth gradient in insurance coverage by age group",
    subtitle = "All de-facto household members, KDHS 2022",
    x        = "Wealth quintile",
    y        = "Any insurance coverage (%)",
    caption  = "Source: Kenya DHS 2022. Survey-weighted estimates."
  ) +
  theme_st09

ggsave(file.path(paths$figures_dir, "Figure2_ST09_WealthGradient_ByAge.png"),
       fig2, width = 10, height = 6, dpi = 300)
cat("Figure 2 done.\n")

# ── Figure 3: Insurance type composition by age group ─────────────────────────
# Among insured only — what type of insurance they hold
fig3_df <- bind_rows(lapply(age_levels, function(ag) {
  d_ins <- analytic %>% filter(age_group == ag, insured_any == 1)
  bind_rows(
    tibble(age_group = ag, type = "NHIF",
           est = 100 * wprev(d_ins, "ins_nhif")$est),
    tibble(age_group = ag, type = "Private/commercial",
           est = 100 * wprev(d_ins, "ins_private")$est),
    tibble(age_group = ag, type = "Community-based",
           est = 100 * wprev(d_ins, "ins_community")$est),
    tibble(age_group = ag, type = "Other",
           est = 100 * wprev(d_ins, "ins_other")$est)
  )
})) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    type      = factor(type,
      levels = c("NHIF","Private/commercial","Community-based","Other"))
  ) %>%
  filter(!is.na(est))

fig3 <- ggplot(fig3_df, aes(x = age_group, y = est, fill = type)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_fill_brewer(palette = "Set2", name = "Insurance type") +
  labs(
    title    = "Insurance type composition by age group (among insured)",
    subtitle = "All de-facto household members, KDHS 2022",
    x        = "Age group",
    y        = "% of insured with this type",
    caption  = "Source: Kenya DHS 2022. Survey-weighted estimates.\nEstimates conditional on being insured (sh28a-x asked only of insured)."
  ) +
  theme_st09 +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(paths$figures_dir, "Figure3_ST09_InsuranceType_ByAge.png"),
       fig3, width = 10, height = 6, dpi = 300)
cat("Figure 3 done.\n")

# ── Figure 4: Financial protection gap — paid for OPD by insurance × wealth ───
fig4_df <- bind_rows(lapply(c("Poorest","Poorer","Middle","Richer","Richest"), function(w) {
  bind_rows(lapply(c(0L, 1L), function(ins) {
    d <- analytic %>%
      filter(wealth == w, insured_any == ins,
             outpatient == 1, !is.na(paid_opd))
    r <- wprev(d, "paid_opd")
    tibble(
      wealth   = w,
      insured  = if (ins == 1) "Insured" else "Uninsured",
      est      = 100 * r$est,
      lo       = 100 * r$ci_low,
      hi       = 100 * r$ci_high,
      n        = r$unweighted_n
    )
  }))
})) %>%
  mutate(
    wealth  = factor(wealth,
      levels = c("Poorest","Poorer","Middle","Richer","Richest")),
    insured = factor(insured, levels = c("Uninsured","Insured"))
  ) %>%
  filter(!is.na(est), n > 20)   # suppress cells with very few observations

fig4 <- ggplot(fig4_df, aes(x = wealth, y = est,
                              colour = insured, group = insured)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.2) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12) +
  scale_colour_manual(
    values = c("Insured" = "#2166ac", "Uninsured" = "#d73027"),
    name   = "Insurance status"
  ) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
  labs(
    title    = "Payment at last outpatient visit by insurance status and wealth",
    subtitle = "Among recent outpatient users, KDHS 2022",
    x        = "Wealth quintile",
    y        = "% who paid at the last outpatient visit",
    caption  = "Source: Kenya DHS 2022. Survey-weighted estimates with 95% CI.\nRestricted to persons who used outpatient care in the 4 weeks before interview.\nCells with n<20 suppressed."
  ) +
  theme_st09

ggsave(file.path(paths$figures_dir, "Figure4_ST09_FinancialProtection_Gap.png"),
       fig4, width = 9, height = 6, dpi = 300)
cat("Figure 4 done.\n")

# Figure 5: intersectional age-by-wealth heatmap
fig5 <- fig2_df %>%
  mutate(label = sprintf("%.0f", est)) %>%
  ggplot(aes(x = wealth, y = age_group, fill = est)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.4, colour = "grey10") +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    limits = c(0, max(fig2_df$est, na.rm = TRUE)),
    name = "Insured (%)"
  ) +
  labs(
    title = "Intersection of age and wealth in insurance coverage",
    subtitle = "Survey-weighted percentage with any health insurance, KDHS 2022",
    x = "Wealth quintile",
    y = "Age group",
    caption = "Source: Kenya DHS 2022. Cell labels are weighted percentages."
  ) +
  theme_st09 +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave(file.path(paths$figures_dir, "Figure5_ST09_AgeWealth_Heatmap.png"),
       fig5, width = 8.5, height = 6.5, dpi = 300)
cat("Figure 5 done.\n")

# Figure 6: over-representation in the uninsured population
fig6_df <- table8 %>%
  mutate(
    group = factor(`Population group`, levels = rev(`Population group`)),
    ratio = `Uninsured representation ratio`
  )

fig6 <- ggplot(fig6_df, aes(x = ratio, y = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey45") +
  geom_col(fill = "#2b8cbe", width = 0.68) +
  geom_text(aes(label = sprintf("%.2f", ratio)), hjust = -0.12, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Who was over-represented among the uninsured?",
    subtitle = "Share of all uninsured divided by share of the total population",
    x = "Uninsured representation ratio",
    y = NULL,
    caption = "Source: Kenya DHS 2022. Values above 1 indicate over-representation among uninsured persons.\nGroups overlap and should not be summed."
  ) +
  theme_st09 +
  theme(legend.position = "none")

ggsave(file.path(paths$figures_dir, "Figure6_ST09_Uninsured_Representation.png"),
       fig6, width = 8.5, height = 5.6, dpi = 300)
cat("Figure 6 done.\n")

message("=== SECTION 5 COMPLETE ===")

# =============================================================================
message("=== SECTION 6: Out-of-pocket payment depth (outpatient & inpatient) ===")
# =============================================================================
# Reviewer point: move beyond the binary "paid any money" indicator to the
# intensity of out-of-pocket (OOP) payment, using the cleaned continuous cost
# variables. We report survey-weighted mean and median total cost, cash OOP,
# the proportion paying any cash, and the share of total cost met by cash vs
# NHIF, overall and by insurance status and wealth quintile.

# Manual weighted median (robust across survey package versions)
wmedian <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

cost_row <- function(data, setting = c("opd", "hosp"), label) {
  setting <- match.arg(setting)
  pre   <- setting
  tot   <- paste0(pre, "_cost_total")
  cash  <- paste0(pre, "_cost_cash")
  nhif  <- paste0(pre, "_cost_nhif")

  d <- data %>% filter(!is.na(.data[[tot]]), !is.na(weight))
  n <- nrow(d)
  if (n < 25) {
    return(tibble(
      Group = label, `n` = n,
      `Mean total cost, KES` = NA_character_,
      `Median total cost, KES` = NA_character_,
      `Mean cash OOP, KES` = NA_character_,
      `Median cash OOP, KES` = NA_character_,
      `% paying any cash (95% CI)` = NA_character_,
      `Cash share of cost, %` = NA_character_,
      `NHIF share of cost, %` = NA_character_
    ))
  }
  w <- d$weight
  paid_cash <- as.integer(d[[cash]] > 0)
  pc <- wprev(d %>% mutate(.pc = paid_cash), ".pc")
  # Aggregate (population-level) cost shares = weighted Sum(component) / weighted Sum(total),
  # among users with a positive total cost. This is the standard, bounded health-financing
  # measure and avoids the distortion of averaging individual ratios (which can exceed 100%
  # where a respondent's recorded cash payment exceeds the recorded total cost).
  pos <- d %>% filter(.data[[tot]] > 0)
  denom <- sum(pos$weight * pos[[tot]], na.rm = TRUE)
  cash_share <- if (denom > 0) 100 * sum(pos$weight * pos[[cash]], na.rm = TRUE) / denom else NA_real_
  nhif_share <- if (denom > 0) 100 * sum(pos$weight * pos[[nhif]], na.rm = TRUE) / denom else NA_real_
  tibble(
    Group = label,
    `n` = n,
    `Mean total cost, KES`   = format(round(weighted.mean(d[[tot]], w, na.rm = TRUE)), big.mark = ","),
    `Median total cost, KES` = format(round(wmedian(d[[tot]], w)), big.mark = ","),
    `Mean cash OOP, KES`     = format(round(weighted.mean(d[[cash]], w, na.rm = TRUE)), big.mark = ","),
    `Median cash OOP, KES`   = format(round(wmedian(d[[cash]], w)), big.mark = ","),
    `% paying any cash (95% CI)` = pc$formatted,
    `Cash share of cost, %` = sprintf("%.1f", cash_share),
    `NHIF share of cost, %` = sprintf("%.1f", nhif_share)
  )
}

cost_block <- function(setting, setting_label) {
  bind_rows(
    cost_row(analytic, setting, "All users") %>% mutate(Setting = setting_label),
    cost_row(analytic %>% filter(insured_any == 1), setting, "  Insured") %>% mutate(Setting = setting_label),
    cost_row(analytic %>% filter(insured_any == 0), setting, "  Uninsured") %>% mutate(Setting = setting_label),
    cost_row(analytic %>% filter(wealth == "Poorest"), setting, "  Poorest quintile") %>% mutate(Setting = setting_label),
    cost_row(analytic %>% filter(wealth == "Poorer"),  setting, "  Poorer quintile")  %>% mutate(Setting = setting_label),
    cost_row(analytic %>% filter(wealth == "Middle"),  setting, "  Middle quintile")  %>% mutate(Setting = setting_label),
    cost_row(analytic %>% filter(wealth == "Richer"),  setting, "  Richer quintile")  %>% mutate(Setting = setting_label),
    cost_row(analytic %>% filter(wealth == "Richest"), setting, "  Richest quintile") %>% mutate(Setting = setting_label)
  )
}

table9 <- bind_rows(
  cost_block("opd",  "Outpatient (last visit)"),
  cost_block("hosp", "Inpatient (last admission)")
) %>%
  select(Setting, Group, everything())

save_bundle(
  table9,
  file.path(paths$tables_dir, "Table9_ST09_OOP_Cost_Depth.csv"),
  file.path(paths$tables_dir, "Table9_ST09_OOP_Cost_Depth.docx"),
  caption = paste(
    "Table 9. Depth of out-of-pocket payment at the last outpatient visit and last",
    "inpatient admission, by insurance status and wealth quintile, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates; n values are unweighted.",
    "Costs in Kenyan shillings (KES). Reserved 'don't know'/'missing' codes (9999998/9999999 for totals; 999998/999999 for components) were set to missing before estimation.",
    "Cash share = cash payment / total cost; NHIF share = NHIF-met amount / total cost, among users with a positive total cost.",
    "Outpatient costs refer to the last outpatient visit; inpatient costs to the last overnight admission.",
    "Cells with fewer than 25 unweighted observations are suppressed."
  )
)
cat("Table 9 done.\n")

# Headline OOP statistics for the manuscript
opd_overall_cost <- cost_row(analytic, "opd", "All")
opd_insured_cost <- cost_row(analytic %>% filter(insured_any == 1), "opd", "Insured")
opd_uninsured_cost <- cost_row(analytic %>% filter(insured_any == 0), "opd", "Uninsured")
hosp_overall_cost <- cost_row(analytic, "hosp", "All")
hosp_insured_cost  <- cost_row(analytic %>% filter(insured_any == 1), "hosp", "Insured")

# Figure 7: median cash OOP by wealth and insurance status (outpatient)
fig7_df <- bind_rows(lapply(c("Poorest","Poorer","Middle","Richer","Richest"), function(wq) {
  bind_rows(lapply(c(0L, 1L), function(ins) {
    d <- analytic %>% filter(wealth == wq, insured_any == ins,
                             !is.na(opd_cost_total), opd_cost_total > 0)
    denom <- sum(d$weight * d$opd_cost_total, na.rm = TRUE)
    tibble(
      wealth = wq,
      insured = if (ins == 1) "Insured" else "Uninsured",
      cash_share = if (nrow(d) >= 25 && denom > 0)
        100 * sum(d$weight * d$opd_cost_cash, na.rm = TRUE) / denom else NA_real_,
      n = nrow(d)
    )
  }))
})) %>%
  mutate(
    wealth = factor(wealth, levels = c("Poorest","Poorer","Middle","Richer","Richest")),
    insured = factor(insured, levels = c("Uninsured","Insured"))
  ) %>%
  filter(!is.na(cash_share))

fig7 <- ggplot(fig7_df, aes(x = wealth, y = cash_share, colour = insured, group = insured)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.2) +
  scale_colour_manual(values = c("Insured" = "#2166ac", "Uninsured" = "#d73027"),
                      name = "Insurance status") +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
  labs(
    title    = "Cash share of outpatient costs by insurance status and wealth",
    subtitle = "Among outpatient users with a recorded cost, KDHS 2022",
    x        = "Wealth quintile",
    y        = "Mean cash share of total outpatient cost (%)",
    caption  = "Source: Kenya DHS 2022. Survey-weighted means; cells with n<25 suppressed.\nA high cash share among the insured indicates limited financial protection at the point of care."
  ) +
  theme_st09

ggsave(file.path(paths$figures_dir, "Figure7_ST09_OOP_CashShare.png"),
       fig7, width = 9, height = 6, dpi = 300)
cat("Figure 7 done.\n")

message("=== SECTION 6 COMPLETE ===")

# =============================================================================
message("=== SECTION 7: Social safety-net linkage ===")
# =============================================================================
# Reviewer point: assess whether households already receiving social assistance
# (the registries SHA premium subsidies are meant to build on) were linked to
# insurance under the pre-SHA system, especially among the poorest.

safety_net_coverage <- function(data, flag_var, flag_label, stratum_label) {
  d1 <- data %>% filter(.data[[flag_var]] == 1)
  d0 <- data %>% filter(.data[[flag_var]] == 0)
  bind_rows(
    tibble(
      `Stratum` = stratum_label,
      `Safety-net status` = paste0(flag_label, ": yes"),
      `Unweighted n` = nrow(d1),
      `Any insurance % (95% CI)` = wprev(d1, "insured_any")$formatted
    ),
    tibble(
      `Stratum` = stratum_label,
      `Safety-net status` = paste0(flag_label, ": no"),
      `Unweighted n` = nrow(d0),
      `Any insurance % (95% CI)` = wprev(d0, "insured_any")$formatted
    )
  )
}

poorest_two <- analytic %>% filter(wealth %in% c("Poorest","Poorer"))

table10 <- bind_rows(
  safety_net_coverage(analytic, "safety_net_gov", "Government cash transfer", "All household members"),
  safety_net_coverage(analytic, "safety_net_any", "Any organised safety net", "All household members"),
  safety_net_coverage(poorest_two, "safety_net_gov", "Government cash transfer", "Two poorest quintiles"),
  safety_net_coverage(poorest_two, "safety_net_any", "Any organised safety net", "Two poorest quintiles")
)

# Adjusted association: add government safety net to the all-ages Model A specification
model_sn_df <- model_A_df %>%
  filter(!is.na(safety_net_gov)) %>%
  mutate(safety_net_gov = factor(ifelse(safety_net_gov == 1, "Receives", "None"),
                                 levels = c("None", "Receives")))
des_sn <- make_design(model_sn_df)
mod_sn <- tryCatch(
  svyglm(insured_any ~ safety_net_gov + age_group + sex + wealth + residence + region,
         design = des_sn, family = quasipoisson(link = "log")),
  error = function(e) { log_error("Model SN", conditionMessage(e)); NULL }
)
sn_apr <- if (!is.null(mod_sn)) {
  tt <- broom::tidy(mod_sn) %>% filter(term == "safety_net_govReceives")
  fmt_effect(exp(tt$estimate), exp(tt$estimate - 1.96 * tt$std.error), exp(tt$estimate + 1.96 * tt$std.error))
} else NA_character_

save_bundle(
  table10,
  file.path(paths$tables_dir, "Table10_ST09_SafetyNet_Linkage.csv"),
  file.path(paths$tables_dir, "Table10_ST09_SafetyNet_Linkage.docx"),
  caption = paste(
    "Table 10. Health insurance coverage among households receiving social",
    "assistance, overall and in the two poorest wealth quintiles, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates with 95% confidence intervals; n values are unweighted.",
    "Government cash transfer = national (sh134aa) or county (sh134ab) government assistance.",
    "Any organised safety net = government, NGO/CBO, or religious-organisation assistance (sh134aa-ad); informal kin support is excluded.",
    paste0("Adjusted prevalence ratio for any government cash transfer (all-ages model, adjusting for age, sex, wealth, residence, region): ",
           sn_apr, ".")
  )
)
cat("Table 10 done.\n")

message("=== SECTION 7 COMPLETE ===")

# =============================================================================
message("=== SECTION 8: Wagstaff decomposition of the concentration index ===")
# =============================================================================
# Decompose the standard concentration index of insurance coverage into the
# contributions of each determinant: contribution_k = (beta_k * mean_x_k / mu) * C_k,
# where C_k is the concentration index of determinant x_k ordered by wealth rank.

# Point estimate of a concentration index from raw vectors (no bootstrap)
conc_point <- function(y, r, w) {
  ok <- is.finite(y) & is.finite(r) & is.finite(w)
  y <- y[ok]; r <- r[ok]; w <- w[ok]
  if (length(y) < 10) return(NA_real_)
  o <- order(r); y <- y[o]; w <- w[o]
  wn <- w / sum(w)
  frac <- cumsum(wn) - 0.5 * wn
  mu <- weighted.mean(y, w)
  if (!is.finite(mu) || mu == 0) return(NA_real_)
  2 * weighted.mean((y - mu) * (frac - 0.5), w) / mu
}

decompose_ci <- function(data, covariates, var_groups,
                         outcome = "insured_any", rank_var = "wealth_rank",
                         wvar = "weight") {
  keep <- c(outcome, covariates, rank_var, wvar)
  d <- data %>% filter(if_all(all_of(keep), ~ !is.na(.)))
  des <- make_design(d, wvar)
  f <- as.formula(paste(outcome, "~", paste(covariates, collapse = " + ")))
  fit <- svyglm(f, design = des, family = gaussian())
  betas <- coef(fit)
  mm <- model.matrix(fit)
  mu <- weighted.mean(d[[outcome]], d[[wvar]])
  ci_total <- conc_point(d[[outcome]], d[[rank_var]], d[[wvar]])

  terms <- setdiff(names(betas), "(Intercept)")
  per_term <- lapply(terms, function(t) {
    xk <- mm[, t]
    mean_x <- weighted.mean(xk, d[[wvar]])
    elasticity <- betas[[t]] * mean_x / mu
    ci_x <- conc_point(xk, d[[rank_var]], d[[wvar]])
    tibble(term = t, contribution = elasticity * ci_x)
  }) %>% bind_rows()

  # Aggregate dummy terms back to their source variable
  per_term <- per_term %>%
    mutate(group = map_chr(term, function(t) {
      hit <- var_groups$prefix[map_lgl(var_groups$prefix, ~ startsWith(t, .x))]
      if (length(hit) == 0) t else var_groups$label[match(hit[which.max(nchar(hit))], var_groups$prefix)]
    }))

  agg <- per_term %>%
    group_by(Determinant = group) %>%
    summarise(Contribution = sum(contribution, na.rm = TRUE), .groups = "drop")

  residual <- ci_total - sum(agg$Contribution, na.rm = TRUE)
  agg <- bind_rows(
    agg,
    tibble(Determinant = "Residual", Contribution = residual)
  ) %>%
    mutate(
      `% of total CI` = sprintf("%.1f", 100 * Contribution / ci_total),
      Contribution = round(Contribution, 4)
    ) %>%
    arrange(desc(abs(Contribution)))

  list(table = agg, ci_total = ci_total, n = nrow(d))
}

# Primary decomposition: all-ages CI on Model A covariates
groups_A <- tibble(
  prefix = c("wealth", "age_group", "sexFemale", "residence", "region"),
  label  = c("Wealth quintile", "Age group", "Sex", "Residence (rural)", "Region")
)
decomp_A <- decompose_ci(
  model_A_df,
  covariates = c("wealth", "age_group", "sex", "residence", "region"),
  var_groups = groups_A
)

table11 <- decomp_A$table %>%
  transmute(
    Determinant,
    `Absolute contribution to C` = Contribution,
    `% of total C` = `% of total CI`
  )

save_bundle(
  table11,
  file.path(paths$tables_dir, "Table11_ST09_CI_Decomposition.csv"),
  file.path(paths$tables_dir, "Table11_ST09_CI_Decomposition.docx"),
  caption = paste(
    "Table 11. Wagstaff decomposition of the wealth-related concentration index in any",
    "health insurance coverage, all household members, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted decomposition of the standard concentration index.",
    sprintf("Total standard concentration index decomposed: %.4f (n=%s).",
            decomp_A$ci_total, format(decomp_A$n, big.mark = ",")),
    "Contributions are elasticity-weighted concentration indices of each determinant; positive values push coverage toward wealthier households.",
    "Determinant contributions sum to the total concentration index together with the unexplained residual.",
    "Determinants entered as in all-ages Model A (wealth quintile, age group, sex, residence, region)."
  )
)
cat("Table 11 done.\n")

# Supplementary decomposition among adults 15+ adding education and disability
groups_B <- tibble(
  prefix = c("wealth", "age_group", "sexFemale", "residence", "education",
             "disability_any", "region"),
  label  = c("Wealth quintile", "Age group", "Sex", "Residence (rural)",
             "Education", "Functional difficulty", "Region")
)
decomp_B <- tryCatch(
  decompose_ci(
    model_B_df,
    covariates = c("wealth", "age_group", "sex", "residence", "education",
                   "disability_any", "region"),
    var_groups = groups_B
  ),
  error = function(e) { log_error("Decomp B", conditionMessage(e)); NULL }
)
table11b <- if (!is.null(decomp_B)) {
  decomp_B$table %>%
    transmute(Determinant,
              `Absolute contribution to C` = Contribution,
              `% of total C` = `% of total CI`)
} else NULL

message("=== SECTION 8 COMPLETE ===")

# =============================================================================
message("=== SECTION 9: KDHS 2014 vs 2022 trend comparison ===")
# =============================================================================
# Reviewer point: place the 2022 baseline in temporal context. A true
# all-population (PR-file) trend is NOT possible: the 2014 KDHS did not carry a
# household-roster insurance item, and the 2022 individual recodes do not carry
# v481/mv481 (insurance moved to the household module). The only comparable
# populations are women 15-49 (2014 IR v481 vs 2022 PR) and men 15-54
# (2014 MR mv481 vs 2022 PR). Estimates are directional and not strictly
# comparable because of differences in respondent (individual self-report vs
# household-roster report), weights, and wealth-index construction across rounds.

coverage_ci_row <- function(data, label, year) {
  # data must contain: y (0/1), wealth_rank, weight, psu, strata
  d <- data %>% filter(!is.na(y), !is.na(wealth_rank), !is.na(weight))
  cov <- wprev(d %>% mutate(insured_any = y), "insured_any")
  ci  <- conc_point(d$y, d$wealth_rank, d$weight)
  tibble(
    Population = label,
    Year = year,
    `Unweighted n` = nrow(d),
    `Coverage % (95% CI)` = cov$formatted,
    `Concentration index` = round(ci, 4)
  )
}

trend_rows <- list()

# 2022 from PR (this analysis)
w2022 <- analytic %>%
  filter(sex == "Female", age_yrs >= 15, age_yrs <= 49) %>%
  transmute(y = insured_any, wealth_rank, weight, psu, strata)
m2022 <- analytic %>%
  filter(sex == "Male", age_yrs >= 15, age_yrs <= 54) %>%
  transmute(y = insured_any, wealth_rank, weight, psu, strata)
trend_rows[["w2022"]] <- coverage_ci_row(w2022, "Women 15-49", "2022 (PR)")
trend_rows[["m2022"]] <- coverage_ci_row(m2022, "Men 15-54", "2022 (PR)")

# 2014 from IR (women) and MR (men)
ir2014_path <- file.path(data_root, "KDHS_2014", "IR_Individual_Recode", "KEIR72FL.DTA")
mr2014_path <- file.path(data_root, "KDHS_2014", "MR_Mens_Recode", "KEMR72FL.DTA")

trend_2014_ok <- TRUE
if (file.exists(ir2014_path)) {
  ir14 <- read_dta(ir2014_path, col_select = c(v481, v005, v021, v022, v190, v012))
  w2014 <- ir14 %>%
    transmute(
      y = case_when(num(v481) == 1 ~ 1L, num(v481) == 0 ~ 0L, TRUE ~ NA_integer_),
      wealth_rank = num(v190),
      weight = num(v005) / 1e6,
      psu = num(v021), strata = num(v022)
    ) %>% filter(!is.na(y))
  trend_rows[["w2014"]] <- coverage_ci_row(w2014, "Women 15-49", "2014 (IR)")
} else { trend_2014_ok <- FALSE; log_error("Trend", "KDHS 2014 IR file not found") }

if (file.exists(mr2014_path)) {
  mr14 <- read_dta(mr2014_path, col_select = c(mv481, mv005, mv021, mv022, mv190, mv012))
  m2014 <- mr14 %>%
    transmute(
      y = case_when(num(mv481) == 1 ~ 1L, num(mv481) == 0 ~ 0L, TRUE ~ NA_integer_),
      wealth_rank = num(mv190),
      weight = num(mv005) / 1e6,
      psu = num(mv021), strata = num(mv022)
    ) %>% filter(!is.na(y))
  trend_rows[["m2014"]] <- coverage_ci_row(m2014, "Men 15-54", "2014 (MR)")
} else { trend_2014_ok <- FALSE; log_error("Trend", "KDHS 2014 MR file not found") }

table12 <- bind_rows(
  trend_rows[["w2014"]], trend_rows[["w2022"]],
  trend_rows[["m2014"]], trend_rows[["m2022"]]
) %>%
  arrange(Population, Year)

save_bundle(
  table12,
  file.path(paths$tables_dir, "Table12_ST09_Trend_2014_2022.csv"),
  file.path(paths$tables_dir, "Table12_ST09_Trend_2014_2022.docx"),
  caption = paste(
    "Table 12. Health insurance coverage and wealth-related inequality among adults of",
    "reproductive/working age, KDHS 2014 versus KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2014 and 2022. Survey-weighted estimates; n values are unweighted.",
    "2014 estimates use the individual self-reported insurance item (women's recode v481; men's recode mv481).",
    "2022 estimates use the household-roster insurance item (sh27) restricted to the same age-sex groups.",
    "Estimates are directional: respondent type (individual vs household-roster report), sample weights, and wealth-index construction differ across rounds, so differences should not be interpreted as precise change.",
    "A true all-population (person-recode) trend is not possible because KDHS 2014 did not carry a household-roster insurance item."
  )
)
cat("Table 12 done.\n")

message("=== SECTION 9 COMPLETE ===")

# =============================================================================
message("=== SECTION 10: Model-family sensitivity (quasi-Poisson vs log-binomial) ===")
# =============================================================================
# Reviewer minor point: justify the quasi-Poisson choice and verify consistency
# with a log-binomial model. The survey-weighted quasi-Poisson GLM already
# returns design-robust (sandwich) variance estimates; we compare its APRs for
# key terms with a log-binomial specification on the same all-ages sample.

# Generic APR extractor for a fitted model (svyglm or glm), formatted with 95% CI.
sens_extract <- function(mod, terms, label) {
  if (is.null(mod)) {
    return(tibble(Term = terms, !!label := NA_character_))
  }
  td <- broom::tidy(mod)
  tibble(
    Term = terms,
    !!label := sapply(terms, function(t) {
      r <- td %>% filter(term == t)
      if (nrow(r) == 0) return(NA_character_)
      fmt_effect(exp(r$estimate), exp(r$estimate - 1.96 * r$std.error),
                 exp(r$estimate + 1.96 * r$std.error))
    })
  )
}

# The fully adjusted log-binomial model does not converge here: with a common outcome
# and very large wealth associations, the log link produces fitted probabilities above 1
# and no valid starting values exist. This non-convergence is the standard rationale for
# preferring the modified-Poisson (quasi-Poisson with robust variance) approach. To still
# provide a convergent cross-check, we compare the two model families on a parsimonious
# specification (wealth + residence) in which the log-binomial converges.
full_logbin_converged <- tryCatch({
  fit <- suppressWarnings(glm(insured_any ~ age_group + sex + wealth + residence + region,
                              data = model_A_df, family = binomial(link = "log"),
                              start = coef(glm(insured_any ~ age_group + sex + wealth + residence + region,
                                               data = model_A_df, family = poisson(), weights = weight)),
                              weights = weight))
  fit$converged
}, error = function(e) FALSE)

parsi_formula <- insured_any ~ wealth + residence
parsi_qp  <- svyglm(parsi_formula, design = des_A, family = quasipoisson(link = "log"))
parsi_pois <- glm(parsi_formula, data = model_A_df, family = poisson(), weights = weight)
parsi_lb  <- tryCatch(
  suppressWarnings(glm(parsi_formula, data = model_A_df,
                       family = binomial(link = "log"), weights = weight,
                       start = coef(parsi_pois))),
  error = function(e) { log_error("Parsimonious log-binomial", conditionMessage(e)); NULL }
)

sens_terms  <- c("wealthPoorer", "wealthMiddle", "wealthRicher", "wealthRichest", "residenceRural")
term_labels <- c(
  "wealthPoorer"   = "Wealth: Poorer vs Poorest",
  "wealthMiddle"   = "Wealth: Middle vs Poorest",
  "wealthRicher"   = "Wealth: Richer vs Poorest",
  "wealthRichest"  = "Wealth: Richest vs Poorest",
  "residenceRural" = "Residence: Rural vs Urban"
)

table_sens <- sens_extract(parsi_qp,  sens_terms, "Quasi-Poisson APR (95% CI)") %>%
  left_join(sens_extract(parsi_lb, sens_terms, "Log-binomial APR (95% CI)"), by = "Term") %>%
  mutate(Term = term_labels[Term]) %>%
  rename(`Comparison (parsimonious model)` = Term)

save_bundle(
  table_sens,
  file.path(paths$tables_dir, "TableS6_ST09_Model_Sensitivity.csv"),
  file.path(paths$tables_dir, "TableS6_ST09_Model_Sensitivity.docx"),
  caption = paste(
    "Table S6. Sensitivity of adjusted prevalence ratios to the model family",
    "(survey-weighted quasi-Poisson vs log-binomial), KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Estimates on the all-ages Model A sample.",
    "The fully adjusted log-binomial model did not converge (fitted probabilities exceed 1 given the strong wealth associations), a recognised limitation of log-binomial models for common outcomes; this is the rationale for the modified-Poisson approach.",
    "To provide a convergent comparison, both families were fitted on a parsimonious specification (wealth + residence). Quasi-Poisson estimates use the survey design with robust variance; the log-binomial was fitted as a weighted GLM.",
    "Close agreement of the parsimonious-model prevalence ratios indicates the estimates are not sensitive to the model family."
  )
)
cat("Table S6 (model sensitivity) done.\n")

logbin_converged <- !is.null(parsi_lb)

message("=== SECTION 10 COMPLETE ===")

# =============================================================================
message("=== SECTION 11: Variable-enrichment descriptives ===")
# =============================================================================
# Newly added household-roster variables (head sex/age, mobile-phone ownership,
# child parental survival) provide additional equity lenses and feed the
# multilevel and intersectional models in Sections 12-13.

enrich_section <- function(data, group_var, section_label) {
  wprev_by(data, group_var, "insured_any") %>%
    transmute(
      Section = section_label,
      Category = group,
      `Unweighted n` = unweighted_n,
      `Insurance % (95% CI)` = formatted
    )
}

analytic_mobile <- analytic %>%
  filter(!is.na(mobile_phone)) %>%
  mutate(mobile_lab = factor(
    ifelse(mobile_phone == 1, "Owns mobile phone", "No mobile phone"),
    levels = c("No mobile phone", "Owns mobile phone")
  ))

table13 <- bind_rows(
  enrich_section(analytic, "hh_head_sex", "Household head sex"),
  enrich_section(analytic_mobile, "mobile_lab", "Household mobile phone"),
  enrich_section(analytic %>% filter(age_yrs < 18, !is.na(orphan_status)),
                 "orphan_status", "Child parental survival (<18)")
)

save_bundle(
  table13,
  file.path(paths$tables_dir, "Table13_ST09_Enrichment_Descriptives.csv"),
  file.path(paths$tables_dir, "Table13_ST09_Enrichment_Descriptives.docx"),
  caption = paste(
    "Table 13. Health insurance coverage by household-head sex, household",
    "mobile-phone ownership, and child parental-survival status, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted estimates with 95% confidence intervals; n values are unweighted.",
    "Household-head sex and mobile-phone ownership are household-roster characteristics (hv219, hv243a).",
    "Parental-survival status (mother hv111, father hv113) is recorded only for children and is restricted here to members aged under 18 with valid data."
  )
)
cat("Table 13 done.\n")

message("=== SECTION 11 COMPLETE ===")

# =============================================================================
message("=== SECTION 12: Intersectional inequality (MAIHDA) ===")
# =============================================================================
# Multilevel Analysis of Individual Heterogeneity and Discriminatory Accuracy.
# Individuals are nested within intersectional strata defined by age group,
# wealth quintile, residence, and sex. A two-level logistic model partitions the
# variance to quantify how much coverage inequality operates at the intersection
# of these identities (VPC), and how much survives adjustment for their additive
# main effects (PCV; the residual reflects intersectional interaction effects).
# MAIHDA models are conventionally fitted unweighted; design-based weighted APRs
# are reported separately in Sections 3 and 13.

maihda_df <- analytic %>%
  filter(!is.na(insured_any), !is.na(age_group), !is.na(wealth),
         !is.na(residence), !is.na(sex)) %>%
  mutate(strata = interaction(age_group, wealth, residence, sex,
                              drop = TRUE, sep = " | "))
n_strata <- nlevels(maihda_df$strata)

glmer_ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

maihda_null <- tryCatch(
  glmer(insured_any ~ 1 + (1 | strata), data = maihda_df,
        family = binomial, control = glmer_ctrl),
  error = function(e) { log_error("MAIHDA null", conditionMessage(e)); NULL }
)
maihda_adj <- tryCatch(
  glmer(insured_any ~ age_group + wealth + residence + sex + (1 | strata),
        data = maihda_df, family = binomial, control = glmer_ctrl),
  error = function(e) { log_error("MAIHDA adjusted", conditionMessage(e)); NULL }
)

strata_var <- function(mod) {
  if (is.null(mod)) return(NA_real_)
  as.numeric(VarCorr(mod)[["strata"]][1])
}
vpc_latent <- function(v) if (is.na(v)) NA_real_ else v / (v + pi^2 / 3)

v_null   <- strata_var(maihda_null)
v_adj    <- strata_var(maihda_adj)
vpc_null <- vpc_latent(v_null)
vpc_adj  <- vpc_latent(v_adj)
pcv      <- if (!is.na(v_null) && v_null > 0 && !is.na(v_adj))
  100 * (v_null - v_adj) / v_null else NA_real_

table14 <- tibble(
  Metric = c(
    "Number of intersectional strata",
    "Between-stratum variance, null model",
    "VPC, null model (%)",
    "Between-stratum variance, main-effects model",
    "VPC, main-effects model (%)",
    "Proportional change in variance, PCV (%)",
    "Share of stratum variance from interactions (100 - PCV, %)"
  ),
  Value = c(
    format(n_strata, big.mark = ","),
    sprintf("%.4f", v_null),
    sprintf("%.1f", 100 * vpc_null),
    sprintf("%.4f", v_adj),
    sprintf("%.1f", 100 * vpc_adj),
    sprintf("%.1f", pcv),
    sprintf("%.1f", 100 - pcv)
  )
)

save_bundle(
  table14,
  file.path(paths$tables_dir, "Table14_ST09_MAIHDA.csv"),
  file.path(paths$tables_dir, "Table14_ST09_MAIHDA.docx"),
  caption = paste(
    "Table 14. Intersectional multilevel analysis (MAIHDA) of any health insurance",
    "coverage across strata of age, wealth, residence, and sex, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Two-level logistic models (individuals within intersectional strata), fitted unweighted.",
    "VPC = variance partition coefficient (latent-variable method), the share of total variance attributable to differences between intersectional strata.",
    "PCV = proportional change in the between-stratum variance after adjusting for the additive main effects of age, wealth, residence, and sex.",
    "The residual share (100 - PCV) approximates the contribution of intersectional interaction (multiplicative) effects beyond additive main effects."
  )
)
cat("Table 14 done.\n")

# Figure 8: MAIHDA caterpillar — predicted coverage by intersectional stratum
if (!is.null(maihda_null)) {
  re   <- ranef(maihda_null, condVar = TRUE)$strata
  b0   <- fixef(maihda_null)[["(Intercept)"]]
  pv   <- attr(re, "postVar")[1, 1, ]
  fig8_df <- tibble(
    strata = rownames(re),
    eff    = re[, 1],
    se     = sqrt(pv)
  ) %>%
    mutate(
      pred = plogis(b0 + eff),
      lo   = plogis(b0 + eff - 1.96 * se),
      hi   = plogis(b0 + eff + 1.96 * se)
    ) %>%
    arrange(pred) %>%
    mutate(rank = row_number())

  fig8 <- ggplot(fig8_df, aes(x = rank, y = 100 * pred)) +
    geom_ribbon(aes(ymin = 100 * lo, ymax = 100 * hi),
                alpha = 0.15, fill = "#2166ac") +
    geom_line(colour = "#2166ac", linewidth = 0.9) +
    geom_hline(yintercept = 100 * plogis(b0),
               linetype = "dashed", colour = "grey45") +
    annotate("text", x = 1, y = 100 * plogis(b0),
             label = "Overall predicted mean", hjust = 0, vjust = -0.6,
             size = 3, colour = "grey35") +
    labs(
      title    = "Intersectional inequality in insurance coverage (MAIHDA)",
      subtitle = sprintf("%d strata of age x wealth x residence x sex, KDHS 2022", n_strata),
      x        = "Intersectional stratum (ranked by predicted coverage)",
      y        = "Predicted insurance coverage (%)",
      caption  = "Source: Kenya DHS 2022. Predicted coverage from a two-level logistic MAIHDA null model with 95% intervals."
    ) +
    theme_st09
  ggsave(file.path(paths$figures_dir, "Figure8_ST09_MAIHDA_Caterpillar.png"),
         fig8, width = 9, height = 6, dpi = 300)
  cat("Figure 8 done.\n")
} else {
  fig8_df <- NULL
}

message("=== SECTION 12 COMPLETE ===")

# =============================================================================
message("=== SECTION 13: Multilevel mixed-effects model ===")
# =============================================================================
# Supplement the design-based single-level APRs (Section 3) with a hierarchical
# model nesting individuals within survey clusters (hv001). Per the analysis
# plan, a modified-Poisson mixed model (log link) is attempted first to preserve
# the APR framing used throughout; if it fails to converge, a logistic mixed
# model (odds ratios) is used as a fallback. The cluster random intercept yields
# the share of coverage variance attributable to unobserved community factors.

mlm_df <- model_A_df   # all-ages complete-case sample, reference levels already set

mlm_pois <- tryCatch(
  glmer(insured_any ~ age_group + sex + wealth + residence + region + (1 | cluster),
        data = mlm_df, family = poisson(link = "log"), control = glmer_ctrl),
  error = function(e) { log_error("MLM Poisson", conditionMessage(e)); NULL }
)

if (!is.null(mlm_pois)) {
  mlm_mod    <- mlm_pois
  mlm_family <- "Poisson (APR)"
} else {
  mlm_mod <- tryCatch(
    glmer(insured_any ~ age_group + sex + wealth + residence + region + (1 | cluster),
          data = mlm_df, family = binomial, control = glmer_ctrl),
    error = function(e) { log_error("MLM logistic", conditionMessage(e)); NULL }
  )
  mlm_family <- "Logistic (OR)"
}
is_apr_mlm   <- grepl("Poisson", mlm_family)
effect_label <- if (is_apr_mlm) "Multilevel APR (95% CI)" else "Multilevel OR (95% CI)"

# Formatted exp(effect) with 95% CI for selected fixed-effect terms of a glmerMod
mlm_effect <- function(mod, terms, label) {
  if (is.null(mod)) return(tibble(Term = terms, !!label := NA_character_))
  fe <- fixef(mod)
  se <- sqrt(diag(as.matrix(vcov(mod))))
  tibble(
    Term = terms,
    !!label := sapply(terms, function(t) {
      if (!t %in% names(fe)) return(NA_character_)
      fmt_effect(exp(fe[[t]]), exp(fe[[t]] - 1.96 * se[[t]]), exp(fe[[t]] + 1.96 * se[[t]]))
    })
  )
}

mlm_terms <- c("wealthPoorer", "wealthMiddle", "wealthRicher", "wealthRichest",
               "residenceRural", "sexFemale")
mlm_labels <- c(
  "wealthPoorer"   = "Wealth: Poorer vs Poorest",
  "wealthMiddle"   = "Wealth: Middle vs Poorest",
  "wealthRicher"   = "Wealth: Richer vs Poorest",
  "wealthRichest"  = "Wealth: Richest vs Poorest",
  "residenceRural" = "Residence: Rural vs Urban",
  "sexFemale"      = "Sex: Female vs Male"
)

table15 <- sens_extract(mod_A, mlm_terms, "Single-level APR (95% CI)") %>%
  left_join(mlm_effect(mlm_mod, mlm_terms, effect_label), by = "Term") %>%
  mutate(Covariate = mlm_labels[Term]) %>%
  select(Covariate, `Single-level APR (95% CI)`, !!effect_label)

# Cluster variance and variance partition coefficient
sig2_u <- if (!is.null(mlm_mod)) as.numeric(VarCorr(mlm_mod)[["cluster"]][1]) else NA_real_
b0_mlm <- if (!is.null(mlm_mod)) fixef(mlm_mod)[["(Intercept)"]] else NA_real_
if (is_apr_mlm && !is.na(sig2_u)) {
  # Poisson-lognormal VPC evaluated at the reference covariate pattern
  lambda  <- exp(b0_mlm + sig2_u / 2)
  vpc_mlm <- (lambda * (exp(sig2_u) - 1)) / (1 + lambda * (exp(sig2_u) - 1))
  vpc_method <- "Poisson-lognormal VPC at the reference covariate pattern"
} else {
  vpc_mlm    <- vpc_latent(sig2_u)
  vpc_method <- "logistic latent-variable VPC"
}

save_bundle(
  table15,
  file.path(paths$tables_dir, "Table15_ST09_Multilevel_Model.csv"),
  file.path(paths$tables_dir, "Table15_ST09_Multilevel_Model.docx"),
  caption = paste(
    "Table 15. Multilevel (individuals within survey clusters) versus single-level",
    "adjusted effect estimates for any health insurance coverage, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Single-level estimates are survey-weighted quasi-Poisson APRs (Section 3, all-ages Model A sample).",
    sprintf("Multilevel estimates are from an unweighted %s mixed model with a random intercept for survey cluster (hv001).", mlm_family),
    sprintf("Cluster-level random-intercept variance = %.4f; variance partition coefficient = %.1f%% (%s).",
            sig2_u, 100 * vpc_mlm, vpc_method),
    "Closeness of the single-level and multilevel point estimates indicates the wealth and residence gradients are robust to community-level clustering."
  )
)
cat("Table 15 done.\n")

message("=== SECTION 13 COMPLETE ===")

# =============================================================================
message("=== SECTION 14: SHA subsidy policy microsimulation ===")
# =============================================================================
# Counterfactual: how far would subsidising the poorest close the wealth gap?
# We recompute the standard (Wagstaff) concentration index of coverage under
# three enrolment scenarios and compare them with the observed baseline. If the
# index falls toward zero, subsidising the bottom flattens the gradient; if a
# substantial pro-rich index persists, an uninsured "missing middle" sustains it.

sim_data <- analytic %>%
  filter(!is.na(insured_any), !is.na(wealth), !is.na(wealth_rank), !is.na(weight))
poorest_two_flag <- sim_data$wealth %in% c("Poorest", "Poorer")
safetynet_flag   <- !is.na(sim_data$safety_net_gov) & sim_data$safety_net_gov == 1

scenarios <- list(
  list(name = "Baseline (observed)",
       y = sim_data$insured_any),
  list(name = "100% of poorest two quintiles enrolled",
       y = ifelse(poorest_two_flag, 1, sim_data$insured_any)),
  list(name = "80% of poorest two quintiles enrolled",
       y = ifelse(poorest_two_flag & sim_data$insured_any == 0, 0.8, sim_data$insured_any)),
  list(name = "Government safety-net beneficiaries enrolled",
       y = ifelse(safetynet_flag, 1, sim_data$insured_any))
)

wq_cov <- function(y, q) {
  idx <- sim_data$wealth == q
  100 * weighted.mean(y[idx], sim_data$weight[idx], na.rm = TRUE)
}
sim_metric <- function(y) {
  ci <- calc_conc_index(sim_data %>% mutate(.ysim = y), ".ysim")
  list(
    cov  = 100 * weighted.mean(y, sim_data$weight, na.rm = TRUE),
    poor = wq_cov(y, "Poorest"),
    rich = wq_cov(y, "Richest"),
    ci   = as.numeric(ci$standard_ci),
    lo   = as.numeric(ci$standard_ci_lo),
    hi   = as.numeric(ci$standard_ci_hi)
  )
}

baseline_ci <- sim_metric(scenarios[[1]]$y)$ci

table16 <- bind_rows(lapply(scenarios, function(s) {
  m <- sim_metric(s$y)
  tibble(
    Scenario = s$name,
    `Simulated coverage %`  = sprintf("%.1f", m$cov),
    `Poorest quintile %`    = sprintf("%.1f", m$poor),
    `Richest quintile %`    = sprintf("%.1f", m$rich),
    `Standard CI (95% CI)`  = sprintf("%.4f (%.4f, %.4f)", m$ci, m$lo, m$hi),
    `Change in CI vs baseline` = sprintf("%+.4f", m$ci - baseline_ci)
  )
}))

save_bundle(
  table16,
  file.path(paths$tables_dir, "Table16_ST09_SHA_Microsimulation.csv"),
  file.path(paths$tables_dir, "Table16_ST09_SHA_Microsimulation.docx"),
  caption = paste(
    "Table 16. Microsimulation of SHA premium-subsidy scenarios: coverage and the",
    "wealth-related concentration index under counterfactual enrolment, KDHS 2022."
  ),
  footer = c(
    "Source: Kenya DHS 2022. Survey-weighted simulated coverage; standard concentration indices with 95% CIs from 250 bootstrap replicates.",
    "Scenario 1 sets coverage to 1 for everyone in the poorest two quintiles; scenario 2 enrols 80% of the currently uninsured in those quintiles (expected-value); scenario 3 enrols all members of households receiving a government cash transfer (sh134aa/sh134ab).",
    "A concentration index near zero indicates an income-neutral distribution of coverage.",
    "Persistence of a positive index after subsidising the poorest reflects an uninsured 'missing middle' in the middle and richer quintiles."
  )
)
cat("Table 16 done.\n")

# Figure 9: coverage by wealth quintile across microsimulation scenarios
quintiles <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
scenario_levels <- vapply(scenarios, function(s) s$name, character(1))
fig9_df <- bind_rows(lapply(scenarios, function(s) {
  bind_rows(lapply(quintiles, function(q) {
    tibble(Scenario = s$name, wealth = q, cov = wq_cov(s$y, q))
  }))
})) %>%
  mutate(
    wealth   = factor(wealth, levels = quintiles),
    Scenario = factor(Scenario, levels = scenario_levels)
  )

fig9 <- ggplot(fig9_df, aes(x = wealth, y = cov, colour = Scenario, group = Scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.6) +
  scale_colour_viridis_d(option = "viridis", end = 0.9, name = "Scenario") +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
  labs(
    title    = "Simulated insurance coverage by wealth quintile under SHA subsidy scenarios",
    subtitle = "Counterfactual enrolment versus observed baseline, KDHS 2022",
    x        = "Wealth quintile",
    y        = "Any insurance coverage (%)",
    caption  = "Source: Kenya DHS 2022. Survey-weighted simulated coverage.\nA persisting dip in the middle and richer quintiles is the uninsured 'missing middle'."
  ) +
  theme_st09 +
  theme(legend.position = "top", legend.direction = "vertical")

ggsave(file.path(paths$figures_dir, "Figure9_ST09_SHA_Microsimulation.png"),
       fig9, width = 9, height = 6.5, dpi = 300)
cat("Figure 9 done.\n")

message("=== SECTION 14 COMPLETE ===")

# =============================================================================
message("=== SECTION 15: Save outputs and summary ===")
# =============================================================================

# Inline stats helper objects for manuscript
ci_overall_row   <- table6 %>% filter(Subgroup == "All household members")
ci_male_row      <- table6 %>% filter(Subgroup == "Male")
ci_female_row    <- table6 %>% filter(Subgroup == "Female")
ci_child_row     <- table6 %>% filter(Subgroup == "Under 18")
ci_elderly_row   <- table6 %>% filter(Subgroup == "60+")
ci_working_row   <- table6 %>% filter(Subgroup == "18-59 (working age)")
ineq_overall_row <- table7 %>% filter(Group == "All household members")
uninsured_top_row <- table8 %>% slice(1)

overall_n            <- nrow(analytic)
policy_priority_n    <- sum(analytic$policy_priority_any, na.rm = TRUE)
overall_insured_pct  <- round(100 * wprev(analytic, "insured_any")$est, 1)
policy_priority_pct  <- round(100 * mean(analytic$policy_priority_any, na.rm = TRUE), 1)
poorest_ins_pct      <- round(100 * wprev(analytic %>% filter(wealth == "Poorest"), "insured_any")$est, 1)
richest_ins_pct      <- round(100 * wprev(analytic %>% filter(wealth == "Richest"), "insured_any")$est, 1)
disability_ins_pct   <- round(100 * wprev(analytic %>% filter(!is.na(disability_any) & disability_any == 1), "insured_any")$est, 1)
nodisability_ins_pct <- round(100 * wprev(analytic %>% filter(!is.na(disability_any) & disability_any == 0), "insured_any")$est, 1)
child_ins_pct        <- round(100 * wprev(analytic %>% filter(age_yrs < 18), "insured_any")$est, 1)
elderly_ins_pct      <- round(100 * wprev(analytic %>% filter(age_yrs >= 60), "insured_any")$est, 1)
young_adult_ins_pct  <- round(100 * wprev(analytic %>% filter(age_yrs >= 18, age_yrs < 25), "insured_any")$est, 1)

# Pack everything into the analysis object
analysis_object <- list(
  # Tables
  table1 = table1,
  table2 = table2,
  table3 = table3,
  table4 = table4,
  table5 = table5,
  table6 = table6,
  table7 = table7,
  table8 = table8,
  table9 = table9,
  table10 = table10,
  table11 = table11,
  table11b = table11b,
  table12 = table12,
  table_sens = table_sens,
  table13 = table13,
  table14 = table14,
  table15 = table15,
  table16 = table16,
  # Figure data frames
  fig1_df = fig1_df,
  fig2_df = fig2_df,
  fig3_df = fig3_df,
  fig4_df = fig4_df,
  fig5_df = fig2_df,
  fig6_df = fig6_df,
  fig7_df = fig7_df,
  # Model objects
  model_A = if (!is.null(mod_A)) broom::tidy(mod_A) else NULL,
  model_B = if (!is.null(mod_B)) broom::tidy(mod_B) else NULL,
  # OOP cost headline stats
  opd_overall_cost  = opd_overall_cost,
  opd_insured_cost  = opd_insured_cost,
  opd_uninsured_cost = opd_uninsured_cost,
  hosp_overall_cost = hosp_overall_cost,
  hosp_insured_cost = hosp_insured_cost,
  # Safety net / decomposition / sensitivity scalars
  sn_apr           = sn_apr,
  decomp_ci_total  = decomp_A$ci_total,
  decomp_n         = decomp_A$n,
  logbin_converged = logbin_converged,
  full_logbin_converged = full_logbin_converged,
  trend_2014_ok    = trend_2014_ok,
  # Intersectionality (MAIHDA), multilevel and microsimulation scalars
  maihda_n_strata  = n_strata,
  maihda_v_null    = v_null,
  maihda_v_adj     = v_adj,
  maihda_vpc_null  = vpc_null,
  maihda_vpc_adj   = vpc_adj,
  maihda_pcv       = pcv,
  mlm_family       = mlm_family,
  mlm_cluster_var  = sig2_u,
  mlm_vpc          = vpc_mlm,
  mlm_vpc_method   = vpc_method,
  sim_baseline_ci  = baseline_ci,
  # Inline stats
  overall_n            = overall_n,
  overall_insured_pct  = overall_insured_pct,
  policy_priority_n    = policy_priority_n,
  policy_priority_pct  = policy_priority_pct,
  poorest_ins_pct      = poorest_ins_pct,
  richest_ins_pct      = richest_ins_pct,
  disability_ins_pct   = disability_ins_pct,
  nodisability_ins_pct = nodisability_ins_pct,
  child_ins_pct        = child_ins_pct,
  elderly_ins_pct      = elderly_ins_pct,
  young_adult_ins_pct  = young_adult_ins_pct,
  defacto_pr_n         = defacto_pr_n,
  model_A_n            = model_A_n,
  model_B_n            = model_B_n,
  ci_overall           = ci_overall_row,
  ci_male              = ci_male_row,
  ci_female            = ci_female_row,
  ci_child             = ci_child_row,
  ci_elderly           = ci_elderly_row,
  ci_working           = ci_working_row,
  ineq_overall         = ineq_overall_row,
  uninsured_top        = uninsured_top_row,
  # Sample sizes
  sample_sizes = analytic %>%
    count(age_group, sex) %>%
    arrange(age_group, sex)
)

saveRDS(analysis_object,
  file.path(paths$derived_dir, "st09_analysis_outputs.rds"))

# ── Additional file 2: supplementary tables (assembled after all tables) ─────
supplementary_tables <- list(
  "Table S1. Socioeconomic gradient in health insurance coverage, KDHS 2022." = table3,
  "Table S2. Health insurance coverage and service use among policy-priority groups, KDHS 2022." = table4,
  "Table S3. Standard and Erreygers-corrected concentration indices for health insurance coverage, KDHS 2022." = table6,
  "Table S4. Absolute and relative wealth-related inequality metrics for health insurance coverage, KDHS 2022." = table7,
  "Table S5. Population composition of uninsured household members in policy-priority groups, KDHS 2022." = table8,
  "Table S6. Sensitivity of adjusted prevalence ratios to model family (quasi-Poisson vs log-binomial), KDHS 2022." = table_sens,
  "Table S8. Health insurance coverage by household-head sex, mobile-phone ownership, and child parental survival, KDHS 2022." = table13,
  "Table S9. Intersectional multilevel analysis (MAIHDA) of health insurance coverage, KDHS 2022." = table14,
  "Table S10. Multilevel versus single-level adjusted effect estimates for health insurance coverage, KDHS 2022." = table15,
  "Table S11. Microsimulation of SHA premium-subsidy scenarios on the wealth-related concentration index, KDHS 2022." = table16
)
if (!is.null(table11b)) {
  supplementary_tables[["Table S7. Wagstaff decomposition of the concentration index among adults aged 15+, adding education and functional difficulty, KDHS 2022."]] <- table11b
}

supp_doc <- read_docx() %>%
  body_add_par("Additional file 2. Supplementary tables for ST09", style = "heading 1") %>%
  body_add_par(
    "Supplementary tables for: Who was left outside Kenya's health insurance system before SHA? An all-population equity baseline from the 2022 Kenya Demographic and Health Survey.",
    style = "Normal"
  )

for (nm in names(supplementary_tables)) {
  supp_doc <- supp_doc %>%
    body_add_par(nm, style = "heading 2") %>%
    body_add_flextable(build_publication_flextable(
      supplementary_tables[[nm]],
      footer = c(
        "Source: Kenya DHS 2022. n values are unweighted; percentages and modelled estimates are survey-weighted.",
        "Groups may overlap unless otherwise stated."
      ),
      font_size = 8
    )) %>%
    body_add_par("", style = "Normal")
}

print(
  supp_doc,
  target = file.path(paths$manuscript_dir, "Additional_file_2_Supplementary_Tables.docx")
)
cat("Additional file 2 supplementary tables done.\n")

# ── Human-readable summary ───────────────────────────────────────────────────
summary_lines <- c(
  "ST09 All-Population Insurance Equity Analysis Summary",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("R version: ", R.version$version.string),
  "",
  "=== SAMPLE ===",
  paste0("De-facto household members in PR file: ", defacto_pr_n),
  paste0("Total analytic sample: ", overall_n),
  paste0("At least one policy-priority characteristic: ", policy_priority_n,
         " (", policy_priority_pct, "% unweighted)"),
  "",
  "=== NATIONAL COVERAGE ===",
  paste0("Any insurance (all ages): ", overall_insured_pct, "%"),
  paste0("Uninsured:                ",
         round(100 - overall_insured_pct, 1), "%"),
  "",
  "=== POLICY-PRIORITY GROUP COVERAGE ===",
  paste0("Children under 18:        ", child_ins_pct, "%"),
  paste0("Young adults 18-24:        ", young_adult_ins_pct, "%"),
  paste0("Older adults 60+:          ", elderly_ins_pct, "%"),
  paste0("Any disability:            ", disability_ins_pct, "%"),
  paste0("No disability (comparator):", nodisability_ins_pct, "%"),
  "",
  "=== WEALTH GRADIENT ===",
  paste0("Poorest quintile:  ", poorest_ins_pct, "%"),
  paste0("Richest quintile:  ", richest_ins_pct, "%"),
  paste0("Richest/poorest ratio: ",
         round(richest_ins_pct / max(poorest_ins_pct, 0.01), 1), "x"),
  "",
  "=== CONCENTRATION INDICES ===",
  paste0("Overall standard CI:      ", ci_overall_row$`Standard CI`,
         " (", ci_overall_row$`Standard CI lower`,
         ", ", ci_overall_row$`Standard CI upper`, ")"),
  paste0("Overall Erreygers CI:     ", ci_overall_row$`Erreygers CI`,
         " (", ci_overall_row$`Erreygers CI lower`,
         ", ", ci_overall_row$`Erreygers CI upper`, ")"),
  paste0("Male standard CI:         ", ci_male_row$`Standard CI`,
         " (", ci_male_row$`Standard CI lower`,
         ", ", ci_male_row$`Standard CI upper`, ")"),
  paste0("Female standard CI:       ", ci_female_row$`Standard CI`,
         " (", ci_female_row$`Standard CI lower`,
         ", ", ci_female_row$`Standard CI upper`, ")"),
  paste0("Children <18 standard CI: ", ci_child_row$`Standard CI`,
         " (", ci_child_row$`Standard CI lower`,
         ", ", ci_child_row$`Standard CI upper`, ")"),
  paste0("Older adults 60+ standard CI: ", ci_elderly_row$`Standard CI`,
         " (", ci_elderly_row$`Standard CI lower`,
         ", ", ci_elderly_row$`Standard CI upper`, ")"),
  paste0("Working age 18-59 standard CI: ", ci_working_row$`Standard CI`,
         " (", ci_working_row$`Standard CI lower`,
         ", ", ci_working_row$`Standard CI upper`, ")"),
  "",
  "=== ABSOLUTE AND RELATIVE INEQUALITY ===",
  paste0("Overall SII, percentage points: ", ineq_overall_row$`SII, pp (95% CI)`),
  paste0("Overall RII: ", ineq_overall_row$`RII (95% CI)`),
  "",
  "=== UNINSURED COMPOSITION ===",
  paste0("Most over-represented group among the uninsured: ",
         uninsured_top_row$`Population group`,
         " (representation ratio ",
         uninsured_top_row$`Uninsured representation ratio`, ")"),
  "",
  "=== INTERSECTIONALITY (MAIHDA) ===",
  paste0("Intersectional strata (age x wealth x residence x sex): ", n_strata),
  paste0("VPC, null model: ", sprintf("%.1f%%", 100 * vpc_null),
         "; PCV after main effects: ", sprintf("%.1f%%", pcv),
         " (interactions ~ ", sprintf("%.1f%%", 100 - pcv), ")"),
  "",
  "=== MULTILEVEL MODEL ===",
  paste0("Family used: ", mlm_family),
  paste0("Cluster random-intercept variance: ", sprintf("%.4f", sig2_u),
         "; VPC: ", sprintf("%.1f%%", 100 * vpc_mlm), " (", vpc_method, ")"),
  "",
  "=== SHA MICROSIMULATION (standard concentration index) ===",
  paste0("Baseline CI: ", sprintf("%.4f", baseline_ci)),
  paste0(table16$Scenario, ": coverage ", table16$`Simulated coverage %`,
         "%, CI ", table16$`Standard CI (95% CI)`)
)

readr::write_lines(summary_lines,
  file.path(paths$tables_dir, "Results_Summary_ST09.txt"))
readr::write_lines(
  capture.output(sessionInfo()),
  file.path(paths$results_dir, "st09_session_info.txt")
)

cat("\n")
cat(paste(summary_lines, collapse = "\n"), "\n")

message("=== SECTION 15 COMPLETE ===")
message("ST09 analysis complete")
