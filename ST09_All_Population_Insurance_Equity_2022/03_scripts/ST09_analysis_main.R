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
    sh27,                         # any insurance
    sh28a, sh28b, sh28c, sh28x,  # NHIF, private, community, other
    sh29, sh31, sh32,             # hospital admission, outpatient, paid OPD
    hdis9                         # disability summary (Washington Group)
  )
)

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

    # SHA vulnerability flag (≥1 exemption category)
    policy_priority_any = as.integer(
      age_yrs < 18 |
        age_yrs >= 60 |
        (!is.na(disability_any) & disability_any == 1) |
        (!is.na(wealth) & wealth %in% c("Poorest","Poorer"))
    )
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

supplementary_tables <- list(
  "Table S1. Socioeconomic gradient in health insurance coverage, KDHS 2022." = table3,
  "Table S2. Health insurance coverage and service use among policy-priority groups, KDHS 2022." = table4,
  "Table S3. Standard and Erreygers-corrected concentration indices for health insurance coverage, KDHS 2022." = table6,
  "Table S4. Absolute and relative wealth-related inequality metrics for health insurance coverage, KDHS 2022." = table7,
  "Table S5. Population composition of uninsured household members in policy-priority groups, KDHS 2022." = table8
)

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
message("=== SECTION 6: Save outputs and summary ===")
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
  # Figure data frames
  fig1_df = fig1_df,
  fig2_df = fig2_df,
  fig3_df = fig3_df,
  fig4_df = fig4_df,
  fig5_df = fig2_df,
  fig6_df = fig6_df,
  # Model objects
  model_A = if (!is.null(mod_A)) broom::tidy(mod_A) else NULL,
  model_B = if (!is.null(mod_B)) broom::tidy(mod_B) else NULL,
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
         uninsured_top_row$`Uninsured representation ratio`, ")")
)

readr::write_lines(summary_lines,
  file.path(paths$tables_dir, "Results_Summary_ST09.txt"))
readr::write_lines(
  capture.output(sessionInfo()),
  file.path(paths$results_dir, "st09_session_info.txt")
)

cat("\n")
cat(paste(summary_lines, collapse = "\n"), "\n")

message("=== SECTION 6 COMPLETE ===")
message("ST09 analysis complete")
