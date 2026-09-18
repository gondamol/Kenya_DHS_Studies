# Study: ST06_Insurance_Trends_2008_2022
# Script: ST06_analysis_main.R
# Author: Nichodemus Werre Amollo
# Date: 2026-03-31
# Purpose: Multi-wave analysis of insurance coverage trends among women 15-49
#          across KDHS 2008, 2014, and 2022 using IR files (v481 insurance variable).

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
})

options(
  survey.lonely.psu = "adjust",
  scipen = 999
)

study_root <- "c:/Users/HFD 2/Research/02_Studies/ST06_Insurance_Trends_2008_2022"
data_root <- "c:/Users/HFD 2/Research/01_DHS_Data"

paths <- list(
  admin_dir = file.path(study_root, "00_Admin"),
  protocol_dir = file.path(study_root, "01_protocol"),
  data_notes_dir = file.path(study_root, "02_data_notes"),
  scripts_dir = file.path(study_root, "03_scripts"),
  tables_dir = file.path(study_root, "04_tables"),
  figures_dir = file.path(study_root, "05_figures"),
  manuscript_dir = file.path(study_root, "06_manuscript"),
  derived_dir = file.path(study_root, "07_derived_data"),
  logs_dir = file.path(study_root, "08_logs"),
  results_logs_dir = file.path(study_root, "results", "logs"),
  errors_log = file.path(study_root, "00_Admin", "errors_log.txt")
)

for (dir_path in unname(paths[c(
  "admin_dir", "data_notes_dir", "tables_dir", "figures_dir",
  "manuscript_dir", "derived_dir", "logs_dir", "results_logs_dir"
)])) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

append_error <- function(section_name, error_text) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] %s: %s", timestamp, section_name, error_text)
  cat(line, "\n", file = paths$errors_log, append = TRUE)
}

to_chr <- function(x) {
  stringr::str_squish(stringr::str_to_lower(as.character(as_factor(x))))
}

yn_flag <- function(x) {
  lbl <- to_chr(x)
  case_when(
    lbl == "yes" ~ 1L,
    lbl == "no" ~ 0L,
    TRUE ~ NA_integer_
  )
}

clean_label <- function(x) {
  out <- stringr::str_to_title(to_chr(x))
  out[out %in% c("Don't Know", "Dk", "Na")] <- NA_character_
  out
}

fmt_pct <- function(est, lo, hi) sprintf("%.1f (%.1f, %.1f)", 100 * est, 100 * lo, 100 * hi)
fmt_effect <- function(est, lo, hi, digits = 2) sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"), est, lo, hi)

make_design <- function(data, weight_var = "weight") {
  svydesign(ids = ~psu, strata = ~strata, weights = as.formula(paste0("~", weight_var)), data = data, nest = TRUE)
}

weighted_binary <- function(data, var, weight_var = "weight") {
  data_use <- data %>% filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]]))
  if (nrow(data_use) == 0) {
    return(tibble(unweighted_n = 0L, est = NA_real_, ci_low = NA_real_, ci_high = NA_real_, formatted = NA_character_))
  }
  d <- make_design(data_use, weight_var)
  e <- svymean(as.formula(paste0("~", var)), d, na.rm = TRUE)
  ci <- suppressWarnings(confint(e))
  tibble(
    unweighted_n = nrow(data_use),
    est = as.numeric(coef(e)[1]),
    ci_low = ci[1, 1],
    ci_high = ci[1, 2],
    formatted = fmt_pct(est, ci_low, ci_high)
  )
}

weighted_by_group <- function(data, group_var, outcome_var, weight_var = "weight") {
  groups <- sort(unique(stats::na.omit(as.character(data[[group_var]]))))
  bind_rows(lapply(groups, function(g) {
    st <- data %>% filter(as.character(.data[[group_var]]) == g) %>% weighted_binary(outcome_var, weight_var)
    tibble(group = g, unweighted_n = st$unweighted_n, estimate = st$est, ci_low = st$ci_low, ci_high = st$ci_high)
  }))
}

weighted_by_wave_group <- function(data, wave_var, group_var, outcome_var, weight_var = "weight") {
  waves <- sort(unique(stats::na.omit(as.character(data[[wave_var]]))))
  groups <- sort(unique(stats::na.omit(as.character(data[[group_var]]))))
  bind_rows(lapply(waves, function(w) {
    d_wave <- data %>% filter(as.character(.data[[wave_var]]) == w)
    bind_rows(lapply(groups, function(g) {
      d_sub <- d_wave %>% filter(as.character(.data[[group_var]]) == g)
      st <- weighted_binary(d_sub, outcome_var, weight_var)
      tibble(wave = w, group = g, unweighted_n = st$unweighted_n, estimate = st$est, ci_low = st$ci_low, ci_high = st$ci_high)
    }))
  }))
}

extract_model_table <- function(model, model_label) {
  broom::tidy(model) %>%
    mutate(
      effect = exp(estimate),
      ci_low = exp(estimate - 1.96 * std.error),
      ci_high = exp(estimate + 1.96 * std.error),
      model = model_label
    ) %>%
    select(model, term, estimate, std.error, statistic, p.value, effect, ci_low, ci_high)
}

extract_effect <- function(model_table, model_label, term_label) {
  row <- model_table %>% filter(model == model_label, term == term_label)
  if (nrow(row) == 0) return(NA_character_)
  fmt_effect(row$effect[[1]], row$ci_low[[1]], row$ci_high[[1]], 2)
}

save_table_bundle <- function(data, csv_path, docx_path, caption_text, footer_lines = c("Source: Kenya DHS 2008, 2014, 2022. Survey-weighted estimates.")) {
  readr::write_csv(data, csv_path)
  ft <- flextable(data) %>% autofit() %>% fontsize(size = 9, part = "all") %>% set_caption(caption = caption_text)
  if (length(footer_lines) > 0) ft <- add_footer_lines(ft, values = footer_lines)
  read_docx() %>% body_add_flextable(ft) %>% print(target = docx_path)
}

message("=== SECTION 0 COMPLETE ===")

message("=== SECTION 1: Data import and harmonization across waves ===")

# 2008 IR
ir08 <- read_dta(file.path(data_root, "KDHS_2008", "IR_Individual_Recode", "KEIR52FL.DTA")) %>%
  transmute(
    wave = "2008",
    cluster = as.numeric(v001),
    household = as.numeric(v002),
    line = as.numeric(v003),
    weight = as.numeric(v005) / 1e6,
    psu = as.numeric(v021),
    strata = as.numeric(v022),
    age = as.numeric(v013),
    education = clean_label(v106),
    wealth = clean_label(v190),
    residence = clean_label(v025),
    region = clean_label(v024),
    marital = clean_label(v502),
    insured_any = yn_flag(v481),
    insured_nhif = yn_flag(v481a),
    insured_other_gov = yn_flag(v481b),
    insured_community = yn_flag(v481c),
    insured_private = yn_flag(v481d),
    insured_other = yn_flag(v481e)
  )

# 2014 IR
ir14 <- read_dta(file.path(data_root, "KDHS_2014", "IR_Individual_Recode", "KEIR72FL.DTA")) %>%
  transmute(
    wave = "2014",
    cluster = as.numeric(v001),
    household = as.numeric(v002),
    line = as.numeric(v003),
    weight = as.numeric(v005) / 1e6,
    psu = as.numeric(v021),
    strata = as.numeric(v022),
    age = as.numeric(v013),
    education = clean_label(v106),
    wealth = clean_label(v190),
    residence = clean_label(v025),
    region = clean_label(v024),
    marital = clean_label(v502),
    insured_any = yn_flag(v481),
    insured_nhif = yn_flag(v481a),
    insured_other_gov = yn_flag(v481b),
    insured_community = yn_flag(v481c),
    insured_private = yn_flag(v481d),
    insured_other = yn_flag(v481e)
  )

# 2022 PR (v481 is NA in 2022 IR; use PR sh27/sh28 and filter to women 15-49)
# hv104: haven_labelled double, 1=Male, 2=Female (has value labels in this DTA).
# sh27: 0=No, 1=Yes, 8=Don't know (treat 8 as NA).
# Insurance types in 2022 KEPR8CFL.DTA:
#   sh28a = NHIF, sh28b = private/commercial, sh28c = community-based, sh28x = other.
# NOTE: sh28d and sh28e do NOT exist in 2022 KEPR8CFL.DTA — using sh28b/sh28x instead.
ir22 <- read_dta(file.path(data_root, "KDHS_2022", "PR_Person_Recode", "KEPR8CFL.DTA"),
  col_select = c(hv001, hv002, hvidx, hv005, hv021, hv022, hv104, hv105, hv106, hv270, hv024, hv025, sh27, sh28a, sh28b, sh28c, sh28x)) %>%
  filter(as.numeric(hv105) >= 15, as.numeric(hv105) <= 49, as.numeric(hv104) == 2) %>%
  transmute(
    wave = "2022",
    cluster = as.numeric(hv001),
    household = as.numeric(hv002),
    line = as.numeric(hvidx),
    weight = as.numeric(hv005) / 1e6,
    psu = as.numeric(hv021),
    strata = as.numeric(hv022),
    age = case_when(
      as.numeric(hv105) >= 15 & as.numeric(hv105) <= 19 ~ 1L,
      as.numeric(hv105) >= 20 & as.numeric(hv105) <= 24 ~ 2L,
      as.numeric(hv105) >= 25 & as.numeric(hv105) <= 29 ~ 3L,
      as.numeric(hv105) >= 30 & as.numeric(hv105) <= 34 ~ 4L,
      as.numeric(hv105) >= 35 & as.numeric(hv105) <= 39 ~ 5L,
      as.numeric(hv105) >= 40 & as.numeric(hv105) <= 44 ~ 6L,
      as.numeric(hv105) >= 45 & as.numeric(hv105) <= 49 ~ 7L
    ),
    education = factor(case_when(
      as.numeric(hv106) == 0 ~ "No Education",
      as.numeric(hv106) == 1 ~ "Primary",
      as.numeric(hv106) == 2 ~ "Secondary",
      as.numeric(hv106) == 3 ~ "Higher",
      TRUE ~ NA_character_
    ), levels = c("No Education", "Primary", "Secondary", "Higher")),
    wealth = factor(case_when(
      as.numeric(hv270) == 1 ~ "Poorest",
      as.numeric(hv270) == 2 ~ "Poorer",
      as.numeric(hv270) == 3 ~ "Middle",
      as.numeric(hv270) == 4 ~ "Richer",
      as.numeric(hv270) == 5 ~ "Richest",
      TRUE ~ NA_character_
    ), levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    residence = factor(case_when(
      as.numeric(hv025) == 1 ~ "Urban",
      as.numeric(hv025) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ), levels = c("Urban", "Rural")),
    region = factor(case_when(
      as.numeric(hv024) == 47 ~ "Nairobi",
      as.numeric(hv024) %in% c(13:23, 36) ~ "Central",
      as.numeric(hv024) %in% c(1:7) ~ "Coast",
      as.numeric(hv024) %in% c(8:12) ~ "North Eastern",
      as.numeric(hv024) %in% c(38:43) ~ "Western",
      as.numeric(hv024) %in% c(24:35) ~ "Rift Valley",
      as.numeric(hv024) %in% c(44:46) ~ "Eastern",
      as.numeric(hv024) %in% c(28:37) ~ "Nyanza",
      TRUE ~ NA_character_
    )),
    marital = NA_character_,
    # sh27: 0=no, 1=yes, 8=don't know -> treat 8 as NA
    insured_any = case_when(
      as.numeric(sh27) == 1 ~ 1L,
      as.numeric(sh27) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    insured_nhif      = as.integer(as.numeric(sh28a) == 1),
    # 2022 PR has no "other government" category; set to NA for harmonization
    insured_other_gov = NA_integer_,
    insured_community = as.integer(as.numeric(sh28c) == 1),
    insured_private   = as.integer(as.numeric(sh28b) == 1),  # sh28b = private/commercial
    insured_other     = as.integer(as.numeric(sh28x) == 1)   # sh28x = other (sh28d/sh28e absent)
  )

# Combine
analytic <- bind_rows(ir08, ir14, ir22) %>%
  mutate(
    wave = factor(wave, levels = c("2008", "2014", "2022")),
    age_group = case_when(
      age == 1 ~ "15-19",
      age == 2 ~ "20-24",
      age == 3 ~ "25-29",
      age == 4 ~ "30-34",
      age == 5 ~ "35-39",
      age == 6 ~ "40-44",
      age == 7 ~ "45-49",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")),
    education = factor(education, levels = c("No Education", "Primary", "Secondary", "Higher")),
    wealth = factor(wealth, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    uninsured = case_when(insured_any == 1 ~ 0L, insured_any == 0 ~ 1L, TRUE ~ NA_integer_)
  ) %>%
  filter(!is.na(insured_any))

cat("Combined analytic sample:", nrow(analytic), "\n")
cat("By wave:\n")
print(table(analytic$wave))

readr::write_csv(analytic, file.path(paths$data_notes_dir, "ST06_combined_analytic.csv"))

message("=== SECTION 1 COMPLETE ===")

message("=== SECTION 2: Descriptive trend tables ===")

# Table 1: National trends by wave
table1 <- bind_rows(
  lapply(c("2008", "2014", "2022"), function(w) {
    d <- analytic %>% filter(wave == w)
    tibble(
      Wave = w,
      `Unweighted n` = nrow(d),
      `Any insurance % (95% CI)` = weighted_binary(d, "insured_any")$formatted,
      `NHIF % (95% CI)` = weighted_binary(d, "insured_nhif")$formatted,
      `Other government % (95% CI)` = weighted_binary(d, "insured_other_gov")$formatted,
      `Community-based % (95% CI)` = weighted_binary(d, "insured_community")$formatted,
      `Private % (95% CI)` = weighted_binary(d, "insured_private")$formatted,
      `Other % (95% CI)` = weighted_binary(d, "insured_other")$formatted,
      `Uninsured % (95% CI)` = weighted_binary(d, "uninsured")$formatted
    )
  })
)

save_table_bundle(
  table1,
  file.path(paths$tables_dir, "Table1_ST06_National_Trends.csv"),
  file.path(paths$tables_dir, "Table1_ST06_National_Trends.docx"),
  "Table 1. National insurance coverage trends among women aged 15-49 in Kenya, KDHS 2008-2022."
)

# Table 2: Subgroup trends
table2 <- bind_rows(
  lapply(c("2008", "2014", "2022"), function(w) {
    d <- analytic %>% filter(wave == w)
    tbl_wealth <- weighted_by_group(d, "wealth", "insured_any") %>% mutate(Subgroup = "Wealth", Wave = w)
    tbl_residence <- weighted_by_group(d, "residence", "insured_any") %>% mutate(Subgroup = "Residence", Wave = w)
    tbl_education <- weighted_by_group(d, "education", "insured_any") %>% mutate(Subgroup = "Education", Wave = w)
    tbl_age <- weighted_by_group(d, "age_group", "insured_any") %>% mutate(Subgroup = "Age", Wave = w)
    tbl_region <- weighted_by_group(d, "region", "insured_any") %>% mutate(Subgroup = "Region", Wave = w)
    bind_rows(tbl_wealth, tbl_residence, tbl_education, tbl_age, tbl_region) %>%
      mutate(
        `Insurance % (95% CI)` = fmt_pct(estimate, ci_low, ci_high),
        `Unweighted n` = unweighted_n
      ) %>%
      select(Wave, Subgroup, group, `Unweighted n`, `Insurance % (95% CI)`)
  })
)

save_table_bundle(
  table2,
  file.path(paths$tables_dir, "Table2_ST06_Subgroup_Trends.csv"),
  file.path(paths$tables_dir, "Table2_ST06_Subgroup_Trends.docx"),
  "Table 2. Insurance coverage by subgroup and survey wave, women aged 15-49, KDHS 2008-2022."
)

message("=== SECTION 2 COMPLETE ===")

message("=== SECTION 3: Regression and inequality analyses ===")

# Model: insurance ~ wave + covariates (test for trend adjusting for composition)
model_df <- analytic %>%
  mutate(
    wave_num = as.numeric(as.character(wave)),
    education = factor(education),
    wealth = factor(wealth),
    residence = factor(residence),
    age_group = factor(age_group),
    region = factor(region),
    marital = factor(marital)
  ) %>%
  filter(complete.cases(insured_any, wave, education, wealth, residence, age_group, region))

design_all <- make_design(model_df)
mod_trend_pr <- svyglm(insured_any ~ wave + education + wealth + residence + age_group + region, design = design_all, family = quasipoisson(link = "log"))
mod_trend_or <- svyglm(insured_any ~ wave + education + wealth + residence + age_group + region, design = design_all, family = quasibinomial())

model_dump <- bind_rows(
  extract_model_table(mod_trend_pr, "Insurance trend (APR model)"),
  extract_model_table(mod_trend_or, "Insurance trend (OR sensitivity)")
) %>%
  mutate(across(c(estimate, std.error, statistic, p.value, effect, ci_low, ci_high), ~ round(.x, 4)))

table3 <- bind_rows(
  tibble(Covariate = "Outcome: Any insurance (APR model)", `APR (95% CI)` = "", `p-value` = ""),
  tibble(Covariate = "2014 vs 2008", `APR (95% CI)` = extract_effect(model_dump, "Insurance trend (APR model)", "wave2014"), `p-value` = sprintf("%.4f", model_dump$p.value[model_dump$model == "Insurance trend (APR model)" & model_dump$term == "wave2014"])),
  tibble(Covariate = "2022 vs 2008", `APR (95% CI)` = extract_effect(model_dump, "Insurance trend (APR model)", "wave2022"), `p-value` = sprintf("%.4f", model_dump$p.value[model_dump$model == "Insurance trend (APR model)" & model_dump$term == "wave2022"])),
  tibble(Covariate = "Secondary vs no education", `APR (95% CI)` = extract_effect(model_dump, "Insurance trend (APR model)", "educationSecondary"), `p-value` = sprintf("%.4f", model_dump$p.value[model_dump$model == "Insurance trend (APR model)" & model_dump$term == "educationSecondary"])),
  tibble(Covariate = "Higher vs no education", `APR (95% CI)` = extract_effect(model_dump, "Insurance trend (APR model)", "educationHigher"), `p-value` = sprintf("%.4f", model_dump$p.value[model_dump$model == "Insurance trend (APR model)" & model_dump$term == "educationHigher"])),
  tibble(Covariate = "Richest vs poorest", `APR (95% CI)` = extract_effect(model_dump, "Insurance trend (APR model)", "wealthRichest"), `p-value` = sprintf("%.4f", model_dump$p.value[model_dump$model == "Insurance trend (APR model)" & model_dump$term == "wealthRichest"])),
  tibble(Covariate = "Urban vs rural", `APR (95% CI)` = extract_effect(model_dump, "Insurance trend (APR model)", "residenceUrban"), `p-value` = sprintf("%.4f", model_dump$p.value[model_dump$model == "Insurance trend (APR model)" & model_dump$term == "residenceUrban"]))
)

save_table_bundle(
  table3,
  file.path(paths$tables_dir, "Table3_ST06_Multivariable_Trend.csv"),
  file.path(paths$tables_dir, "Table3_ST06_Multivariable_Trend.docx"),
  "Table 3. Multivariable survey-weighted adjusted prevalence ratios for insurance coverage trends, women aged 15-49."
)

# Inequality: concentration index by wave
calc_ci_wave <- function(data, wave_val) {
  d <- data %>% filter(wave == wave_val, !is.na(insured_any), !is.na(wealth)) %>%
    mutate(
      y = insured_any,
      rank_x = as.numeric(wealth),
      wt = weight
    )
  if (nrow(d) < 30 || mean(d$y, na.rm = TRUE) == 0) {
    return(tibble(Wave = wave_val, `Concentration index` = NA_real_, `95% CI lower` = NA_real_, `95% CI upper` = NA_real_))
  }
  calc_ci <- function(input) {
    ranked <- input %>% arrange(rank_x) %>% mutate(w_norm = wt / sum(wt), frac_rank = cumsum(w_norm) - 0.5 * w_norm)
    mu <- weighted.mean(ranked$y, ranked$wt)
    if (!is.finite(mu) || mu == 0) return(NA_real_)
    cov_val <- weighted.mean((ranked$y - mu) * (ranked$frac_rank - 0.5), ranked$wt)
    2 * cov_val / mu
  }
  set.seed(20260331)
  point <- calc_ci(d)
  boot <- replicate(400, { idx <- sample.int(nrow(d), replace = TRUE); calc_ci(d[idx, , drop = FALSE]) })
  boot <- boot[is.finite(boot)]
  tibble(
    Wave = wave_val,
    `Concentration index` = round(point, 4),
    `95% CI lower` = round(as.numeric(quantile(boot, 0.025, na.rm = TRUE)), 4),
    `95% CI upper` = round(as.numeric(quantile(boot, 0.975, na.rm = TRUE)), 4)
  )
}

table4 <- bind_rows(
  calc_ci_wave(analytic, "2008"),
  calc_ci_wave(analytic, "2014"),
  calc_ci_wave(analytic, "2022")
)

save_table_bundle(
  table4,
  file.path(paths$tables_dir, "Table4_ST06_Concentration_Indices.csv"),
  file.path(paths$tables_dir, "Table4_ST06_Concentration_Indices.docx"),
  "Table 4. Wealth-related concentration indices for insurance coverage by survey wave, women aged 15-49."
)

message("=== SECTION 3 COMPLETE ===")

message("=== SECTION 4: Figures ===")

# Figure 1: National trend line
fig1_df <- bind_rows(
  lapply(c("2008", "2014", "2022"), function(w) {
    d <- analytic %>% filter(wave == w)
    st <- weighted_binary(d, "insured_any")
    tibble(wave = w, estimate_pct = 100 * st$est, ci_low_pct = 100 * st$ci_low, ci_high_pct = 100 * st$ci_high)
  })
) %>% mutate(wave_num = as.numeric(wave))

fig1 <- ggplot(fig1_df, aes(x = wave_num, y = estimate_pct, group = 1)) +
  geom_line(color = "#0f6e8c", linewidth = 1.2) +
  geom_point(color = "#0f6e8c", size = 3.5) +
  geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct), width = 0.15, color = "#0f6e8c") +
  scale_x_continuous(breaks = c(2008, 2014, 2022), labels = c("2008", "2014", "2022")) +
  labs(
    title = "Insurance coverage trends among women aged 15-49 in Kenya",
    subtitle = "Kenya Demographic and Health Surveys 2008, 2014, 2022",
    x = "Survey year",
    y = "Any insurance coverage (%)",
    caption = "Source: Kenya DHS 2008, 2014, 2022. Survey-weighted estimates."
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(paths$figures_dir, "Figure1_ST06_National_Trend.png"), fig1, width = 8, height = 5, dpi = 300)

# Figure 2: Wealth gradient by wave
fig2_df <- bind_rows(
  lapply(c("2008", "2014", "2022"), function(w) {
    d <- analytic %>% filter(wave == w)
    weighted_by_group(d, "wealth", "insured_any") %>%
      mutate(wave = w, estimate_pct = 100 * estimate, ci_low_pct = 100 * ci_low, ci_high_pct = 100 * ci_high)
  })
) %>% mutate(
  wealth = factor(group, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
  wave = factor(wave, levels = c("2008", "2014", "2022"))
)

fig2 <- ggplot(fig2_df, aes(x = wealth, y = estimate_pct, color = wave, group = wave)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct), width = 0.1) +
  scale_color_manual(values = c("2008" = "#999999", "2014" = "#0f6e8c", "2022" = "#2f855a")) +
  labs(
    title = "Wealth gradient in insurance coverage across survey waves",
    subtitle = "Women aged 15-49 in Kenya, KDHS 2008, 2014, 2022",
    x = "Wealth quintile",
    y = "Any insurance coverage (%)",
    color = "Survey year",
    caption = "Source: Kenya DHS 2008, 2014, 2022. Survey-weighted estimates."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(file.path(paths$figures_dir, "Figure2_ST06_Wealth_Gradient_Trend.png"), fig2, width = 9, height = 6, dpi = 300)

# Figure 3: Inequality trend (concentration indices)
fig3 <- ggplot(table4, aes(x = as.numeric(Wave), y = `Concentration index`)) +
  geom_line(color = "#cf5c36", linewidth = 1.1) +
  geom_point(color = "#cf5c36", size = 3) +
  geom_errorbar(aes(ymin = `95% CI lower`, ymax = `95% CI upper`), width = 0.15, color = "#cf5c36") +
  scale_x_continuous(breaks = c(2008, 2014, 2022), labels = c("2008", "2014", "2022")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Wealth-related inequality in insurance coverage over time",
    subtitle = "Concentration indices with 95% confidence intervals, women aged 15-49",
    x = "Survey year",
    y = "Concentration index",
    caption = "Source: Kenya DHS 2008, 2014, 2022. Bootstrap CIs (400 replicates)."
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(paths$figures_dir, "Figure3_ST06_Inequality_Trend.png"), fig3, width = 8, height = 5, dpi = 300)

# Figure 4: Insurance type composition by wave
fig4_df <- bind_rows(
  lapply(c("2008", "2014", "2022"), function(w) {
    d <- analytic %>% filter(wave == w)
    bind_rows(
      tibble(type = "NHIF", stat = weighted_binary(d, "insured_nhif")),
      tibble(type = "Other government", stat = weighted_binary(d, "insured_other_gov")),
      tibble(type = "Community-based", stat = weighted_binary(d, "insured_community")),
      tibble(type = "Private", stat = weighted_binary(d, "insured_private")),
      tibble(type = "Other", stat = weighted_binary(d, "insured_other"))
    ) %>% mutate(wave = w, estimate_pct = 100 * stat$est)
  })
) %>% mutate(
  type = factor(type, levels = c("NHIF", "Other government", "Community-based", "Private", "Other")),
  wave = factor(wave, levels = c("2008", "2014", "2022"))
)

fig4 <- ggplot(fig4_df, aes(x = wave, y = estimate_pct, fill = type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Composition of insurance types across survey waves",
    subtitle = "Women aged 15-49 in Kenya, KDHS 2008, 2014, 2022",
    x = "Survey year",
    y = "Coverage (%)",
    fill = NULL,
    caption = "Source: Kenya DHS 2008, 2014, 2022. Survey-weighted estimates."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(file.path(paths$figures_dir, "Figure4_ST06_Insurance_Composition.png"), fig4, width = 9, height = 6, dpi = 300)

message("=== SECTION 4 COMPLETE ===")

message("=== SECTION 5: Save outputs and write summary ===")

analysis_object <- list(
  table1 = table1,
  table2 = table2,
  table3 = table3,
  table4 = table4,
  model_dump = model_dump,
  fig1_df = fig1_df,
  fig2_df = fig2_df,
  sample_sizes = table(analytic$wave)
)

saveRDS(analysis_object, file.path(paths$derived_dir, "st06_analysis_outputs.rds"))

summary_lines <- c(
  "ST06 insurance trends analysis summary",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Sample sizes by wave:",
  paste0("  2008: ", sum(analytic$wave == "2008")),
  paste0("  2014: ", sum(analytic$wave == "2014")),
  paste0("  2022: ", sum(analytic$wave == "2022")),
  "",
  "National insurance coverage:",
  paste0("  2008: ", table1$`Any insurance % (95% CI)`[table1$Wave == "2008"]),
  paste0("  2014: ", table1$`Any insurance % (95% CI)`[table1$Wave == "2014"]),
  paste0("  2022: ", table1$`Any insurance % (95% CI)`[table1$Wave == "2022"]),
  "",
  "Concentration indices:",
  paste0("  2008: ", table4$`Concentration index`[table4$Wave == "2008"], " (", table4$`95% CI lower`[table4$Wave == "2008"], ", ", table4$`95% CI upper`[table4$Wave == "2008"], ")"),
  paste0("  2014: ", table4$`Concentration index`[table4$Wave == "2014"], " (", table4$`95% CI lower`[table4$Wave == "2014"], ", ", table4$`95% CI upper`[table4$Wave == "2014"], ")"),
  paste0("  2022: ", table4$`Concentration index`[table4$Wave == "2022"], " (", table4$`95% CI lower`[table4$Wave == "2022"], ", ", table4$`95% CI upper`[table4$Wave == "2022"], ")")
)

readr::write_lines(summary_lines, file.path(paths$tables_dir, "Results_Summary_ST06.txt"))
readr::write_lines(capture.output(sessionInfo()), file.path(paths$results_logs_dir, "st06_session_info.txt"))

message("=== SECTION 5 COMPLETE ===")
message("ST06 analysis complete")
