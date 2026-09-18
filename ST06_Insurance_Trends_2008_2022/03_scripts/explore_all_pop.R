# =============================================================================
# ST06 (revised) - Feasibility Exploration: All-Population Insurance Analysis
# Script  : explore_all_pop.R
# Purpose : Determine whether all three KDHS PR files can support a
#           whole-population (all ages, both sexes) insurance trend study,
#           document eligible sample sizes, variable availability, and
#           cross-wave harmonisation constraints.
# Author  : Nichodemus Werre Amollo
# Date    : 2026-03-31
# =============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(labelled)
  library(tidyr)
})

options(scipen = 999)

data_root  <- "c:/Users/HFD 2/Research/01_DHS_Data"
study_root <- "c:/Users/HFD 2/Research/02_Studies/ST06_Insurance_Trends_2008_2022"
out_dir    <- file.path(study_root, "02_data_notes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(out_dir, "all_pop_exploration_log.txt")
sink(log_file, split = TRUE)    # write to console AND file

cat("=================================================================\n")
cat("All-Population Insurance Feasibility Exploration\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================================\n\n")

# ── Helper: describe one variable ─────────────────────────────────────────────
describe_var <- function(df, varname, label_override = NULL) {
  if (!varname %in% names(df)) {
    cat(sprintf("  %-14s : NOT PRESENT\n", varname))
    return(invisible(NULL))
  }
  x   <- df[[varname]]
  lbl <- if (!is.null(label_override)) label_override else attr(x, "label")
  val_labs <- attr(x, "labels")
  n_na <- sum(is.na(x))
  uniq <- sort(unique(as.numeric(x)))

  cat(sprintf("  %-14s : %s\n", varname,
              if (is.null(lbl)) "(no label)" else lbl))
  cat(sprintf("               NA=%d/%d  unique_numeric: %s\n",
              n_na, length(x),
              paste(head(uniq, 12), collapse = ", ")))
  if (!is.null(val_labs) && length(val_labs) <= 12) {
    cat("               labels:", paste(sprintf("%g=%s", val_labs, names(val_labs)), collapse = "  "), "\n")
  }
}

# =============================================================================
# SECTION 1: 2022 PR FILE
# =============================================================================
cat("=================================================================\n")
cat("SECTION 1: 2022 KDHS PR (KEPR8CFL.DTA)\n")
cat("=================================================================\n\n")

pr22_path <- file.path(data_root, "KDHS_2022", "PR_Person_Recode", "KEPR8CFL.DTA")

# --- 1a. Header scan ---------------------------------------------------------
pr22_hdr <- read_dta(pr22_path, n_max = 0)
cat("Total columns in 2022 PR:", ncol(pr22_hdr), "\n\n")

cat("--- Key household-member identifiers and eligibility flags ---\n")
id_vars <- c("hv001","hv002","hvidx","hv003","hv004",
             "hv015","hv103","hv104","hv105","hv106","hv270",
             "hv024","hv025","hv009")
for (v in id_vars) describe_var(pr22_hdr, v)

cat("\n--- Insurance module variables ---\n")
ins_vars <- c("sh27","sh28a","sh28b","sh28c","sh28x",
              "sh29","sh30","sh31","sh32","sh33","sh34")
for (v in ins_vars) describe_var(pr22_hdr, v)

cat("\n--- Disability module variables (hdis*) ---\n")
hdis_vars <- grep("^hdis", names(pr22_hdr), value = TRUE)
cat("hdis columns found:", length(hdis_vars), "\n")
for (v in head(hdis_vars, 10)) describe_var(pr22_hdr, v)

cat("\n--- Employment / occupation (hv*) ---\n")
emp_vars <- c("hv216","hv221","hv226","hv227","hv228",
              "sh35","sh36","sh37","sh38")
for (v in emp_vars) describe_var(pr22_hdr, v)

# --- 1b. Load core columns and characterise eligibility ----------------------
cat("\n--- Loading core columns for eligibility analysis ---\n")
pr22 <- read_dta(
  pr22_path,
  col_select = c(hv001, hv002, hvidx, hv003, hv004,
                 hv015, hv103, hv104, hv105, hv106,
                 hv270, hv024, hv025,
                 sh27, sh28a, sh28b, sh28c, sh28x,
                 sh29, sh30, sh31, sh32, sh33)
)
cat("Loaded rows:", nrow(pr22), "\n\n")

cat("--- hv103 (de-facto resident) x sh27 missingness ---\n")
print(table(
  `hv103 (de_facto)` = as.numeric(pr22$hv103),
  `sh27_missing`     = is.na(pr22$sh27),
  useNA = "ifany"
))

cat("\n--- hv015 (slept here last night) x sh27 missingness ---\n")
print(table(
  `hv015 (slept_here)` = as.numeric(pr22$hv015),
  `sh27_missing`       = is.na(pr22$sh27),
  useNA = "ifany"
))

# De-facto household members with sh27 data
pr22_defacto <- pr22 %>% filter(as.numeric(hv103) == 1)
pr22_sh27    <- pr22 %>% filter(!is.na(sh27))

cat("\n--- De-facto household members (hv103==1):", nrow(pr22_defacto), "---\n")
cat("--- Members with sh27 non-missing:         ", nrow(pr22_sh27),    "---\n\n")

# Age × sex distribution of sh27 respondents
cat("--- Age distribution of sh27 respondents (all ages) ---\n")
pr22_sh27 <- pr22_sh27 %>%
  mutate(
    age_group = cut(as.numeric(hv105),
      breaks = c(-1, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 59, 69, 98),
      labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-59","60-69","70+"),
      right  = TRUE
    ),
    sex = case_when(as.numeric(hv104) == 1 ~ "Male",
                    as.numeric(hv104) == 2 ~ "Female",
                    TRUE ~ NA_character_)
  )

age_sex_n <- pr22_sh27 %>%
  count(age_group, sex) %>%
  pivot_wider(names_from = sex, values_from = n, values_fill = 0L) %>%
  mutate(Total = Male + Female)

cat("Age group   Male     Female   Total\n")
print(as.data.frame(age_sex_n), row.names = FALSE)

cat("\nTotal with sh27 data by sex:\n")
print(table(pr22_sh27$sex))

cat("\n--- sh27 prevalence by broad age group ---\n")
pr22_sh27 %>%
  mutate(broad_age = case_when(
    as.numeric(hv105) <  5 ~ "Under 5",
    as.numeric(hv105) <  15 ~ "5-14",
    as.numeric(hv105) <  18 ~ "15-17 (adolescent)",
    as.numeric(hv105) <  25 ~ "18-24",
    as.numeric(hv105) <  40 ~ "25-39",
    as.numeric(hv105) <  60 ~ "40-59",
    TRUE                    ~ "60+"
  )) %>%
  group_by(broad_age) %>%
  summarise(
    n          = n(),
    insured_n  = sum(as.numeric(sh27) == 1, na.rm = TRUE),
    pct        = round(100 * insured_n / n, 1),
    .groups = "drop"
  ) %>%
  arrange(broad_age) %>%
  print()

cat("\n--- sh27 insurance prevalence summary for all ages ---\n")
cat("Overall: n =", nrow(pr22_sh27),
    "| insured =", sum(as.numeric(pr22_sh27$sh27) == 1, na.rm = TRUE),
    "| pct =",
    round(100 * mean(as.numeric(pr22_sh27$sh27) == 1, na.rm = TRUE), 1), "%\n")

cat("\n--- sh29 (hospital admission last 12m) available? ---\n")
cat("sh29 non-NA:", sum(!is.na(pr22$sh29)), "\n")
cat("sh30 non-NA:", sum(!is.na(pr22$sh30)), "\n")
cat("sh31 non-NA:", sum(!is.na(pr22$sh31)), "\n")
cat("sh32 non-NA:", sum(!is.na(pr22$sh32)), "\n")
cat("sh33 non-NA:", sum(!is.na(pr22$sh33)), "\n")

# =============================================================================
# SECTION 2: 2022 PR - DISABILITY & RELATIONSHIP VARIABLES
# =============================================================================
cat("\n=================================================================\n")
cat("SECTION 2: 2022 PR — disability, relationship, household-head\n")
cat("=================================================================\n\n")

pr22_dis <- read_dta(
  pr22_path,
  col_select = c(hv001, hv002, hvidx, hv104, hv105, hv101,
                 starts_with("hdis"), sh27)
)

cat("hv101 (relationship to HH head) labels:\n")
describe_var(pr22_dis, "hv101")

cat("\nDisability columns available:\n")
hdis_avail <- grep("^hdis", names(pr22_dis), value = TRUE)
cat(paste(hdis_avail, collapse = ", "), "\n\n")
for (v in hdis_avail) describe_var(pr22_dis, v)

# Derive any-disability flag
if ("hdis9" %in% names(pr22_dis)) {
  cat("\nhdis9 (summary disability) distribution:\n")
  print(table(as.numeric(pr22_dis$hdis9), useNA = "always"))
} else if (length(hdis_avail) > 0) {
  cat("\n(No hdis9; individual domain flags available)\n")
}

# =============================================================================
# SECTION 3: CHECK 2008 AND 2014 PR FILES
# =============================================================================
cat("\n=================================================================\n")
cat("SECTION 3: 2008 KDHS PR file check\n")
cat("=================================================================\n\n")

pr08_dir  <- file.path(data_root, "KDHS_2008", "PR_Person_Recode")
pr08_files <- list.files(pr08_dir, pattern = "\\.DTA$", ignore.case = TRUE, full.names = TRUE)

if (length(pr08_files) == 0) {
  cat("NO PR file found in:", pr08_dir, "\n")
  cat("Contents of KDHS_2008:\n")
  print(list.dirs(file.path(data_root, "KDHS_2008"), recursive = FALSE))
} else {
  cat("PR file found:", basename(pr08_files[1]), "\n")
  pr08_hdr <- read_dta(pr08_files[1], n_max = 0)
  cat("Columns:", ncol(pr08_hdr), "\n\n")

  cat("--- Insurance-related variables in 2008 PR ---\n")
  ins_candidates <- grep("(sh27|sh28|v481|insur)", names(pr08_hdr),
                         value = TRUE, ignore.case = TRUE)
  cat("Matches:", paste(ins_candidates, collapse = ", "), "\n")
  for (v in ins_candidates) describe_var(pr08_hdr, v)

  cat("\n--- Standard HH-member variables in 2008 PR ---\n")
  for (v in c("hv103","hv104","hv105","hv106","hv270","hv024","hv025")) {
    describe_var(pr08_hdr, v)
  }

  # Sample rows to check actual coverage
  pr08_sm <- read_dta(pr08_files[1],
    col_select = any_of(c("hv103","hv104","hv105","hv270","hv024","hv025",
                          "sh27","sh28a","hv481","v481")))
  cat("\nRows in 2008 PR:", nrow(pr08_sm), "\n")

  ins_cols_08 <- intersect(c("sh27","sh28a","hv481","v481"), names(pr08_sm))
  for (v in ins_cols_08) {
    cat(sprintf("\n%s non-NA: %d\n", v, sum(!is.na(pr08_sm[[v]]))))
    print(table(as.numeric(pr08_sm[[v]]), useNA = "always"))
  }
}

cat("\n=================================================================\n")
cat("SECTION 4: 2014 KDHS PR file check\n")
cat("=================================================================\n\n")

pr14_dir  <- file.path(data_root, "KDHS_2014", "PR_Person_Recode")
pr14_files <- list.files(pr14_dir, pattern = "\\.DTA$", ignore.case = TRUE, full.names = TRUE)

if (length(pr14_files) == 0) {
  cat("NO PR file found in:", pr14_dir, "\n")
  cat("Contents of KDHS_2014:\n")
  print(list.dirs(file.path(data_root, "KDHS_2014"), recursive = FALSE))
} else {
  cat("PR file found:", basename(pr14_files[1]), "\n")
  pr14_hdr <- read_dta(pr14_files[1], n_max = 0)
  cat("Columns:", ncol(pr14_hdr), "\n\n")

  cat("--- Insurance-related variables in 2014 PR ---\n")
  ins_candidates14 <- grep("(sh27|sh28|v481|insur)", names(pr14_hdr),
                           value = TRUE, ignore.case = TRUE)
  cat("Matches:", paste(ins_candidates14, collapse = ", "), "\n")
  for (v in ins_candidates14) describe_var(pr14_hdr, v)

  pr14_sm <- read_dta(pr14_files[1],
    col_select = any_of(c("hv103","hv104","hv105","hv270","hv024","hv025",
                          "sh27","sh28a","hv481","v481")))
  cat("\nRows in 2014 PR:", nrow(pr14_sm), "\n")

  ins_cols_14 <- intersect(c("sh27","sh28a","hv481","v481"), names(pr14_sm))
  for (v in ins_cols_14) {
    cat(sprintf("\n%s non-NA: %d\n", v, sum(!is.na(pr14_sm[[v]]))))
    print(table(as.numeric(pr14_sm[[v]]), useNA = "always"))
  }
}

# =============================================================================
# SECTION 5: WHAT WOULD A WHOLE-POPULATION STUDY LOOK LIKE?
# =============================================================================
cat("\n=================================================================\n")
cat("SECTION 5: Proposed all-population sample profile (2022 only)\n")
cat("=================================================================\n\n")

pr22_full <- read_dta(
  pr22_path,
  col_select = c(hv001, hv002, hvidx, hv103, hv104, hv105,
                 hv106, hv270, hv024, hv025, hv101,
                 sh27, sh28a, sh28b, sh28c, sh28x)
)

# Restrict to de-facto members with sh27 data
all_pop <- pr22_full %>%
  filter(as.numeric(hv103) == 1, !is.na(sh27)) %>%
  mutate(
    sex       = case_when(as.numeric(hv104) == 1 ~ "Male",
                          as.numeric(hv104) == 2 ~ "Female", TRUE ~ NA_character_),
    age_yrs   = as.numeric(hv105),
    age_group = case_when(
      age_yrs < 5  ~ "Under 5",
      age_yrs < 15 ~ "5-14",
      age_yrs < 18 ~ "15-17",
      age_yrs < 25 ~ "18-24",
      age_yrs < 35 ~ "25-34",
      age_yrs < 45 ~ "35-44",
      age_yrs < 60 ~ "45-59",
      age_yrs < 70 ~ "60-69",
      TRUE          ~ "70+"
    ),
    insured = case_when(as.numeric(sh27) == 1 ~ 1L,
                        as.numeric(sh27) == 0 ~ 0L,
                        TRUE ~ NA_integer_),
    wealth = case_when(
      as.numeric(hv270) == 1 ~ "Poorest",
      as.numeric(hv270) == 2 ~ "Poorer",
      as.numeric(hv270) == 3 ~ "Middle",
      as.numeric(hv270) == 4 ~ "Richer",
      as.numeric(hv270) == 5 ~ "Richest",
      TRUE ~ NA_character_
    ),
    residence = case_when(
      as.numeric(hv025) == 1 ~ "Urban",
      as.numeric(hv025) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    education = case_when(
      as.numeric(hv106) == 0 ~ "None",
      as.numeric(hv106) == 1 ~ "Primary",
      as.numeric(hv106) == 2 ~ "Secondary",
      as.numeric(hv106) == 3 ~ "Higher",
      TRUE ~ NA_character_
    ),
    hh_relation = case_when(
      as.numeric(hv101) == 1 ~ "Head",
      as.numeric(hv101) == 2 ~ "Wife/Partner",
      as.numeric(hv101) == 3 ~ "Son/Daughter",
      as.numeric(hv101) == 4 ~ "Son/Dau-in-law",
      as.numeric(hv101) == 5 ~ "Grandchild",
      as.numeric(hv101) == 6 ~ "Parent",
      as.numeric(hv101) == 7 ~ "Parent-in-law",
      as.numeric(hv101) == 8 ~ "Sibling",
      as.numeric(hv101) == 11 ~ "Not related",
      TRUE ~ paste0("Code_", as.numeric(hv101))
    )
  ) %>%
  filter(!is.na(insured))

cat("All-population analytic sample (de-facto, sh27 non-missing, insured non-NA):\n")
cat("  Total n =", nrow(all_pop), "\n")
cat("  Insured =", sum(all_pop$insured), "(", round(100*mean(all_pop$insured),1), "%)\n\n")

cat("--- Sample by sex ---\n")
print(all_pop %>% group_by(sex) %>%
  summarise(n=n(), insured_pct=round(100*mean(insured),1), .groups="drop"))

cat("\n--- Sample by broad age group ---\n")
age_ord <- c("Under 5","5-14","15-17","18-24","25-34","35-44","45-59","60-69","70+")
print(all_pop %>%
  mutate(age_group = factor(age_group, levels = age_ord)) %>%
  group_by(age_group) %>%
  summarise(n=n(), insured_pct=round(100*mean(insured),1), .groups="drop") %>%
  arrange(age_group))

cat("\n--- Sample by wealth quintile ---\n")
print(all_pop %>% group_by(wealth) %>%
  summarise(n=n(), insured_pct=round(100*mean(insured),1), .groups="drop"))

cat("\n--- Sample by residence ---\n")
print(all_pop %>% group_by(residence) %>%
  summarise(n=n(), insured_pct=round(100*mean(insured),1), .groups="drop"))

cat("\n--- Sample by education (adults 15+) ---\n")
print(all_pop %>% filter(age_yrs >= 15) %>%
  group_by(education) %>%
  summarise(n=n(), insured_pct=round(100*mean(insured),1), .groups="drop"))

cat("\n--- Sample by household relationship ---\n")
print(all_pop %>% group_by(hh_relation) %>%
  summarise(n=n(), insured_pct=round(100*mean(insured),1), .groups="drop") %>%
  arrange(desc(n)))

cat("\n--- Insurance by sex x age group (key equity matrix) ---\n")
print(all_pop %>%
  filter(!is.na(sex)) %>%
  mutate(age_group = factor(age_group, levels = age_ord)) %>%
  group_by(sex, age_group) %>%
  summarise(n=n(), insured_pct=round(100*mean(insured),1), .groups="drop") %>%
  arrange(sex, age_group))

# =============================================================================
# SECTION 6: IDENTIFY UNIQUE CONTRIBUTION vs EXISTING LITERATURE
# =============================================================================
cat("\n=================================================================\n")
cat("SECTION 6: Publication gap assessment summary\n")
cat("=================================================================\n\n")

cat("Existing published studies (from literature search):\n")
cat("  1. Kimani et al 2014 (IJEOH): Women 15-49, 2008-09 KDHS IR — determinants\n")
cat("  2. Kazungu & Barasa 2017 (TMIH): Women 15-49, 2014 KDHS IR — levels, correlates\n")
cat("  3. Barasa et al 2021 (BMJ GH): 36 SSA countries, all pop, single cross-section\n")
cat("  4. Medrxiv 2025 (Aldousari): All Kenyans 2022 KDHS — media/digital access only\n")
cat("  5. FSD Kenya 2025: National household survey 2024 — not DHS-based\n")
cat("\n")
cat("GAPS not covered by any published study:\n")
cat("  - Multi-wave (2008, 2014, 2022) TREND analysis for ALL population groups\n")
cat("  - Children under 5, adolescents 5-17, older adults 60+ — NEVER analyzed\n")
cat("  - Sex-stratified trends (men vs women at all ages)\n")
cat("  - Disability x insurance interaction across time\n")
cat("  - Concentration index TREND (inequality trajectory) for all ages\n")
cat("  - Vulnerable group profiling: children, elderly, disabled, rural poor\n")
cat("  - All of this using PR files from 3 waves — population-representative\n\n")

cat("PROPOSED NEW STUDY (ST06 revised or new ST09):\n")
cat("  Title candidate:\n")
cat("  'Who is left behind? Health insurance coverage, equity trends, and\n")
cat("   vulnerable group profiles across the Kenyan population 2008-2022:\n")
cat("   A multi-wave analysis of KDHS household survey data'\n\n")
cat("  Key additions over ST06:\n")
cat("   * Full population, not just women 15-49\n")
cat("   * Sex disaggregation\n")
cat("   * Life-course age groups (children, adolescents, working-age, elderly)\n")
cat("   * Disability interaction\n")
cat("   * HH-head status (are heads better covered than other members?)\n")
cat("   * Insurance type trends across population sub-groups\n")
cat("   * Policy relevance: directly informs SHA targeting for exemptions\n\n")

# =============================================================================
# SECTION 7: CROSS-WAVE PR VARIABLE AVAILABILITY MATRIX
# =============================================================================
cat("=================================================================\n")
cat("SECTION 7: Cross-wave variable availability matrix\n")
cat("=================================================================\n\n")

avail_matrix <- tibble::tribble(
  ~Variable,             ~Concept,                          ~`2008_PR`, ~`2014_PR`, ~`2022_PR`,
  "hv103",              "De-facto resident",                "CHECK",    "CHECK",    "YES",
  "hv104",              "Sex",                              "CHECK",    "CHECK",    "YES",
  "hv105",              "Age",                              "CHECK",    "CHECK",    "YES",
  "hv106",              "Education",                        "CHECK",    "CHECK",    "YES",
  "hv270",              "Wealth quintile",                  "CHECK",    "CHECK",    "YES",
  "hv024",              "Region/county",                    "CHECK",    "CHECK",    "YES",
  "hv025",              "Residence",                        "CHECK",    "CHECK",    "YES",
  "hv101",              "HH relationship",                  "CHECK",    "CHECK",    "YES",
  "sh27",               "Any insurance",                    "CHECK",    "CHECK",    "YES(~81k)",
  "sh28a",              "NHIF",                             "CHECK",    "CHECK",    "YES(cond)",
  "sh28b",              "Private insurance",                "CHECK",    "CHECK",    "YES(cond)",
  "sh28c",              "Community-based",                  "CHECK",    "CHECK",    "YES(cond)",
  "sh28x",              "Other insurance",                  "NO",       "NO",       "YES(cond)",
  "hdis9",              "Disability summary",               "NO",       "NO",       "YES",
  "sh29",               "Hospital admission",               "CHECK",    "CHECK",    "YES(~81k)",
  "sh31",               "Outpatient visit",                 "CHECK",    "CHECK",    "YES(~81k)"
)

print(as.data.frame(avail_matrix), row.names = FALSE)

cat("\n NOTE: CHECK = need to verify in actual file; YES = confirmed present\n")
cat("       cond  = conditional on sh27==1 (asked only of insured members)\n")
cat("       ~81k  = approximately 81,355 de-facto members with data\n\n")

# =============================================================================
# SECTION 8: SAVE PROFILE TO CSV FOR REFERENCE
# =============================================================================
cat("=================================================================\n")
cat("SECTION 8: Saving 2022 all-population profile\n")
cat("=================================================================\n\n")

profile_summary <- all_pop %>%
  mutate(age_group = factor(age_group, levels = age_ord)) %>%
  group_by(sex, age_group, wealth, residence) %>%
  summarise(
    n           = n(),
    insured_n   = sum(insured, na.rm = TRUE),
    insured_pct = round(100 * mean(insured, na.rm = TRUE), 2),
    nhif_n      = sum(as.numeric(sh28a) == 1, na.rm = TRUE),
    private_n   = sum(as.numeric(sh28b) == 1, na.rm = TRUE),
    community_n = sum(as.numeric(sh28c) == 1, na.rm = TRUE),
    .groups = "drop"
  )

out_csv <- file.path(out_dir, "2022_allpop_insurance_profile.csv")
readr::write_csv(profile_summary, out_csv)
cat("Profile saved to:", out_csv, "\n")
cat("Profile rows:", nrow(profile_summary), "\n")

cat("\n=================================================================\n")
cat("Exploration complete.\n")
cat("=================================================================\n")

sink()
cat("\nLog written to:", log_file, "\n")
