# =============================================================================
# ST06 - Full Diagnostic Script for 2022 KDHS PR File
# Purpose: Diagnose all variable encoding issues before running main analysis
# =============================================================================

library(haven)
library(dplyr)
library(labelled)

cat("=================================================================\n")
cat("ST06 Full 2022 PR Diagnostic\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================================\n\n")

pr_path <- "c:/Users/HFD 2/Research/01_DHS_Data/KDHS_2022/PR_Person_Recode/KEPR8CFL.DTA"

cat("--- STEP 1: Load minimal columns ---\n")
pr_min <- read_dta(pr_path, col_select = c(hv001, hv002, hvidx, hv104, hv105))
cat("Total rows loaded:", nrow(pr_min), "\n")
cat("Columns:", paste(names(pr_min), collapse = ", "), "\n\n")

cat("--- STEP 2: hv104 (sex) encoding ---\n")
cat("Class:", paste(class(pr_min$hv104), collapse = ", "), "\n")
cat("Storage type:", typeof(pr_min$hv104), "\n")
cat("Is labelled:", inherits(pr_min$hv104, "haven_labelled"), "\n")
cat("Attributes:\n")
print(attributes(pr_min$hv104))
cat("\nRaw numeric unique values:", paste(sort(unique(as.numeric(pr_min$hv104))), collapse = ", "), "\n")
cat("as_factor unique labels:   ", paste(sort(unique(as.character(as_factor(pr_min$hv104)))), collapse = " | "), "\n")
cat("NA count:", sum(is.na(pr_min$hv104)), "\n\n")

cat("--- STEP 3: Filter method comparison ---\n")
n_numeric2   <- sum(as.numeric(pr_min$hv104) == 2, na.rm = TRUE)
n_label_fem  <- sum(as.character(as_factor(pr_min$hv104)) == "female", na.rm = TRUE)
n_raw_eq2    <- sum(pr_min$hv104 == 2, na.rm = TRUE)
cat("as.numeric(hv104) == 2        :", n_numeric2, "\n")
cat("as_factor label == 'female'   :", n_label_fem, "\n")
cat("hv104 == 2 (raw haven compare) :", n_raw_eq2, "\n\n")

cat("--- STEP 4: Age filter (15-49) ---\n")
cat("Class of hv105:", paste(class(pr_min$hv105), collapse = ", "), "\n")
cat("Range of hv105:", range(as.numeric(pr_min$hv105), na.rm = TRUE), "\n")
n_age_only      <- sum(as.numeric(pr_min$hv105) >= 15 & as.numeric(pr_min$hv105) <= 49, na.rm = TRUE)
n_fem_age_num   <- sum(as.numeric(pr_min$hv104) == 2 & as.numeric(pr_min$hv105) >= 15 & as.numeric(pr_min$hv105) <= 49, na.rm = TRUE)
n_fem_age_lbl   <- sum(as.character(as_factor(pr_min$hv104)) == "female" & as.numeric(pr_min$hv105) >= 15 & as.numeric(pr_min$hv105) <= 49, na.rm = TRUE)
cat("All aged 15-49:                    ", n_age_only, "\n")
cat("Females 15-49 (numeric method):    ", n_fem_age_num, "\n")
cat("Females 15-49 (label method):      ", n_fem_age_lbl, "\n\n")

cat("--- STEP 5: Load insurance columns ---\n")
pr_ins <- tryCatch(
  read_dta(pr_path, col_select = c(hv001, hv002, hvidx, hv104, hv105, sh27, sh28a, sh28b, sh28c, sh28d, sh28e)),
  error = function(e) { cat("ERROR loading sh27/sh28 cols:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(pr_ins)) {
  cat("Insurance columns loaded. nrow:", nrow(pr_ins), "\n")
  cat("Columns:", paste(names(pr_ins), collapse = ", "), "\n\n")

  cat("--- STEP 5a: sh27 (any insurance) ---\n")
  cat("Class:", paste(class(pr_ins$sh27), collapse = ", "), "\n")
  cat("Is labelled:", inherits(pr_ins$sh27, "haven_labelled"), "\n")
  cat("Attributes:\n")
  print(attributes(pr_ins$sh27))
  cat("Raw numeric unique values:", paste(sort(unique(as.numeric(pr_ins$sh27))), collapse = ", "), "\n")
  if (inherits(pr_ins$sh27, "haven_labelled")) {
    cat("as_factor unique labels:  ", paste(sort(unique(as.character(as_factor(pr_ins$sh27)))), collapse = " | "), "\n")
  }
  cat("NA count:", sum(is.na(pr_ins$sh27)), "/", nrow(pr_ins), "\n")
  cat("Value counts:\n")
  print(table(as.numeric(pr_ins$sh27), useNA = "always"))

  cat("\n--- STEP 5b: sh28a-sh28e (insurance type) ---\n")
  for (v in c("sh28a", "sh28b", "sh28c", "sh28d", "sh28e")) {
    if (v %in% names(pr_ins)) {
      col <- pr_ins[[v]]
      cat(v, "-> class:", paste(class(col), collapse=","),
          "| NA:", sum(is.na(col)),
          "| unique numeric:", paste(sort(unique(as.numeric(col))), collapse=","), "\n")
    } else {
      cat(v, "-> NOT FOUND in dataframe\n")
    }
  }

  cat("\n--- STEP 5c: Insurance among females 15-49 ---\n")
  fem_15_49 <- pr_ins %>%
    filter(as.numeric(hv104) == 2, as.numeric(hv105) >= 15, as.numeric(hv105) <= 49)
  cat("Females 15-49 rows:", nrow(fem_15_49), "\n")

  if (nrow(fem_15_49) > 0) {
    cat("sh27 NA count among females 15-49:", sum(is.na(fem_15_49$sh27)), "\n")
    cat("sh27 value distribution among females 15-49:\n")
    print(table(as.numeric(fem_15_49$sh27), useNA = "always"))
    cat("sh27 == 1 count (insured):", sum(as.numeric(fem_15_49$sh27) == 1, na.rm = TRUE), "\n")
    cat("sh27 == 0 count (not insured):", sum(as.numeric(fem_15_49$sh27) == 0, na.rm = TRUE), "\n")
  }
} else {
  cat("Trying to find insurance columns with alternate names...\n")
  pr_all_cols <- read_dta(pr_path, n_max = 1)
  ins_cols <- grep("^sh2", names(pr_all_cols), value = TRUE, ignore.case = TRUE)
  cat("Columns starting with sh2:", paste(ins_cols, collapse = ", "), "\n")
  s27_cols <- grep("sh27|s127|s_27", names(pr_all_cols), value = TRUE, ignore.case = TRUE)
  cat("Columns matching sh27 variants:", paste(s27_cols, collapse = ", "), "\n")
}

cat("\n--- STEP 6: Load all needed columns ---\n")
pr_full <- tryCatch(
  read_dta(pr_path,
    col_select = c(hv001, hv002, hvidx, hv005, hv021, hv022,
                   hv104, hv105, hv106, hv270, hv024, hv025,
                   sh27, sh28a, sh28b, sh28c, sh28d, sh28e)),
  error = function(e) { cat("ERROR loading full col set:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(pr_full)) {
  cat("Full column set loaded. nrow:", nrow(pr_full), "\n")

  cat("\n--- STEP 6a: hv106 (education) ---\n")
  cat("Unique numeric:", paste(sort(unique(as.numeric(pr_full$hv106))), collapse=", "), "\n")
  if (inherits(pr_full$hv106, "haven_labelled")) {
    cat("Labels:", paste(sort(unique(as.character(as_factor(pr_full$hv106)))), collapse=" | "), "\n")
  }

  cat("\n--- STEP 6b: hv270 (wealth) ---\n")
  cat("Unique numeric:", paste(sort(unique(as.numeric(pr_full$hv270))), collapse=", "), "\n")
  if (inherits(pr_full$hv270, "haven_labelled")) {
    cat("Labels:", paste(sort(unique(as.character(as_factor(pr_full$hv270)))), collapse=" | "), "\n")
  }

  cat("\n--- STEP 6c: hv025 (residence) ---\n")
  cat("Unique numeric:", paste(sort(unique(as.numeric(pr_full$hv025))), collapse=", "), "\n")
  if (inherits(pr_full$hv025, "haven_labelled")) {
    cat("Labels:", paste(sort(unique(as.character(as_factor(pr_full$hv025)))), collapse=" | "), "\n")
  }

  cat("\n--- STEP 6d: hv024 (region/county) range ---\n")
  cat("Unique numeric:", paste(sort(unique(as.numeric(pr_full$hv024))), collapse=", "), "\n")

  cat("\n--- STEP 7: Simulate the full transmute pipeline ---\n")
  ir22_test <- pr_full %>%
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
      education = case_when(
        as.numeric(hv106) == 0 ~ "No Education",
        as.numeric(hv106) == 1 ~ "Primary",
        as.numeric(hv106) == 2 ~ "Secondary",
        as.numeric(hv106) == 3 ~ "Higher",
        TRUE ~ NA_character_
      ),
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
      region = as.numeric(hv024),
      marital = NA_character_,
      insured_any = as.integer(as.numeric(sh27) == 1),
      insured_nhif = as.integer(as.numeric(sh28a) == 1),
      insured_other_gov = as.integer(as.numeric(sh28b) == 1),
      insured_community = as.integer(as.numeric(sh28c) == 1),
      insured_private = as.integer(as.numeric(sh28d) == 1),
      insured_other = as.integer(as.numeric(sh28e) == 1)
    )

  cat("Rows after filter + transmute:", nrow(ir22_test), "\n")
  cat("insured_any distribution:\n")
  print(table(ir22_test$insured_any, useNA = "always"))
  cat("Rows with non-NA insured_any:", sum(!is.na(ir22_test$insured_any)), "\n")
  cat("Rows with insured_any == 1:", sum(ir22_test$insured_any == 1, na.rm = TRUE), "\n")
  cat("Education distribution:\n")
  print(table(ir22_test$education, useNA = "always"))
  cat("Wealth distribution:\n")
  print(table(ir22_test$wealth, useNA = "always"))
  cat("Residence distribution:\n")
  print(table(ir22_test$residence, useNA = "always"))
  cat("\nSample rows:\n")
  print(head(ir22_test[, c("wave","weight","education","wealth","residence","insured_any","insured_nhif")], 10))
}

cat("\n--- STEP 8: Check all column names in DTA file ---\n")
pr_header <- read_dta(pr_path, n_max = 0)
cat("Total columns in PR DTA:", ncol(pr_header), "\n")
sh_cols  <- grep("^sh", names(pr_header), value = TRUE, ignore.case = TRUE)
hv_cols  <- grep("^hv", names(pr_header), value = TRUE, ignore.case = TRUE)
cat("All sh* columns:", paste(sh_cols, collapse = ", "), "\n")
cat("All hv* columns (first 40):", paste(head(hv_cols, 40), collapse = ", "), "\n")

cat("\n=================================================================\n")
cat("Diagnostic complete.\n")
cat("=================================================================\n")
