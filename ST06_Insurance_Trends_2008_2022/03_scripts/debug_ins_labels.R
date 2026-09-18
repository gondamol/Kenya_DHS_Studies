# =============================================================================
# ST06 - Insurance Column Label Diagnostics for 2022 KDHS PR File
# Purpose: Identify correct variable names and encodings for all insurance
#          type variables to replace the broken sh28d / sh28e references.
# =============================================================================

library(haven)
library(dplyr)
library(labelled)

pr_path <- "c:/Users/HFD 2/Research/01_DHS_Data/KDHS_2022/PR_Person_Recode/KEPR8CFL.DTA"

cat("=================================================================\n")
cat("Insurance Column Diagnostics - 2022 KDHS PR\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================================\n\n")

# ── Load only the insurance-related sh* columns that actually exist ──────────
ins_cols <- c("hv001", "hv002", "hvidx", "hv104", "hv105",
              "sh27",
              "sh28a", "sh28b", "sh28c", "sh28x",
              "sh29",
              "sh205a", "sh205b", "sh205c", "sh205d", "sh205e")

cat("Loading columns:", paste(ins_cols, collapse = ", "), "\n\n")
pr <- read_dta(pr_path, col_select = all_of(ins_cols))
cat("Rows loaded:", nrow(pr), "\n\n")

# ── Helper: summarise one column ─────────────────────────────────────────────
describe_col <- function(df, col_name) {
  if (!col_name %in% names(df)) {
    cat(sprintf("  %-12s : NOT FOUND\n", col_name))
    return(invisible(NULL))
  }
  x   <- df[[col_name]]
  lbl <- attr(x, "label")   # Stata variable label
  val_labs <- attr(x, "labels") # Stata value labels

  cat(sprintf("  Variable  : %s\n", col_name))
  cat(sprintf("  Label     : %s\n", if (is.null(lbl)) "(none)" else lbl))
  cat(sprintf("  Class     : %s\n", paste(class(x), collapse = ", ")))
  cat(sprintf("  NA count  : %d / %d\n", sum(is.na(x)), length(x)))

  if (!is.null(val_labs) && length(val_labs) > 0) {
    cat("  Value labels:\n")
    for (i in seq_along(val_labs)) {
      cat(sprintf("    %g = %s\n", val_labs[i], names(val_labs)[i]))
    }
  } else {
    cat("  Value labels: (none — raw numeric)\n")
  }

  raw_vals <- sort(unique(as.numeric(x)))
  cat(sprintf("  Raw unique values : %s\n", paste(raw_vals, collapse = ", ")))
  cat("  Frequency table (raw numeric):\n")
  tbl <- table(as.numeric(x), useNA = "always")
  for (i in seq_along(tbl)) {
    cat(sprintf("    %s : %d\n", names(tbl)[i], tbl[[i]]))
  }
  cat("\n")
}

# ── Section A: Primary insurance block (sh27 / sh28*) ───────────────────────
cat("================================================================\n")
cat("SECTION A: Primary insurance block (sh27, sh28a-c, sh28x, sh29)\n")
cat("================================================================\n\n")

for (v in c("sh27", "sh28a", "sh28b", "sh28c", "sh28x", "sh29")) {
  describe_col(pr, v)
}

# ── Section B: Possible secondary insurance block (sh205*) ──────────────────
cat("================================================================\n")
cat("SECTION B: Secondary/supplemental block (sh205a-sh205e)\n")
cat("================================================================\n\n")

for (v in c("sh205a", "sh205b", "sh205c", "sh205d", "sh205e")) {
  describe_col(pr, v)
}

# ── Section C: Cross-tabulate sh27 vs sh28a-c + sh28x among females 15-49 ───
cat("================================================================\n")
cat("SECTION C: Insurance type breakdown — females aged 15-49\n")
cat("================================================================\n\n")

fem <- pr %>%
  filter(as.numeric(hv104) == 2,
         as.numeric(hv105) >= 15,
         as.numeric(hv105) <= 49)

cat("Females 15-49 in dataset:", nrow(fem), "\n\n")

insured <- fem %>% filter(as.numeric(sh27) == 1)
cat("Of those, sh27 == 1 (insured):", nrow(insured), "\n\n")

cat("Among ALL females 15-49:\n")
for (v in c("sh27", "sh28a", "sh28b", "sh28c", "sh28x", "sh29",
            "sh205a", "sh205b", "sh205c", "sh205d", "sh205e")) {
  if (v %in% names(fem)) {
    n1  <- sum(as.numeric(fem[[v]]) == 1, na.rm = TRUE)
    n0  <- sum(as.numeric(fem[[v]]) == 0, na.rm = TRUE)
    nna <- sum(is.na(fem[[v]]))
    cat(sprintf("  %-10s : 1=%d  0=%d  NA=%d\n", v, n1, n0, nna))
  }
}

cat("\nAmong INSURED females 15-49 (sh27 == 1):\n")
if (nrow(insured) > 0) {
  for (v in c("sh28a", "sh28b", "sh28c", "sh28x", "sh29",
              "sh205a", "sh205b", "sh205c", "sh205d", "sh205e")) {
    if (v %in% names(insured)) {
      n1  <- sum(as.numeric(insured[[v]]) == 1, na.rm = TRUE)
      n0  <- sum(as.numeric(insured[[v]]) == 0, na.rm = TRUE)
      nna <- sum(is.na(insured[[v]]))
      cat(sprintf("  %-10s : 1=%d  0=%d  NA=%d\n", v, n1, n0, nna))
    }
  }
} else {
  cat("  (No insured females found with current filter)\n")
}

# ── Section D: Check if any sh28* columns are co-indicators (1-of-many) ──────
cat("\n================================================================\n")
cat("SECTION D: Mutual-exclusivity check for sh28a/b/c/x among insured\n")
cat("================================================================\n\n")

type_cols <- intersect(c("sh28a", "sh28b", "sh28c", "sh28x", "sh29"), names(fem))
if (length(type_cols) > 0 && nrow(insured) > 0) {
  mat <- sapply(type_cols, function(v) as.integer(as.numeric(insured[[v]]) == 1))
  row_sums <- rowSums(mat, na.rm = TRUE)
  cat("Among insured females 15-49, number of type flags set per person:\n")
  print(table(row_sums, useNA = "always"))
  cat("\nCorrelation matrix of type flags:\n")
  print(round(cor(mat, use = "complete.obs"), 3))
}

cat("\n=================================================================\n")
cat("Diagnostic complete.\n")
cat("=================================================================\n")
