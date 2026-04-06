# Study: ST02_Disability_Insurance_Equity
# Script: 01_data_import.R
# Author: Nichodemus Werre Amollo
# Date: 2026-04-05
# Purpose: Read KDHS 2022 PR source data and save an import-ready object for ST02.

message("=== SECTION 1: Data Import ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

required_file <- file.path(paths$dhs_root, "PR_Person_Recode", "KEPR8CFL.DTA")

if (!file.exists(required_file)) {
  stop("Missing raw DHS file: ", required_file)
}

append_log("Reading KDHS 2022 PR source file for ST02.", also_message = TRUE)

pr <- haven::read_dta(
  required_file,
  col_select = c(
    hv001, hv002, hvidx, hv102, hv103, hv104, hv105, hv005, hv021, hv022,
    hv024, hv025, hv106, hv270, shshort,
    sh27, sh28a, sh28b, sh28c, sh29, sh31, sh32,
    hdis1, hdis2, hdis3, hdis4, hdis5, hdis6, hdis7, hdis8, hdis9
  )
)

save_rds_output(list(pr = pr), "st02_import_raw.rds")

import_diagnostics <- tibble::tibble(
  source_file = "PR",
  rows = nrow(pr),
  columns = ncol(pr)
)

readr::write_csv(import_diagnostics, file.path(paths$logs_dir, "st02_import_diagnostics.csv"))
append_log("ST02 import-ready raw PR object saved to 07_derived_data.")

message("=== SECTION 1 COMPLETE ===")
