# Study: ST03_NCD_Insurance_Service_Use
# Script: 01_data_import.R
# Author: Nichodemus Werre Amollo
# Date: 2026-03-29
# Purpose: Read KDHS 2022 source files and save import-ready objects for the ST03 workflow.

message("=== SECTION 1: Data Import ===")

source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

required_files <- c(
  file.path(paths$dhs_root, "PR_Person_Recode", "KEPR8CFL.DTA"),
  file.path(paths$dhs_root, "IR_Individual_Recode", "KEIR8CFL.DTA"),
  file.path(paths$dhs_root, "MR_Mens_Recode", "KEMR8CFL.DTA")
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing raw DHS files: ", paste(missing_files, collapse = "; "))
}

append_log("Reading PR, IR, and MR source files.", also_message = TRUE)

pr <- haven::read_dta(
  file.path(paths$dhs_root, "PR_Person_Recode", "KEPR8CFL.DTA"),
  col_select = c(
    hv001, hv002, hvidx, hv102, hv103, hv104, hv105, hv118, hv219,
    hv005, hv021, hv022, hv024, hv025, hv028, hv270,
    sh27, sh28a, sh28b, sh28c, sh31, sh32, hdis9
  )
)

ir <- haven::read_dta(
  file.path(paths$dhs_root, "IR_Individual_Recode", "KEIR8CFL.DTA"),
  col_select = c(v001, v002, v003, v005, v021, v022, v024, v012, v106, v717, sshort, v467b, v467c, v467d, chd02, chd05, chd07, chd10)
)

mr <- haven::read_dta(
  file.path(paths$dhs_root, "MR_Mens_Recode", "KEMR8CFL.DTA"),
  col_select = c(mv001, mv002, mv003, mv005, mv021, mv022, mv024, mv012, mv106, mv717, mchd02, mchd05, mchd07, mchd10)
)

import_object <- list(
  pr = pr,
  ir = ir,
  mr = mr
)

save_rds_output(import_object, "st03_import_raw.rds")

import_diagnostics <- tibble::tibble(
  source_file = c("PR", "IR", "MR"),
  rows = c(nrow(pr), nrow(ir), nrow(mr)),
  columns = c(ncol(pr), ncol(ir), ncol(mr))
)

readr::write_csv(import_diagnostics, file.path(paths$logs_dir, "st03_import_diagnostics.csv"))
append_log("Import-ready raw objects saved to 07_derived_data.")

message("=== SECTION 1 COMPLETE ===")
