# Study: ST11_Mental_Health_Treatment_Gap
# Script: 01_data_import.R
# Purpose: Read KDHS 2022 women's (IR), men's (MR), and household-member (PR) files.
#          PR is used only to attach household health-insurance status (sh27),
#          which is not carried in the IR/MR recodes.

message("=== SECTION 1: Data Import ===")
source(file.path(find_study_root(), "03_scripts", "00_setup.R"), local = TRUE)

ir_file <- file.path(paths$dhs_root, "IR_Individual_Recode", "KEIR8CFL.DTA")
mr_file <- file.path(paths$dhs_root, "MR_Mens_Recode", "KEMR8CFL.DTA")
pr_file <- file.path(paths$dhs_root, "PR_Person_Recode", "KEPR8CFL.DTA")

for (f in c(ir_file, mr_file)) if (!file.exists(f)) stop("Missing required DHS file: ", f)

append_log("Reading KDHS 2022 IR and MR source files for ST11.", also_message = TRUE)

chd_items <- c("chd02", "chd05", "chd07", "chd10", "chd11", "chd12",
               "chd13", "chd14", "chd17", "chd18", "chd19", "chd20", "chd21")
ir <- haven::read_dta(
  ir_file,
  col_select = dplyr::all_of(c(
    "v001", "v002", "v003", "v005", "v012", "v021", "v022", "v024", "v025",
    "v106", "v149", "v190", chd_items
  ))
)

mchd_items <- paste0("m", chd_items)
mr <- haven::read_dta(
  mr_file,
  col_select = dplyr::all_of(c(
    "mv001", "mv002", "mv003", "mv005", "mv012", "mv021", "mv022", "mv024", "mv025",
    "mv106", "mv149", "mv190", mchd_items
  ))
)

# Household insurance (sh27) from PR, keyed on cluster/household/line.
pr_insurance <- NULL
if (file.exists(pr_file)) {
  pr <- haven::read_dta(pr_file, col_select = dplyr::all_of(c("hv001", "hv002", "hvidx", "sh27")))
  pr_insurance <- pr %>%
    dplyr::transmute(
      cluster = as.numeric(hv001),
      household = as.numeric(hv002),
      line = as.numeric(hvidx),
      insured_any = yn_flag(sh27)
    )
  append_log(sprintf("Attached PR insurance lookup (%s rows).", format(nrow(pr_insurance), big.mark = ",")))
} else {
  append_log("PR file not found; insurance covariate will be unavailable.", also_message = TRUE)
}

save_rds_output(list(ir = ir, mr = mr, pr_insurance = pr_insurance), "st11_import_raw.rds")

readr::write_csv(
  tibble::tibble(source_file = c("IR", "MR"), rows = c(nrow(ir), nrow(mr)), columns = c(ncol(ir), ncol(mr))),
  file.path(paths$logs_dir, "st11_import_diagnostics.csv")
)
append_log("ST11 import-ready objects saved to 07_derived_data.")
message("=== SECTION 1 COMPLETE ===")
