library(haven)
library(dplyr)
library(readr)
library(stringr)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

root <- "C:/Users/HFD 2/Research/01_DHS_Data"
st06_dir <- "C:/Users/HFD 2/Research/02_Studies/ST06_Insurance_Trends_2008_2022/02_data_notes"
admin_log <- "C:/Users/HFD 2/Research/02_Studies/ST03_NCD_Insurance_Service_Use/00_Admin/errors_log.txt"

dir.create(st06_dir, recursive = TRUE, showWarnings = FALSE)

log_error <- function(msg) {
  cat(
    sprintf("[%s] TASK 8 CROSSWALK: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg),
    file = admin_log,
    append = TRUE
  )
}

find_first_dta <- function(path) {
  files <- list.files(path, pattern = "\\.DTA$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) NA_character_ else files[[1]]
}

read_vars <- function(path, vars) {
  if (is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    read_dta(path, col_select = any_of(vars)),
    error = function(e) {
      log_error(paste("Failed reading", path, "-", e$message))
      NULL
    }
  )
}

ir2008_path <- find_first_dta(file.path(root, "KDHS_2008", "IR_Individual_Recode"))
hr2008_path <- find_first_dta(file.path(root, "KDHS_2008", "HR_Household_Recode"))
ir2014_path <- find_first_dta(file.path(root, "KDHS_2014", "IR_Individual_Recode"))
hr2014_path <- find_first_dta(file.path(root, "KDHS_2014", "HR_Household_Recode"))
pr2022_path <- file.path(root, "KDHS_2022", "PR_Person_Recode", "KEPR8CFL.DTA")

vars_2008_2014 <- c("v481", "v482a", "v482b", "v190", "v025", "v106", "v024", "hv219")
vars_2022 <- c("sh27", "sh28a", "hv270", "hv025", "hv024", "hv106", "hv109", "hv104", "hv105")

ir2008 <- read_vars(ir2008_path, vars_2008_2014)
hr2008 <- read_vars(hr2008_path, c("hv219"))
ir2014 <- read_vars(ir2014_path, vars_2008_2014)
hr2014 <- read_vars(hr2014_path, c("hv219"))
pr2022 <- read_vars(pr2022_path, vars_2022)

if (is.null(ir2008) || is.null(ir2014) || is.null(pr2022)) {
  log_error("Required older-wave files were not available for harmonisation crosswalk.")
}

pick_var <- function(df, candidates, fallback = NA_character_) {
  if (is.null(df)) return(fallback)
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0) fallback else hits[[1]]
}

notes <- c(
  "2008 and 2014 insurance variables are searched in IR because respondent-level insurance was stored in women's interview files.",
  "2022 insurance is stored in PR, not IR/MR.",
  "2022 person-level education could not be validated from the briefing and is searched among hv106/hv109 candidates."
)

crosswalk <- tibble(
  Concept = c(
    "Any insurance",
    "NHIF specifically",
    "Wealth quintile",
    "Urban/rural",
    "Education level",
    "Region"
  ),
  `2008-09 var` = c(
    pick_var(ir2008, c("v481"), fallback = "v481"),
    pick_var(ir2008, c("v481c"), fallback = "v481c"),
    pick_var(ir2008, c("v190"), fallback = "v190"),
    pick_var(ir2008, c("v025"), fallback = "v025"),
    pick_var(ir2008, c("v106"), fallback = "v106"),
    pick_var(ir2008, c("v024"), fallback = "v024")
  ),
  `2014 var` = c(
    pick_var(ir2014, c("v481"), fallback = "v481"),
    pick_var(ir2014, c("v481c"), fallback = "v481c"),
    pick_var(ir2014, c("v190"), fallback = "v190"),
    pick_var(ir2014, c("v025"), fallback = "v025"),
    pick_var(ir2014, c("v106"), fallback = "v106"),
    pick_var(ir2014, c("v024"), fallback = "v024")
  ),
  `2022 var (PR)` = c(
    pick_var(pr2022, c("sh27"), fallback = "sh27"),
    pick_var(pr2022, c("sh28a"), fallback = "sh28a"),
    pick_var(pr2022, c("hv270"), fallback = "hv270"),
    pick_var(pr2022, c("hv025"), fallback = "hv025"),
    pick_var(pr2022, c("hv106", "hv109"), fallback = "hv106"),
    pick_var(pr2022, c("hv024"), fallback = "hv024")
  ),
  Notes = c(
    "2008/2014 mapping to v481 is standard DHS respondent-level insurance. 2022 v481 is empty in IR/MR and insurance moved to PR sh27.",
    "2014 NHIF maps to v481c (National) per DHS User Forum message #28060. 2008 is provisionally mapped the same way pending authenticated file validation.",
    "Older waves use respondent wealth quintile in IR; 2022 uses household member wealth in PR.",
    "Label consistency usually stable across waves.",
    "2022 education is locally validated in PR as hv106/hv109, with hv106 preferred for highest educational level attained.",
    "2022 region is county-level in PR. 2008 and 2014 use v024 in IR; coding should be rechecked against authenticated files when available."
  )
)

write_csv(crosswalk, file.path(st06_dir, "harmonisation_crosswalk.csv"))
