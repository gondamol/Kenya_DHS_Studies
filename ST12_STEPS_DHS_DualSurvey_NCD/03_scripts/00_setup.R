# ST12 — R session setup
# Uses user library installed for this workspace

options(stringsAsFactors = FALSE, scipen = 999)

user_lib <- Sys.getenv("R_LIBS_USER", unset = path.expand("~/R/library"))
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

required <- c("survey", "haven", "dplyr", "tidyr", "readr", "stringr", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Missing R packages: ", paste(missing, collapse = ", "),
    "\nInstall into ", user_lib, " then re-run.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(survey)
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

# Survey options: avoid lonely PSU issues where possible
options(survey.lonely.psu = "adjust")

find_study_root <- function() {
  # 03_scripts -> study root
  normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."), mustWork = FALSE)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a))) b else a

study_root <- function() {
  # Prefer explicit env; else locate from this file when sourced via source()
  env <- Sys.getenv("ST12_ROOT", unset = "")
  if (nzchar(env) && dir.exists(env)) return(normalizePath(env))
  # Walk up from cwd
  cwd <- normalizePath(getwd())
  candidates <- c(
    cwd,
    file.path(cwd, ".."),
    file.path(cwd, "../.."),
    "/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD"
  )
  for (p in candidates) {
    p <- normalizePath(p, mustWork = FALSE)
    if (dir.exists(file.path(p, "03_scripts")) && dir.exists(file.path(p, "04_tables"))) {
      return(p)
    }
  }
  stop("Cannot locate ST12 study root. Set ST12_ROOT.", call. = FALSE)
}

paths_st12 <- function(root = study_root()) {
  list(
    root = root,
    scripts = file.path(root, "03_scripts"),
    tables = file.path(root, "04_tables"),
    figures = file.path(root, "05_figures"),
    derived = file.path(root, "07_derived_data"),
    logs = file.path(root, "08_logs"),
    steps_csv = "/mnt/c/Users/HFD 2/Research/Kenya - STEPS 2015/ken2015.csv",
    steps_dta = "/mnt/c/Users/HFD 2/Research/Kenya - STEPS 2015/ken2015.dta",
    dhs = "/mnt/c/Users/HFD 2/Research/01_DHS_Data/KDHS_2022",
    ir = "/mnt/c/Users/HFD 2/Research/01_DHS_Data/KDHS_2022/IR_Individual_Recode/KEIR8CFL.DTA",
    mr = "/mnt/c/Users/HFD 2/Research/01_DHS_Data/KDHS_2022/MR_Mens_Recode/KEMR8CFL.DTA",
    pr = "/mnt/c/Users/HFD 2/Research/01_DHS_Data/KDHS_2022/PR_Person_Recode/KEPR8CFL.DTA"
  )
}

ensure_dirs <- function(p) {
  for (d in c(p$tables, p$figures, p$derived, p$logs)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible()
}

yes1 <- function(x) {
  # STEPS: 1=Yes, 2=No (haven may label)
  x <- as.numeric(as.character(x))
  out <- rep(NA_real_, length(x))
  out[x == 1] <- 1
  out[x == 2] <- 0
  out
}

yn_dhs <- function(x) {
  x <- as.numeric(as.character(x))
  out <- rep(NA_real_, length(x))
  out[x == 1] <- 1
  out[x %in% c(0, 2)] <- 0
  out
}

num_clean <- function(x) {
  x <- as.numeric(as.character(x))
  x[x %in% c(77, 88, 99, 777, 888, 999, 7777, 8888, 9999)] <- NA_real_
  x
}

fmt_pct_ci <- function(est, lo, hi, digits = 1) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
          100 * est, 100 * lo, 100 * hi)
}

# Design-based mean of a binary/continuous variable
svy_prop_row <- function(design, formula, label = NULL) {
  fml <- as.formula(formula)
  est <- tryCatch(
    svymean(fml, design, na.rm = TRUE),
    error = function(e) NULL
  )
  if (is.null(est)) {
    return(tibble(
      label = label %||% as.character(formula),
      estimate = NA_real_, se = NA_real_,
      ci_low = NA_real_, ci_high = NA_real_,
      pct = NA_real_, pct_low = NA_real_, pct_high = NA_real_,
      n = NA_integer_
    ))
  }
  mu <- as.numeric(est[1])
  se <- as.numeric(SE(est)[1])
  # For proportions, confint may use logit method depending on survey version
  ci <- tryCatch(
    confint(est, df = degf(design)),
    error = function(e) cbind(mu - 1.96 * se, mu + 1.96 * se)
  )
  lo <- max(0, as.numeric(ci[1, 1]))
  hi <- min(1, as.numeric(ci[1, 2]))
  # unweighted n for complete cases of the variable
  vn <- all.vars(fml)[1]
  n <- sum(!is.na(design$variables[[vn]]))
  tibble(
    label = label %||% vn,
    estimate = mu,
    se = se,
    ci_low = lo,
    ci_high = hi,
    pct = 100 * mu,
    pct_low = 100 * lo,
    pct_high = 100 * hi,
    n = as.integer(n)
  )
}

message("ST12 R setup OK | R ", getRversion(), " | lib: ", .libPaths()[[1]])
