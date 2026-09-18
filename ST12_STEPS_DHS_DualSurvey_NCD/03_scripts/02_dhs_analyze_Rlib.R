#!/usr/bin/env Rscript
# ST12 DHS analysis using data.table + design-based SEs from analytic slim CSV

user_lib <- Sys.getenv("R_LIBS_USER", unset = path.expand("~/R/library"))
.libPaths(c(user_lib, .libPaths()))
suppressPackageStartupMessages(library(data.table))

root <- "/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD"
tab_dir <- file.path(root, "04_tables")
der_dir <- file.path(root, "07_derived_data")
slim <- file.path(der_dir, "st12_dhs_analytic_slim.csv")
if (!file.exists(slim)) stop("Missing ", slim, " — run Python 02_dhs first or R haven pipeline.")

cat("R", as.character(getRversion()), "| loading", slim, "\n")
d <- fread(slim)
cat("  n=", nrow(d), "\n")

cluster_mean <- function(dt, y, w = "weight", psu_col = "psu", strata_col = "stratum") {
  ok <- !is.na(dt[[y]]) & !is.na(dt[[w]]) & dt[[w]] > 0 & !is.na(dt[[psu_col]])
  dd <- copy(dt[ok])
  if (!nrow(dd)) return(list(estimate = NA_real_, se = NA_real_, n = 0L))
  yy <- dd[[y]]; ww <- dd[[w]]
  mu <- sum(ww * yy) / sum(ww)
  dd[, u_w := ww * (yy - mu)]
  dd[, psu_id := dd[[psu_col]]]
  dd[, st_id := dd[[strata_col]]]
  g <- dd[, .(u = sum(u_w), st = st_id[1]), by = psu_id]
  var_num <- 0
  for (stv in unique(g$st)) {
    sub <- g[st == stv]
    nh <- nrow(sub)
    if (nh < 2) next
    var_num <- var_num + (nh / (nh - 1)) * sum((sub$u - mean(sub$u))^2)
  }
  se <- if (var_num > 0) sqrt(var_num) / sum(ww) else NA_real_
  list(estimate = mu, se = se, n = nrow(dd))
}

prop_row <- function(dt, y, label) {
  r <- cluster_mean(dt, y)
  z <- qnorm(0.975)
  lo <- max(0, r$estimate - z * r$se)
  hi <- min(1, r$estimate + z * r$se)
  data.table(
    label = label, estimate = r$estimate, se = r$se,
    ci_low = lo, ci_high = hi,
    pct = 100 * r$estimate, pct_low = 100 * lo, pct_high = 100 * hi,
    n = r$n
  )
}

fmt <- function(e, lo, hi) sprintf("%.1f (%.1f–%.1f)", 100*e, 100*lo, 100*hi)

htn <- d[htn_dx == 1]
dm <- d[dm_dx == 1]
anyd <- d[any_dx == 1]

rows <- rbindlist(list(
  prop_row(d, "htn_dx", "Self-reported HTN diagnosis prevalence")[, domain := "dhs_all"],
  prop_row(d, "dm_dx", "Self-reported DM diagnosis prevalence")[, domain := "dhs_all"],
  prop_row(d, "any_dx", "Self-reported HTN or DM diagnosis")[, domain := "dhs_all"],
  prop_row(htn, "htn_treated_if_dx", "Treated among diagnosed HTN")[, domain := "dhs_htn_dx"],
  prop_row(dm, "dm_treated_if_dx", "Treated among diagnosed DM")[, domain := "dhs_dm_dx"],
  prop_row(anyd, "any_treated_if_dx", "Treated among any diagnosed")[, domain := "dhs_any_dx"],
  prop_row(htn, "insured_any", "Insured among diagnosed HTN")[, domain := "dhs_htn_dx"],
  prop_row(dm, "insured_any", "Insured among diagnosed DM")[, domain := "dhs_dm_dx"],
  prop_row(anyd, "insured_any", "Insured among any diagnosed")[, domain := "dhs_any_dx"],
  prop_row(htn[insured_any == 1], "htn_treated_if_dx", "HTN tx among insured diagnosed")[, domain := "dhs_htn_ins"],
  prop_row(htn[insured_any == 0], "htn_treated_if_dx", "HTN tx among uninsured diagnosed")[, domain := "dhs_htn_ins"]
), fill = TRUE)

for (i in seq_len(nrow(rows))) {
  cat(sprintf("  %s: %s n=%s\n", rows$label[i],
              fmt(rows$estimate[i], rows$ci_low[i], rows$ci_high[i]), rows$n[i]))
}
fwrite(rows, file.path(tab_dir, "Table5_DHS_KeyEstimates_R.csv"))

pyf <- file.path(tab_dir, "Table5_DHS_KeyEstimates.csv")
if (file.exists(pyf)) {
  py <- fread(pyf)
  cmp <- merge(
    rows[, .(label, pct_R = pct, n_R = n)],
    py[, .(label, pct_Py = pct, n_Py = n)],
    by = "label", all = TRUE
  )
  cmp[, diff_pp := pct_R - pct_Py]
  fwrite(cmp, file.path(tab_dir, "Table_Compare_DHS_R_vs_Python.csv"))
  cat("\n=== R vs Python DHS ===\n")
  print(cmp[, .(label, pct_R = round(pct_R, 2), pct_Py = round(pct_Py, 2),
                diff_pp = round(diff_pp, 3), n_R, n_Py)])
}

cat("\nDHS R-library analysis complete.\n")
