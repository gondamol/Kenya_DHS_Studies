#!/usr/bin/env Rscript
# ST12 STEPS analysis using available R user library:
#   data.table + sandwich (+ base stats)
# Reads ken2015.csv (or derived slim if present).
# Produces Table*_R.csv and comparison with Python.

user_lib <- Sys.getenv("R_LIBS_USER", unset = path.expand("~/R/library"))
.libPaths(c(user_lib, .libPaths()))
suppressPackageStartupMessages({
  library(data.table)
  library(sandwich)
})

root <- "/mnt/c/Users/HFD 2/Research/02_Studies/ST12_STEPS_DHS_DualSurvey_NCD"
tab_dir <- file.path(root, "04_tables")
der_dir <- file.path(root, "07_derived_data")
log_dir <- file.path(root, "08_logs")
for (d in c(tab_dir, der_dir, log_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

cat("R", as.character(getRversion()), "| lib:", .libPaths()[[1]], "\n")
cat("data.table", as.character(packageVersion("data.table")),
    "| sandwich", as.character(packageVersion("sandwich")), "\n")

# ---- design-based mean (ultimate cluster, with strata) ----
cluster_mean <- function(dt, y, w, psu_col = "psu", strata_col = NULL) {
  ok <- !is.na(dt[[y]]) & !is.na(dt[[w]]) & dt[[w]] > 0 & !is.na(dt[[psu_col]])
  d <- copy(dt[ok])
  if (!nrow(d)) return(list(estimate = NA_real_, se = NA_real_, n = 0L, n_psu = 0L))
  yy <- d[[y]]
  ww <- d[[w]]
  mu <- sum(ww * yy) / sum(ww)
  d[, u_w := ww * (yy - mu)]
  d[, w_w := ww]
  d[, psu_id := d[[psu_col]]]
  if (!is.null(strata_col) && strata_col %in% names(d)) {
    d[, st_id := d[[strata_col]]]
    g <- d[, .(u = sum(u_w), w = sum(w_w), st = st_id[1]), by = psu_id]
    var_num <- 0
    for (stv in unique(g$st)) {
      sub <- g[st == stv]
      nh <- nrow(sub)
      if (nh < 2) next
      var_num <- var_num + (nh / (nh - 1)) * sum((sub$u - mean(sub$u))^2)
    }
  } else {
    g <- d[, .(u = sum(u_w)), by = psu_id]
    nc <- nrow(g)
    var_num <- (nc / (nc - 1)) * sum((g$u - mean(g$u))^2)
  }
  W <- sum(ww)
  se <- if (var_num > 0) sqrt(var_num) / W else NA_real_
  list(estimate = mu, se = se, n = nrow(d), n_psu = uniqueN(d$psu_id))
}

prop_row <- function(dt, y, w, psu_col = "psu", strata_col = "stratum", label = y) {
  r <- cluster_mean(dt, y, w, psu_col, strata_col)
  z <- qnorm(0.975)
  lo <- max(0, r$estimate - z * r$se)
  hi <- min(1, r$estimate + z * r$se)
  data.table(
    label = label, estimate = r$estimate, se = r$se,
    ci_low = lo, ci_high = hi,
    pct = 100 * r$estimate, pct_low = 100 * lo, pct_high = 100 * hi,
    n = r$n, n_psu = r$n_psu
  )
}

fmt <- function(est, lo, hi) sprintf("%.1f (%.1f–%.1f)", 100*est, 100*lo, 100*hi)

# ---- load / construct ----
slim <- file.path(der_dir, "st12_steps_analytic_slim.csv")
if (file.exists(slim)) {
  cat("Loading derived slim CSV…\n")
  d <- fread(slim)
} else {
  cat("Constructing from ken2015.csv…\n")
  raw <- fread("/mnt/c/Users/HFD 2/Research/Kenya - STEPS 2015/ken2015.csv")
  setnames(raw, tolower(names(raw)))
  numc <- function(x) {
    x <- as.numeric(x)
    x[x %in% c(77, 88, 99, 777, 888, 999)] <- NA_real_
    x
  }
  yes1 <- function(x) {
    x <- as.numeric(x)
    out <- rep(NA_real_, length(x))
    out[x == 1] <- 1
    out[x == 2] <- 0
    out
  }
  d <- copy(raw)
  d[, `:=`(
    sbp = (numc(m5a) + numc(m6a)) / 2,
    dbp = (numc(m5b) + numc(m6b)) / 2,
    sbp1 = numc(m4a), dbp1 = numc(m4b),
    height_cm = numc(m11), weight_kg = numc(m12),
    glucose = numc(b5), cholesterol = numc(b8),
    bp_ever_measured = yes1(h1), told_high_bp = yes1(h2a), on_bp_meds = yes1(h3),
    gluc_ever_measured = yes1(h6), told_high_gluc = yes1(h7a), on_dm_meds = yes1(h8),
    wstep1 = as.numeric(wstep1), wstep2 = as.numeric(wstep2), wstep3 = as.numeric(wstep3),
    psu = as.numeric(psu), stratum = as.numeric(stratum)
  )]
  d[is.na(sbp), sbp := sbp1]
  d[is.na(dbp), dbp := dbp1]
  d[, on_bp_meds_pop := on_bp_meds]
  d[bp_ever_measured == 0 | told_high_bp == 0, on_bp_meds_pop := 0]
  d[, on_dm_meds_pop := on_dm_meds]
  d[gluc_ever_measured == 0 | told_high_gluc == 0, on_dm_meds_pop := 0]
  d[, htn_measured := as.numeric((!is.na(sbp) & sbp >= 140) | (!is.na(dbp) & dbp >= 90) | on_bp_meds_pop == 1)]
  d[is.na(sbp) & is.na(dbp) & (is.na(on_bp_meds_pop) | on_bp_meds_pop != 1), htn_measured := NA_real_]
  d[, dm_measured := as.numeric((!is.na(glucose) & glucose >= 7) | on_dm_meds_pop == 1)]
  d[is.na(glucose) & (is.na(on_dm_meds_pop) | on_dm_meds_pop != 1), dm_measured := NA_real_]
  d[, htn_aware := ifelse(htn_measured == 1, as.numeric(told_high_bp == 1 | on_bp_meds_pop == 1), NA_real_)]
  d[, htn_treated := ifelse(htn_measured == 1, as.numeric(on_bp_meds_pop == 1), NA_real_)]
  d[, htn_controlled := ifelse(htn_measured == 1,
    as.numeric(on_bp_meds_pop == 1 & sbp < 140 & dbp < 90), NA_real_)]
  d[, dm_aware := ifelse(dm_measured == 1, as.numeric(told_high_gluc == 1 | on_dm_meds_pop == 1), NA_real_)]
  d[, dm_treated := ifelse(dm_measured == 1, as.numeric(on_dm_meds_pop == 1), NA_real_)]
  d[, dm_controlled := ifelse(dm_measured == 1,
    as.numeric(on_dm_meds_pop == 1 & glucose < 7), NA_real_)]
  d[, bmi := weight_kg / ((height_cm / 100)^2)]
  d[!is.finite(bmi) | bmi < 10 | bmi > 80, bmi := NA_real_]
  d[, overweight_obese := ifelse(is.na(bmi), NA_real_, as.numeric(bmi >= 25))]
  d[, obese := ifelse(is.na(bmi), NA_real_, as.numeric(bmi >= 30))]
  d[, chol_raised := ifelse(is.na(cholesterol), NA_real_, as.numeric(cholesterol >= 5))]
  d[, sex_f := fifelse(grepl("Women|Female", as.character(sex), ignore.case = TRUE), "Women",
                fifelse(grepl("Men|Male", as.character(sex), ignore.case = TRUE), "Men", as.character(sex)))]
  d[, htn_pop_controlled := ifelse(is.na(htn_measured), NA_real_,
                                    ifelse(htn_measured == 1, htn_controlled, 0))]
}

# Harmonize column names from slim export
if (!"sex_f" %in% names(d) && "sex" %in% names(d)) {
  d[, sex_f := as.character(sex)]
}
if (!"htn_pop_controlled" %in% names(d)) {
  d[, htn_pop_controlled := ifelse(is.na(htn_measured), NA_real_,
                                    ifelse(htn_measured == 1, htn_controlled, 0))]
}
if (!"htn_ctrl_among_tx" %in% names(d) && "htn_controlled_among_treated" %in% names(d)) {
  d[, htn_ctrl_among_tx := htn_controlled_among_treated]
}

s2 <- d[!is.na(wstep2) & wstep2 > 0]
s3 <- d[!is.na(wstep3) & wstep3 > 0]
htn <- s2[htn_measured == 1]
dm <- s3[dm_measured == 1]
tx <- htn[htn_treated == 1]

cat("Step2 n=", nrow(s2), " HTN n=", nrow(htn), " Step3 n=", nrow(s3), " DM n=", nrow(dm), "\n")

rows <- rbindlist(list(
  prop_row(s2, "htn_measured", "wstep2", "psu", "stratum", "Measured hypertension prevalence")[, domain := "step2_all"],
  prop_row(s2, "overweight_obese", "wstep2", "psu", "stratum", "Overweight or obese (BMI>=25)")[, domain := "step2_all"],
  prop_row(s2, "obese", "wstep2", "psu", "stratum", "Obesity (BMI>=30)")[, domain := "step2_all"],
  prop_row(s2, "bp_ever_measured", "wstep2", "psu", "stratum", "Ever had blood pressure measured")[, domain := "step2_all"],
  prop_row(s2, "htn_pop_controlled", "wstep2", "psu", "stratum", "Population on controlled HTN treatment")[, domain := "step2_all"],
  prop_row(htn, "htn_aware", "wstep2", "psu", "stratum", "HTN: aware (told or on meds)")[, domain := "step2_htn"],
  prop_row(htn, "htn_treated", "wstep2", "psu", "stratum", "HTN: on medication")[, domain := "step2_htn"],
  prop_row(htn, "htn_controlled", "wstep2", "psu", "stratum", "HTN: controlled (among all HTN)")[, domain := "step2_htn"],
  prop_row(tx, if ("htn_ctrl_among_tx" %in% names(tx)) "htn_ctrl_among_tx" else "htn_controlled",
           "wstep2", "psu", "stratum", "HTN: controlled among treated")[, domain := "step2_htn"],
  prop_row(s3, "dm_measured", "wstep3", "psu", "stratum", "Measured diabetes prevalence")[, domain := "step3_all"],
  prop_row(s3, "gluc_ever_measured", "wstep3", "psu", "stratum", "Ever had blood glucose measured")[, domain := "step3_all"],
  prop_row(s3, "chol_raised", "wstep3", "psu", "stratum", "Raised total cholesterol (>=5.0 mmol/L)")[, domain := "step3_all"],
  prop_row(dm, "dm_aware", "wstep3", "psu", "stratum", "DM: aware")[, domain := "step3_dm"],
  prop_row(dm, "dm_treated", "wstep3", "psu", "stratum", "DM: on medication")[, domain := "step3_dm"],
  prop_row(dm, "dm_controlled", "wstep3", "psu", "stratum", "DM: controlled")[, domain := "step3_dm"]
), fill = TRUE)

for (i in seq_len(nrow(rows))) {
  cat(sprintf("  %s: %s n=%s\n", rows$label[i],
              fmt(rows$estimate[i], rows$ci_low[i], rows$ci_high[i]), rows$n[i]))
}

fwrite(rows, file.path(tab_dir, "Table1_STEPS_KeyEstimates_R.csv"))

cascade <- rbindlist(list(
  rows[label == "HTN: aware (told or on meds)", .(condition = "Hypertension", stage = "Aware", pct, pct_low, pct_high, n,
    fmt = sprintf("%.1f (%.1f–%.1f)", pct, pct_low, pct_high))],
  rows[label == "HTN: on medication", .(condition = "Hypertension", stage = "Treated", pct, pct_low, pct_high, n,
    fmt = sprintf("%.1f (%.1f–%.1f)", pct, pct_low, pct_high))],
  rows[label == "HTN: controlled (among all HTN)", .(condition = "Hypertension", stage = "Controlled", pct, pct_low, pct_high, n,
    fmt = sprintf("%.1f (%.1f–%.1f)", pct, pct_low, pct_high))],
  rows[label == "DM: aware", .(condition = "Diabetes", stage = "Aware", pct, pct_low, pct_high, n,
    fmt = sprintf("%.1f (%.1f–%.1f)", pct, pct_low, pct_high))],
  rows[label == "DM: on medication", .(condition = "Diabetes", stage = "Treated", pct, pct_low, pct_high, n,
    fmt = sprintf("%.1f (%.1f–%.1f)", pct, pct_low, pct_high))],
  rows[label == "DM: controlled", .(condition = "Diabetes", stage = "Controlled", pct, pct_low, pct_high, n,
    fmt = sprintf("%.1f (%.1f–%.1f)", pct, pct_low, pct_high))]
))
fwrite(cascade, file.path(tab_dir, "Table2_STEPS_Cascade_R.csv"))

# Sex stratification HTN
sex_rows <- list()
for (sx in c("Men", "Women")) {
  ss <- s2[sex_f == sx]
  hh <- htn[sex_f == sx]
  for (pair in list(
    list(ss, "htn_measured", "Prevalence"),
    list(hh, "htn_aware", "Aware"),
    list(hh, "htn_treated", "Treated"),
    list(hh, "htn_controlled", "Controlled")
  )) {
    r <- prop_row(pair[[1]], pair[[2]], "wstep2", "psu", "stratum", pair[[3]])
    r[, `:=`(condition = "Hypertension", stratifier = "Sex", category = sx, stage = pair[[3]],
             fmt = sprintf("%.1f (%.1f–%.1f)", pct, pct_low, pct_high))]
    sex_rows[[length(sex_rows) + 1]] <- r
  }
}
strat <- rbindlist(sex_rows, fill = TRUE)
fwrite(strat[, .(condition, stratifier, category, stage, pct, pct_low, pct_high, n, fmt)],
       file.path(tab_dir, "Table4_STEPS_Cascade_BySex_R.csv"))

# Compare to Python
pyf <- file.path(tab_dir, "Table1_STEPS_KeyEstimates.csv")
if (file.exists(pyf)) {
  py <- fread(pyf)
  cmp <- merge(
    rows[, .(label, pct_R = pct, n_R = n, estimate_R = estimate)],
    py[, .(label, pct_Py = pct, n_Py = n, estimate_Py = estimate)],
    by = "label", all = TRUE
  )
  cmp[, diff_pp := pct_R - pct_Py]
  fwrite(cmp, file.path(tab_dir, "Table_Compare_STEPS_R_vs_Python.csv"))
  cat("\n=== R (data.table/sandwich design SE) vs Python ===\n")
  print(cmp[, .(label, pct_R = round(pct_R, 2), pct_Py = round(pct_Py, 2),
                diff_pp = round(diff_pp, 3), n_R, n_Py)])
  cat("\nMax |diff| pp:", max(abs(cmp$diff_pp), na.rm = TRUE), "\n")
}

# Optional: sandwich HC1 on simple weighted OLS for HTN prevalence (cluster-robust check)
if (requireNamespace("sandwich", quietly = TRUE)) {
  s2c <- s2[!is.na(htn_measured)]
  # weighted mean via lm with weights; cluster SE by PSU
  fit <- lm(htn_measured ~ 1, data = s2c, weights = wstep2)
  # meat of sandwich by cluster
  # Use multiwayvcov or manual
  if (requireNamespace("multiwayvcov", quietly = TRUE)) {
    V <- multiwayvcov::cluster.vcov(fit, s2c$psu)
    se_sw <- sqrt(V[1, 1])
    cat(sprintf("\nSandwich cluster SE (lm intercept HTN): est=%.4f se=%.4f\n",
                coef(fit)[1], se_sw))
  } else {
    # manual cluster sandwich for intercept-only weighted model is non-trivial;
    # our ultimate-cluster estimator above is the primary design-based SE.
    cat("\n(multiwayvcov not installed; ultimate-cluster SE used as primary)\n")
  }
}

cat("\nSTEPS R-library analysis complete →", tab_dir, "\n")
