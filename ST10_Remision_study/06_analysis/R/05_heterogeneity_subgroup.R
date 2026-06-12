# Study: ST10_Remision_study
# Script: 05_heterogeneity_subgroup.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Heterogeneity summary (Q, I2, tau2, prediction interval) and the
#          pre-specified subgroup analyses (>=3 studies per subgroup required).
#          Primary subgroups per protocol: income tier, intervention type,
#          baseline disease duration.

message("=== 05: HETEROGENEITY & SUBGROUPS ===")

if (!exists("m_rr")) m_rr <- readRDS(file.path(paths$results, "meta_binary.rds"))$rr

# Heterogeneity readout -----------------------------------------------------
i2 <- m_rr$I2 * 100
cat(sprintf("\nI2 = %.1f%% (%s); Cochran Q p = %.3f; tau^2 = %.4f\n",
            i2, heterogeneity_label(i2), m_rr$pval.Q, m_rr$tau2))
if (!is.na(i2) && i2 > 50)
  message("I2 > 50%: do NOT lead with a single pooled estimate (protocol) - ",
          "explore via subgroups + narrative synthesis.")

# Subgroup helper (only runs where >=3 studies per level) -------------------
subgroup_if_enough <- function(m, byvar, min_studies = 3, file = NULL) {
  by <- m$data[[byvar]]
  ok <- names(which(table(by[!is.na(by)]) >= min_studies))
  if (length(ok) < 2) {
    message("Subgroup '", byvar, "': <2 levels with >=", min_studies,
            " studies - reporting narratively, not pooled.")
    return(invisible(NULL))
  }
  mu <- update(m, subgroup = by, subgroup.name = byvar, tau.common = FALSE)
  cat("\n-- Subgroup by ", byvar, " --\n", sep = ""); print(summary(mu))
  if (!is.null(file)) save_plot(forest(mu), file, height = 6)
  mu
}

# Attach moderators onto the meta object's data for subgrouping
m_rr$data$income_tier           <- dat$income_tier[match(m_rr$studlab, dat$study_id)]
m_rr$data$intervention_category <- dat$intervention_category[match(m_rr$studlab, dat$study_id)]
m_rr$data$duration_cat <- cut(dat$disease_duration_years[match(m_rr$studlab, dat$study_id)],
                              breaks = c(-Inf, 5, Inf), labels = c("<5y", ">=5y"))

sg_income   <- subgroup_if_enough(m_rr, "income_tier",           file = "fig_subgroup_income.png")
sg_inttype  <- subgroup_if_enough(m_rr, "intervention_category", file = "fig_subgroup_intervention.png")
sg_duration <- subgroup_if_enough(m_rr, "duration_cat",          file = "fig_subgroup_duration.png")

saveRDS(list(income = sg_income, intervention = sg_inttype, duration = sg_duration),
        file.path(paths$results, "subgroups.rds"))

message("=== 05 COMPLETE ===")
