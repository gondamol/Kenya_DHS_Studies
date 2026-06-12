# Study: ST10_Remision_study
# Script: 08_publication_bias.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Small-study / publication-bias assessment - ONLY where >=10 studies
#          contribute (protocol). Contour-enhanced funnel plot, Egger's test (p<0.10),
#          and trim-and-fill as an EXPLORATORY adjunct (not a correction).

message("=== 08: PUBLICATION BIAS ===")

if (!exists("m_rr")) m_rr <- readRDS(file.path(paths$results, "meta_binary.rds"))$rr

if (m_rr$k < 10) {
  message(sprintf("Only %d studies (<10): publication-bias tests NOT performed ",
                  m_rr$k), "(protocol). Note this as a limitation.")
} else {
  save_plot(funnel(m_rr, contour = c(0.9, 0.95, 0.99),
                   col.contour = c("grey80", "grey60", "grey40")),
            "fig_funnel_remission_RR.png")
  egger <- metabias(m_rr, method.bias = "linreg", k.min = 10)
  cat("\n-- Egger's test --\n"); print(egger)

  tf <- trimfill(m_rr)
  cat("\n-- Trim-and-fill (exploratory) --\n"); print(summary(tf))
  save_plot(funnel(tf), "fig_funnel_trimfill.png")

  saveRDS(list(egger = egger, trimfill = tf),
          file.path(paths$results, "publication_bias.rds"))
}

message("=== 08 COMPLETE ===")
