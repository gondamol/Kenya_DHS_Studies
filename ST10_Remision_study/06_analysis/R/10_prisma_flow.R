# Study: ST10_Remision_study
# Script: 10_prisma_flow.R
# Author: Nichodemus Werre Amollo
# Date: 2026-06-12
# Purpose: Build the PRISMA 2020 flow diagram from the counts recorded in
#          ../../08_reporting/prisma_flow_counts.csv.

message("=== 10: PRISMA 2020 FLOW DIAGRAM ===")

if (!file.exists(paths$prisma_csv)) {
  message("No prisma_flow_counts.csv - fill it in ../08_reporting first. Skipping.")
} else {
  counts <- readr::read_csv(paths$prisma_csv, show_col_types = FALSE)
  cat("\n-- PRISMA counts --\n"); print(counts, row.names = FALSE)

  if (requireNamespace("PRISMA2020", quietly = TRUE)) {
    message("PRISMA2020 installed: build the diagram with PRISMA2020::PRISMA_flowdiagram(). ",
            "See the package template CSV for the exact field names it expects, and map ",
            "prisma_flow_counts.csv onto it.")
  } else {
    message("Install 'PRISMA2020' (or use the Haddaway shinyapp / draw.io) to render the ",
            "diagram from these counts.")
  }
}

message("=== 10 COMPLETE ===")
