# 06_analysis — Meta-analysis pipeline (R)

Reproducible synthesis implementing the protocol's pre-specified methods.

## Run it
```bash
Rscript 06_analysis/R/run_meta_workflow.R
```
First run: open `R/00_setup.R` and uncomment the `install_if_missing(...)` line (needs
`meta`, `metafor`, `metasens`, `robvis`, `PRISMA2020`, tidyverse). The pipeline uses the
synthetic `data_input/example_meta_input.csv` until you drop a real `meta_input.csv` in
`data_input/` (same columns — see the extraction codebook).

## Scripts
| Script | Does |
|--------|------|
| `00_setup.R` | Packages, paths, REML+HKSJ defaults, prediction intervals on. |
| `01_import.R` | Load + validate the analysis-ready input. |
| `02_meta_proportion.R` | Pooled single-arm remission **proportion** (logit + Freeman-Tukey; keeps zero-event studies). |
| `03_meta_binary.R` | Remission **risk ratio** (REML+HKSJ, 0.5 continuity correction); Peto OR + GLMM sensitivity. |
| `04_meta_continuous.R` | **HbA1c** and **SBP** change (WMD/SMD). |
| `05_heterogeneity_subgroup.R` | Q, I², τ², prediction interval; pre-specified subgroups (≥3 studies). |
| `06_sensitivity.R` | Pre-specified sensitivity analyses → `results/sensitivity_summary.csv`. |
| `07_metaregression_patient_chars.R` | Objective 3: descriptive baseline comparison + (guarded) meta-regression. |
| `08_publication_bias.R` | Funnel + Egger + trim-and-fill (only if ≥10 studies). |
| `09_rob_grade_viz.R` | robvis plots from the RoB 2 CSV. |
| `10_prisma_flow.R` | PRISMA 2020 flow from `08_reporting/prisma_flow_counts.csv`. |

## Inputs / outputs
- **Input:** `data_input/meta_input.csv` (tracked). Derived from the extraction form.
- **Output:** `results/` — forest/funnel plots, RDS objects, summary CSVs. **Git-ignored.**

## Built-in protocol guards (verified on example data)
Subgroups require ≥3 studies/level; meta-regression requires ≥10 studies (and ≥10 per
covariate); publication-bias tests require ≥10 studies. Below threshold, the scripts
report narratively instead of forcing an unstable pooled estimate — exactly as the
protocol specifies.
