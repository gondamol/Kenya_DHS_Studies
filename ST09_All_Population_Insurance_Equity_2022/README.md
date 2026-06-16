# ST09: All-Population Insurance Equity Baseline Before SHA (KDHS 2022)

This folder contains the public reproducible source package for the ST09 study:
an all-ages, both-sexes, disability-inclusive analysis of health insurance
coverage, wealth-related inequality, and payment at outpatient contact in Kenya
immediately before the Social Health Authority (SHA) transition.

## Included in version control

- `02_data_notes/`: variable-mapping notes and the analytic-variable dictionary
- `03_scripts/`: the analysis workflow (`ST09_analysis_main.R`)
- `06_manuscript/`: Quarto manuscript source, STROBE checklist source, and bibliography

## Generated locally, not versioned

Running the workflow creates:

- `04_tables/`: exported tables (Tables 1-8)
- `05_figures/`: exported figures (Figures 1-6)
- `07_derived_data/`: derived R objects (`st09_analysis_outputs.rds`)
- `08_logs/` and `results/logs/`: validation logs, session info, and execution records
- rendered `.docx`/`.html` outputs in `06_manuscript/`, including the
  Additional file 1 (STROBE checklist) and Additional file 2 (supplementary tables)

## Rebuild steps

1. Place the approved KDHS 2022 recode files in `../01_DHS_Data/KDHS_2022/` relative to the repository root.
2. From the study root, run:

```bash
Rscript 03_scripts/ST09_analysis_main.R
```

3. Render the manuscript and the STROBE checklist:

```bash
cd 06_manuscript
quarto render manuscript.qmd --to docx,html
quarto render strobe_checklist.qmd --to docx
```

## Notes

- This analysis uses the KDHS 2022 person recode (`PR`; `KEPR8CFL.DTA`) file, which
  carries household-member insurance and disability items in the full household
  questionnaire subsample.
- The public repository includes manuscript source, not journal-admin files or generated outputs.
- The script creates its required output directories automatically.
