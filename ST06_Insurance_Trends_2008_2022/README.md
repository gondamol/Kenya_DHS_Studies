# ST06: Fourteen Years of Expanding but Unequal Insurance Coverage (KDHS 2008, 2014, 2022)

Public reproducible source package for the ST06 study: a multi-wave analysis of health
insurance coverage, subgroup inequalities, and wealth-related concentration among Kenyan
women aged 15-49 across three KDHS rounds spanning the full NHIF policy era before the
Social Health Authority (SHA) transition.

## Included in version control

- `01_protocol/`: literature-search notes
- `02_data_notes/`: variable-mapping and harmonisation notes across the three waves
- `03_scripts/`: analysis workflow (`ST06_analysis_main.R`, `ST06_harmonisation_crosswalk.R`)
- `06_manuscript/`: Quarto manuscript source and bibliography

## Generated locally, not versioned

Running the workflow creates:

- `04_tables/`: exported tables (Tables 1-4 plus the results summary)
- `05_figures/`: exported figures
- `07_derived_data/`: derived R objects (`st06_analysis_outputs.rds`)
- `08_logs/` and `results/logs/`: validation logs and execution records
- rendered `.docx`/`.html` outputs in `06_manuscript/`

## Rebuild steps

1. Place the approved KDHS recode files relative to the repository root:
   - `../01_DHS_Data/KDHS_2008/` individual recode (`KEIR52FL.DTA`)
   - `../01_DHS_Data/KDHS_2014/` individual recode (`KEIR72FL.DTA`)
   - `../01_DHS_Data/KDHS_2022/` person recode (`KEPR8CFL.DTA`)
2. From the study root, run:

```bash
Rscript 03_scripts/ST06_analysis_main.R
```

3. Render the manuscript:

```bash
cd 06_manuscript
quarto render manuscript.qmd --to docx
```

## Key methodological note

The 2022 wave uses the **person recode (PR)**, not the individual recode (IR): `v481` is
entirely missing from the KDHS 2022 IR file, so coverage is taken from `sh27` (any
insurance) and `sh28a-c`, `sh28x` (insurance types). The 2008 and 2014 waves use the
standard IR insurance variables (`v481`, `v481a-e`). The harmonisation crosswalk that
reconciles these is in `03_scripts/ST06_harmonisation_crosswalk.R` and is the main
methodological contribution of the analysis; it is documented in Methods.

## Notes

- The public repository includes manuscript source, not journal-admin files or generated outputs.
- The workflow creates its required output directories automatically.
