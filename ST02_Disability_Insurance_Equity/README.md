# ST02: Disability, Insurance, and Payment at Outpatient Contact Before SHA

This folder contains the public reproducible source package for the ST02 study.

## Included in version control

- `02_data_notes/`: variable-mapping notes used to document the KDHS 2022 PR disability and insurance items
- `03_scripts/`: modular analysis workflow, run in the order `00_setup.R`, `01_data_import.R`,
  `02_variable_construction.R`, `03_analysis_main.R` (estimates within the disability subpopulation),
  `03b_analysis_whole_sample.R` (the disability contrast itself, severity contrasts, outpatient
  amounts and payers, bounds, sensitivity analyses), `04_tables_figures.R`, and
  `05_render_outputs.R` with its helper `05_fix_docx_schema_order.py`
- `06_manuscript/`: Quarto manuscript source and bibliography

## Generated locally, not versioned

Running the workflow creates:

- `04_tables/`: exported tables
- `05_figures/`: exported figures
- `07_derived_data/`: derived R objects and summary text
- `08_logs/`: validation logs and execution records
- rendered `.docx` outputs in `06_manuscript/`

## Rebuild steps

1. Place the approved KDHS 2022 recode files in `../01_DHS_Data/KDHS_2022/` relative to the repository root.
2. From the study root, run:

```bash
Rscript 03_scripts/run_st02_workflow.R
```

3. Render the manuscript and its companion documents:

```bash
Rscript 03_scripts/05_render_outputs.R
```

That renders `manuscript.qmd`, `supplementary_tables.qmd`, `strobe_checklist.qmd` and
`cover_letter.qmd`, then runs `03_scripts/05_fix_docx_schema_order.py` over the results.

The repair step is not optional. Word validates property containers such as `w:pPr` and
`w:rPr` against a fixed child order, and flextable writes them in its own order, so a
freshly rendered manuscript asks to be opened in repair mode. The script reorders those
children and changes nothing else; it needs Python 3 on PATH. Rendering with
`quarto render` alone produces a document Word will offer to repair.

## Notes

- The public repository includes manuscript source, not journal-admin files or generated outputs.
- Workflow scripts create their required output directories automatically.
