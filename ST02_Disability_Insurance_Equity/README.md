# ST02: Disability, Insurance, and Payment at Outpatient Contact Before SHA

This folder contains the public reproducible source package for the ST02 study.

## Included in version control

- `02_data_notes/`: variable-mapping notes used to document the KDHS 2022 PR disability and insurance items
- `03_scripts/`: modular analysis workflow
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

3. Render the manuscript:

```bash
cd 06_manuscript
quarto render manuscript.qmd --to docx
```

## Notes

- The public repository includes manuscript source, not journal-admin files or generated outputs.
- Workflow scripts create their required output directories automatically.
