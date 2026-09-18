# ST12: Beyond the diagnosed — dual-survey NCD unmet need (STEPS 2015 × KDHS 2022)

Full study package: measured hypertension/diabetes **care cascades** from Kenya STEPS 2015, self-report and insurance from KDHS 2022, and a **validated dual-survey hybrid estimator** of population treatment coverage.

## Headline results

| Metric | Estimate |
|--------|----------|
| STEPS measured HTN prevalence | 24.2% (21.8–26.7) |
| Aware / treated / controlled among HTN | 23.7% / 7.8% / 2.7% |
| Population with controlled HTN | 0.6% of adults |
| KDHS self-reported HTN diagnosis | 4.4% (detection ratio 0.18) |
| Treated among diagnosed HTN (KDHS) | 34.0% |
| Hybrid τ (awareness × treatment\|dx) | 8.0% ≈ STEPS-direct 7.8% |

## Folder layout

```
ST12_STEPS_DHS_DualSurvey_NCD/
├── 01_protocol/          protocol
├── 02_data_notes/        variable map
├── 03_scripts/           Python analysis pipeline
├── 04_tables/            publication tables (CSV)
├── 05_figures/           PNG + TIFF 300 DPI
├── 06_manuscript/        manuscript.qmd + DOCX + bib
├── 07_derived_data/      analytic extracts (local only; gitignored)
└── 08_logs/              JSON summaries
```

## Reproduce

### R workflow (preferred for design-based SEs; uses `~/R/library`)

**Packages used now:** `data.table`, `sandwich` (ultimate-cluster variance + sandwich cluster check).  
**Full `survey`/`haven`/`dplyr` scripts** also exist (`01_steps_construct_analyze.R`, `02_dhs_construct_analyze.R`, `run_st12_workflow.R`) for when those packages are installed.

```bash
export R_LIBS_USER="$HOME/R/library"
cd 03_scripts
Rscript run_st12_Rlib.R
```

Outputs: `04_tables/Table*_R.csv`, `Table_Compare_*_R_vs_Python.csv` (validation).

Cross-check (already run): R vs Python max |diff| ≈ 0 for all matched cascade estimates; sandwich cluster SE for HTN prevalence = 0.0125 (matches design SE).

### Python workflow

**Prerequisites:** Python 3 with `pandas`, `numpy`, `matplotlib`, `pyreadstat`.  
**Data (local, never commit):**

- `Research/Kenya - STEPS 2015/ken2015.csv` (or `.dta`)
- `Research/01_DHS_Data/KDHS_2022/` IR, MR, PR `.dta` files

```bash
cd 03_scripts
python3 run_st12_workflow.py
```

```bash
cd 06_manuscript
quarto render manuscript.qmd --to docx
```

## Manuscript

- Source: `06_manuscript/manuscript.qmd`
- Rendered: `06_manuscript/manuscript.docx`
- Target journals: *BMC Public Health* or *International Journal for Equity in Health*

## Data privacy

Do **not** commit STEPS or DHS microdata (`.dta`, `.sav`, `ken2015.csv`, person-level analytic extracts). Only scripts, protocol, bibliography, and aggregated tables/figures belong in version control.

## Relation to portfolio

Complements **ST03** (treatment gaps among *diagnosed* adults with insurance). ST12 shows that the diagnosed are a minority of measured disease and supplies a methods bridge between STEPS and DHS.
