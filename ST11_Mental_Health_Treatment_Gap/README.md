# ST11: Depression and Anxiety Treatment Gap in Kenya (KDHS 2022)

Public reproducible source package for the ST11 study: the national depression/anxiety **treatment gap**
(diagnosed but untreated) and its socioeconomic and insurance patterning, using the KDHS 2022 women's and
men's recodes. This is the first analysis to use the survey's mental-health *treatment* item, complementing
existing KDHS-2022 papers that examined diagnosis prevalence and correlates only.

## Included in version control
- `01_protocol/`: study protocol
- `02_data_notes/`: variable-mapping notes
- `03_scripts/`: modular analysis workflow
- `06_manuscript/`: Quarto manuscript source, STROBE checklist source, bibliography

## Generated locally, not versioned
- `04_tables/`, `05_figures/`, `07_derived_data/`, `08_logs/`, and rendered `.docx`/`.html`.

## Rebuild steps
1. Place the approved KDHS 2022 recode files in `../01_DHS_Data/KDHS_2022/`
   (women `IR_Individual_Recode/KEIR8CFL.DTA`, men `MR_Mens_Recode/KEMR8CFL.DTA`,
   and `PR_Person_Recode/KEPR8CFL.DTA` for the insurance merge).
2. From the study root:

```bash
Rscript 03_scripts/run_st11_workflow.R
```

3. Render the manuscript and STROBE checklist:

```bash
cd 06_manuscript
quarto render manuscript.qmd --to docx,html
quarto render strobe_checklist.qmd --to docx
```

## Key variables
- Outcome: diagnosed-but-untreated depression/anxiety (`chd17/18/19`, `mchd17/18/19`).
- Insurance from PR (`sh27`); NCD comorbidity from `chd02/07/11/13/20`; wealth `v190/mv190`.

## Notes
- The mental-health module was administered to ~31,000 adults; the treatment-gap denominator is the subset
  reporting a depression/anxiety diagnosis. Self-reported diagnosis is access-dependent and is interpreted as
  such (the treatment gap is the research question, not a nuisance).
- The workflow creates its output directories automatically.
