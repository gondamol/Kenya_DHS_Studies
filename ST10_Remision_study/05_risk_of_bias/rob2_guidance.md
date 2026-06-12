# Risk of bias — Cochrane RoB 2 guidance

Assess **per study × per outcome** (a study contributes multiple rows — at minimum the
primary remission outcome; add BP-normalisation and key secondary outcomes). For
cluster-randomised trials, use the **RoB 2 cluster extension** (adds a randomisation/
timing-of-identification consideration). Two assessors independently; adjudicate
disagreements. Record each domain and the overall judgement in `rob2_template.csv`.

Each domain judgement is one of: **`low`**, **`some_concerns`**, **`high`**.

| Col | Domain | Core question |
|-----|--------|---------------|
| `d1_randomisation` | Bias arising from the randomisation process | Was the allocation sequence random and concealed? Baseline imbalances suggesting a problem? |
| `d2_deviations` | Deviations from intended interventions | Were participants/carers blind? Deviations beyond what would be expected, analysed appropriately (ITT)? |
| `d3_missing_data` | Missing outcome data | Were outcome data available for ~all randomised? Evidence result not biased by missingness? |
| `d4_measurement` | Measurement of the outcome | Appropriate method; outcome assessors blind; ascertainment comparable across arms? *(Key here: remission ascertainment — HbA1c standardisation, medication-cessation verification.)* |
| `d5_selection_reporting` | Selection of the reported result | Pre-specified analysis (registered/protocol); result not selected from multiple outcomes/analyses? |

## Overall (`overall_rob`) algorithm
- **low** — low risk in *all* domains.
- **some_concerns** — some concerns in ≥1 domain, but high risk in none.
- **high** — high risk in ≥1 domain, *or* multiple domains with some concerns
  substantially lowering confidence.

## Review-specific notes
- **Medication-cessation verification (D4)** is pivotal for this review: remission
  hinges on documented, sustained withdrawal of *all* glucose-lowering drugs. Trials
  asserting cessation without a credible source → at least *some concerns* on D4.
- **FPG-based vs HbA1c-based** ascertainment is handled under GRADE *indirectness*
  (`../07_grade/`), not RoB — but note it in `rationale_notes`.
- Visualisation: `../06_analysis/R/09_rob_grade_viz.R` reads this CSV and produces the
  **robvis** traffic-light + weighted-bar plots.

> The RoB 2 tool, Excel templates, and full signalling questions:
> https://www.riskofbias.info/welcome/rob-2-0-tool
