# ST13: The self-report denominator in Kenya's NCD evidence (STEPS 2015 × KDHS 2022)

Public reproducible source package for ST13: how much measured non-communicable disease burden is
excluded by the self-reported-diagnosis denominator that household-survey financing research relies on.

**Supersedes ST12.** ST13 is the publishable successor to `../ST12_STEPS_DHS_DualSurvey_NCD/`. It drops
that study's dual-survey hybrid estimator entirely — see
[`../ST12_STEPS_DHS_DualSurvey_NCD/METHODS_REVIEW_hybrid_estimator.md`](../ST12_STEPS_DHS_DualSurvey_NCD/METHODS_REVIEW_hybrid_estimator.md)
for the diagnostic that established why — and is built only on quantities requiring no new methods.

## Headline results (ages 18-49, age-comparable base)

| Metric | Estimate (95% CI) |
|--------|-------------------|
| Measured hypertension prevalence (STEPS 2015) | 19.8% (17.5-22.2) |
| Self-reported hypertension diagnosis (KDHS 2022) | 4.2% (3.9-4.5) |
| **Detection ratio** | **0.212 (0.184-0.244)** |
| Underdiagnosis index | 78.8% (75.6-81.6) |
| All adults with unaware hypertension | 16.2% (14.3-18.2) |
| Treated among diagnosed (KDHS) | 32.0% (28.7-35.3) |
| Population treatment coverage via self-report | 1.3% (1.2-1.5) |

Full age ranges (secondary): detection ratio 0.184 (0.163-0.207). Diabetes detection ratio
0.567 (0.390-0.862) at 18-49, from only 60 measured cases — indicative only.

## Included in version control

- `01_protocol/`: scope note recording what is and is not novel relative to prior work
- `02_data_notes/`: variable map across both surveys
- `03_scripts/`: analysis pipeline
- `06_manuscript/`: Quarto manuscript source, STROBE checklist, cover letter, bibliography

## Generated locally, not versioned

`04_tables/`, `05_figures/`, `07_derived_data/`, `08_logs/`, and rendered `.docx`/`.html`.

## Reproduce

Requires Python 3 with `pandas`, `numpy`, `pyreadstat`, plus local copies of both datasets:

- `Research/Kenya - STEPS 2015/ken2015.csv`
- `Research/01_DHS_Data/KDHS_2022/` IR, MR, PR `.DTA` files

Data paths resolve automatically on Windows or WSL.

```bash
cd 03_scripts
python 01_steps_construct_analyze.py     # measured cascade, both awareness definitions
python 02_dhs_construct_analyze.py       # self-report, treatment, insurance
python 03_underdiagnosis_analysis.py     # detection ratios with bootstrap intervals
```

Then render:

```bash
cd 06_manuscript
quarto render manuscript.qmd --to docx
quarto render strobe_checklist.qmd --to docx
quarto render cover_letter.qmd --to docx
```

## Methodological commitments

1. **No transported quantities.** Every number is a within-survey estimate or a ratio of two
   independent within-survey estimates. There is no hybrid estimator and no transportability
   assumption.
2. **Age-comparable base is primary.** KDHS truncates women at 49 and men at 54; STEPS runs to 69.
   Comparisons on the 18-49 overlap are primary because the restriction materially changes the
   inputs (measured prevalence 24.2% → 19.8%).
3. **Both awareness definitions reported.** "Ever told" (`h2a`) and "told within 12 months" (`h2b`)
   give 23.7% and 16.0% awareness respectively on the full range. The published national cascade
   used the narrower definition; reporting both is what permits reconciliation rather than silent
   divergence.
4. **The detection ratio is never a time trend.** STEPS 2015 and KDHS 2022 are seven years apart;
   the ratio compares measurement systems, and the manuscript says so in Methods, Results and
   Discussion.
5. **Intervals propagate across surveys.** A stratified PSU bootstrap (B=2000, seed 20260918)
   resamples each survey independently, so cross-survey ratios carry honest intervals. ST12
   reported detection ratios as bare point estimates.
