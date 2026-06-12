# ST10 — Lifestyle modification for T2DM remission and drug-free hypertension normalisation in LMICs

A systematic review and meta-analysis of randomised controlled trials.

**PROSPERO:** [CRD420261409919](https://www.crd.york.ac.uk/PROSPERO/view/CRD420261409919) (registered 29 May 2026)
**Status:** Protocol registered; infrastructure set up; **interim MEDLINE/PubMed execution complete** (single-reviewer feasibility run — see [`09_manuscript/results_report.md`](09_manuscript/results_report.md)). Full 13-database, dual-reviewer review pending team onboarding.

**Interim headline (not definitive):** Of 340 PubMed records, only **2 RCTs** met the drug-free-remission definition with a comparator — **both from China** (CMNT and CR-LCD). Pooled remission ~54% (very wide CI; Peto OR 5.4, p=0.008; I²=79%). **No eligible RCT from Africa, South Asia, Latin America, or MENA**, and **no LMIC RCT reported drug-free BP normalisation** — empirically confirming the evidence gap that motivates a primary trial.
**Lead / guarantor:** Nichodemus Werre Amollo (NWA), JOOUST — nwere@ejooust.ac.ke

---

## What this review asks

> In adults with established T2DM and/or hypertension in low- and middle-income
> countries, can structured lifestyle modification achieve **sustained, drug-free
> remission** (T2DM, primary) and **blood-pressure normalisation** (exploratory
> secondary), and how do LMIC trial populations differ from the high-income-country
> populations in which the remission evidence base was generated?

Full PICOS, outcomes, and analysis plan are in [`01_protocol/`](01_protocol/).

## Team & roles

See [`10_admin/team_roster.md`](10_admin/team_roster.md) and the RACI matrix in
[`10_admin/roles_RACI.md`](10_admin/roles_RACI.md). In brief: NWA (lead, guarantor,
analysis), Japheth Ogola (second reviewer, methods, supervision), Leah Herman
(clinical framing, review), Nellicent Achieng Opondo (data curation, analysis).

## How the folders map to the review workflow

| Folder | Stage | Contents |
|--------|-------|----------|
| `01_protocol/` | Registration | Protocol (docx/pdf/txt), PROSPERO record, amendments log |
| `02_search/` | Searching | Master strategy, per-database adapted strings, search log, PRESS peer review, dedup log |
| `03_screening/` | Screening | TIAB + full-text criteria, pilot logs, excluded-with-reasons list, inter-rater kappa script |
| `04_extraction/` | Data extraction | Extraction form template, codebook, author-contact log |
| `05_risk_of_bias/` | RoB | RoB 2 assessment template + signalling-question guidance |
| `06_analysis/` | Synthesis | R project: meta-analysis, subgroup/sensitivity, meta-regression, publication bias, PRISMA flow |
| `07_grade/` | Certainty | GRADE Summary-of-Findings template + domain guidance |
| `08_reporting/` | Reporting | PRISMA 2020 checklist, flow-diagram counts, reporting guidance |
| `10_admin/` | Coordination | Roster, RACI, timeline, decision log, team next-steps |

## The workflow, end to end

1. **Search** every database in `02_search/database_search_log.csv`; export to RIS/CSV.
2. **Deduplicate + screen** in [Rayyan](https://rayyan.ai); two independent reviewers
   (NWA, JO). Log decisions; compute Cohen's κ (`03_screening/inter_rater_kappa.R`).
   Target κ ≥ 0.75 before proceeding.
3. **Extract** eligible studies in duplicate using `04_extraction/data_extraction_form.csv`
   (codebook defines every field). Save the cleaned long file to
   `06_analysis/data_input/`.
4. **Assess RoB** with RoB 2 (`05_risk_of_bias/`).
5. **Synthesise** with the R pipeline in `06_analysis/R/` (run `run_meta_workflow.R`).
6. **GRADE** every primary/secondary outcome (`07_grade/`).
7. **Report** against PRISMA 2020 (`08_reporting/`); deposit code on OSF.

## Reproducibility & conventions

- Analysis is in **R (≥ 4.4)** using `meta`, `metafor`, `metasens`, `robvis`,
  `PRISMAstatement`/`PRISMA2020`, and `ggplot2`. See `06_analysis/R/00_setup.R`.
- Data templates are **CSV** (diff-friendly, open in Excel) with a written codebook.
- Build artefacts (docx/pdf/html), `results/`, and derived data are git-ignored per
  the repo `.gitignore`; **source files and data templates are tracked**.
- No primary data are generated. All analysis code will be deposited on OSF prior to
  publication (Data availability statement in the protocol).

## Citing

This study is part of the wider research compendium; see the repository root
`CITATION.cff`. The review itself should be cited via its PROSPERO record until the
protocol preprint/publication DOI is available.
