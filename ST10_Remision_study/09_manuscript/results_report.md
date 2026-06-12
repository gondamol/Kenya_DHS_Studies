# Lifestyle modification for T2DM remission in LMICs — interim findings from a MEDLINE/PubMed execution

**Registered review:** PROSPERO CRD420261409919
**Document type:** Interim findings report (reproducible first-pass execution)
**Date:** 2026-06-12 · **Analyst:** NWA

> **Scope and status — read first.** This report documents a *real, reproducible*
> execution of the registered search–screen–extract–synthesise pipeline, run on
> **MEDLINE/PubMed only** by a **single reviewer**, using a Boolean-reduced query (the
> live search interface caps Boolean operators). It is **not** the completed systematic
> review. The registered protocol specifies **13 databases**, **dual independent**
> screening and extraction, a librarian-led PRESS review, and registry/grey-literature
> searching — all still to be done by the team. Treat the pooled estimates here as a
> **feasibility signal and a scoping of the evidence base**, not as definitive. Every
> number below traces to a named, retrievable study; no data are simulated.
> *Bibliographic source: PubMed. DOIs are linked at each study's first mention.*

---

## 1. Methods as executed (and deviations from protocol)

| Protocol specifies | Executed here | Deviation |
|--------------------|---------------|-----------|
| 13 databases | MEDLINE via PubMed only | Other 12 pending (team) |
| Full Boolean strategy (66 operators) | Reduced clinical-concept query (≤20 operators) + LMIC applied at screening | Tool operator cap; full strategy preserved in `02_search/strings/01_MEDLINE_PubMed.txt` |
| Dual independent screening, κ≥0.75 | Single reviewer (NWA) | Dual screening pending |
| Dual extraction | Single extractor; data from full text (PMC) or structured abstract | Dual extraction pending |
| RoB 2 dual assessment | Single provisional RoB 2 | Dual assessment pending |
| Final search within 30 days of registration | Snapshot 2026-06-12 | Interim |

**Executed query (PubMed):**
`("type 2 diabetes"[tiab] OR "T2DM"[tiab]) AND ("remission"[tiab] OR "reversal"[tiab] OR "drug-free"[tiab] OR "drug free"[tiab]) AND ("lifestyle"[tiab] OR "diet"[tiab] OR "physical activity"[tiab] OR "weight loss"[tiab] OR "caloric restriction"[tiab]) AND ("randomized"[tiab] OR "randomised"[tiab] OR "Randomized Controlled Trial"[pt])`
→ **340 records** (2026-06-12). LMIC setting applied at screening (more sensitive than a
geographic search filter, given inconsistent geographic indexing). Two supplementary
geography-restricted queries (East/South Asia + Latin America; and Pakistan/Egypt/
Mexico/Iran/Nigeria/South-Africa/etc.) were run to ensure no LMIC remission RCT was
missed; the latter returned **0** records.

## 2. Study selection (PRISMA 2020 — as executed)

```
Records identified (MEDLINE/PubMed)............ 340
Other sources (Digital-Twin primary RCT)....... 1
Duplicates removed............................. 1
Records screened (title/abstract).............. 340
  Excluded at TIAB (clearly HIC/non-RCT/non-remission/off-topic). 320
Full-text/abstract assessed (LMIC candidates).. 20
  Excluded with reasons........................ 15   (see 03_screening/excluded_fulltext_reasons.csv)
Studies included............................... 3   (4 reports)
  → contributing drug-free remission counts to meta-analysis ... 2
```

**Excluded at full text (key reasons):** high-income setting — DIADEM-I/Qatar
([10.1016/S2213-8587(20)30117-0](https://doi.org/10.1016/S2213-8587(20)30117-0) area;
PMID 32445735) and a New Zealand DiRECT pilot; non-lifestyle interventions — bariatric
surgery (×2), insulin therapy, pharmacotherapy (DiaRem-1); non-RCT designs —
retrospective cohort, prospective cohort, N-of-1; wrong population — prediabetes; and
study protocols without results (China CMNT-NAFLD, Lebanon, USA HEAL).

## 3. Included studies

| Study | Country (income) | Design | n | Intervention | Comparator | Remission definition |
|-------|------------------|--------|---|--------------|------------|----------------------|
| **CMNT — Yan et al. 2023** ([10.1210/clinem/dgac661](https://doi.org/10.1210/clinem/dgac661)) | China (upper-middle) | RCT, parallel 1:1 | 72 | Chinese Medical Nutrition Therapy (intermittent calorie restriction; 6×15-day cycles) | Usual care | HbA1c<48 mmol/mol ≥3 mo off **all** antidiabetic drugs (2021 consensus) |
| **CR-LCD — 2026** ([10.1155/jdr/7230214](https://doi.org/10.1155/jdr/7230214)) | China (upper-middle) | RCT, parallel 1:1 | 68 | Calorie-restricted low-carbohydrate diet (meds stopped day 1) | Energy-balanced conventional diabetes diet | HbA1c<6.5% off glucose-lowering meds |
| CMNT 3-yr follow-up — 2026 ([10.3389/fendo.2025.1733840](https://doi.org/10.3389/fendo.2025.1733840)) | China | (follow-up of CMNT) | 72 | — | — | Maintained remission at 3 y |
| Digital-Twin — Joshi et al. 2023 ([10.1016/j.eprac.2023.08.016](https://doi.org/10.1016/j.eprac.2023.08.016)) | India (lower-middle) | RCT, parallel 2:1 | 319 | Whole-body Digital Twin (AI-guided nutrition/activity/sleep + CGM + remote coaching) | Standard care | Remission (secondary endpoint) |

According to PubMed, all four reports are indexed as randomised controlled trials.

## 4. Primary outcome — drug-free T2DM remission

Real per-arm data:

| Study | Remission, intervention | Remission, control |
|-------|------------------------|--------------------|
| CMNT (Yan 2023) | 17/36 (47.2%) | 1/36 (2.8%) |
| CR-LCD (2026) | 20/32 (62.5%) | 12/34 (35.3%) |

**Pooled single-arm remission proportion (random-effects, REML):**
**54.5%** (95% CI 2.3%–98.4%; Freeman-Tukey 54.6%). The interval spans almost the whole
range — expected with k=2.

**Comparative effect (intervention vs control):**
- Random-effects **RR ≈ 4.47**; the protocol's REML + Hartung-Knapp-Sidik-Jonkman
  interval is **uninformative** at k=2 (t-distribution, df=1 → CI 0–6×10⁶). This is the
  known small-k HKSJ pathology, not a coding error.
- Pre-specified robustness estimators (no HKSJ t-inflation):
  - **Peto OR 5.39 (95% CI 1.56–18.59), p=0.008**
  - **Binomial-normal GLMM OR 7.40 (95% CI 1.79–30.52), p=0.006**

**Heterogeneity: I² = 79%** (Cochran Q p=0.029). The driver is the very different
control-arm remission (CMNT usual-care 2.8% vs CR-LCD active-diet comparator 35.3%, the
latter because all glucose-lowering medication was withdrawn on day 1 in early-stage
disease). **Per the protocol's own rule (I²>50% with <5 studies), we do not present a
single pooled estimate as the headline finding** — the direction is consistent and
favours lifestyle modification, but the magnitude is uncertain.

Forest plots: `06_analysis/results/fig_forest_remission_proportion.png`,
`fig_forest_remission_RR.png`.

## 5. Secondary and narrative findings

- **Durability (CMNT 3-year follow-up):** 75% of the CMNT group maintained remission at
  3 years versus 0% of controls; maintenance correlated with the extent of insulin/
  insulinotropic-agent withdrawal during the intervention.
- **Digital-Twin (India):** reported **72.7% T2D remission at 1 year** in the DT arm and
  94% discontinuation of all glucose-lowering medication; in the hypertension secondary
  analysis ([10.1016/j.jacadv.2024.101172](https://doi.org/10.1016/j.jacadv.2024.101172)),
  **HTN remission 50% vs 0%** and 68.2% remained off antihypertensives. **Not pooled**
  because (a) the control-arm remission rate is not reported in the available abstract/
  full text, and (b) the digital-delivery model raises a genuine eligibility question
  under the protocol (digital platforms require a structured supervised component) — a
  decision flagged for team adjudication.
- **Medication reduction (Qingdao LCD vs LFD):** greater reduction in medication-effect
  score and HbA1c with a low-carbohydrate diet, but **no drug-free remission endpoint** —
  hence excluded from the primary analysis, retained here as narrative.

## 6. Hypertension strand (exploratory)

**No eligible LMIC RCT reported the protocol-defined drug-free BP-normalisation endpoint.**
The only LMIC signal is the India Digital-Twin secondary analysis (HTN remission 50% vs
0%). This confirms, empirically, the evidence gap the protocol anticipated; the strand is
reported narratively.

## 7. Risk of bias (provisional, RoB 2)

Both pooled trials: **some concerns** overall — open-label dietary interventions
(performance bias) with otherwise adequate randomisation, objective lab-measured HbA1c,
and registered, pre-specified consensus-based remission outcomes. Figures:
`06_analysis/results/fig_rob2_traffic_light.png`, `fig_rob2_summary_bar.png`. Formal dual
assessment pending.

## 8. Certainty of evidence (GRADE)

Primary remission outcome: **very low certainty** — downgraded for risk of bias (open
label), inconsistency (I²=79%), indirectness (both trials China-only; not generalisable to
Africa/South Asia/LATAM/MENA), and imprecision (k=2, very wide intervals). See
`07_grade/grade_sof_template.csv`.

## 9. Interpretation

1. **The LMIC drug-free-remission RCT evidence base is extremely thin and geographically
   narrow.** Only two trials meeting the consensus remission definition with a comparator
   were identifiable in MEDLINE, **both from China**. A targeted multi-country geographic
   search returned **no** qualifying trials from Africa, South Asia (excluding the digital
   India trial), Latin America, or MENA (Qatar is high-income).
2. **Direction is encouraging but uncertain:** structured dietary lifestyle modification
   was associated with substantially higher drug-free remission than control in both
   trials (Peto OR 5.4, GLMM OR 7.4), with 3-year durability in one — but I²=79% and k=2
   preclude a robust pooled magnitude.
3. **The sub-Saharan Africa gap is total** at the RCT level — exactly the rationale for a
   contextually adapted primary trial, and the strongest single finding for a funding case.
4. The hypertension-normalisation question is **unanswered** by LMIC RCTs.

## 10. Limitations

Single database, single reviewer, operator-reduced query, and a snapshot date — so this
under-counts the true evidence base relative to the registered 13-database, dual-reviewer
protocol (e.g., non-MEDLINE-indexed Chinese/African journals, registries, and grey
literature are not yet searched). The pooled estimates rest on two small open-label
Chinese RCTs and should not inform practice. These are the reasons the full protocol exists.

## 11. Reproducibility

Every step is scripted and re-runnable:

```bash
# Search records + PMIDs:     02_search/pubmed_all_pmids.txt, database_search_log.csv
# Screening helper:           python 03_screening/screen_pubmed.py <metadata.json>
# Extraction helper:          python 04_extraction/extract_fulltext.py <fulltext.json>
# Analysis-ready data:        06_analysis/data_input/meta_input.csv
# Meta-analysis (all of it):  Rscript 06_analysis/R/run_meta_workflow.R
# Decisions/PRISMA:           03_screening/{included_studies,excluded_fulltext_reasons}.csv
#                             08_reporting/prisma_flow_counts.csv
```

*Bibliographic data retrieved from PubMed (NLM). DOIs linked above.*
