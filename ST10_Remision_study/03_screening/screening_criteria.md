# Screening criteria & decision rules

Operationalises the protocol eligibility criteria into a checklist two reviewers (NWA,
JO) apply independently. Use the **same** rules at title/abstract and full-text stages;
at TIAB, when in doubt **include** (screen liberally — exclude only on clear grounds).

## Stage 1 — Title / abstract (liberal)
Exclude only if the record is **clearly** ineligible on ≥1 criterion below. Anything
ambiguous goes to full text.

## Stage 2 — Full text (definitive)
Apply every criterion. Record the **single primary reason** for exclusion (the first
that applies, in the order below) in `excluded_fulltext_reasons.csv`.

---

## INCLUDE only if ALL are met

| # | Domain | Include if… |
|---|--------|-------------|
| P | Population | Adults ≥18 y with **established** T2DM, hypertension, or both (diagnosis per protocol thresholds). |
| I | Intervention | Structured lifestyle modification (diet, physical activity, and/or multicomponent behavioural support), **≥12 weeks** duration. Combined lifestyle+drug OK **if** lifestyle component explicit and a comparator arm lacks it. |
| C | Comparator | Usual care, less-intensive lifestyle, waitlist, or pharmacotherapy-alone. (A comparator must exist.) |
| O | Outcome | Reports T2DM remission (HbA1c <48 mmol/mol ≥3 mo off all glucose-lowering drugs) **or** drug-free BP normalisation (<130/80 ≥6 mo off antihypertensives) **or** a secondary outcome listed in the protocol. |
| S | Study design | RCT — parallel, cluster, or cross-over (cross-over: pre-crossover data only). |
| Setting | LMIC | Conducted in a World Bank low-/middle-income economy at time of recruitment. Multi-country: LMIC participants ≥80% of sample (≥50% examined in sensitivity analysis). |

## EXCLUDE if ANY apply (use as the coded exclusion reason)

| Code | Exclusion reason |
|------|------------------|
| `E1_design` | Not an RCT (observational, quasi-randomised, single-arm, review, protocol-only). |
| `E2_population` | T1DM / gestational diabetes / pre-diabetes / pre-hypertension / secondary or resistant hypertension; paediatric (<18 y). |
| `E3_intervention` | No structured lifestyle component; pharmacological-only; bariatric/metabolic surgery; <12 weeks; digital-only without supervised component; single counselling session. |
| `E4_prevention` | Prevention in people without established disease (e.g. Da Qing, DPP, IDPP-1 — IGT, not T2DM). |
| `E5_setting` | Conducted in a high-income country; or multi-country trial with LMIC participants <80% (and <50% in sensitivity). |
| `E6_comparator` | No comparator arm. |
| `E7_outcome` | None of the eligible outcomes reported. |
| `E8_publication_type` | Conference abstract only (no full publication); duplicate of an included record. |
| `E9_unobtainable` | Full text or translation unobtainable after reasonable effort. |

## Tie-break / adjudication
Disagreements resolved by discussion; if unresolved, a third reviewer adjudicates.
Log adjudications in `screening_pilot_log.md` (pilot) or the Rayyan decision export.

## Pilot
Pilot Stage 1 on a random **50 records** and Stage 2 on **all pilot full texts** before
full screening. Compute Cohen's κ (`inter_rater_kappa.R`); proceed only when **κ ≥ 0.75**.
If below, clarify criteria, re-pilot a fresh sample.
