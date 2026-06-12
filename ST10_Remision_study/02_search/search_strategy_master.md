# Master search strategy

**Review:** Lifestyle modification for T2DM remission and drug-free hypertension
normalisation in LMICs (PROSPERO CRD420261409919)
**Master interface:** MEDLINE via PubMed. All other databases are adapted from this.
**Date drafted:** 2026-06-12 · **Drafted by:** NWA · **PRESS reviewed by:** _pending_

---

## Conceptual structure (PICOS → search sets)

The strategy combines seven concept sets. Condition (T2DM **OR** hypertension) AND
intervention (lifestyle) AND outcome (remission **OR** normalisation) AND setting
(LMIC) AND design (RCT):

```
(#1 T2DM  OR  #2 Hypertension)
   AND #3 Lifestyle intervention
   AND (#4 T2DM remission  OR  #5 HTN normalisation)
   AND #6 LMIC setting
   AND #7 RCT design
```

A deliberately broad synonym set is used for the outcome concept (remission, reversal,
resolution, drug-free, medication discontinuation/withdrawal, normoglycaemia,
normalisation, metabolic recovery) to catch non-standard reporting.

---

## Set 1 — Type 2 diabetes
```
"Diabetes Mellitus, Type 2"[MeSH] OR "type 2 diabetes"[tiab] OR "type II diabetes"[tiab]
OR "non-insulin dependent diabetes"[tiab] OR "NIDDM"[tiab] OR "T2DM"[tiab]
```

## Set 2 — Hypertension
```
"Hypertension"[MeSH] OR "hypertension"[tiab] OR "high blood pressure"[tiab]
OR "elevated blood pressure"[tiab] OR "arterial hypertension"[tiab]
```

## Set 3 — Lifestyle intervention
```
"Life Style"[MeSH] OR "Diet"[MeSH] OR "Exercise"[MeSH] OR "lifestyle modification"[tiab]
OR "lifestyle intervention"[tiab] OR "dietary intervention"[tiab] OR "physical activity"[tiab]
OR "caloric restriction"[tiab] OR "weight loss"[tiab] OR "low-calorie diet"[tiab]
OR "low carbohydrate"[tiab] OR "DASH diet"[tiab] OR "Mediterranean diet"[tiab]
OR "health coaching"[tiab] OR "behaviour change"[tiab] OR "self-management"[tiab]
```

## Set 4 — T2DM remission
```
"remission"[tiab] OR "reversal"[tiab] OR "resolution"[tiab] OR "drug-free"[tiab]
OR "medication discontinuation"[tiab] OR "off medication"[tiab] OR "normalisation"[tiab]
OR "normalization"[tiab]
```

## Set 5 — Hypertension normalisation
```
"blood pressure normalisation"[tiab] OR "blood pressure normalization"[tiab]
OR "hypertension remission"[tiab] OR "antihypertensive discontinuation"[tiab]
OR "medication withdrawal"[tiab]
```

## Set 6 — LMIC setting
```
"developing countries"[MeSH] OR "low-income countries"[tiab] OR "middle-income countries"[tiab]
OR "LMIC"[tiab] OR "sub-Saharan Africa"[tiab] OR "South Asia"[tiab] OR "Southeast Asia"[tiab]
OR "Latin America"[tiab] OR "Africa"[MeSH] OR "Asia"[MeSH] OR "Kenya"[tiab] OR "Nigeria"[tiab]
OR "India"[tiab] OR "China"[tiab] OR "Brazil"[tiab] OR "Ethiopia"[tiab] OR "Tanzania"[tiab]
OR "Uganda"[tiab] OR "Pakistan"[tiab] OR "Bangladesh"[tiab] OR "Indonesia"[tiab]
```
> Adapt with the full World Bank LMIC country list + the Cochrane EPOC LMIC filter for
> MEDLINE/Embase. The list above is illustrative, not exhaustive — the executed string
> in `strings/01_MEDLINE_PubMed.txt` carries the complete country enumeration.

## Set 7 — RCT design
```
"Randomized Controlled Trial"[pt] OR "randomised controlled trial"[tiab]
OR "randomized controlled trial"[tiab] OR "RCT"[tiab] OR "cluster-randomised"[tiab]
OR "cluster-randomized"[tiab]
```

## Final combination
```
(#1 OR #2) AND #3 AND (#4 OR #5) AND #6 AND #7
```

---

## Adaptation notes per database family

| Database (interface) | Controlled vocab | Notes |
|----------------------|------------------|-------|
| MEDLINE (PubMed) | MeSH | Master. Use `[tiab]`, `[MeSH]`, `[pt]`. |
| Embase (Elsevier) | Emtree | Map MeSH→Emtree; `/exp`, `:ti,ab`; add Cochrane EPOC Embase LMIC filter. |
| CENTRAL (Cochrane) | — | Title-Abstract-Keyword; drop the RCT filter (CENTRAL is trials-only). |
| Web of Science Core | — | Topic (`TS=`) search; no controlled vocab; rely on free text. |
| CINAHL (EBSCOhost) | CINAHL headings | `MH` for headings, `TI`/`AB` for free text. |
| Global Health (CABI) | CABI terms | Strong LMIC indexing; keep country terms. |
| AJOL | none/native | Native regional indexing + country + free-text; no EPOC filter. |
| LILACS | DeCS | Use DeCS terms incl. Spanish/Portuguese; consider Spanish synonyms. |
| IndMED | none/native | Native indexing + India/free text; no EPOC filter. |
| WHO Global Index Medicus | — | Aggregates regional indexes; free-text + country terms. |
| ICTRP | — | Registry: condition + intervention terms only; no design/outcome filter. |
| ClinicalTrials.gov | — | Condition + intervention + "interventional"; LMIC by country facet. |
| Grey lit (ProQuest D&T, Grey Lit Report, repositories) | — | Simplified free-text; document each source separately. |

Each executed string is saved verbatim in `strings/` and logged in
`database_search_log.csv`.
