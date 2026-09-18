# ST12 Protocol — Dual-survey estimation of underdiagnosis and population unmet need for hypertension and diabetes care in Kenya

**Version:** 1.0  
**Date:** 2026-07-15  
**Lead:** Nichodemus Werre Amollo  
**Co-author:** Japheth Ogol  

## Research question

Among Kenyan adults, what is the magnitude of the **measured** hypertension and diabetes care cascade (STEPS 2015), how does self-reported diagnosis (KDHS 2022) compare as a detection system, and can a transparent dual-survey estimator recover population-level treatment coverage consistent with measured data?

## Novelty

1. Full measured cascade to **control** (not only prevalence) with design-based CIs and equity (Erreygers by education).  
2. Explicit **dual-survey framework** linking STEPS (biology) to DHS (self-report + insurance) with a hybrid estimator validated against STEPS ground truth.  
3. Reinterprets ST03-style “treatment gap among the diagnosed” as the visible tip of a much larger population unmet-need iceberg.  
4. Does **not** re-publish Mohamed et al. prevalence alone; focuses on cascade leakage, sex gaps, and methods for cross-survey inference.

## Data

- Kenya STEPS 2015 microdata (`ken2015.dta`/`csv`), n≈4,500, ages 18–69.  
- KDHS 2022 IR + MR + PR insurance merge (`sh27`), ages 18–69 restricted (women max 49, men max 54).  

## Primary outcomes

### STEPS
- Measured HTN; measured DM  
- Among disease: aware, treated, controlled  
- Population treatment and control coverage  

### DHS
- Self-reported diagnosis prevalence  
- Treatment among diagnosed  
- Insurance among diagnosed  

### Dual-survey
- Detection ratio = p_dx_DHS / p_measured_STEPS  
- Hybrid treated-among-measured ≈ a_STEPS × t_DHS  
- Sensitivity grid on transportability assumptions  

## Analysis

Survey-weighted means with ultimate-cluster SEs (PSU × stratum). Erreygers CI with cluster bootstrap. No causal claims.

## Ethics

Secondary analysis of de-identified public survey data; DHS authorisation 220623. STEPS data obtained under WHO/MoH data access terms.

## Target journal

*BMC Public Health* or *International Journal for Equity in Health* (methods + cascade equity angle).
