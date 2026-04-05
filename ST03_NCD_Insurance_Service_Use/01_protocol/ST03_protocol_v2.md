# ST03 Analysis Protocol v2

## Working Title
Insurance coverage and treatment gaps among adults with diagnosed hypertension or diabetes before Kenya's Social Health Authority transition: a sex-stratified secondary analysis of the 2022 Kenya Demographic and Health Survey

## Version
2.0  
18 March 2026

## Rationale
Kenya's 2022 Demographic and Health Survey (KDHS 2022) is the last nationally representative household survey fielded before the replacement of the National Health Insurance Fund (NHIF) by the Social Health Authority (SHA) on 1 October 2024. It therefore provides the closest available national pre-SHA baseline for examining whether adults who already know they have hypertension or diabetes were insured and receiving treatment. Existing Kenya DHS studies established that insurance coverage was low and pro-rich in the general population, but they did not focus on the diagnosed non-communicable disease (NCD) population using KDHS 2022. Existing NCD financial-protection work in Kenya has been valuable but has relied on cohort data from selected counties rather than a national DHS sample.

## Core Questions
1. Among adults with self-reported hypertension and/or diabetes in KDHS 2022, what proportion had any health insurance and what proportion had NHIF coverage?
2. Among diagnosed adults, what proportion reported a treatment gap, defined as not currently taking medication for one or more diagnosed conditions?
3. Are insurance coverage and treatment gaps patterned by sex, wealth, residence, and province?
4. Is insurance coverage associated with lower treatment gaps after adjustment for demographic and socioeconomic factors?
5. How large is wealth-related inequality in insurance coverage and treatment gaps in this diagnosed population?

## Data Sources
Primary files:
- `KEIR8CFL.DTA` (`IR`): women's interview file
- `KEMR8CFL.DTA` (`MR`): men's interview file
- `KEPR8CFL.DTA` (`PR`): household member/person roster

Supporting references:
- KDHS 2022 final report and appendices
- DHS recode guidance and Guide to DHS Statistics
- DHS forum guidance on person-level merging and male-subsample pooling

## Why These Files
- The chronic disease diagnosis and medication variables are stored in `IR` and `MR`.
- The usable KDHS 2022 insurance variables are not `v481` and `mv481`; those fields are empty in 2022.
- Insurance must therefore be taken from `PR` using `sh27` (any insurance), `sh28a` (NHIF), `sh28b` (private), and `sh28c` (community-based coverage).

## Merge Rules
- `PR` to `IR`: `hv001 = v001`, `hv002 = v002`, `hvidx = v003`
- `PR` to `MR`: `hv001 = mv001`, `hv002 = mv002`, `hvidx = mv003`

The interview files remain the analytic bases. `PR` contributes insurance and selected household-member variables.

## Study Population
Women:
- Interviewed women aged 18 to 49 years
- Restricted to the long-questionnaire subsample because the women's chronic disease module is only observed there
- Self-reported diagnosis of hypertension and/or diabetes
- Non-missing insurance and treatment-gap information

Men:
- Interviewed men aged 18 to 54 years
- Self-reported diagnosis of hypertension and/or diabetes
- Non-missing insurance and treatment-gap information

## Exclusions
- Women younger than 18 years or older than 49 years
- Men younger than 18 years or older than 54 years
- Respondents without a self-reported diagnosis of hypertension or diabetes
- Records with missing insurance exposure after merging from `PR`
- Records with indeterminate medication status for the diagnosed condition set

## Variable Definitions

### Exposure
- `insured_any`: `PR sh27`
- `insured_nhif`: `PR sh28a`, recoded to 0 when `insured_any = 0`

### Outcome
Treatment gap:
- Diagnosed adults who reported not taking medication for at least one diagnosed condition
- Women:
  - hypertension diagnosis `chd02`, medication `chd05`
  - diabetes diagnosis `chd07`, medication `chd10`
- Men:
  - hypertension diagnosis `mchd02`, medication `mchd05`
  - diabetes diagnosis `mchd07`, medication `mchd10`

### Diagnosis Profile
- Hypertension only
- Diabetes only
- Both hypertension and diabetes

### Covariates
- Age in years
- Wealth quintile
- Education
- Residence
- Employment
- Province
- Sex in pooled secondary models

### Exploratory Women-Only Access Barriers
- Permission to seek care: `v467b`
- Money for treatment: `v467c`
- Distance to facility: `v467d`
- Recoded as `big problem` vs `not a big problem/no problem`

## Weighting and Survey Design
Primary sex-specific analyses use the interview-file design variables:
- Women: `v005`, `v021`, `v022`
- Men: `mv005`, `mv021`, `mv022`

Pooled descriptive and pooled secondary regression results use a male-subsample correction informed by DHS guidance because men were interviewed in a subsample of households in KDHS 2022. The adjustment factor is derived within strata from the `PR` file using eligible men and household weights.

Survey design specification in R:
- `ids = ~psu`
- `strata = ~strata`
- weights = interview weight divided by `1e6`
- `nest = TRUE`

## Statistical Analysis

### Descriptive
- Weighted sample characteristics
- Weighted treatment-cascade summaries by sex and diagnosis profile
- Weighted comparisons of women-only access barriers by insurance status

### Regression
Primary models:
- Survey-weighted generalized linear models with quasi-Poisson family and log link
- Report adjusted prevalence ratios (APR) and 95% confidence intervals
- Separate women's and men's models

Secondary sensitivity:
- Survey-weighted logistic models reported as odds-ratio sensitivity analyses only

### Inequality
- Concentration indices for any insurance, NHIF coverage, and treatment gap
- Bootstrap uncertainty intervals

## Main Methodological Constraints
- KDHS 2022 does not support an age range beyond the interviewed adult populations for this module.
- `BR` is not used; the study is based on self-reported diagnosis and self-reported current medication.
- The women's chronic disease module is observed only in the long-questionnaire subsample.
- Associations are cross-sectional and not causal estimates of insurance impact.

## Manuscript Positioning
This paper should be framed as a pre-SHA national baseline for structural exclusion in NCD care. The central question is not whether insurance caused treatment uptake, but whether diagnosed adults were insured and effectively covered at the end of the NHIF era.

