# ST03 DHS Methods Audit

## Scope
This audit translates the DHS rules in dhs_methods_reference_2026-03-18.md into concrete implications for ST03.

## Current strengths
- The study already identified the correct 2022 insurance variables in PR (sh27, sh28a).
- The study already identified the key chronic disease variables in IR and MR.
- The study already recognised that BR is not a defensible adult blood-pressure file for this paper.
- The current outputs already establish a meaningful policy signal: insurance coverage among diagnosed adults is incomplete and the treatment gap is large.

## Problems that must be corrected before manuscript submission

### 1. Wrong analytic base in the first version
The first build treated PR as the main analytic file and then pulled interview variables into it. That is backwards for ST03. The outcomes and most covariates are observed in IR and MR, so those files must be the analytic bases, with PR contributing insurance and other household-member variables.

### 2. Wrong weights in the first version
The first version used hv005, hv021, and hv022 throughout. That is not defensible once the analysis moves to adult interview outcomes.
- Women should use 005, 021, 022.
- Men should use mv005, mv021, mv022.
- Any pooled women-plus-men analysis requires explicit adjustment for the male subsample before appending.

### 3. Unsupported age range
The protocol still states ages 18-69. KDHS 2022 does not support that for this module.
- Women: 18-49
- Men: 18-54
The manuscript must correct this everywhere, including title, methods, and sample-flow language.

### 4. Incorrect biomarker claim
The protocol still says BR contains measured blood pressure for this analysis. That is not supported by local file validation. All biomarker language should be removed from ST03 unless a new valid adult biomarker source is identified.

### 5. Women's chronic disease data are a long-questionnaire subsample
The manuscript currently does not explain why women with chd* data are fewer than all interviewed women. Local validation shows that chd02 is observed only among sshort = long questionnaire women. This is survey-design missingness, not conventional item nonresponse, and needs to be described explicitly.

### 6. Odds ratios are weak as the primary effect measure
The treatment-gap outcome is common, at about two-thirds of the analytic sample. Odds ratios are therefore harder to interpret and more vulnerable to reviewer criticism. The revised primary models should use adjusted prevalence ratios, with odds ratios retained only as sensitivity analyses if desired.

### 7. Over-parameterisation risk in the men's model
The first regression used many employment categories plus province terms in a relatively small men's analytic sample. That is not fatal, but it is vulnerable. The revised models should collapse sparse employment categories and add diagnosis profile only if sample support is adequate.

### 8. Pooled estimates need care
A pooled combined-sex estimate is still useful for a national baseline, but it must be secondary unless men's weights are corrected for the male subsample. The revised analysis should either:
- keep primary inference sex-specific and pooled results descriptive, or
- implement DHS-style male reweighting before pooled inference.

## Recommended revised analytic structure

### Primary descriptive analyses
- Women and men analysed separately with the correct interview weights.
- Pooled descriptive estimates only after correcting men's weights for the male subsample.
- Explicit sample-flow table showing: interview file size, long-questionnaire restriction for women, diagnosed sample, and final analytic sample.

### Primary models
- Outcome: treatment gap among adults with self-reported diagnosed hypertension and/or diabetes.
- Model family: survey-weighted Poisson with log link or quasi-Poisson to estimate adjusted prevalence ratios.
- Separate women's and men's models.
- Core covariates: age, wealth, education, residence, employment (collapsed), province, diagnosis profile.

### Secondary analyses
- NHIF-specific descriptive contrasts against uninsured adults.
- Women's care-access barriers (467b, 467c, 467d) by insurance status among diagnosed women.
- Pooled secondary model with sex term only after corrected male reweighting.
- Wealth-related concentration indices for insurance coverage, NHIF coverage, and treatment gap.

## Protocol changes required immediately
- Update title to remove any implication of ages 55-69 or biomarker measurement.
- Replace BR language with a self-reported diagnosis and treatment framing.
- Replace the old weight statement with sex-specific interview-weight language.
- Replace the old age statement with observable KDHS eligibility.
- Add an explicit note that women's chronic disease items were collected only in the long questionnaire.
- Add a STROBE statement in the methods draft.

## Manuscript framing guidance
The most defensible policy framing is:
- This paper is the pre-SHA national baseline for diagnosed NCD adults.
- It asks two linked questions: who is insured, and among those already diagnosed, who still remains untreated?
- It does not claim to estimate true hypertension or diabetes prevalence.
- It does not claim causal protection from insurance; it estimates weighted associations and inequality patterns in the final nationally representative survey before the SHA transition.
