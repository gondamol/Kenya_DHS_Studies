# ST11 Protocol — Diagnosed but untreated: the national depression and anxiety treatment gap in Kenya (KDHS 2022)

## Background and rationale
Common mental disorders cluster with non-communicable diseases (NCDs) in Kenya. KDHS 2022 is the first
nationally representative Kenyan survey with a mental-health module. Existing KDHS-2022 analyses
(Omondi 2025; Melkam 2025; an *Acta Psychologica* concentration-index paper; Okyere 2024) describe the
prevalence and correlates of *diagnosed* depression/anxiety and its wealth inequality. None has analysed the
*treatment* item — i.e., the diagnosed-but-untreated gap and its socioeconomic/insurance patterning. This study
fills that gap and provides national preliminary evidence for a larger study on integrated psychosocial NCD care.

## Objectives / questions
1. What proportion of adults reporting a depression or anxiety diagnosis are not receiving treatment?
2. How does the untreated proportion vary by wealth, insurance, education, residence, sex, and age?
3. Does physical-NCD comorbidity modify the treatment gap?
4. How concentrated is the treatment gap across the wealth distribution?

## Design
Cross-sectional secondary analysis of KDHS 2022.

## Data and population
- KDHS 2022 individual (women 15–49) and men's (15–54) recodes, pooled.
- Health insurance merged from the household member (PR) file (`sh27`) on cluster/household/line.
- Treatment-gap denominator: adults reporting a diagnosis of depression or anxiety with non-missing treatment status.

## Variables
- **Outcome:** untreated = diagnosed depression (`chd17`/`mchd17`) or anxiety (`chd18`/`mchd18`) AND not receiving treatment (`chd19`/`mchd19`).
- **Exposures/covariates:** wealth quintile (`v190`/`mv190`), insurance (`sh27`), education (`v149`/`mv149`),
  residence (`v025`/`mv025`), region (`v024`/`mv024`), sex, age group; NCD comorbidity count
  (hypertension `chd02`, diabetes `chd07`, heart `chd11`, lung `chd13`, arthritis `chd20`).

## Analysis
Survey-weighted (weights, PSU, strata). Weighted prevalence with 95% CIs; quasi-Poisson (log-link) model for
being untreated; wealth-ranked concentration index (standard + Erreygers, cluster-bootstrap CI). R + `survey`.

## Limitations
Cross-sectional; self-reported diagnosis and treatment (access-dependent, so true unmet need is larger);
small diagnosed subsample limits precision in some cells; insurance merge depends on PR linkage.

## Outputs
Peer-reviewed paper; policy brief (MoH Division of NCDs / Division of Mental Health); preliminary evidence for
a larger Wellcome / NIHR Global Health Research / GACD application.

## Reporting
STROBE (see `06_manuscript/strobe_checklist.qmd`).
