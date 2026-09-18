# ST12 methods review: the dual-survey hybrid estimator

Recorded 17 September 2026. **Status: blocking. Do not submit or preprint ST12 until items 1-3 are resolved.**

The rest of ST12 (STEPS cascades, KDHS self-report and insurance, detection ratios) is sound and
well executed. The problem is confined to the hybrid estimator and the claim made for it — which
is unfortunately the paper's headline methods contribution and the centrepiece of the cover letter.

## 1. The validation is circular

The paper defines

> tau_hybrid = a_STEPS x t_DHS

and validates it by comparing tau_hybrid against STEPS-direct treatment among measured disease,
concluding that "agreement within one percentage point supports transportability".

In STEPS, awareness is defined as *told high BP* **OR** *on medication*
(`03_scripts/01_steps_construct_analyze.py:177`). Every treated person is therefore aware by
construction, so treated is a strict subset of aware and the two inputs are not independent.
That makes the comparison an algebraic identity rather than a test of the estimator:

| Quantity | Value |
|---|---|
| STEPS treated among measured | 0.077600 (7.76%) |
| STEPS aware among measured | 0.236979 (23.70%) |
| Implied STEPS treated-among-aware | 0.077600 / 0.236979 = **0.32745** |
| DHS treated among diagnosed | **0.339648** |
| tau_hybrid = 0.236979 x 0.339648 | 0.080489 (8.05%) |

The discrepancy between hybrid and direct is exactly

> a_STEPS x (t_DHS - t_STEPS|aware) = 0.236979 x (0.339648 - 0.327450) = 0.00289 = **0.29 pp**

So the "validation" restates that 34.0% is close to 32.7%. The agreement *is* the transportability
assumption measured, not independent evidence that the estimator recovers a true quantity. A
referee will reach this in one pass. The claim must be reframed from validation to a
**consistency check of a single assumption**, and the cover letter's "recovers STEPS-measured
treatment rates" claim withdrawn.

## 2. The hybrid has no confidence interval

`Table6_DualSurvey_Publication.csv` reports the hybrid as a bare point estimate (8.0, 19.6) while
every neighbouring column carries a 95% CI. STEPS-direct is 7.8 (5.7-9.8) — an interval 4.1 pp
wide. Nothing in that range could fail to "agree within one percentage point", so the comparison
is close to unfalsifiable as stated.

Fix: propagate variance across the two independent surveys, either by the delta method on the
product of two proportions or by a paired bootstrap (resample STEPS PSUs within strata and DHS
clusters within strata independently, recompute tau_hybrid, take percentile intervals). The
existing 200-replicate cluster bootstrap used for the Erreygers indices is a usable pattern.

## 3. Age ranges are not comparable in the transported quantity

STEPS covers adults 18-69. KDHS truncates women at 49 and men at 54. Both hypertension prevalence
and treatment-conditional-on-diagnosis rise steeply with age, so `t_DHS` is estimated on a
substantially younger population than `a_STEPS`. The manuscript acknowledges the truncation in
Methods ("noting that DHS still truncates...") but applies no correction in the one quantity being
transported.

Fix: age-standardise `t_DHS` to the STEPS age structure over the overlapping window, or restrict
both surveys to 18-49 for the hybrid and report the restricted estimate as primary.

## 4. The sensitivity grid does not probe the actual failure mode

`03_dual_survey_synthesis.py` multiplies `a` and `t` by fixed factors (0.8, 1.0, 1.2). Because the
conclusion of interest (large population untreated disease) is driven mainly by high measured
prevalence, it survives any such perturbation — which is why the reported range is a narrow
21-23%. This demonstrates robustness to input scaling, not robustness to the transportability
assumption failing. A meaningful sensitivity analysis varies the *gap* between `t_DHS` and
`t_STEPS|aware` directly.

## 5. Minor: weighting step under-specified

The strata-specific male subsample factor is described by citation to a prior study
(`amollo2026st03`) rather than specified in the manuscript. Since it affects every DHS estimate in
the paper, it should be stated explicitly, with the formula, for reproducibility.

## What survives unchanged

The detection ratios (HTN 0.18, DM 0.41) and the underdiagnosis indices are straightforward
cross-survey comparisons, correctly caveated in the text as not being time trends. The STEPS
cascade results — 24.2% measured hypertension prevalence, 2.7% controlled, 0.6% of all adults with
controlled hypertension — are the strongest material in the paper and do not depend on the hybrid
at all.

**Suggested route:** rebuild the paper around the measured cascade and the underdiagnosis contrast,
and demote the hybrid to a clearly labelled exploratory consistency check with an interval attached.
That paper is publishable and the claim is defensible. The current framing is not.
