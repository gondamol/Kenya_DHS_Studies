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

---

# Empirical diagnostic, 18 September 2026

Run via `03_scripts/05_transportability_diagnostic.py` (stratified PSU bootstrap, B=2000,
seed 20260918; the two surveys resampled independently). Outputs:
`04_tables/Table8_Transportability_Diagnostic.csv`, `08_logs/st12_transportability_diagnostic.txt`.

Because `treated|measured = a_S x t_S` in STEPS by construction, the hybrid's departure from the
direct estimate is exactly `a_S x (t_D - t_S)`. So the assumption was tested where it actually
lives: on `t_D - t_S`.

| Subset | a_S | t_S (STEPS) | t_D (DHS) | gap t_D - t_S | tau_hybrid | direct |
|---|---|---|---|---|---|---|
| Full | 0.2370 | 0.3275 | 0.3396 | **+0.0122 (-0.0591, 0.0827)** | 0.0805 (0.0657, 0.0973) | 0.0776 (0.0581, 0.0992) |
| Age-comparable 18-49 | 0.1806 | 0.2950 | 0.3198 | +0.0248 (-0.0732, 0.1358) | 0.0577 (0.0435, 0.0738) | 0.0533 (0.0306, 0.0810) |
| 18-49, women | 0.2809 | 0.3454 | 0.3206 | -0.0247 (-0.1860, 0.1333) | 0.0901 | 0.0970 |
| 18-49, men | 0.0980 | 0.1764 | 0.3184 | **+0.1420 (-0.0063, 0.2843)** | 0.0312 | 0.0173 |

Unweighted denominators: STEPS measured 1,229 / aware **332**; 18-49 measured 740 / aware **157**;
18-49 men measured 327 / aware **37**. DHS diagnosed 1,770 (18-49: 1,678).

## What this establishes

**1. The validation was underpowered, not confirmatory.** The gap is +1.22 pp with a 95% interval
of (-5.9, +8.3) pp on a quantity of ~0.33 — roughly +/-21% in relative terms. The assumption is
"not rejected" only because the comparison cannot detect a violation of less than about a fifth of
the transported quantity. `tau - direct` is 0.0029 (-0.0140, 0.0192), again spanning +/-22% of a
7.8% estimate. "Agreement within one percentage point" is therefore true and uninformative: no
plausible violation would have produced disagreement.

**2. The age truncation is a real confound, not a caveat.** Restricting both surveys to 18-49
moves awareness from 0.2370 to 0.1806 and the direct estimate from 0.0776 to 0.0533. The published
headline figures pair a STEPS input spanning 18-69 with a DHS input truncated at 49/54, so they are
not age-comparable. Any retained hybrid must be computed on the overlapping window.

**3. Transportability plausibly fails in men.** Among men 18-49, DHS treatment-among-diagnosed
(0.3184) is roughly double STEPS treatment-among-aware (0.1764); the gap is +14.2 pp with an
interval that barely includes the null (-0.0063, 0.2843) and a ratio of 1.81 (0.98, 6.62). The
pooled estimate conceals this because women dominate the aware subgroup. This is the most
substantively interesting result in the diagnostic and it points the opposite way from the
manuscript's claim.

**4. The binding constraint is STEPS awareness, not DHS.** `t_S` is estimated on 332 aware adults
overall and **37 aware men** aged 18-49. Precision on the transported quantity is therefore
structurally poor and cannot be improved with these data. This is why the estimator cannot be
validated to a useful tolerance in Kenya.

## Consequence for the paper

The within-Kenya validation framing is not recoverable. `t_S` is directly estimable from STEPS, so
wherever the hybrid can be checked it is redundant, and wherever it would be needed (a country with
DHS but no STEPS) it cannot be checked. That circularity is intrinsic to the design, not a
presentational flaw.

The defensible reframing is **transfer**: calibrate the estimator where both surveys exist and
apply it where only DHS does. Under that framing the results above become the contribution rather
than the problem — a quantification of how well a DHS-only cascade proxy can be expected to
perform, an explicit demonstration that the calibration tolerance is about +/-20% relative, and
evidence that the assumption is sex-dependent and so should not be applied to pooled populations.
That paper is honest, novel, and publishable. The current one is not.

Note also that no published study appears to combine STEPS and DHS within a single country to build
a hybrid cascade estimator. Multi-country cascade work (e.g. Geldsetzer et al., Lancet 2019) pools
surveys that each carry **measured** blood pressure and at least two cascade steps; KDHS 2022 carries
no measured BP, which is precisely why Kenya is absent from that literature and why this design was
attempted. The formal home for the method is two-sample estimation (Angrist & Krueger 1992,
TSIV/TS2SLS), whose identification condition — that the transported behaviour be stable across
samples — is exactly what the diagnostic above puts a bound on.
