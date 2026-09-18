# ST13 scope: what is and is not novel

Written 18 September 2026, at study inception, to keep the contribution claim honest.

ST13 is the publishable successor to ST12. It drops the dual-survey hybrid estimator entirely
(see `../ST12_STEPS_DHS_DualSurvey_NCD/METHODS_REVIEW_hybrid_estimator.md` for why) and is built
only on quantities that need no new methods.

## The constraint: the measured hypertension cascade is already published

**Mohamed SF, et al. Prevalence, awareness, treatment and control of hypertension and their
determinants: results from a national survey in Kenya. *BMC Public Health* 2018;18:1219.
doi:10.1186/s12889-018-6052-y** — uses **the same Kenya STEPS 2015 data, the same 18-69 age range**,
and reports:

| Quantity | Mohamed 2018 |
|---|---|
| Age-standardised prevalence | 24.5% (22.6-26.6) |
| Aware among hypertensives | 15.6% (12.4-18.9) |
| Treated among aware | 26.9% (17.1-36.4) |
| Controlled among treated | 51.7% (33.5-69.9) |

ST13 therefore **cannot lead with the measured hypertension cascade**. Doing so would duplicate a
well-cited national analysis of the identical dataset.

## Definitional reconciliation (blocking for any cascade figure we report)

ST12's cascade diverges from Mohamed's, and the divergence is definitional, not an error:

| Awareness definition | STEPS variable | Awareness among measured | Treated among aware | n aware |
|---|---|---|---|---|
| Ever told high BP | `h2a` | 23.7% | 32.7% | 332 |
| Told within past 12 months | `h2b` | 14.8% | 44.7% | 216 |
| Mohamed 2018 (age-standardised) | — | 15.6% | 26.9% | — |

Mohamed evidently used the **12-month** definition; ST12 used **ever told** without stating or
defending the choice, while claiming "definitional fidelity" with Mohamed on the basis of the
prevalence estimate alone. Note also that `h2a` is asked only of respondents who ever had their
blood pressure measured (`h1`=1), so the skip pattern must be handled explicitly — those never
measured are correctly non-aware, not missing.

Any cascade number ST13 reports must state its definition, report both where they differ
materially, and reconcile explicitly with Mohamed 2018.

Relatedly: everyone on medication in these data had also been told, so *treated* is a subset of
*aware* empirically as well as by clinical logic. This is intrinsic to cascade structure and is why
no definitional adjustment rescues the ST12 hybrid.

## What remains genuinely unpublished

1. **The denominator problem.** Kenya's health-financing and UHC evidence base — including
   DHS-based treatment-gap and insurance analyses (our own ST03 among them) — conditions on
   *self-reported diagnosis*. Benchmarking KDHS 2022 self-report (4.4% hypertension, 0.8% diabetes)
   against STEPS-measured prevalence (24.2%, 2.0%) quantifies how much true disease that denominator
   excludes. No published study appears to make this comparison for Kenya.

2. **What it implies for interpreting financing results.** Treatment-gap and insurance statistics
   computed among the diagnosed describe a small, positively selected minority of people with
   disease. Stating that quantitatively changes how ST03-type findings should be read.

3. **The diabetes cascade**, which Mohamed 2018 does not cover (hypertension only). Requires a
   prior-work check before claiming novelty; STEPS diabetes numerators are small (n=117 measured)
   and intervals are wide.

4. **Equity patterning across cascade stages** via Erreygers concentration indices. Mohamed reports
   regression determinants, not equity indices. Note the ST12 result was null (indices near zero,
   intervals spanning zero), which is a legitimate but modest finding.

## Standing limitations that must frame any comparison

- STEPS 2015 and KDHS 2022 are **seven years apart**. Detection ratios are structural comparisons
  of measurement systems, never time trends, and must be labelled as such throughout.
- **Age ranges do not match**: STEPS covers 18-69; KDHS truncates women at 49 and men at 54.
  Comparisons should be computed on the overlapping 18-49 window as primary, with the full range as
  secondary. The ST12 diagnostic showed this shift is material, not cosmetic.
- Self-report and measurement are different constructs; question wording and skip patterns differ
  between the two instruments.
- Detection ratios in ST12 were reported as bare point estimates (0.18, 0.41). ST13 must attach
  intervals, propagated across the two independent surveys.
