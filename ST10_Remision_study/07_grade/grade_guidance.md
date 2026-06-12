# GRADE — certainty of evidence

Assess certainty for **all primary and key secondary outcomes**. Each body of evidence
starts at **high** (RCTs) and may be rated down across five domains. Two assessors;
consensus. Produce Summary-of-Findings (SoF) tables in **GRADEpro GDT**; record the
working judgements in `grade_sof_template.csv`.

| Domain | Rate down when… |
|--------|------------------|
| **Risk of bias** | Pooled estimate dominated by studies at high RoB 2 (see `../05_risk_of_bias/`). |
| **Inconsistency** | Unexplained heterogeneity (I² high, non-overlapping CIs, wide prediction interval). |
| **Indirectness** | *Review-specific (see below).* |
| **Imprecision** | Few events / wide CI crossing a decision threshold; OIS not met. |
| **Publication bias** | Asymmetry / small-study effects (only assessable with ≥10 studies). |

Certainty levels: **high · moderate · low · very low**.

## Review-specific indirectness triggers (from the protocol)
Consider downgrading for indirectness where:
- remission is ascertained by **fasting plasma glucose** rather than HbA1c, or HbA1c is
  not standardised/quality-assured;
- dietary/behavioural interventions are **not culturally adapted** to the target
  population;
- the intervention's **contact intensity or delivery model exceeds** what is feasible in
  typical LMIC primary-care capacity (limits real-world applicability).

## Process
1. Lock the pooled estimates (`../06_analysis/results/`).
2. For each outcome, fill one row of `grade_sof_template.csv` with the five domain
   judgements + rationale.
3. Build the SoF table in GRADEpro GDT; export for the manuscript.
4. Two-reviewer consensus; document disagreements in `../10_admin/decision_log.md`.
