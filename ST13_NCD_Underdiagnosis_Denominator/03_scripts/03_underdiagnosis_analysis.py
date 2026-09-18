"""
ST13 — the self-report denominator: how much measured NCD burden it excludes.

This is the analysis for the denominator-led paper that succeeds ST12. It
contains no hybrid estimator and no transported quantity: every number here is
either a within-survey estimate or a ratio of two independent within-survey
estimates, reported with an interval.

Quantities
----------
  p_meas   measured prevalence                      (STEPS 2015, wstep2/wstep3)
  p_dx     self-reported diagnosis prevalence       (KDHS 2022)
  DR       detection ratio = p_dx / p_meas
  UDI      underdiagnosis index = max(0, 1 - DR)
  a_ever   awareness among measured, ever told      (STEPS)
  a_12m    awareness among measured, told <=12 months (STEPS; comparable with
           Mohamed et al., BMC Public Health 2018)
  t_dx     treatment among self-reported diagnosed  (KDHS)
  ins_dx   insurance among self-reported diagnosed  (KDHS)

DR compares two measurement systems seven years apart and is never a time
trend. It is reported primarily on the 18-49 window, where both surveys
overlap, because KDHS truncates women at 49 and men at 54 while STEPS runs to
69 and both prevalence and awareness rise steeply with age.

Inference is by stratified PSU bootstrap (PSUs resampled with replacement
within strata), with the two surveys resampled independently, so ratios across
surveys carry honest intervals.

Outputs:
  04_tables/Table1_Measured_vs_SelfReport.csv
  04_tables/Table2_Cascade_Both_Definitions.csv
  04_tables/Table3_Diagnosed_Denominator.csv
  08_logs/st13_underdiagnosis.txt
"""
from __future__ import annotations

import sys
from pathlib import Path

import numpy as np
import pandas as pd

STUDY_ROOT = Path(__file__).resolve().parents[1]
DERIVED = STUDY_ROOT / "07_derived_data"
OUT_TABLES = STUDY_ROOT / "04_tables"
OUT_LOGS = STUDY_ROOT / "08_logs"
for p in (OUT_TABLES, OUT_LOGS):
    p.mkdir(parents=True, exist_ok=True)

N_BOOT = 2000
SEED = 20260918
ALPHA = 0.05


# --------------------------------------------------------------------------
# design-based bootstrap machinery
# --------------------------------------------------------------------------
def wmean(y: np.ndarray, w: np.ndarray) -> float:
    m = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if not m.any():
        return np.nan
    sw = w[m].sum()
    return float((w[m] * y[m]).sum() / sw) if sw > 0 else np.nan


def dmean(df: pd.DataFrame, ycol: str, wcol: str, mask=None) -> float:
    d = df if mask is None else df.loc[mask]
    if d.empty or ycol not in d:
        return np.nan
    return wmean(d[ycol].to_numpy(float), d[wcol].to_numpy(float))


def psu_index(df: pd.DataFrame, psu_col: str, strata_col: str):
    rows_by_psu = dict(df.groupby(psu_col, sort=False).indices)
    stratum_of_psu = df.groupby(psu_col, sort=False)[strata_col].first()
    psus_by_stratum: dict = {}
    for psu, stratum in stratum_of_psu.items():
        psus_by_stratum.setdefault(stratum, []).append(psu)
    return rows_by_psu, {k: np.array(v, dtype=object) for k, v in psus_by_stratum.items()}


def boot_rows(rng, rows_by_psu, psus_by_stratum) -> np.ndarray:
    picked = []
    for _s, psus in psus_by_stratum.items():
        k = len(psus)
        if k < 2:
            picked.extend(rows_by_psu[p] for p in psus)
            continue
        for p in psus[rng.integers(0, k, size=k)]:
            picked.append(rows_by_psu[p])
    return np.concatenate(picked) if picked else np.array([], dtype=int)


def pct_ci(v: np.ndarray):
    v = v[np.isfinite(v)]
    if v.size == 0:
        return (np.nan, np.nan)
    return (
        float(np.percentile(v, 100 * ALPHA / 2)),
        float(np.percentile(v, 100 * (1 - ALPHA / 2))),
    )


# --------------------------------------------------------------------------
# estimands
# --------------------------------------------------------------------------
def steps_block(s: pd.DataFrame, cond: str) -> dict:
    """Measured prevalence and cascade for one condition, from STEPS."""
    if cond == "htn":
        wt, meas = "wstep2", s["htn_measured"]
        aware_ever, aware_12m, treated = s.get("htn_aware"), s.get("htn_aware_12m"), s.get("htn_treated")
        ctrl = s.get("htn_controlled")
    else:
        wt, meas = "wstep3", s["dm_measured"]
        aware_ever, aware_12m, treated = s.get("dm_aware"), None, s.get("dm_treated")
        ctrl = s.get("dm_controlled")

    mmask = meas == 1
    out = {
        "p_meas": dmean(s, f"{cond}_measured", wt),
        "a_ever": dmean(s, f"{cond}_aware", wt, mmask),
        "t_among_meas": dmean(s, f"{cond}_treated", wt, mmask),
        "ctrl_among_meas": dmean(s, f"{cond}_controlled", wt, mmask),
    }
    out["a_12m"] = dmean(s, f"{cond}_aware_12m", wt, mmask) if aware_12m is not None else np.nan
    # treatment conditional on each awareness definition
    out["t_given_aware_ever"] = dmean(s, f"{cond}_treated", wt, mmask & (s[f"{cond}_aware"] == 1))
    out["t_given_aware_12m"] = (
        dmean(s, f"{cond}_treated", wt, mmask & (s[f"{cond}_aware_12m"] == 1))
        if aware_12m is not None
        else np.nan
    )
    # population-level: share of ALL adults with undiagnosed (unaware) disease
    out["pop_unaware"] = out["p_meas"] * (1 - out["a_ever"]) if np.isfinite(out["a_ever"]) else np.nan
    out["pop_uncontrolled"] = (
        out["p_meas"] * (1 - out["ctrl_among_meas"]) if np.isfinite(out["ctrl_among_meas"]) else np.nan
    )
    return out


def dhs_block(d: pd.DataFrame, cond: str) -> dict:
    dx = d[f"{cond}_dx"] == 1
    return {
        "p_dx": dmean(d, f"{cond}_dx", "weight"),
        "t_dx": dmean(d, f"{cond}_med", "weight", dx),
        "ins_dx": dmean(d, "insured_any", "weight", dx),
    }


def combine(sb: dict, db: dict) -> dict:
    p_meas, p_dx = sb["p_meas"], db["p_dx"]
    dr = p_dx / p_meas if p_meas and np.isfinite(p_meas) else np.nan
    out = dict(sb)
    out.update(db)
    out["detection_ratio"] = dr
    out["underdiagnosis_index"] = max(0.0, 1 - dr) if np.isfinite(dr) else np.nan
    # population treated, as seen through the self-report denominator only
    out["pop_treated_via_selfreport"] = (
        p_dx * db["t_dx"] if np.isfinite(p_dx) and np.isfinite(db["t_dx"]) else np.nan
    )
    return out


def run(s: pd.DataFrame, d: pd.DataFrame, cond: str, label: str, rng) -> pd.DataFrame:
    point = combine(steps_block(s, cond), dhs_block(d, cond))
    keys = list(point.keys())

    s_rows, s_psus = psu_index(s, "psu", "stratum")
    d_rows, d_psus = psu_index(d, "psu", "stratum")
    reps = {k: np.full(N_BOOT, np.nan) for k in keys}
    for b in range(N_BOOT):
        sb_df = s.iloc[boot_rows(rng, s_rows, s_psus)]
        db_df = d.iloc[boot_rows(rng, d_rows, d_psus)]
        try:
            rep = combine(steps_block(sb_df, cond), dhs_block(db_df, cond))
        except Exception:
            continue
        for k in keys:
            reps[k][b] = rep.get(k, np.nan)

    rows = []
    for k in keys:
        lo, hi = pct_ci(reps[k])
        rows.append(
            {
                "condition": cond,
                "base": label,
                "quantity": k,
                "estimate": point[k],
                "ci_low": lo,
                "ci_high": hi,
            }
        )
    return pd.DataFrame(rows)


def main() -> int:
    s = pd.read_pickle(DERIVED / "st13_steps_analytic.pkl")
    d = pd.read_pickle(DERIVED / "st13_dhs_analytic.pkl")
    for c in ("psu", "stratum"):
        s[c] = s[c].astype(str)
        d[c] = d[c].astype(str)
    s["age"] = pd.to_numeric(s["age"], errors="coerce")
    d["age"] = pd.to_numeric(d["age"], errors="coerce")

    rng = np.random.default_rng(SEED)
    frames = []
    bases = [
        ("18-49 (age-comparable, primary)", s[(s.age >= 18) & (s.age <= 49)], d[(d.age >= 18) & (d.age <= 49)]),
        ("full (STEPS 18-69; DHS truncated)", s, d),
    ]
    for label, ss, ds in bases:
        for cond in ("htn", "dm"):
            frames.append(run(ss.copy(), ds.copy(), cond, label, rng))

    res = pd.concat(frames, ignore_index=True)
    res.to_csv(OUT_TABLES / "Table1_Measured_vs_SelfReport.csv", index=False)

    lines = ["ST13 — self-report denominator analysis",
             f"stratified PSU bootstrap, B={N_BOOT}, seed={SEED}", ""]
    for (cond, base), grp in res.groupby(["condition", "base"], sort=False):
        lines.append(f"--- {cond.upper()}  |  base: {base}")
        for _, r in grp.iterrows():
            lines.append(
                "  {:<26} {:>8.4f}  ({:>7.4f}, {:>7.4f})".format(
                    r["quantity"], r["estimate"], r["ci_low"], r["ci_high"]
                )
            )
        lines.append("")
    text = "\n".join(lines)
    (OUT_LOGS / "st13_underdiagnosis.txt").write_text(text, encoding="utf8")
    print(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
