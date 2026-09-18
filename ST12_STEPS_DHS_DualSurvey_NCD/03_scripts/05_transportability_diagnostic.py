"""
ST12 — transportability diagnostic for the dual-survey hybrid estimator.

Addresses items 1-3 of METHODS_REVIEW_hybrid_estimator.md.

The hybrid estimator is tau = a_S x t_D, where

    a_S = awareness among measured disease            (STEPS)
    t_D = treatment among self-reported diagnosed     (KDHS)

The manuscript "validates" tau against the STEPS-direct treated-among-measured
estimate. That comparison is an identity, because STEPS defines awareness as
"told OR on medication", so

    treated|measured = a_S x t_S,  where t_S = treated|aware  (STEPS, directly estimable)

and therefore

    tau - direct = a_S x (t_D - t_S).

The entire content of the "validation" is whether t_D equals t_S. This script
estimates that difference directly, with a design-based interval, instead of
reporting the derived agreement. It also:

  * propagates variance into tau (which the published table reports as a bare
    point estimate), treating the two surveys as independent samples;
  * repeats everything on an age-comparable subset (18-49 in both surveys),
    since KDHS truncates women at 49 and men at 54 while STEPS runs to 69 and
    both awareness and treatment rise steeply with age;
  * stratifies by sex, because STEPS awareness differs markedly by sex.

Inference is by stratified PSU bootstrap (resampling primary sampling units
with replacement within strata), which handles the domain estimates correctly
and yields the cross-survey difference without a delta-method approximation.
The two surveys are resampled independently.

Outputs:
  04_tables/Table8_Transportability_Diagnostic.csv
  08_logs/st12_transportability_diagnostic.txt
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
# weighted helpers
# --------------------------------------------------------------------------
def wmean(y: np.ndarray, w: np.ndarray) -> float:
    """Weighted mean over finite, positively weighted observations."""
    m = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if not m.any():
        return np.nan
    sw = w[m].sum()
    if sw <= 0:
        return np.nan
    return float((w[m] * y[m]).sum() / sw)


def domain_wmean(df: pd.DataFrame, ycol: str, wcol: str, mask: pd.Series) -> float:
    """Weighted mean of ycol within a domain defined by mask."""
    d = df.loc[mask]
    if d.empty:
        return np.nan
    return wmean(d[ycol].to_numpy(float), d[wcol].to_numpy(float))


def psu_index(df: pd.DataFrame, psu_col: str, strata_col: str):
    """Map each stratum to its array of PSU ids, and each PSU to row positions."""
    rows_by_psu = {}
    for psu, idx in df.groupby(psu_col, sort=False).indices.items():
        rows_by_psu[psu] = idx
    psus_by_stratum = {}
    stratum_of_psu = df.groupby(psu_col, sort=False)[strata_col].first()
    for psu, stratum in stratum_of_psu.items():
        psus_by_stratum.setdefault(stratum, []).append(psu)
    psus_by_stratum = {k: np.array(v, dtype=object) for k, v in psus_by_stratum.items()}
    return rows_by_psu, psus_by_stratum


def boot_rows(rng, rows_by_psu, psus_by_stratum) -> np.ndarray:
    """One stratified PSU bootstrap replicate: resample PSUs within each stratum."""
    picked = []
    for _stratum, psus in psus_by_stratum.items():
        k = len(psus)
        if k < 2:
            # single-PSU stratum contributes no between-PSU variance
            picked.extend(rows_by_psu[p] for p in psus)
            continue
        draw = psus[rng.integers(0, k, size=k)]
        picked.extend(rows_by_psu[p] for p in draw)
    if not picked:
        return np.array([], dtype=int)
    return np.concatenate(picked)


def pct_ci(v: np.ndarray):
    v = v[np.isfinite(v)]
    if v.size == 0:
        return (np.nan, np.nan)
    return (
        float(np.percentile(v, 100 * ALPHA / 2)),
        float(np.percentile(v, 100 * (1 - ALPHA / 2))),
    )


# --------------------------------------------------------------------------
# quantities of interest
# --------------------------------------------------------------------------
def steps_quantities(s: pd.DataFrame) -> dict:
    """a_S, t_S (= treated|aware) and the direct treated|measured, from STEPS."""
    meas = s["htn_measured"] == 1
    aware = meas & (s["htn_aware"] == 1)
    return {
        "a_S": domain_wmean(s, "htn_aware", "wstep2", meas),
        "t_S": domain_wmean(s, "htn_treated", "wstep2", aware),
        "direct": domain_wmean(s, "htn_treated", "wstep2", meas),
    }


def dhs_quantities(d: pd.DataFrame) -> dict:
    """t_D = treatment among self-reported diagnosed, from KDHS."""
    dx = d["htn_dx"] == 1
    return {"t_D": domain_wmean(d, "htn_med", "weight", dx)}


def combine(sq: dict, dq: dict) -> dict:
    """Hybrid, its identity decomposition, and the transportability gap."""
    a_S, t_S, direct = sq["a_S"], sq["t_S"], sq["direct"]
    t_D = dq["t_D"]
    tau = a_S * t_D
    return {
        "a_S": a_S,
        "t_S": t_S,
        "t_D": t_D,
        "gap_tD_minus_tS": t_D - t_S,
        "ratio_tD_over_tS": t_D / t_S if t_S and np.isfinite(t_S) else np.nan,
        "tau_hybrid": tau,
        "direct": direct,
        "tau_minus_direct": tau - direct,
    }


def run(s: pd.DataFrame, d: pd.DataFrame, label: str, rng) -> pd.DataFrame:
    point = combine(steps_quantities(s), dhs_quantities(d))

    s_rows, s_psus = psu_index(s, "psu", "stratum")
    d_rows, d_psus = psu_index(d, "psu", "stratum")

    keys = list(point.keys())
    reps = {k: np.empty(N_BOOT) for k in keys}
    for b in range(N_BOOT):
        sb = s.iloc[boot_rows(rng, s_rows, s_psus)]
        db = d.iloc[boot_rows(rng, d_rows, d_psus)]
        try:
            rep = combine(steps_quantities(sb), dhs_quantities(db))
        except Exception:
            rep = {k: np.nan for k in keys}
        for k in keys:
            reps[k][b] = rep[k]

    rows = []
    for k in keys:
        lo, hi = pct_ci(reps[k])
        rows.append(
            {
                "subset": label,
                "quantity": k,
                "estimate": point[k],
                "ci_low": lo,
                "ci_high": hi,
                "boot_se": float(np.nanstd(reps[k], ddof=1)),
            }
        )
    return pd.DataFrame(rows)


def main() -> int:
    s = pd.read_pickle(DERIVED / "st12_steps_analytic.pkl")
    d = pd.read_pickle(DERIVED / "st12_dhs_analytic.pkl")

    for col in ("psu", "stratum"):
        s[col] = s[col].astype(str)
        d[col] = d[col].astype(str)
    s["age"] = pd.to_numeric(s["age"], errors="coerce")
    d["age"] = pd.to_numeric(d["age"], errors="coerce")

    rng = np.random.default_rng(SEED)
    out = [run(s, d, "full (STEPS 18-69; DHS as analysed)", rng)]

    # age-comparable subset: 18-49 in both surveys
    s49 = s[(s["age"] >= 18) & (s["age"] <= 49)].copy()
    d49 = d[(d["age"] >= 18) & (d["age"] <= 49)].copy()
    out.append(run(s49, d49, "age-comparable (18-49 both)", rng))

    # sex-stratified, on the age-comparable subset
    for sex_label in ("Women", "Men"):
        ss = s49[s49["sex"].astype(str).str.strip().str.lower() == sex_label.lower()]
        ds = d49[d49["sex"].astype(str).str.strip().str.lower() == sex_label.lower()]
        if len(ss) > 50 and len(ds) > 50:
            out.append(run(ss, ds, f"age-comparable, {sex_label}", rng))

    res = pd.concat(out, ignore_index=True)
    res.to_csv(OUT_TABLES / "Table8_Transportability_Diagnostic.csv", index=False)

    lines = []
    lines.append("ST12 transportability diagnostic")
    lines.append(f"stratified PSU bootstrap, B={N_BOOT}, seed={SEED}")
    lines.append("")
    for subset, grp in res.groupby("subset", sort=False):
        lines.append(f"--- {subset}")
        for _, r in grp.iterrows():
            lines.append(
                "  {:<22} {:>8.4f}  ({:>7.4f}, {:>7.4f})".format(
                    r["quantity"], r["estimate"], r["ci_low"], r["ci_high"]
                )
            )
        lines.append("")
    text = "\n".join(lines)
    (OUT_LOGS / "st12_transportability_diagnostic.txt").write_text(text, encoding="utf8")
    print(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
