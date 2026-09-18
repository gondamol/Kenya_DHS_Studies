"""
ST12 survey utilities — design-based (cluster) estimators for STEPS / DHS.

Uses the ultimate-cluster (linearised) variance estimator with sampling
weights, suitable when primary sampling units (PSUs) are available.
"""
from __future__ import annotations

import math
from typing import Dict, Iterable, Optional, Sequence, Tuple

import numpy as np
import pandas as pd


def _finite(x: np.ndarray) -> np.ndarray:
    return np.asarray(x, dtype=float)


def weighted_mean(y: np.ndarray, w: np.ndarray) -> float:
    y = _finite(y)
    w = _finite(w)
    m = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if m.sum() == 0:
        return float("nan")
    return float(np.sum(w[m] * y[m]) / np.sum(w[m]))


def cluster_se_mean(
    df: pd.DataFrame,
    y_col: str,
    w_col: str,
    psu_col: str,
    strata_col: Optional[str] = None,
) -> Tuple[float, float, int, int]:
    """
    Weighted mean and design SE via ultimate-cluster method.

    Returns: (mean, se, n_unweighted, n_psu)
    """
    d = df[[y_col, w_col, psu_col] + ([strata_col] if strata_col else [])].copy()
    d = d.replace([np.inf, -np.inf], np.nan).dropna(subset=[y_col, w_col, psu_col])
    d = d[d[w_col] > 0]
    if d.empty:
        return float("nan"), float("nan"), 0, 0

    y = d[y_col].astype(float).values
    w = d[w_col].astype(float).values
    mu = float(np.sum(w * y) / np.sum(w))
    n = int(len(d))

    # Cluster totals of weighted residuals
    d = d.assign(_u=w * (y - mu), _w=w)
    g = d.groupby(psu_col, sort=False).agg(u=("_u", "sum"), w=("_w", "sum"))
    n_psu = int(len(g))
    if n_psu < 2:
        return mu, float("nan"), n, n_psu

    if strata_col and strata_col in d.columns:
        # attach stratum of each PSU (mode)
        psu_stratum = d.groupby(psu_col)[strata_col].agg(
            lambda s: s.mode().iloc[0] if len(s.mode()) else s.iloc[0]
        )
        g = g.join(psu_stratum.rename("stratum"))
        # within-stratum variance of cluster totals, then sum
        # ratio estimator linearisation: var(mu) ≈ (1/W^2) * sum_h [n_h/(n_h-1) * sum (u_hi - bar_u_h)^2]
        W = float(np.sum(w))
        var_num = 0.0
        for _, sub in g.groupby("stratum"):
            nh = len(sub)
            if nh < 2:
                continue
            u = sub["u"].values
            var_num += (nh / (nh - 1.0)) * np.sum((u - u.mean()) ** 2)
        se = math.sqrt(var_num) / W if var_num > 0 else float("nan")
    else:
        W = float(np.sum(w))
        u = g["u"].values
        n_c = len(u)
        var_num = (n_c / (n_c - 1.0)) * np.sum((u - u.mean()) ** 2)
        se = math.sqrt(var_num) / W if var_num > 0 else float("nan")

    return mu, float(se), n, n_psu


def prop_ci(mean: float, se: float, alpha: float = 0.05) -> Tuple[float, float]:
    if not np.isfinite(mean) or not np.isfinite(se):
        return float("nan"), float("nan")
    z = 1.959963984540054
    lo = max(0.0, mean - z * se)
    hi = min(1.0, mean + z * se)
    return lo, hi


def estimate_prop(
    df: pd.DataFrame,
    y_col: str,
    w_col: str,
    psu_col: str = "psu",
    strata_col: str = "stratum",
    label: str = "",
) -> Dict:
    mu, se, n, n_psu = cluster_se_mean(df, y_col, w_col, psu_col, strata_col)
    lo, hi = prop_ci(mu, se)
    return {
        "label": label or y_col,
        "estimate": mu,
        "se": se,
        "ci_low": lo,
        "ci_high": hi,
        "pct": 100.0 * mu if np.isfinite(mu) else float("nan"),
        "pct_low": 100.0 * lo if np.isfinite(lo) else float("nan"),
        "pct_high": 100.0 * hi if np.isfinite(hi) else float("nan"),
        "n": n,
        "n_psu": n_psu,
    }


def estimate_by_group(
    df: pd.DataFrame,
    y_col: str,
    group_col: str,
    w_col: str,
    psu_col: str = "psu",
    strata_col: str = "stratum",
) -> pd.DataFrame:
    rows = []
    for g, sub in df.groupby(group_col, dropna=False):
        r = estimate_prop(sub, y_col, w_col, psu_col, strata_col, label=str(g))
        r["group"] = g
        r["group_var"] = group_col
        rows.append(r)
    return pd.DataFrame(rows)


def erreygers_ci(
    df: pd.DataFrame,
    y_col: str,
    rank_col: str,
    w_col: str,
) -> Dict:
    """
    Erreygers-corrected concentration index for binary outcome.
    Rank is SES (higher = richer). Uses weighted fractional rank.
    CI_E = 4 * mu * CI_standard, with CI_standard = 2*cov(y,R)/mu - 1.
    """
    d = df[[y_col, rank_col, w_col]].dropna()
    d = d[d[w_col] > 0]
    if len(d) < 30:
        return {"erreygers": float("nan"), "mu": float("nan"), "n": len(d)}

    d = d.sort_values(rank_col)
    w = d[w_col].astype(float).values
    y = d[y_col].astype(float).values
    # weighted fractional rank (Kakwani / O'Donnell)
    w_sum = w.sum()
    # mid-point ranks
    cum = np.cumsum(w)
    rank = (cum - w / 2.0) / w_sum
    mu = np.sum(w * y) / w_sum
    if mu <= 0 or mu >= 1:
        # still compute but flag
        pass
    cov_yr = np.sum(w * (y - mu) * (rank - 0.5)) / w_sum
    # standard CI for health = 2*cov/(mu) when continuous; for binary Erreygers:
    # E = 8 * cov(y, R)  (since for binary, E = 4*mu*CI and CI=2cov/mu => E=8cov)
    e = 8.0 * cov_yr
    return {"erreygers": float(e), "mu": float(mu), "n": int(len(d)), "cov": float(cov_yr)}


def format_pct_ci(row: Dict, digits: int = 1) -> str:
    if not np.isfinite(row.get("pct", np.nan)):
        return "—"
    return f"{row['pct']:.{digits}f} ({row['pct_low']:.{digits}f}–{row['pct_high']:.{digits}f})"


def bootstrap_erreygers(
    df: pd.DataFrame,
    y_col: str,
    rank_col: str,
    w_col: str,
    psu_col: str,
    n_boot: int = 200,
    seed: int = 42,
) -> Dict:
    """Cluster bootstrap of Erreygers CI (resample PSUs with replacement)."""
    rng = np.random.default_rng(seed)
    base = erreygers_ci(df, y_col, rank_col, w_col)
    # Pre-split by PSU once (avoids repeated full-frame filters)
    groups = {k: g for k, g in df.groupby(psu_col, sort=False)}
    psus = np.array(list(groups.keys()))
    if len(psus) < 5:
        return {**base, "boot_low": float("nan"), "boot_high": float("nan")}
    boots = []
    for _ in range(n_boot):
        draw = rng.choice(psus, size=len(psus), replace=True)
        bdf = pd.concat([groups[p] for p in draw], ignore_index=True)
        boots.append(erreygers_ci(bdf, y_col, rank_col, w_col)["erreygers"])
    boots = np.array([b for b in boots if np.isfinite(b)])
    if len(boots) < 10:
        return {**base, "boot_low": float("nan"), "boot_high": float("nan")}
    lo, hi = np.quantile(boots, [0.025, 0.975])
    return {
        **base,
        "boot_low": float(lo),
        "boot_high": float(hi),
        "boot_n": int(len(boots)),
    }
