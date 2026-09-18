#!/usr/bin/env python3
"""
ST12 — KDHS 2022 NCD module for adults 18–69 (triangulation with STEPS).

Outcomes:
- Self-reported HTN / DM diagnosis prevalence (full adult sample)
- Treatment among diagnosed
- Insurance among diagnosed (PR merge sh27)

Uses IR (women 15-49) + MR (men 15-54) restricted to 18-69, with men's
subsample weight correction as in ST03.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))
from st12_survey_utils import estimate_by_group, estimate_prop, format_pct_ci  # noqa: E402

STUDY_ROOT = SCRIPT_DIR.parent
DHS_ROOT = Path("/mnt/c/Users/HFD 2/Research/01_DHS_Data/KDHS_2022")
IR_PATH = DHS_ROOT / "IR_Individual_Recode" / "KEIR8CFL.DTA"
MR_PATH = DHS_ROOT / "MR_Mens_Recode" / "KEMR8CFL.DTA"
PR_PATH = DHS_ROOT / "PR_Person_Recode" / "KEPR8CFL.DTA"

OUT_DERIVED = STUDY_ROOT / "07_derived_data"
OUT_TABLES = STUDY_ROOT / "04_tables"
OUT_LOGS = STUDY_ROOT / "08_logs"
for p in (OUT_DERIVED, OUT_TABLES, OUT_LOGS):
    p.mkdir(parents=True, exist_ok=True)


def read_dta(path: Path, cols: list) -> pd.DataFrame:
    """Read selected columns from Stata file."""
    try:
        import pyreadstat

        df, _ = pyreadstat.read_dta(str(path), usecols=cols)
        return df
    except Exception as e1:
        print(f"pyreadstat failed: {e1}", flush=True)
    try:
        # pandas might use pyreadstat backend
        return pd.read_stata(str(path), columns=cols, convert_categoricals=False)
    except Exception as e2:
        print(f"pd.read_stata failed: {e2}", flush=True)
    # last resort: R
    import subprocess
    import tempfile

    tmp = Path(tempfile.mkstemp(suffix=".csv")[1])
    rcode = f"""
    .libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths()))
    if (!requireNamespace('haven', quietly=TRUE)) quit(status=2)
    d <- haven::read_dta('{path.as_posix()}')
    cols <- c({', '.join([repr(c) for c in cols])})
    cols <- intersect(cols, names(d))
    d <- d[cols]
    # strip labels
    for (nm in names(d)) d[[nm]] <- as.vector(d[[nm]])
    write.csv(d, '{tmp.as_posix()}', row.names=FALSE)
    """
    rscript = Path(tempfile.mkstemp(suffix=".R")[1])
    rscript.write_text(rcode)
    subprocess.check_call(["Rscript", str(rscript)])
    df = pd.read_csv(tmp)
    tmp.unlink(missing_ok=True)
    rscript.unlink(missing_ok=True)
    return df


def yn(series) -> pd.Series:
    s = pd.to_numeric(series, errors="coerce")
    # DHS: 1=yes, 0=no typically after haven; sometimes 1/2
    out = pd.Series(np.nan, index=s.index, dtype=float)
    out = out.mask(s == 1, 1.0)
    out = out.mask(s.isin([0, 2]), 0.0)
    return out


def main():
    print("Reading KDHS 2022 IR/MR/PR…", flush=True)
    ir_cols = [
        "v001",
        "v002",
        "v003",
        "v005",
        "v012",
        "v021",
        "v022",
        "v024",
        "v025",
        "v106",
        "v190",
        "chd01",
        "chd02",
        "chd05",
        "chd06",
        "chd07",
        "chd10",
    ]
    mr_cols = [
        "mv001",
        "mv002",
        "mv003",
        "mv005",
        "mv012",
        "mv021",
        "mv022",
        "mv024",
        "mv025",
        "mv106",
        "mv190",
        "mchd01",
        "mchd02",
        "mchd05",
        "mchd06",
        "mchd07",
        "mchd10",
    ]
    pr_cols = [
        "hv001",
        "hv002",
        "hvidx",
        "hv005",
        "hv021",
        "hv022",
        "hv102",
        "hv103",
        "hv104",
        "hv105",
        "hv118",
        "hv028",
        "sh27",
        "sh28a",
    ]

    ir = read_dta(IR_PATH, ir_cols)
    print(f"  IR n={len(ir)}", flush=True)
    mr = read_dta(MR_PATH, mr_cols)
    print(f"  MR n={len(mr)}", flush=True)
    pr = read_dta(PR_PATH, pr_cols)
    print(f"  PR n={len(pr)}", flush=True)

    # Normalize PR
    pr = pr.rename(
        columns={
            "hv001": "cluster",
            "hv002": "household",
            "hvidx": "line",
            "sh27": "insured_any",
            "sh28a": "insured_nhif",
        }
    )
    pr["insured_any"] = yn(pr["insured_any"])
    pr["insured_nhif"] = yn(pr["insured_nhif"])
    pr_keep = pr[["cluster", "household", "line", "insured_any", "insured_nhif"]].copy()

    # Men's subsample correction factors by strata
    pr2 = pr.copy()
    pr2["sex"] = pd.to_numeric(pr2.get("hv104"), errors="coerce")
    pr2["age"] = pd.to_numeric(pr2.get("hv105"), errors="coerce")
    pr2["hh_weight"] = pd.to_numeric(pr2.get("hv005"), errors="coerce") / 1e6
    pr2["male_eligible"] = yn(pr2.get("hv118"))
    pr2["strata"] = pd.to_numeric(pr2.get("hv022"), errors="coerce")
    men_pr = pr2[
        (pr2["sex"] == 1)
        & (pr2["age"] >= 15)
        & (pr2["age"] <= 54)
        & pr2["hh_weight"].notna()
        & pr2["strata"].notna()
    ]
    # usual resident or slept: hv102/hv103 if present
    factors = (
        men_pr.groupby("strata")
        .apply(
            lambda g: pd.Series(
                {
                    "total_w": g["hh_weight"].sum(),
                    "sub_w": g.loc[g["male_eligible"] == 1, "hh_weight"].sum(),
                }
            )
        )
        .reset_index()
    )
    factors["male_pool_factor"] = np.where(
        factors["sub_w"] > 0, factors["total_w"] / factors["sub_w"], 1.0
    )
    factor_map = dict(zip(factors["strata"], factors["male_pool_factor"]))

    women = pd.DataFrame(
        {
            "cluster": ir["v001"],
            "household": ir["v002"],
            "line": ir["v003"],
            "sex": "Women",
            "age": pd.to_numeric(ir["v012"], errors="coerce"),
            "weight": pd.to_numeric(ir["v005"], errors="coerce") / 1e6,
            "psu": pd.to_numeric(ir["v021"], errors="coerce"),
            "stratum": pd.to_numeric(ir["v022"], errors="coerce"),
            "residence": ir["v025"].astype(str),
            "education": ir["v106"].astype(str),
            "wealth": ir["v190"].astype(str),
            "bp_ever": yn(ir["chd01"]),
            "htn_dx": yn(ir["chd02"]),
            "htn_med": yn(ir["chd05"]),
            "gluc_ever": yn(ir["chd06"]),
            "dm_dx": yn(ir["chd07"]),
            "dm_med": yn(ir["chd10"]),
        }
    )

    men = pd.DataFrame(
        {
            "cluster": mr["mv001"],
            "household": mr["mv002"],
            "line": mr["mv003"],
            "sex": "Men",
            "age": pd.to_numeric(mr["mv012"], errors="coerce"),
            "weight_raw": pd.to_numeric(mr["mv005"], errors="coerce") / 1e6,
            "psu": pd.to_numeric(mr["mv021"], errors="coerce"),
            "stratum": pd.to_numeric(mr["mv022"], errors="coerce"),
            "residence": mr["mv025"].astype(str),
            "education": mr["mv106"].astype(str),
            "wealth": mr["mv190"].astype(str),
            "bp_ever": yn(mr["mchd01"]),
            "htn_dx": yn(mr["mchd02"]),
            "htn_med": yn(mr["mchd05"]),
            "gluc_ever": yn(mr["mchd06"]),
            "dm_dx": yn(mr["mchd07"]),
            "dm_med": yn(mr["mchd10"]),
        }
    )
    men["male_pool_factor"] = men["stratum"].map(factor_map).fillna(1.0)
    men["weight"] = men["weight_raw"] * men["male_pool_factor"]

    adults = pd.concat([women, men], ignore_index=True)
    adults = adults.merge(pr_keep, on=["cluster", "household", "line"], how="left")

    # Age 18-69 to align with STEPS (women max 49, men max 54 in DHS — note limitation)
    adults = adults[(adults["age"] >= 18) & (adults["age"] <= 69)].copy()
    adults["htn_dx"] = adults["htn_dx"].fillna(0)
    adults["dm_dx"] = adults["dm_dx"].fillna(0)
    adults["any_dx"] = ((adults["htn_dx"] == 1) | (adults["dm_dx"] == 1)).astype(float)

    # Treatment among diagnosed HTN: on meds
    adults["htn_treated_if_dx"] = np.where(
        adults["htn_dx"] == 1, (adults["htn_med"] == 1).astype(float), np.nan
    )
    adults["dm_treated_if_dx"] = np.where(
        adults["dm_dx"] == 1, (adults["dm_med"] == 1).astype(float), np.nan
    )
    adults["any_treated_if_dx"] = np.where(
        adults["any_dx"] == 1,
        (
            ((adults["htn_dx"] == 1) & (adults["htn_med"] == 1))
            | ((adults["dm_dx"] == 1) & (adults["dm_med"] == 1))
        ).astype(float),
        np.nan,
    )
    adults["insured_any"] = adults["insured_any"].astype(float)

    # Clean residence labels
    adults["residence"] = adults["residence"].replace(
        {"1": "Urban", "2": "Rural", "urban": "Urban", "rural": "Rural"}
    )
    # wealth as rank for CI if numeric codes
    wmap = {
        "1": 1,
        "2": 2,
        "3": 3,
        "4": 4,
        "5": 5,
        "poorest": 1,
        "poorer": 2,
        "middle": 3,
        "richer": 4,
        "richest": 5,
    }
    adults["wealth_rank"] = (
        adults["wealth"].astype(str).str.lower().str.strip().map(wmap)
    )
    # if still missing, try numeric
    adults.loc[adults["wealth_rank"].isna(), "wealth_rank"] = pd.to_numeric(
        adults.loc[adults["wealth_rank"].isna(), "wealth"], errors="coerce"
    )

    adults.to_pickle(OUT_DERIVED / "st12_dhs_analytic.pkl")
    adults[
        [
            "sex",
            "age",
            "weight",
            "psu",
            "stratum",
            "residence",
            "education",
            "wealth",
            "wealth_rank",
            "htn_dx",
            "dm_dx",
            "any_dx",
            "htn_treated_if_dx",
            "dm_treated_if_dx",
            "any_treated_if_dx",
            "insured_any",
            "bp_ever",
            "gluc_ever",
        ]
    ].to_csv(OUT_DERIVED / "st12_dhs_analytic_slim.csv", index=False)

    print(f"Analytic adults 18-69 n={len(adults)} women={(adults.sex=='Women').sum()} men={(adults.sex=='Men').sum()}", flush=True)

    results = []
    for y, lab in [
        ("htn_dx", "Self-reported HTN diagnosis prevalence"),
        ("dm_dx", "Self-reported DM diagnosis prevalence"),
        ("any_dx", "Self-reported HTN or DM diagnosis"),
        ("bp_ever", "Ever had BP measured (DHS)"),
        ("gluc_ever", "Ever had glucose measured (DHS)"),
    ]:
        r = estimate_prop(adults.dropna(subset=[y]), y, "weight", label=lab)
        r["domain"] = "dhs_all"
        results.append(r)
        print(f"  {lab}: {format_pct_ci(r)} n={r['n']}", flush=True)

    htn_dx = adults[adults["htn_dx"] == 1]
    dm_dx = adults[adults["dm_dx"] == 1]
    any_dx = adults[adults["any_dx"] == 1]

    for subset, y, lab, domain in [
        (htn_dx, "htn_treated_if_dx", "Treated among diagnosed HTN", "dhs_htn_dx"),
        (dm_dx, "dm_treated_if_dx", "Treated among diagnosed DM", "dhs_dm_dx"),
        (any_dx, "any_treated_if_dx", "Treated among any diagnosed", "dhs_any_dx"),
        (htn_dx, "insured_any", "Insured among diagnosed HTN", "dhs_htn_dx"),
        (dm_dx, "insured_any", "Insured among diagnosed DM", "dhs_dm_dx"),
        (any_dx, "insured_any", "Insured among any diagnosed", "dhs_any_dx"),
    ]:
        r = estimate_prop(subset.dropna(subset=[y]), y, "weight", label=lab)
        r["domain"] = domain
        results.append(r)
        print(f"  {lab}: {format_pct_ci(r)} n={r['n']}", flush=True)

    # Sex stratified
    strat_rows = []
    for gvar in ["sex", "residence"]:
        for y, lab in [
            ("htn_dx", "HTN dx prevalence"),
            ("dm_dx", "DM dx prevalence"),
        ]:
            tab = estimate_by_group(adults.dropna(subset=[y, gvar]), y, gvar, "weight")
            tab["outcome"] = lab
            strat_rows.append(tab)
        tab = estimate_by_group(
            htn_dx.dropna(subset=["htn_treated_if_dx", gvar]),
            "htn_treated_if_dx",
            gvar,
            "weight",
        )
        tab["outcome"] = "HTN treated|dx"
        strat_rows.append(tab)
        tab = estimate_by_group(
            htn_dx.dropna(subset=["insured_any", gvar]), "insured_any", gvar, "weight"
        )
        tab["outcome"] = "Insured|HTN dx"
        strat_rows.append(tab)

    strat = pd.concat(strat_rows, ignore_index=True)
    strat.to_csv(OUT_TABLES / "Table5_DHS_Stratified.csv", index=False)
    pd.DataFrame(results).to_csv(OUT_TABLES / "Table5_DHS_KeyEstimates.csv", index=False)

    summary = {
        "n_adults_18_69": int(len(adults)),
        "n_women": int((adults.sex == "Women").sum()),
        "n_men": int((adults.sex == "Men").sum()),
        "n_htn_dx": int(len(htn_dx)),
        "n_dm_dx": int(len(dm_dx)),
        "results": results,
        "note": "Women 18-49 and men 18-54 only (DHS age ceilings); STEPS is 18-69 both sexes.",
    }
    with open(OUT_LOGS / "st12_dhs_summary.json", "w") as f:
        json.dump(summary, f, indent=2, default=str)
    print("DHS analysis complete.", flush=True)
    return summary


if __name__ == "__main__":
    main()
