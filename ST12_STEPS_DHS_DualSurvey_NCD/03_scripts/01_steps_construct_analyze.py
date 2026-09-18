#!/usr/bin/env python3
"""
ST12 — STEPS 2015 construction + cascade analysis.

Definitions follow WHO STEPS / Kenya STEPS 2015 practice:
- Mean SBP/DBP = average of last two readings (readings 2 and 3)
- Hypertension: SBP>=140 or DBP>=90 OR on BP medication in past 2 weeks
- Diabetes: fasting glucose >=7.0 mmol/L OR currently on diabetes medication
- Cascade among those with measured disease:
  aware | treated | controlled
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

# Allow import of local utils
SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))
from st12_survey_utils import (  # noqa: E402
    bootstrap_erreygers,
    estimate_by_group,
    estimate_prop,
    format_pct_ci,
)

STUDY_ROOT = SCRIPT_DIR.parent
STEPS_CSV = Path("/mnt/c/Users/HFD 2/Research/Kenya - STEPS 2015/ken2015.csv")
OUT_DERIVED = STUDY_ROOT / "07_derived_data"
OUT_TABLES = STUDY_ROOT / "04_tables"
OUT_LOGS = STUDY_ROOT / "08_logs"
for p in (OUT_DERIVED, OUT_TABLES, OUT_LOGS):
    p.mkdir(parents=True, exist_ok=True)


def yes1(series: pd.Series) -> pd.Series:
    """Map STEPS yes/no (1=Yes, 2=No) to 0/1; other -> NA."""
    s = pd.to_numeric(series, errors="coerce")
    out = pd.Series(np.nan, index=s.index, dtype=float)
    out = out.mask(s == 1, 1.0)
    out = out.mask(s == 2, 0.0)
    return out


def num(series: pd.Series) -> pd.Series:
    s = pd.to_numeric(series, errors="coerce")
    # common STEPS missing codes
    s = s.mask(s.isin([77, 88, 99, 777, 888, 999, 7777, 8888, 9999]))
    return s


def education_rank(c5: pd.Series) -> pd.Series:
    """Ordinal SES proxy from education (higher = more education)."""
    order = {
        "No formal schooling": 1,
        "Less than primary school": 1,
        "Primary school incomplete": 2,
        "Primary school completed": 3,
        "Secondary school incomplete": 4,
        "A-Level incomplete": 4,
        "Secondary school completed": 5,
        "A-Level completed": 5,
        "College/University completed": 6,
        "Post-graduate degree": 7,
    }
    out = c5.astype(str).str.strip().map(order)
    xs = c5.astype(str).str.lower()
    miss = out.isna()
    out = out.copy()
    out.loc[miss & xs.str.contains("no formal|less than primary", na=False)] = 1
    out.loc[miss & xs.str.contains("primary", na=False) & xs.str.contains("incomplet", na=False)] = 2
    out.loc[miss & xs.str.contains("primary", na=False) & xs.str.contains("complet", na=False)] = 3
    out.loc[miss & xs.str.contains("secondary", na=False) & xs.str.contains("incomplet", na=False)] = 4
    out.loc[miss & xs.str.contains("secondary", na=False) & xs.str.contains("complet", na=False)] = 5
    out.loc[miss & xs.str.contains("a-level", na=False)] = 5
    out.loc[miss & xs.str.contains("college|university", na=False)] = 6
    out.loc[miss & xs.str.contains("post", na=False)] = 7
    return out.astype(float)


def educ_collapse(c5: pd.Series) -> pd.Series:
    r = education_rank(c5)
    return pd.cut(
        r,
        bins=[0, 1.5, 3.5, 5.5, 10],
        labels=["None/less than primary", "Primary", "Secondary", "Tertiary"],
    ).astype(str)


def construct(df: pd.DataFrame) -> pd.DataFrame:
    d = df.copy()
    d.columns = [c.strip().lower() for c in d.columns]

    # BP: average of readings 2 and 3
    sbp2, sbp3 = num(d["m5a"]), num(d["m6a"])
    dbp2, dbp3 = num(d["m5b"]), num(d["m6b"])
    d["sbp"] = (sbp2 + sbp3) / 2.0
    d["dbp"] = (dbp2 + dbp3) / 2.0
    # if one of last two missing, use available; if both missing use reading 1
    sbp1, dbp1 = num(d["m4a"]), num(d["m4b"])
    d.loc[d["sbp"].isna(), "sbp"] = sbp1[d["sbp"].isna()]
    d.loc[d["dbp"].isna(), "dbp"] = dbp1[d["dbp"].isna()]
    # still try average if only one of 2/3 present
    only2 = sbp2.notna() & sbp3.isna()
    only3 = sbp3.notna() & sbp2.isna()
    d.loc[only2, "sbp"] = sbp2[only2]
    d.loc[only3, "sbp"] = sbp3[only3]
    only2d = dbp2.notna() & dbp3.isna()
    only3d = dbp3.notna() & dbp2.isna()
    d.loc[only2d, "dbp"] = dbp2[only2d]
    d.loc[only3d, "dbp"] = dbp3[only3d]

    d["height_cm"] = num(d["m11"])
    d["weight_kg"] = num(d["m12"])
    d["bmi"] = d["weight_kg"] / ((d["height_cm"] / 100.0) ** 2)
    d.loc[(d["bmi"] < 10) | (d["bmi"] > 80), "bmi"] = np.nan
    d["overweight_obese"] = (d["bmi"] >= 25).astype(float)
    d.loc[d["bmi"].isna(), "overweight_obese"] = np.nan
    d["obese"] = (d["bmi"] >= 30).astype(float)
    d.loc[d["bmi"].isna(), "obese"] = np.nan

    d["glucose"] = num(d["b5"])
    d["cholesterol"] = num(d["b8"])

    # History
    d["bp_ever_measured"] = yes1(d["h1"])
    d["told_high_bp"] = yes1(d["h2a"])  # only among measured
    d["on_bp_meds"] = yes1(d["h3"])  # among told high BP
    # For cascade: among all, on meds is 1 only if h3==1; else if not told, treat as 0 if ever measured path not taken
    # WHO: currently taking meds = among those diagnosed. For measured HTN definition, on_meds contributes to prevalence.
    # Reconstruct population on_bp_meds: h3 asked only if told high BP. If never told, not on meds.
    on_med_raw = yes1(d["h3"])
    d["on_bp_meds_pop"] = on_med_raw
    # if never told high BP (h2a==2 or h1==2), set on_meds_pop = 0 when we know they're not in treatment path
    # if h2a missing because h1==2 (never measured), on_meds = 0
    never_measured = d["bp_ever_measured"] == 0
    never_told = d["told_high_bp"] == 0
    d.loc[never_measured | never_told, "on_bp_meds_pop"] = d.loc[
        never_measured | never_told, "on_bp_meds_pop"
    ].fillna(0.0)
    # if told but h3 missing, leave NA

    d["gluc_ever_measured"] = yes1(d["h6"])
    d["told_high_gluc"] = yes1(d["h7a"])
    d["on_dm_meds"] = yes1(d["h8"])
    on_dm_raw = yes1(d["h8"])
    d["on_dm_meds_pop"] = on_dm_raw
    never_g = d["gluc_ever_measured"] == 0
    never_tg = d["told_high_gluc"] == 0
    d.loc[never_g | never_tg, "on_dm_meds_pop"] = d.loc[
        never_g | never_tg, "on_dm_meds_pop"
    ].fillna(0.0)

    # Measured hypertension (Step 2 sample)
    d["htn_measured"] = (
        (d["sbp"] >= 140) | (d["dbp"] >= 90) | (d["on_bp_meds_pop"] == 1)
    ).astype(float)
    d.loc[d["sbp"].isna() & d["dbp"].isna() & d["on_bp_meds_pop"].isna(), "htn_measured"] = np.nan
    # if BP missing but known not on meds and no BP — leave NA for step2 incomplete
    d.loc[d["sbp"].isna() & d["dbp"].isna() & (d["on_bp_meds_pop"] != 1), "htn_measured"] = np.nan

    # Diabetes (Step 3)
    d["dm_measured"] = ((d["glucose"] >= 7.0) | (d["on_dm_meds_pop"] == 1)).astype(float)
    d.loc[d["glucose"].isna() & (d["on_dm_meds_pop"] != 1), "dm_measured"] = np.nan
    d.loc[d["glucose"].isna() & d["on_dm_meds_pop"].isna(), "dm_measured"] = np.nan

    # Raised cholesterol (optional)
    d["chol_raised"] = (d["cholesterol"] >= 5.0).astype(float)
    d.loc[d["cholesterol"].isna(), "chol_raised"] = np.nan

    # Cascade among hypertensives
    # Aware: told high BP OR on meds
    d["htn_aware"] = np.where(
        d["htn_measured"] == 1,
        ((d["told_high_bp"] == 1) | (d["on_bp_meds_pop"] == 1)).astype(float),
        np.nan,
    )
    d["htn_treated"] = np.where(
        d["htn_measured"] == 1,
        (d["on_bp_meds_pop"] == 1).astype(float),
        np.nan,
    )
    # Controlled: on meds AND SBP<140 AND DBP<90 (among all with HTN — population control)
    d["htn_controlled"] = np.where(
        d["htn_measured"] == 1,
        (
            (d["on_bp_meds_pop"] == 1)
            & (d["sbp"] < 140)
            & (d["dbp"] < 90)
        ).astype(float),
        np.nan,
    )
    # Control among treated
    d["htn_controlled_among_treated"] = np.where(
        (d["htn_measured"] == 1) & (d["htn_treated"] == 1),
        ((d["sbp"] < 140) & (d["dbp"] < 90)).astype(float),
        np.nan,
    )

    # Diabetes cascade
    d["dm_aware"] = np.where(
        d["dm_measured"] == 1,
        ((d["told_high_gluc"] == 1) | (d["on_dm_meds_pop"] == 1)).astype(float),
        np.nan,
    )
    d["dm_treated"] = np.where(
        d["dm_measured"] == 1,
        (d["on_dm_meds_pop"] == 1).astype(float),
        np.nan,
    )
    # Control among diabetes: on meds and glucose <7.0 (among measured DM)
    d["dm_controlled"] = np.where(
        d["dm_measured"] == 1,
        ((d["on_dm_meds_pop"] == 1) & (d["glucose"] < 7.0)).astype(float),
        np.nan,
    )

    # Demographics
    d["sex"] = d["sex"].astype(str).str.strip()
    d["sex"] = d["sex"].replace({"Women": "Women", "Men": "Men", "1": "Men", "2": "Women"})
    d["age"] = num(d["age"])
    d["agerange"] = d["agerange"].astype(str)
    d["residence"] = d["urbrur"].map({1: "Urban", 2: "Rural", "1": "Urban", "2": "Rural"})
    d["educ_rank"] = education_rank(d["c5"])
    d["educ"] = educ_collapse(d["c5"])
    d["psu"] = pd.to_numeric(d["psu"], errors="coerce")
    d["stratum"] = pd.to_numeric(d["stratum"], errors="coerce")
    for w in ("wstep1", "wstep2", "wstep3"):
        d[w] = pd.to_numeric(d[w], errors="coerce")

    # Multimorbidity measured
    d["htn_or_dm"] = np.nan
    both_known = d["htn_measured"].notna() & d["dm_measured"].notna()
    d.loc[both_known, "htn_or_dm"] = (
        (d.loc[both_known, "htn_measured"] == 1) | (d.loc[both_known, "dm_measured"] == 1)
    ).astype(float)
    d["htn_and_dm"] = np.nan
    d.loc[both_known, "htn_and_dm"] = (
        (d.loc[both_known, "htn_measured"] == 1) & (d.loc[both_known, "dm_measured"] == 1)
    ).astype(float)

    # Screening coverage (step 1 weight domain)
    d["screened_bp"] = d["bp_ever_measured"]
    d["screened_gluc"] = d["gluc_ever_measured"]

    return d


def main():
    print("Loading STEPS CSV…", flush=True)
    raw = pd.read_csv(STEPS_CSV, low_memory=False)
    print(f"  raw n={len(raw)} cols={len(raw.columns)}", flush=True)
    d = construct(raw)
    print("  construct done", flush=True)
    d.to_pickle(OUT_DERIVED / "st12_steps_analytic.pkl")
    # also a slim CSV without raw microdata dump of all originals — derived only for local use
    keep = [
        c
        for c in d.columns
        if c
        in {
            "pid",
            "psu",
            "stratum",
            "wstep1",
            "wstep2",
            "wstep3",
            "sex",
            "age",
            "agerange",
            "residence",
            "educ",
            "educ_rank",
            "sbp",
            "dbp",
            "bmi",
            "glucose",
            "cholesterol",
            "htn_measured",
            "dm_measured",
            "htn_aware",
            "htn_treated",
            "htn_controlled",
            "htn_controlled_among_treated",
            "dm_aware",
            "dm_treated",
            "dm_controlled",
            "bp_ever_measured",
            "gluc_ever_measured",
            "overweight_obese",
            "obese",
            "htn_or_dm",
            "htn_and_dm",
            "on_bp_meds_pop",
            "on_dm_meds_pop",
            "told_high_bp",
            "told_high_gluc",
            "chol_raised",
        }
    ]
    d[keep].to_csv(OUT_DERIVED / "st12_steps_analytic_slim.csv", index=False)
    print(f"  analytic saved; htn_measured rate (unw)={d['htn_measured'].mean():.3f}")

    results = []

    # --- Step 2 domain: hypertension ---
    s2 = d[d["wstep2"] > 0].copy()
    print(f"Step2 domain n={len(s2)}")

    for var, lab in [
        ("htn_measured", "Measured hypertension prevalence"),
        ("overweight_obese", "Overweight or obese (BMI>=25)"),
        ("obese", "Obesity (BMI>=30)"),
        ("bp_ever_measured", "Ever had blood pressure measured"),
    ]:
        r = estimate_prop(s2.dropna(subset=[var]), var, "wstep2", label=lab)
        r["domain"] = "step2_all"
        results.append(r)
        print(f"  {lab}: {format_pct_ci(r)} n={r['n']}")

    htn = s2[s2["htn_measured"] == 1].copy()
    print(f"Hypertensives n={len(htn)}")
    for var, lab in [
        ("htn_aware", "HTN: aware (told or on meds)"),
        ("htn_treated", "HTN: on medication"),
        ("htn_controlled", "HTN: controlled (among all HTN)"),
        ("htn_controlled_among_treated", "HTN: controlled among treated"),
    ]:
        sub = htn.dropna(subset=[var]) if var == "htn_controlled_among_treated" else htn
        if var == "htn_controlled_among_treated":
            sub = htn[htn["htn_treated"] == 1].dropna(subset=[var])
        r = estimate_prop(sub, var, "wstep2", label=lab)
        r["domain"] = "step2_htn"
        results.append(r)
        print(f"  {lab}: {format_pct_ci(r)} n={r['n']}")

    # Sex and residence stratified HTN prevalence and cascade
    strat_rows = []
    for gvar in ["sex", "residence", "educ", "agerange"]:
        for y, lab, domain_df, w in [
            ("htn_measured", "HTN prevalence", s2, "wstep2"),
            ("htn_aware", "HTN aware", htn, "wstep2"),
            ("htn_treated", "HTN treated", htn, "wstep2"),
            ("htn_controlled", "HTN controlled", htn, "wstep2"),
        ]:
            tab = estimate_by_group(domain_df.dropna(subset=[y, gvar]), y, gvar, w)
            tab["outcome"] = lab
            tab["y"] = y
            strat_rows.append(tab)
    strat = pd.concat(strat_rows, ignore_index=True)
    strat.to_csv(OUT_TABLES / "TableS1_STEPS_Stratified.csv", index=False)

    # Erreygers CI for HTN outcomes by education rank
    eq_rows = []
    for y, lab, domain_df in [
        ("htn_measured", "HTN prevalence", s2),
        ("htn_aware", "HTN awareness among HTN", htn),
        ("htn_treated", "HTN treatment among HTN", htn),
        ("htn_controlled", "HTN control among HTN", htn),
    ]:
        sub = domain_df.dropna(subset=[y, "educ_rank", "wstep2"])
        e = bootstrap_erreygers(sub, y, "educ_rank", "wstep2", "psu", n_boot=200)
        eq_rows.append({"outcome": lab, "y": y, **e})
        print(
            f"  Erreygers {lab}: {e['erreygers']:.3f} "
            f"({e.get('boot_low', float('nan')):.3f}–{e.get('boot_high', float('nan')):.3f})"
        )
    pd.DataFrame(eq_rows).to_csv(OUT_TABLES / "Table3_STEPS_Erreygers.csv", index=False)

    # --- Step 3 domain: diabetes ---
    s3 = d[d["wstep3"] > 0].copy()
    print(f"Step3 domain n={len(s3)}")
    for var, lab in [
        ("dm_measured", "Measured diabetes prevalence"),
        ("gluc_ever_measured", "Ever had blood glucose measured"),
        ("chol_raised", "Raised total cholesterol (>=5.0 mmol/L)"),
    ]:
        r = estimate_prop(s3.dropna(subset=[var]), var, "wstep3", label=lab)
        r["domain"] = "step3_all"
        results.append(r)
        print(f"  {lab}: {format_pct_ci(r)} n={r['n']}")

    dm = s3[s3["dm_measured"] == 1].copy()
    print(f"Diabetes n={len(dm)}")
    for var, lab in [
        ("dm_aware", "DM: aware"),
        ("dm_treated", "DM: on medication"),
        ("dm_controlled", "DM: controlled (among all DM)"),
    ]:
        r = estimate_prop(dm.dropna(subset=[var]), var, "wstep3", label=lab)
        r["domain"] = "step3_dm"
        results.append(r)
        print(f"  {lab}: {format_pct_ci(r)} n={r['n']}")

    for gvar in ["sex", "residence", "educ"]:
        for y, lab, domain_df, w in [
            ("dm_measured", "DM prevalence", s3, "wstep3"),
            ("dm_aware", "DM aware", dm, "wstep3"),
            ("dm_treated", "DM treated", dm, "wstep3"),
            ("dm_controlled", "DM controlled", dm, "wstep3"),
        ]:
            if len(domain_df.dropna(subset=[y, gvar])) < 20:
                continue
            tab = estimate_by_group(domain_df.dropna(subset=[y, gvar]), y, gvar, w)
            tab["outcome"] = lab
            tab["y"] = y
            strat_rows.append(tab)

    # Multimorbidity in step3 with both measures
    both = s3.dropna(subset=["htn_measured", "dm_measured"])
    for var, lab in [
        ("htn_or_dm", "HTN or DM (measured)"),
        ("htn_and_dm", "HTN and DM (measured multimorbidity)"),
    ]:
        r = estimate_prop(both.dropna(subset=[var]), var, "wstep3", label=lab)
        r["domain"] = "step3_multi"
        results.append(r)
        print(f"  {lab}: {format_pct_ci(r)} n={r['n']}")

    # Step 1 screening
    s1 = d[d["wstep1"] > 0].copy()
    for var, lab in [
        ("bp_ever_measured", "Ever BP measured (Step1 weight)"),
        ("gluc_ever_measured", "Ever glucose measured (Step1 weight)"),
    ]:
        r = estimate_prop(s1.dropna(subset=[var]), var, "wstep1", label=lab)
        r["domain"] = "step1_all"
        results.append(r)

    res_df = pd.DataFrame(results)
    res_df.to_csv(OUT_TABLES / "Table1_STEPS_KeyEstimates.csv", index=False)

    # Cascade table (publication format)
    cascade_rows = []
    # HTN cascade as % of all hypertensives and as sequential
    for lab_key, y in [
        ("Aware", "htn_aware"),
        ("Treated", "htn_treated"),
        ("Controlled", "htn_controlled"),
    ]:
        r = estimate_prop(htn.dropna(subset=[y]), y, "wstep2", label=lab_key)
        cascade_rows.append(
            {
                "condition": "Hypertension",
                "stage": lab_key,
                "pct": r["pct"],
                "pct_low": r["pct_low"],
                "pct_high": r["pct_high"],
                "n": r["n"],
                "fmt": format_pct_ci(r),
            }
        )
    for lab_key, y in [
        ("Aware", "dm_aware"),
        ("Treated", "dm_treated"),
        ("Controlled", "dm_controlled"),
    ]:
        r = estimate_prop(dm.dropna(subset=[y]), y, "wstep3", label=lab_key)
        cascade_rows.append(
            {
                "condition": "Diabetes",
                "stage": lab_key,
                "pct": r["pct"],
                "pct_low": r["pct_low"],
                "pct_high": r["pct_high"],
                "n": r["n"],
                "fmt": format_pct_ci(r),
            }
        )
    pd.DataFrame(cascade_rows).to_csv(OUT_TABLES / "Table2_STEPS_Cascade.csv", index=False)

    # Population-level effective coverage = prevalence * control among disease
    # = proportion of whole population with controlled disease... actually
    # controlled among population = mean(htn_controlled * htn_measured) but htn_controlled only defined for HTN
    s2 = s2.copy()
    s2["htn_pop_controlled"] = np.where(
        s2["htn_measured"] == 1, s2["htn_controlled"], 0.0
    )
    s2.loc[s2["htn_measured"].isna(), "htn_pop_controlled"] = np.nan
    r = estimate_prop(
        s2.dropna(subset=["htn_pop_controlled"]),
        "htn_pop_controlled",
        "wstep2",
        label="Population on controlled HTN treatment",
    )
    results.append(r)
    print(f"  Pop HTN controlled: {format_pct_ci(r)}")

    # Leakage percentages (of measured disease)
    htn_prev = estimate_prop(s2.dropna(subset=["htn_measured"]), "htn_measured", "wstep2")
    htn_aware = estimate_prop(htn.dropna(subset=["htn_aware"]), "htn_aware", "wstep2")
    htn_tx = estimate_prop(htn.dropna(subset=["htn_treated"]), "htn_treated", "wstep2")
    htn_ctrl = estimate_prop(htn.dropna(subset=["htn_controlled"]), "htn_controlled", "wstep2")

    summary = {
        "n_total": int(len(d)),
        "n_step2": int(len(s2)),
        "n_step3": int(len(s3)),
        "n_htn": int(len(htn)),
        "n_dm": int(len(dm)),
        "htn_prevalence_pct": htn_prev["pct"],
        "htn_aware_pct": htn_aware["pct"],
        "htn_treated_pct": htn_tx["pct"],
        "htn_controlled_pct": htn_ctrl["pct"],
        "underdiagnosis_among_htn_pct": 100 - htn_aware["pct"] if np.isfinite(htn_aware["pct"]) else None,
        "untreated_among_htn_pct": 100 - htn_tx["pct"] if np.isfinite(htn_tx["pct"]) else None,
        "key_estimates": results,
        "equality": eq_rows,
    }
    with open(OUT_LOGS / "st12_steps_summary.json", "w") as f:
        json.dump(summary, f, indent=2, default=str)

    # Publication Table 1 sample characteristics
    char_rows = []
    for gvar, w, domain, lab in [
        ("sex", "wstep1", s1, "Sex"),
        ("residence", "wstep1", s1, "Residence"),
        ("educ", "wstep1", s1, "Education"),
        ("agerange", "wstep1", s1, "Age group"),
    ]:
        # distribution of groups
        for g, sub in domain.groupby(gvar):
            if pd.isna(g) or str(g) == "nan":
                continue
            # weighted share
            r = estimate_prop(
                domain.assign(_g=(domain[gvar] == g).astype(float)),
                "_g",
                w,
                label=f"{lab}={g}",
            )
            char_rows.append(
                {
                    "characteristic": lab,
                    "category": g,
                    "pct": r["pct"],
                    "pct_low": r["pct_low"],
                    "pct_high": r["pct_high"],
                    "n": int((domain[gvar] == g).sum()),
                    "fmt": format_pct_ci(r),
                }
            )
    pd.DataFrame(char_rows).to_csv(OUT_TABLES / "Table0_STEPS_Sample.csv", index=False)

    # Stratified cascade publication table
    pub_strat = []
    for gvar, glab in [("sex", "Sex"), ("residence", "Residence"), ("educ", "Education")]:
        for y, ylab, domain_df, w in [
            ("htn_measured", "Prevalence", s2, "wstep2"),
            ("htn_aware", "Aware", htn, "wstep2"),
            ("htn_treated", "Treated", htn, "wstep2"),
            ("htn_controlled", "Controlled", htn, "wstep2"),
        ]:
            tab = estimate_by_group(domain_df.dropna(subset=[y, gvar]), y, gvar, w)
            for _, row in tab.iterrows():
                pub_strat.append(
                    {
                        "condition": "Hypertension",
                        "stratifier": glab,
                        "category": row["group"],
                        "stage": ylab,
                        "pct": row["pct"],
                        "pct_low": row["pct_low"],
                        "pct_high": row["pct_high"],
                        "n": row["n"],
                        "fmt": format_pct_ci(row),
                    }
                )
        for y, ylab, domain_df, w in [
            ("dm_measured", "Prevalence", s3, "wstep3"),
            ("dm_aware", "Aware", dm, "wstep3"),
            ("dm_treated", "Treated", dm, "wstep3"),
            ("dm_controlled", "Controlled", dm, "wstep3"),
        ]:
            sub = domain_df.dropna(subset=[y, gvar])
            if len(sub) < 30:
                continue
            tab = estimate_by_group(sub, y, gvar, w)
            for _, row in tab.iterrows():
                pub_strat.append(
                    {
                        "condition": "Diabetes",
                        "stratifier": glab,
                        "category": row["group"],
                        "stage": ylab,
                        "pct": row["pct"],
                        "pct_low": row["pct_low"],
                        "pct_high": row["pct_high"],
                        "n": row["n"],
                        "fmt": format_pct_ci(row),
                    }
                )
    pd.DataFrame(pub_strat).to_csv(OUT_TABLES / "Table4_STEPS_Cascade_ByStratum.csv", index=False)

    print("\nSTEPS analysis complete.")
    print(f"Tables in {OUT_TABLES}")
    return summary


if __name__ == "__main__":
    main()
