#!/usr/bin/env python3
"""
ST12 — Dual-survey synthesis: STEPS 2015 (measured) × KDHS 2022 (self-report + insurance).

Core estimators
---------------
1. Detection ratio (cross-survey):
   DR = p_dx_DHS / p_measured_STEPS
   Underdiagnosis index (UDI) = 1 - DR  (if DR < 1; else 0)
   Interpreted as a *structural* detection gap, not a time trend, because
   surveys differ by 7 years and by measurement modality.

2. Population treatment coverage (PTC) identity (Gelman-style synthetic):
   PTC = p_measured × p(treated | measured)
   STEPS estimates both factors directly (biological cascade).

3. Hybrid PTC under alternative detection scenarios (methods contribution):
   Let a_S = awareness among measured (STEPS)
   Let t_D = treatment among diagnosed (DHS)
   Hybrid treated-among-measured ≈ a_S × t_D
   (assumes treatment conditional on awareness equals DHS treatment among
   self-reported diagnosed — transportability assumption, sensitivity-tested)

4. Insurance-conditioned hybrid (extends ST03):
   Among DHS diagnosed: t | insured vs uninsured
   Apply STEPS awareness to scale up to measured disease:
   Population untreated = p_measured × (1 - a_S × t_D)

Sensitivity: a_S varied ±20% relative; t_D varied by insurance stratum.
"""
from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd

STUDY_ROOT = Path(__file__).resolve().parent.parent
OUT_TABLES = STUDY_ROOT / "04_tables"
OUT_LOGS = STUDY_ROOT / "08_logs"
OUT_DERIVED = STUDY_ROOT / "07_derived_data"


def _get(df: pd.DataFrame, label_substr: str, domain: str = None):
    m = df["label"].astype(str).str.contains(label_substr, case=False, regex=False)
    if domain and "domain" in df.columns:
        m = m & (df["domain"] == domain)
    sub = df.loc[m]
    if sub.empty:
        # try exact-ish
        m = df["label"].astype(str).str.contains(label_substr.split()[0], case=False)
        sub = df.loc[m]
    if sub.empty:
        return None
    return sub.iloc[0].to_dict()


def main():
    steps = pd.read_csv(OUT_TABLES / "Table1_STEPS_KeyEstimates.csv")
    dhs = pd.read_csv(OUT_TABLES / "Table5_DHS_KeyEstimates.csv")
    cascade = pd.read_csv(OUT_TABLES / "Table2_STEPS_Cascade.csv")

    htn_meas = _get(steps, "Measured hypertension")
    dm_meas = _get(steps, "Measured diabetes")
    htn_aware = _get(steps, "HTN: aware")
    htn_tx = _get(steps, "HTN: on medication")
    htn_ctrl = _get(steps, "HTN: controlled (among all")
    dm_aware = _get(steps, "DM: aware")
    dm_tx = _get(steps, "DM: on medication")
    dm_ctrl = _get(steps, "DM: controlled")

    dhs_htn = _get(dhs, "Self-reported HTN")
    dhs_dm = _get(dhs, "Self-reported DM")
    dhs_htn_tx = _get(dhs, "Treated among diagnosed HTN")
    dhs_dm_tx = _get(dhs, "Treated among diagnosed DM")
    dhs_htn_ins = _get(dhs, "Insured among diagnosed HTN")
    dhs_dm_ins = _get(dhs, "Insured among diagnosed DM")

    rows = []

    def add(section, metric, value, note=""):
        rows.append({"section": section, "metric": metric, "value": value, "note": note})

    # --- Detection ratios ---
    for name, p_m, p_d in [
        ("Hypertension", htn_meas, dhs_htn),
        ("Diabetes", dm_meas, dhs_dm),
    ]:
        pm, pd_ = p_m["estimate"], p_d["estimate"]
        dr = pd_ / pm if pm and pm > 0 else np.nan
        udi = max(0.0, 1.0 - dr) if np.isfinite(dr) else np.nan
        add(name, "p_measured_STEPS", pm, f"{p_m['pct']:.1f}% (95% CI {p_m['pct_low']:.1f}–{p_m['pct_high']:.1f})")
        add(name, "p_dx_DHS", pd_, f"{p_d['pct']:.1f}% (95% CI {p_d['pct_low']:.1f}–{p_d['pct_high']:.1f})")
        add(name, "detection_ratio_DHS_over_STEPS", dr, "Cross-survey; not a time trend")
        add(name, "underdiagnosis_index_1_minus_DR", udi, "1 - detection ratio (floored at 0)")
        add(name, "implied_undiagnosed_share_of_true_disease", udi, "Among true disease, share not captured by DHS-like self-report")

    # --- STEPS biological cascade (direct) ---
    for name, aware, tx, ctrl, p_m in [
        ("Hypertension", htn_aware, htn_tx, htn_ctrl, htn_meas),
        ("Diabetes", dm_aware, dm_tx, dm_ctrl, dm_meas),
    ]:
        add(name, "aware_among_measured_STEPS", aware["estimate"], format_pct(aware))
        add(name, "treated_among_measured_STEPS", tx["estimate"], format_pct(tx))
        add(name, "controlled_among_measured_STEPS", ctrl["estimate"], format_pct(ctrl))
        # Population-level metrics
        ptc = p_m["estimate"] * tx["estimate"]  # treated share of population
        pcc = p_m["estimate"] * ctrl["estimate"]  # controlled share of population
        add(name, "population_treatment_coverage_STEPS", ptc, "p_meas × p(tx|meas)")
        add(name, "population_control_coverage_STEPS", pcc, "p_meas × p(ctrl|meas)")
        add(
            name,
            "population_unmet_need_not_controlled",
            p_m["estimate"] * (1 - ctrl["estimate"]),
            "p_meas × (1 - control|meas)",
        )

    # --- Hybrid estimators ---
    # Hybrid treated-among-measured ≈ awareness_STEPS × treatment_among_dx_DHS
    for name, aware, dhs_tx, p_m in [
        ("Hypertension", htn_aware, dhs_htn_tx, htn_meas),
        ("Diabetes", dm_aware, dhs_dm_tx, dm_meas),
    ]:
        a = aware["estimate"]
        t = dhs_tx["estimate"]
        hybrid_tx_among_meas = a * t
        hybrid_ptc = p_m["estimate"] * hybrid_tx_among_meas
        add(name, "treatment_among_dx_DHS", t, format_pct(dhs_tx))
        add(
            name,
            "hybrid_treated_among_measured",
            hybrid_tx_among_meas,
            "a_STEPS × t_DHS (transportability)",
        )
        add(
            name,
            "hybrid_population_treatment_coverage",
            hybrid_ptc,
            "p_meas × a_STEPS × t_DHS",
        )
        add(
            name,
            "hybrid_population_untreated_disease",
            p_m["estimate"] * (1 - hybrid_tx_among_meas),
            "p_meas × (1 - a×t)",
        )

    # --- Sensitivity grid for HTN ---
    a0 = htn_aware["estimate"]
    t0 = dhs_htn_tx["estimate"]
    pm0 = htn_meas["estimate"]
    sens = []
    for a_mult in [0.8, 1.0, 1.2]:
        for t_mult in [0.8, 1.0, 1.2]:
            a = min(1.0, a0 * a_mult)
            t = min(1.0, t0 * t_mult)
            hybrid = a * t
            sens.append(
                {
                    "a_multiplier": a_mult,
                    "t_multiplier": t_mult,
                    "awareness": a,
                    "tx_among_dx": t,
                    "treated_among_measured": hybrid,
                    "population_tx_coverage": pm0 * hybrid,
                    "population_untreated_disease": pm0 * (1 - hybrid),
                    "unmet_need_not_controlled_proxy": pm0
                    * (1 - hybrid * 0.344),  # use STEPS control-among-treated ~34.4%
                }
            )
    sens_df = pd.DataFrame(sens)
    sens_df.to_csv(OUT_TABLES / "Table7_Hybrid_Sensitivity.csv", index=False)

    # --- Insurance context from DHS (for discussion / hybrid extension) ---
    add("Hypertension", "insured_among_dx_DHS", dhs_htn_ins["estimate"], format_pct(dhs_htn_ins))
    add("Diabetes", "insured_among_dx_DHS", dhs_dm_ins["estimate"], format_pct(dhs_dm_ins))

    # Insurance-stratified treatment if available in derived DHS
    dhs_path = OUT_DERIVED / "st12_dhs_analytic.pkl"
    if dhs_path.exists():
        import sys

        sys.path.insert(0, str(Path(__file__).resolve().parent))
        from st12_survey_utils import estimate_prop

        dadults = pd.read_pickle(dhs_path)
        htn = dadults[dadults["htn_dx"] == 1]
        for ins_val, lab in [(1, "insured"), (0, "uninsured")]:
            sub = htn[htn["insured_any"] == ins_val]
            if len(sub) > 30:
                r = estimate_prop(
                    sub.dropna(subset=["htn_treated_if_dx"]),
                    "htn_treated_if_dx",
                    "weight",
                    label=f"HTN tx among {lab} diagnosed",
                )
                add(
                    "Hypertension",
                    f"tx_among_dx_{lab}_DHS",
                    r["estimate"],
                    format_pct(r),
                )
                # hybrid with this t
                hybrid = a0 * r["estimate"]
                add(
                    "Hypertension",
                    f"hybrid_ptc_if_all_dx_were_{lab}",
                    pm0 * hybrid,
                    "Illustrative: apply this group's t to full measured pop after awareness",
                )

    out = pd.DataFrame(rows)
    out.to_csv(OUT_TABLES / "Table6_DualSurvey_Synthesis.csv", index=False)

    # Publication-ready comparison table
    pub = []
    for cond, pm, pd_, aware, tx_s, ctrl, tx_d, ins in [
        (
            "Hypertension",
            htn_meas,
            dhs_htn,
            htn_aware,
            htn_tx,
            htn_ctrl,
            dhs_htn_tx,
            dhs_htn_ins,
        ),
        (
            "Diabetes",
            dm_meas,
            dhs_dm,
            dm_aware,
            dm_tx,
            dm_ctrl,
            dhs_dm_tx,
            dhs_dm_ins,
        ),
    ]:
        dr = pd_["estimate"] / pm["estimate"]
        pub.append(
            {
                "Condition": cond,
                "Measured prevalence STEPS 2015 % (95% CI)": format_pct(pm),
                "Self-reported diagnosis KDHS 2022 % (95% CI)": format_pct(pd_),
                "Detection ratio": f"{dr:.2f}",
                "Aware among measured STEPS %": format_pct(aware),
                "Treated among measured STEPS %": format_pct(tx_s),
                "Controlled among measured STEPS %": format_pct(ctrl),
                "Treated among diagnosed KDHS %": format_pct(tx_d),
                "Insured among diagnosed KDHS %": format_pct(ins),
                "Hybrid treated among measured %": f"{100*aware['estimate']*tx_d['estimate']:.1f}",
                "Population unmet need (not controlled) STEPS %": f"{100*pm['estimate']*(1-ctrl['estimate']):.1f}",
            }
        )
    pd.DataFrame(pub).to_csv(OUT_TABLES / "Table6_DualSurvey_Publication.csv", index=False)

    summary = {
        "n_metrics": len(rows),
        "htn_detection_ratio": float(dhs_htn["estimate"] / htn_meas["estimate"]),
        "dm_detection_ratio": float(dhs_dm["estimate"] / dm_meas["estimate"]),
        "htn_pop_unmet_not_controlled_pct": float(
            100 * htn_meas["estimate"] * (1 - htn_ctrl["estimate"])
        ),
        "htn_hybrid_treated_among_measured_pct": float(
            100 * htn_aware["estimate"] * dhs_htn_tx["estimate"]
        ),
    }
    with open(OUT_LOGS / "st12_dual_summary.json", "w") as f:
        json.dump(summary, f, indent=2)
    print(json.dumps(summary, indent=2))
    print("Dual-survey synthesis written.")
    return summary


def format_pct(r):
    if r is None:
        return "—"
    return f"{r['pct']:.1f} ({r['pct_low']:.1f}–{r['pct_high']:.1f})"


if __name__ == "__main__":
    main()
