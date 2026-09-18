#!/usr/bin/env python3
"""ST12 publication figures."""
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib

matplotlib.use("Agg")
import numpy as np
import pandas as pd

STUDY_ROOT = Path(__file__).resolve().parent.parent
OUT_TABLES = STUDY_ROOT / "04_tables"
OUT_FIG = STUDY_ROOT / "05_figures"
OUT_FIG.mkdir(parents=True, exist_ok=True)

# Portfolio-consistent style
plt.rcParams.update(
    {
        "font.family": "DejaVu Sans",
        "font.size": 10,
        "axes.spines.top": False,
        "axes.spines.right": False,
        "figure.dpi": 150,
        "savefig.dpi": 300,
        "savefig.bbox": "tight",
    }
)


def fig1_cascade():
    """Horizontal cascade bars for HTN and DM (STEPS)."""
    cas = pd.read_csv(OUT_TABLES / "Table2_STEPS_Cascade.csv")
    key = pd.read_csv(OUT_TABLES / "Table1_STEPS_KeyEstimates.csv")
    # Add prevalence as first stage of absolute cascade (% of population)
    htn_prev = key.loc[key["label"].str.contains("Measured hypertension"), "pct"].iloc[0]
    dm_prev = key.loc[key["label"].str.contains("Measured diabetes"), "pct"].iloc[0]

    fig, axes = plt.subplots(1, 2, figsize=(10, 4.2), sharey=False)
    for ax, cond, prev in zip(axes, ["Hypertension", "Diabetes"], [htn_prev, dm_prev]):
        sub = cas[cas["condition"] == cond]
        # Two views: among disease (cascade %)
        stages = ["Aware", "Treated", "Controlled"]
        vals = [sub.loc[sub["stage"] == s, "pct"].iloc[0] for s in stages]
        lo = [sub.loc[sub["stage"] == s, "pct_low"].iloc[0] for s in stages]
        hi = [sub.loc[sub["stage"] == s, "pct_high"].iloc[0] for s in stages]
        y = np.arange(len(stages))
        colors = ["#3B6D9A", "#C47A2C", "#2E7D4F"]
        ax.barh(y, vals, color=colors, height=0.6, edgecolor="white")
        ax.errorbar(
            vals,
            y,
            xerr=[np.array(vals) - np.array(lo), np.array(hi) - np.array(vals)],
            fmt="none",
            ecolor="#333333",
            capsize=3,
            lw=1,
        )
        for i, v in enumerate(vals):
            ax.text(min(v + 3, 92), i, f"{v:.1f}%", va="center", fontsize=9)
        ax.set_yticks(y)
        ax.set_yticklabels(stages)
        ax.set_xlim(0, 100)
        ax.set_xlabel("% of adults with measured disease")
        ax.set_title(f"{cond}\n(prevalence {prev:.1f}% of adults 18–69)", fontsize=11)
        ax.axvline(100, color="#cccccc", lw=0.5)
        # annotate leakage
        ax.text(
            0.98,
            0.02,
            f"Not controlled: {100-vals[2]:.1f}% of disease",
            transform=ax.transAxes,
            ha="right",
            va="bottom",
            fontsize=8,
            color="#555555",
        )
    fig.suptitle(
        "Kenya STEPS 2015: measured NCD care cascade",
        fontsize=12,
        fontweight="bold",
        y=1.02,
    )
    fig.tight_layout()
    fig.savefig(OUT_FIG / "Figure1_STEPS_Cascade.png")
    fig.savefig(OUT_FIG / "Figure1_STEPS_Cascade.tiff")
    plt.close(fig)
    print("Figure 1 saved")


def fig2_sex_cascade():
    """Sex-stratified HTN cascade."""
    t = pd.read_csv(OUT_TABLES / "Table4_STEPS_Cascade_ByStratum.csv")
    sub = t[(t["condition"] == "Hypertension") & (t["stratifier"] == "Sex")]
    stages = ["Prevalence", "Aware", "Treated", "Controlled"]
    x = np.arange(len(stages))
    width = 0.35
    fig, ax = plt.subplots(figsize=(8, 4.5))
    for i, (sex, color) in enumerate([("Men", "#3B6D9A"), ("Women", "#B85C38")]):
        s = sub[sub["category"] == sex]
        vals = [s.loc[s["stage"] == st, "pct"].iloc[0] for st in stages]
        lo = [s.loc[s["stage"] == st, "pct_low"].iloc[0] for st in stages]
        hi = [s.loc[s["stage"] == st, "pct_high"].iloc[0] for st in stages]
        pos = x + (i - 0.5) * width
        ax.bar(pos, vals, width, label=sex, color=color, edgecolor="white")
        ax.errorbar(
            pos,
            vals,
            yerr=[np.array(vals) - np.array(lo), np.array(hi) - np.array(vals)],
            fmt="none",
            ecolor="#333",
            capsize=2,
            lw=0.9,
        )
    ax.set_xticks(x)
    ax.set_xticklabels(stages)
    ax.set_ylabel("Percent")
    ax.set_ylim(0, 45)
    ax.legend(frameon=False)
    ax.set_title(
        "Hypertension cascade by sex, Kenya STEPS 2015\n"
        "(Aware/Treated/Controlled are % of those with measured hypertension)"
    )
    # Note: Prevalence is % of all adults; other stages % of HTN — different denominators
    ax.text(
        0.01,
        -0.18,
        "Note: Prevalence uses all adults as denominator; Aware/Treated/Controlled use hypertensives.",
        transform=ax.transAxes,
        fontsize=8,
        color="#555",
    )
    fig.tight_layout()
    fig.savefig(OUT_FIG / "Figure2_HTN_Cascade_BySex.png")
    fig.savefig(OUT_FIG / "Figure2_HTN_Cascade_BySex.tiff")
    plt.close(fig)
    print("Figure 2 saved")


def fig3_dual_survey():
    """Comparison of STEPS measured vs DHS self-report + cascade schematic."""
    dual = pd.read_csv(OUT_TABLES / "Table6_DualSurvey_Publication.csv")
    fig, axes = plt.subplots(1, 2, figsize=(10, 4.5))

    for ax, cond in zip(axes, ["Hypertension", "Diabetes"]):
        row = dual[dual["Condition"] == cond].iloc[0]
        # parse measured and dx
        def parse_first(s):
            return float(str(s).split()[0])

        meas = parse_first(row["Measured prevalence STEPS 2015 % (95% CI)"])
        dx = parse_first(row["Self-reported diagnosis KDHS 2022 % (95% CI)"])
        labels = ["Measured\n(STEPS 2015)", "Self-reported\ndiagnosis\n(KDHS 2022)"]
        vals = [meas, dx]
        colors = ["#2E7D4F", "#C47A2C"]
        bars = ax.bar(labels, vals, color=colors, width=0.55, edgecolor="white")
        for b, v in zip(bars, vals):
            ax.text(b.get_x() + b.get_width() / 2, v + 0.3, f"{v:.1f}%", ha="center", fontsize=10)
        ax.set_ylabel("Prevalence among adults (%)")
        dr = float(row["Detection ratio"])
        ax.set_title(f"{cond}\nDetection ratio = {dr:.2f}")
        ymax = max(vals) * 1.35
        ax.set_ylim(0, ymax if ymax > 0 else 1)
        # arrow annotation
        if meas > 0:
            udi = max(0, 1 - dr)
            ax.annotate(
                f"Cross-survey\nunder-capture ≈ {100*udi:.0f}%",
                xy=(0.5, min(meas, dx) / 2),
                xytext=(0.5, max(vals) * 0.75),
                ha="center",
                fontsize=8,
                color="#444",
                arrowprops=dict(arrowstyle="->", color="#888"),
            )
    fig.suptitle(
        "Dual-survey detection comparison (not a time trend)",
        fontsize=12,
        fontweight="bold",
        y=1.02,
    )
    fig.tight_layout()
    fig.savefig(OUT_FIG / "Figure3_DualSurvey_Detection.png")
    fig.savefig(OUT_FIG / "Figure3_DualSurvey_Detection.tiff")
    plt.close(fig)
    print("Figure 3 saved")


def fig4_population_unmet():
    """Population-level cascade: absolute % of all adults."""
    key = pd.read_csv(OUT_TABLES / "Table1_STEPS_KeyEstimates.csv")
    htn_p = key.loc[key["label"].str.contains("Measured hypertension"), "estimate"].iloc[0]
    htn_a = key.loc[key["label"].str.contains("HTN: aware"), "estimate"].iloc[0]
    htn_t = key.loc[key["label"].str.contains("HTN: on medication"), "estimate"].iloc[0]
    htn_c = key.loc[key["label"].str.contains("HTN: controlled \\(among all"), "estimate"].iloc[0]
    # absolute population fractions
    stages = [
        "Has measured\nhypertension",
        "Aware",
        "On treatment",
        "Controlled",
    ]
    vals = np.array(
        [
            100 * htn_p,
            100 * htn_p * htn_a,
            100 * htn_p * htn_t,
            100 * htn_p * htn_c,
        ]
    )
    fig, ax = plt.subplots(figsize=(7.5, 4.2))
    colors = ["#8B1E3F", "#C47A2C", "#3B6D9A", "#2E7D4F"]
    bars = ax.bar(stages, vals, color=colors, edgecolor="white", width=0.65)
    for b, v in zip(bars, vals):
        ax.text(b.get_x() + b.get_width() / 2, v + 0.15, f"{v:.1f}%", ha="center", fontsize=10)
    ax.set_ylabel("% of all adults aged 18–69")
    ax.set_title(
        "Population-level hypertension cascade, Kenya STEPS 2015\n"
        "Only 0.6% of adults had controlled hypertension"
    )
    ax.set_ylim(0, max(vals) * 1.25)
    fig.tight_layout()
    fig.savefig(OUT_FIG / "Figure4_Population_HTN_Cascade.png")
    fig.savefig(OUT_FIG / "Figure4_Population_HTN_Cascade.tiff")
    plt.close(fig)
    print("Figure 4 saved")


def main():
    fig1_cascade()
    fig2_sex_cascade()
    # fig3 needs dual table
    dual_path = OUT_TABLES / "Table6_DualSurvey_Publication.csv"
    if dual_path.exists():
        fig3_dual_survey()
    else:
        print("Skipping Figure 3 — dual table not yet available")
    fig4_population_unmet()
    print("Figures complete:", OUT_FIG)


if __name__ == "__main__":
    main()
