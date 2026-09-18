"""
ST13 — figures.

Two figures carry the paper's argument.

Figure 1 sets measured prevalence beside self-reported diagnosis for the same
population. It is the whole point of the study in one panel: men have the
higher measured burden yet appear far less often in the self-report denominator
that financing analyses use.

Figure 2 expresses that as the detection ratio, the fraction of measured disease
visible to self-report, with a reference line at 1.0 for complete detection.

Both are drawn on the age-comparable 18-49 base, the study's primary analysis,
because KDHS truncates women at 49 and men at 54 while STEPS runs to 69.

Outputs: 05_figures/Figure1_Measured_vs_SelfReport.{png,tiff}
         05_figures/Figure2_Detection_Ratio_By_Sex.{png,tiff}
"""
from __future__ import annotations

import sys
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

STUDY_ROOT = Path(__file__).resolve().parents[1]
TABLES = STUDY_ROOT / "04_tables"
FIGS = STUDY_ROOT / "05_figures"
FIGS.mkdir(parents=True, exist_ok=True)

PRIMARY = "18-49 (age-comparable, primary)"
ORDER = ["Overall", "Women", "Men"]

# Okabe-Ito: distinguishable in greyscale and to colour-vision-deficient readers
C_MEAS = "#0072B2"   # measured, STEPS
C_SELF = "#D55E00"   # self-reported, KDHS
C_DOT = "#333333"

plt.rcParams.update({
    "font.family": "DejaVu Sans",
    "axes.spines.top": False,
    "axes.spines.right": False,
    "axes.grid": True,
    "grid.alpha": 0.25,
    "grid.linestyle": "-",
    "axes.axisbelow": True,
})


def load() -> pd.DataFrame:
    d = pd.read_csv(TABLES / "Table1_Measured_vs_SelfReport.csv")
    return d[(d.base == PRIMARY) & (d.condition == "htn")]


def pick(d: pd.DataFrame, quantity: str) -> pd.DataFrame:
    s = d[d.quantity == quantity].set_index("subgroup")
    return s.reindex(ORDER)


def fig1(d: pd.DataFrame) -> None:
    meas, self_ = pick(d, "p_meas"), pick(d, "p_dx")
    x = np.arange(len(ORDER))
    w = 0.36
    fig, ax = plt.subplots(figsize=(8.2, 5.0))

    for off, src, colour, label in [(-w/2, meas, C_MEAS, "Measured (STEPS 2015)"),
                                    (w/2, self_, C_SELF, "Self-reported diagnosis (KDHS 2022)")]:
        est = 100 * src.estimate.to_numpy()
        lo = est - 100 * src.ci_low.to_numpy()
        hi = 100 * src.ci_high.to_numpy() - est
        ax.bar(x + off, est, w, color=colour, label=label, zorder=2)
        ax.errorbar(x + off, est, yerr=[lo, hi], fmt="none",
                    ecolor="#222222", capsize=3.5, lw=1.1, zorder=3)
        for xi, e, h in zip(x + off, est, 100 * src.ci_high.to_numpy()):
            ax.text(xi, h + 0.6, f"{e:.1f}%", ha="center", va="bottom", fontsize=8.5,
                    color="#333333")

    # the sex contrast, annotated clear of the value labels
    ax.annotate("", xy=(2 - w/2, 28.2), xytext=(2 + w/2, 28.2),
                arrowprops=dict(arrowstyle="<->", color="#666666", lw=1.0))
    ax.text(2, 28.7, "men: highest measured burden,\nlowest self-report",
            ha="center", va="bottom", fontsize=8.5, color="#444444", style="italic")

    ax.set_xticks(x)
    ax.set_xticklabels(ORDER)
    ax.set_ylabel("Hypertension prevalence (%)")
    ax.set_ylim(0, 32)
    ax.set_title("Measured hypertension versus self-reported diagnosis, adults aged 18–49",
                 fontweight="bold", fontsize=11.5, loc="left")
    ax.legend(frameon=False, loc="upper left", fontsize=9)
    fig.text(0.02, -0.02,
             "Kenya STEPS 2015 (measured blood pressure) and KDHS 2022 (self-report only), "
             "restricted to the overlapping 18–49 age window.\nBars are survey-weighted "
             "prevalence; whiskers are 95% confidence intervals from a stratified PSU "
             "bootstrap with the two surveys\nresampled independently. The surveys are seven "
             "years apart, so the gap compares measurement systems, not time.",
             fontsize=7.5, color="#555555", ha="left", va="top")
    fig.tight_layout()
    for ext, kw in (("png", {}), ("tiff", {"pil_kwargs": {"compression": "tiff_lzw"}})):
        fig.savefig(FIGS / f"Figure1_Measured_vs_SelfReport.{ext}", dpi=300,
                    bbox_inches="tight", **kw)
    plt.close(fig)
    print("wrote Figure1_Measured_vs_SelfReport")


def fig2(d: pd.DataFrame) -> None:
    dr = pick(d, "detection_ratio")
    y = np.arange(len(ORDER))[::-1]
    est = dr.estimate.to_numpy()
    lo, hi = dr.ci_low.to_numpy(), dr.ci_high.to_numpy()

    fig, ax = plt.subplots(figsize=(8.2, 3.9))
    ax.axvline(1.0, color="#999999", lw=1.0, ls="--", zorder=1)
    ax.text(1.0, len(ORDER) - 0.35, "complete detection", rotation=90,
            va="top", ha="right", fontsize=8, color="#777777")

    ax.hlines(y, lo, hi, color=C_DOT, lw=1.6, zorder=2)
    ax.plot(est, y, "o", ms=8, color=C_DOT, zorder=3)
    for yi, e, h in zip(y, est, hi):
        ax.text(h + 0.02, yi, f"{e:.3f}", va="center", ha="left", fontsize=9.5,
                fontweight="bold", color="#222222")

    ax.set_yticks(y)
    ax.set_yticklabels(ORDER)
    ax.set_xlim(0, 1.12)
    ax.set_xlabel("Detection ratio  (self-reported diagnosis ÷ measured prevalence)")
    ax.set_title("How much hypertension self-report captures, by sex",
                 fontweight="bold", fontsize=11.5, loc="left")
    fig.text(0.02, -0.04,
             "A ratio of 1.0 would mean every measured case appears in the self-report "
             "denominator. Among men it is 0.151: roughly six in seven\nmen with "
             "hypertension are invisible to a survey that can only ask. Whiskers are 95% "
             "confidence intervals. Age-comparable 18–49 base.",
             fontsize=7.5, color="#555555", ha="left", va="top")
    fig.tight_layout()
    for ext, kw in (("png", {}), ("tiff", {"pil_kwargs": {"compression": "tiff_lzw"}})):
        fig.savefig(FIGS / f"Figure2_Detection_Ratio_By_Sex.{ext}", dpi=300,
                    bbox_inches="tight", **kw)
    plt.close(fig)
    print("wrote Figure2_Detection_Ratio_By_Sex")


def main() -> int:
    d = load()
    if d.empty:
        print("no rows for the primary base; run 03_underdiagnosis_analysis.py first")
        return 1
    fig1(d)
    fig2(d)
    print(f"figures in {FIGS}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
