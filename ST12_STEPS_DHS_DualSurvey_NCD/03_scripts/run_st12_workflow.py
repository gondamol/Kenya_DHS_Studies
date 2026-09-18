#!/usr/bin/env python3
"""Orchestrate ST12 analysis: STEPS → DHS → dual synthesis → figures."""
from __future__ import annotations

import runpy
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent


def run(script: str) -> None:
    path = HERE / script
    print(f"\n{'='*60}\nRunning {script}\n{'='*60}", flush=True)
    runpy.run_path(str(path), run_name="__main__")


def main():
    for s in [
        "01_steps_construct_analyze.py",
        "02_dhs_construct_analyze.py",
        "03_dual_survey_synthesis.py",
        "04_figures.py",
    ]:
        run(s)
    print("\nST12 workflow complete.")


if __name__ == "__main__":
    main()
