# 03_screening — Title/abstract & full-text screening

Two independent reviewers (NWA, JO), with third-reviewer adjudication.

## Files
- **`screening_criteria.md`** — the include/exclude decision rules + exclusion codes
  (E1–E9). Both reviewers apply these independently.
- **`screening_pilot_log.md`** — calibration record; full screening starts only after
  **Cohen's κ ≥ 0.75**.
- **`inter_rater_kappa.R`** — computes κ (with 95% CI) from a paired-decision export.
- **`excluded_fulltext_reasons.csv`** — every full-text exclusion with its coded reason
  (becomes a PRISMA supplementary file).

## Process
1. Import the deduplicated set into Rayyan (blind mode on).
2. **Pilot** Stage 1 on 50 random records; run `inter_rater_kappa.R`; log in pilot log.
3. Screen all titles/abstracts independently; resolve conflicts.
4. Retrieve full texts of passing records; screen against full criteria; record the
   primary exclusion code for each exclusion.
5. Export decision counts into `../08_reporting/prisma_flow_counts.csv`.

> Tool note: Rayyan exports CSV with reviewer decision columns — point
> `inter_rater_kappa.R` at that file and set the column names at the top of the script.
