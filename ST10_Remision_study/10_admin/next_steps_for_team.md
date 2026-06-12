# Next steps — for the team (and supervisors)

This is the one-page orientation to share when bringing co-authors and supervisors in.
The review is **registered** (PROSPERO CRD420261409919) and the reproducible workspace
is **set up**. We have not yet run any searches.

## What's already in place
- Protocol (registered) — `01_protocol/`
- Complete, PRESS-ready search strategy + per-database string files + search log —
  `02_search/`
- Screening criteria (incl. exclusion codes) + Cohen's κ script — `03_screening/`
- Data extraction form + codebook + author-contact log — `04_extraction/`
- RoB 2 template + guidance — `05_risk_of_bias/`
- **Working R meta-analysis pipeline** (runs end-to-end; protocol guards verified) —
  `06_analysis/`
- GRADE SoF template — `07_grade/`
- PRISMA 2020 checklist + flow-diagram inputs — `08_reporting/`
- Roster, RACI, timeline, decision log — `10_admin/`

## What I need from co-authors / supervisors
1. **Confirm roles** against `roles_RACI.md`; flag anything you'd own differently.
2. **Medical librarian** — name someone to run the PRESS review (`02_search/`) and adapt
   the 13 database strategies. This is on the critical path.
3. **Third reviewer / adjudicator** — for screening, extraction, RoB, GRADE conflicts.
   Pencilled in for a supervisor.
4. **ORCID iDs** from everyone (needed for preprint + submission).
5. **Author order + CRediT** sign-off once supervisors are added.

## Decisions to make together
- **Hard deadline:** final search within 30 days of registration (~28 Jun 2026). Are we
  resourced to hit that, or do we log an amendment?
- **Repository home:** ST10 is a systematic review; the existing GitHub repo
  (`Kenya_DHS_Studies`) is a DHS-insurance compendium. Decide whether ST10 lives there,
  in a renamed broader compendium, or in its own repo. (Lead's note: an SR doesn't fit
  the "DHS microdata" scope cleanly — leaning toward a dedicated repo or a
  general-purpose "research compendium" rename. See `decision_log.md`.)
- **Preprint:** post the protocol to medRxiv now, or after search completion?

## How to work in this folder
- Read each subfolder's `README.md`; it says exactly what goes where.
- Two-reviewer steps are done **independently, then reconciled** — don't peek.
- Keep data as the CSV templates; run analysis with
  `Rscript 06_analysis/R/run_meta_workflow.R`.
- Log every methods decision in `decision_log.md`; log protocol changes in
  `../01_protocol/amendments_log.md` **and** on PROSPERO.
