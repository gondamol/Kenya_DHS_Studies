# 04_extraction — Data extraction

Duplicate extraction (NWA + JO/NAO); third-reviewer adjudication; pilot on 5 studies.

## Files
- **`data_extraction_form.csv`** — the extraction template (one row per study). Open in
  Excel or any editor; keep it CSV for version control.
- **`extraction_codebook.md`** — defines every field, units, and allowed values. **Read
  this before extracting.**
- **`author_contact_log.csv`** — log of requests to corresponding authors for missing
  data (≥2 attempts over 30 days per protocol).

## Workflow
1. Pilot on 5 randomly selected eligible studies; reconcile; refine the codebook.
2. Two reviewers extract every study independently into separate copies.
3. Reconcile discrepancies (track in `notes`); adjudicate if needed.
4. Where data are missing, log an author request in `author_contact_log.csv`.
5. Produce the cleaned analysis-ready file for `../06_analysis/data_input/`.

## Units (enforced at extraction)
- HbA1c → **mmol/mol** (convert from %: `mmol/mol = (% − 2.15) × 10.929`).
- Weight change → **kg**; BP → **mmHg**; duration → as specified per field.
