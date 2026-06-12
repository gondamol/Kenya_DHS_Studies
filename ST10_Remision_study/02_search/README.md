# 02_search — Search strategy and execution

This folder holds everything needed to run, document, and peer-review the literature
search across the 13 sources named in the protocol.

## Files

- **`search_strategy_master.md`** — the conceptual strategy (7 search sets) and the
  full MEDLINE/PubMed line-by-line search. This is the master from which every other
  database is adapted.
- **`strings/`** — one plain-text file per database with the *exact* string run, so
  the search is fully reproducible and auditable.
- **`database_search_log.csv`** — the running log: for each database, the date run,
  who ran it, interface/coverage, hits returned, and export filename. **Fill one row
  per database per run.** The final search is run within 30 days of PROSPERO
  registration (i.e. by ~28 June 2026) — see the protocol.
- **`PRESS_peer_review_checklist.md`** — the PRESS 2015 form a medical librarian
  completes *before* the searches are executed.
- **`deduplication_log.md`** — record of records imported, duplicates removed
  (automatic vs manual), and the net unique count carried into screening.

## Order of operations

1. Finalise the master strategy → get it **PRESS peer-reviewed** (complete the
   checklist) → revise.
2. Adapt the master to each database (`strings/`), respecting each interface's syntax
   and controlled vocabulary (MeSH, Emtree, CINAHL headings).
3. Run each search; record a row in `database_search_log.csv`; export results.
4. Import all exports into Rayyan; deduplicate; complete `deduplication_log.md`.
5. Hand the deduplicated set to screening (`../03_screening/`).

## Notes carried from the protocol

- Search **from inception** to the final-search date, no date or language limit.
- Conference-abstract-only records are excluded at full text (insufficient detail).
- Supplement database searching with **forward + backward citation chasing** on all
  included full texts and on Sherifali 2025, O'Donoghue 2021, Sagastume 2022.
- The Cochrane **EPOC LMIC filter** is used for MEDLINE/Embase only; region-specific
  databases (AJOL, IndMED, LILACS) rely on native indexing + country/free-text terms,
  documented per database in `strings/`.
