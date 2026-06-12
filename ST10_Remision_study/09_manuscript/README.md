# 09_manuscript — Protocol manuscript (Quarto)

Reproducible source for the **protocol paper** (suitable for a journal protocol/registered
report or a medRxiv preprint). Written from `manuscript.qmd` and rendered to a formatted
Word document via the shared `_TEMPLATES/` assets — the same workflow as ST02/ST03.

## Files
- **`manuscript.qmd`** — the manuscript source (seeded from the registered protocol).
- **`references.bib`** — the 23 references as BibTeX.
- `manuscript.docx` — rendered output (git-ignored; regenerate with the command below).

## Render
```bash
cd 09_manuscript
quarto render manuscript.qmd --to docx   # uses ../../_TEMPLATES/reference_doc.docx + vancouver.csl
# or:  quarto render manuscript.qmd --to html
```
No R chunks are used, so rendering is pure Pandoc (fast, no analysis dependencies). When
the review reports results, add `{r}` chunks that read objects from
`../06_analysis/results/` to embed forest plots and pooled estimates.

## Before submission — open items
- **ORCID iDs** for LH, NAO, JO (NWA's is in the YAML). Add under each author's block.
- **Funding statement** — currently a placeholder marked `[TO CONFIRM]` in two places
  (YAML `funding:` and the Declarations section); confirm with co-authors.
- **Verify the Baena 2014 blood-pressure CI figures** against the source — the registered
  protocol's reported confidence intervals for the SBP reductions appear garbled
  (point estimates 11.4 and 6.0 mmHg are reproduced here; the CIs were omitted pending
  a check against the original paper).
- Confirm target journal, then add the matching Quarto journal extension to `format:`.

> The content mirrors the registered protocol (PROSPERO CRD420261409919). Any change to
> registered methods must also be logged in `../01_protocol/amendments_log.md` and on
> PROSPERO.
