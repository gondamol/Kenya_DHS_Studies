# Target journal & submission notes

## Recommended: *Systematic Reviews* (BMC)
The natural home for a registered SR/MA **protocol**.

| Why it fits | Detail |
|-------------|--------|
| Scope | Explicitly publishes systematic-review *protocols* (and the later review). |
| Reporting | Built around PRISMA-P 2015 — which this protocol already follows item-by-item (Appendix A). |
| Indexing | MEDLINE, PMC, Scopus, DOAJ; open access. |
| Fit with track record | Same publisher family as the team's BMC Public Health [@amollo2026financial] and BMC Health Services Research [@ogol2025] papers. |
| Format | Accepts Word submission; structured abstract + BMC declarations — the current `manuscript.docx` already matches this structure. |

**Article type:** Protocol. **APC:** BMC open-access fee applies (check for a waiver — JOOUST/Kenya may qualify for the BMC waiver/discount programme).

## Alternative: *BMJ Open*
Also publishes protocols; broad clinical/public-health readership; requires registration (have PROSPERO ✓). Slightly broader scope, strong visibility. Structured abstract differs slightly.

## Other possibilities
- *BMC Public Health* / *BMC Endocrine Disorders* — if positioned as a public-health or endocrine review respectively (less protocol-focused).
- *PLOS ONE* — accepts protocols, very broad, high visibility.

## Pre-submission checklist (for *Systematic Reviews*)
- [ ] PROSPERO registration number in abstract + methods — **done** (CRD420261409919)
- [ ] PRISMA-P 2015 checklist as an additional file — **done** (Appendix A; also `../08_reporting`)
- [ ] Full MEDLINE search strategy as an additional file — **done** (`../02_search/strings/01_MEDLINE_PubMed.txt`)
- [ ] Structured abstract (Background/Methods/Discussion) — **done**
- [ ] Declarations (ethics, consent, data availability, competing interests, funding, contributions) — **done** (confirm funding + ORCIDs)
- [ ] ORCID iDs for all authors — **pending** (NWA done)
- [ ] Cover letter — **done** (`cover_letter.md`)
- [ ] Suggested reviewers (3–5, no conflicts) — to compile
- [ ] medRxiv preprint posted + DOI cited — optional, recommended for early visibility

## Formatting note
No LaTeX journal extension is needed: BMC accepts Word, and the rendered `manuscript.docx`
already carries the structured abstract, numbered sections, BMC-style declarations, and
Vancouver references. If a typeset preprint is wanted, the Quarto `nature-pdf` extension
(`quarto add christopherkenny/nature`) gives a Springer-Nature-styled PDF from the same
source.
