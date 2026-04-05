# Contributing

This repository is a reproducible research repository. Contributions are welcome, but they need to preserve scientific traceability and data restrictions.

## Core rules

- Do not commit DHS microdata or restricted derivatives.
- Keep analytical workflows reproducible from source.
- Open an issue before making major structural or methodological changes.
- Update documentation when code or study design changes.

## What to contribute

- bug fixes in analysis code
- documentation improvements
- reusable helper functions
- workflow hardening and validation checks

## Before opening a pull request

1. Run the relevant study workflow end to end.
2. Re-render the manuscript if analytical or textual content changed.
3. Confirm that no restricted data are staged.
4. Update the relevant `README.md` files.

## Commit style

Use short, descriptive commit messages such as:

- `Fix ST03 table caption rendering`
- `Clarify ST03 concentration index methods`
- `Harden Quarto workflow path detection`
