# Kenya DHS Studies

This repository contains reproducible source materials for DHS-based analyses of health insurance, financial protection, and service-use equity in Kenya.

The current public study package is:

- `ST03_NCD_Insurance_Service_Use/`
  - Insurance coverage and treatment gaps among adults with diagnosed hypertension or diabetes before Kenya's Social Health Authority transition

## Repository contents

- `ST03_NCD_Insurance_Service_Use/`: study-specific protocol notes, analysis scripts, and manuscript source
- `_TEMPLATES/`: shared Quarto and manuscript-formatting assets used by the study workflow

Only source materials are versioned. Rendered Word files, exported tables and figures, logs, and derived data are generated locally and are not committed.

## Structure

```text
Kenya_DHS_Studies/
|-- ST03_NCD_Insurance_Service_Use/
|   |-- README.md
|   |-- 01_protocol/
|   |-- 03_scripts/
|   `-- 06_manuscript/
|-- _TEMPLATES/
|-- README.md
|-- CONTRIBUTING.md
|-- CITATION.cff
|-- LICENSE
`-- .gitignore
```

## Reproducing ST03

Prerequisites:

- R 4.3 or newer
- Quarto
- Approved local access to KDHS 2022 recode files from the DHS Program

From the `ST03_NCD_Insurance_Service_Use` folder:

```bash
Rscript 03_scripts/run_st03_workflow.R
cd 06_manuscript
quarto render manuscript.qmd --to docx
```

Study-specific details are in [ST03_NCD_Insurance_Service_Use/README.md](ST03_NCD_Insurance_Service_Use/README.md).

## Data access

This repository does not include DHS microdata. Reproduction requires an approved DHS data-access request and local placement of the relevant KDHS 2022 files.

- DHS data portal: https://dhsprogram.com/data/
- Kenya DHS 2022 report: https://www.dhsprogram.com/pubs/pdf/FR380/FR380.pdf

## Citation

Please cite this repository using [CITATION.cff](CITATION.cff) and cite the relevant manuscript if you use study-specific methods or code.

Repository URL: https://github.com/gondamol/Kenya_DHS_Studies

## License

Code and repository infrastructure are released under the [MIT License](LICENSE).

## Contact

Nichodemus Werre Amollo  
Jaramogi Oginga Odinga University of Science and Technology  
Email: nwere@ejooust.ac.ke  
GitHub: https://github.com/gondamol

