# Kenya DHS Studies

This repository is a compendium of reproducible source materials for health research in Kenya and comparable low- and middle-income settings: DHS-based analyses of health insurance, financial protection, service use, and disability-related equity, alongside evidence-synthesis studies on non-communicable disease care.

The current public study packages are:

- `ST02_Disability_Insurance_Equity/`
  - Insurance coverage, service use, and payment at outpatient contact among adults with disability before Kenya's Social Health Authority transition
- `ST03_NCD_Insurance_Service_Use/`
  - Insurance coverage and treatment gaps among adults with diagnosed hypertension or diabetes before Kenya's Social Health Authority transition
- `ST10_Remision_study/`
  - Systematic review and meta-analysis: lifestyle modification for type 2 diabetes remission and drug-free hypertension normalisation in low- and middle-income countries (PROSPERO CRD420261409919). Uses no DHS microdata.

## Repository contents

- `ST02_Disability_Insurance_Equity/`: study-specific notes, analysis scripts, and manuscript source
- `ST03_NCD_Insurance_Service_Use/`: study-specific protocol notes, analysis scripts, and manuscript source
- `ST10_Remision_study/`: registered systematic-review protocol, search strategies, screening/extraction/risk-of-bias templates, and an R meta-analysis pipeline (see its `README.md`)
- `_TEMPLATES/`: shared Quarto and manuscript-formatting assets used by the study workflow

Only source materials are versioned. Rendered Word files, exported tables and figures, logs, and derived data are generated locally and are not committed.

## Structure

```text
Kenya_DHS_Studies/
|-- ST02_Disability_Insurance_Equity/
|   |-- README.md
|   |-- 02_data_notes/
|   |-- 03_scripts/
|   `-- 06_manuscript/
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

## Reproducing ST02 or ST03

Prerequisites:

- R 4.3 or newer
- Quarto
- Approved local access to KDHS 2022 recode files from the DHS Program

From a study folder such as `ST02_Disability_Insurance_Equity` or `ST03_NCD_Insurance_Service_Use`:

```bash
Rscript 03_scripts/run_<study>_workflow.R
cd 06_manuscript
quarto render manuscript.qmd --to docx
```

Study-specific details are in [ST02_Disability_Insurance_Equity/README.md](ST02_Disability_Insurance_Equity/README.md) and [ST03_NCD_Insurance_Service_Use/README.md](ST03_NCD_Insurance_Service_Use/README.md).

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

