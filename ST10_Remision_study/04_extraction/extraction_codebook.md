# Data extraction codebook

Defines every field in `data_extraction_form.csv`. One row per **study** (add an arm
suffix to `study_id`, e.g. `Smith2019_armB`, only when a study contributes >2 arms).
Extract in duplicate (NWA + JO/NAO); a third reviewer adjudicates discrepancies.
Pilot on **5** studies before full extraction.

Conventions: leave a cell **blank** if not reported (do **not** enter 0); use `NR` only
where you want to distinguish "explicitly not reported" from "not yet extracted". Units
are fixed below — convert at extraction (e.g. HbA1c always mmol/mol; weight change in kg).

## Administrative
| Field | Definition / allowed values |
|-------|------------------------------|
| `study_id` | Unique key: FirstauthorYear (e.g. `Lean2018`). |
| `extractor` / `extraction_date` / `verified_by` | Initials; ISO date; second-extractor initials. |
| `first_author`, `year`, `journal`, `country`, `language` | As published. Multi-country: list all in `country`. |
| `wb_income_tier_at_conduct` | `low` / `lower-middle` / `upper-middle` — World Bank class in the recruitment year. |
| `registration_number`, `prospero_or_trial_reg` | Trial/PROSPERO IDs if any. |
| `multicountry` | `yes`/`no`. |
| `lmic_participant_pct` | % of enrolled sample from LMIC sites (eligibility ≥80%; sensitivity ≥50%). |

## Study design
| Field | Definition |
|-------|-----------|
| `design` | `parallel` / `cluster` / `crossover`. |
| `allocation_concealment`, `blinding` | As reported (free text or `adequate/unclear/none`). |
| `sample_size_total/_intervention/_comparator` | Randomised n. |
| `followup_duration_months` | Longest follow-up reported. |
| `setting_type` | `facility` / `community` / `digital-combined`. |
| `rural_urban` | `rural` / `urban` / `mixed`. |
| `single_multisite` | `single` / `multi`. |

## Population (baseline)
| Field | Unit |
|-------|------|
| `mean_age_years`, `age_sd` | years |
| `pct_female` | % |
| `ethnicity` | free text |
| `mean_bmi`, `bmi_sd` | kg/m² |
| `disease_duration_years` | years |
| `baseline_hba1c_mmolmol` | mmol/mol (convert from % if needed: mmol/mol = (%−2.15)×10.929) |
| `baseline_sbp_mmhg`, `baseline_dbp_mmhg` | mmHg |
| `comorbid_hypertension_pct` | % |
| `ses_proxy` | free text (income, education, insurance) |

## Intervention
| Field | Allowed values |
|-------|----------------|
| `intervention_category` | `diet_only` / `physical_activity_only` / `multicomponent`. |
| `diet_modality` | e.g. caloric restriction, VLCD, total diet replacement, low-carb, DASH, Mediterranean, plant-based. |
| `physical_activity_type` | aerobic / resistance / combined / NR. |
| `physical_activity_intensity` | as prescribed (free text). |
| `behavioural_support` | education / self-management / motivational interviewing / coaching / peer support / none. |
| `contact_frequency` | free text (e.g. weekly ×12 then monthly). |
| `intervention_duration_weeks` | weeks (eligibility ≥12; 8–12 in sensitivity). |
| `delivery_agent` | clinician / CHW / lay / peer / digital. |
| `delivery_platform` | facility / community / digital-combined. |
| `caloric_or_weightloss_target` | free text. |

## Comparator
| Field | Allowed values |
|-------|----------------|
| `comparator_type` | usual_care / active_control / waitlist / pharmacotherapy_only. |

## Primary outcomes
| Field | Definition |
|-------|-----------|
| `t2dm_remission_definition_used` | The study's own definition (verbatim/short). |
| `consistent_with_2021_consensus` | `yes` / `no` / `partial`. |
| `remission_ascertainment_source` | investigator / pharmacy / prescription / participant-report. |
| `n_remission_intervention`, `n_assessed_intervention` | events / denominator (intervention arm). |
| `n_remission_comparator`, `n_assessed_comparator` | events / denominator (comparator arm). |
| `remission_timepoint_months` | months off all glucose-lowering drugs at assessment (≥3). |
| `bp_normalisation_def_used` | study definition for drug-free BP normalisation. |
| `n_bpnorm_*`, `n_assessed_bp_*` | events / denominators per arm. |
| `bp_norm_timepoint_months` | months off antihypertensives at assessment (≥6). |

## Secondary outcomes
| Field | Definition |
|-------|-----------|
| `partial_remission_n_int/_comp` | partial remission events (HbA1c 39–47 off drugs). |
| `relapse_12m/24m/36m` | relapse counts/rates at each horizon. |
| `mean_weight_change_int/_comp` | kg. |
| `med_discontinuation_n_int/_comp` | participants discontinuing medication. |
| `cv_events_int/_comp` | fatal/non-fatal MI, stroke, HF counts. |
| `qol_measure`, `qol_result` | instrument + result. |
| `adverse_events_int/_comp` | severe hypoglycaemia, hypertensive crisis, any SAE. |

## Continuous outcomes (for WMD/SMD)
| Field | Unit |
|-------|------|
| `hba1c_change_mean_int/_sd_int/_mean_comp/_sd_comp` | mmol/mol |
| `sbp_change_mean_int/_sd_int/_mean_comp/_sd_comp` | mmHg |

## Statistical reporting
| Field | Definition |
|-------|-----------|
| `effect_measure_reported` | RR / OR / RD / MD / SMD as the study reported. |
| `effect_estimate`, `effect_ci_low`, `effect_ci_high` | point estimate + 95% CI. |
| `followup_completion_pct` | % completing follow-up. |
| `imputation_method` | any imputation used. |
| `notes` | anything needing adjudication or author contact. |

> The cleaned, analysis-ready long file derived from this form lives in
> `../06_analysis/data_input/` and is consumed by the R pipeline. See
> `../06_analysis/data_input/example_meta_input.csv` for the minimal columns the
> meta-analysis scripts expect.
