# ST09 geospatial module

Closes the subnational mapping gap raised in `../Professor_Critical_Review.md`, which asked for
high-resolution geostatistical modelling of insurance exclusion rather than estimates across the
eight former provinces.

## Pipeline

```bash
Rscript 09_geospatial/01_build_cluster_data.R      # PR -> cluster aggregates + GPS join
Rscript 09_geospatial/02_fit_spatial_models.R      # three nested binomial GAMs
Rscript 09_geospatial/03_predict_and_map.R         # 5 km surface, exceedance, Figures 8a-8d
Rscript 09_geospatial/04_county_and_calibration.R  # 47-county estimates, calibration, Figure 8e
```

Requires `sf`, `terra`, `mgcv`, `survey`, `haven`, `dplyr`, `ggplot2`, `viridis`, `gstat`.

## Data

- KDHS 2022 person recode (`KEPR8CFL.DTA`) — same restriction as the main analysis: de-facto
  household members with a valid `sh27` response (75,561 persons, reproducing the published n).
- DHS GPS cluster coordinates: `01_DHS_Data/gps data/KEGE8AFL/KEGE8AFL.shp`. **Not** the folder
  named `KDHS_2022/GPS/`, which despite its name contains `KEGR8CFL`, the pregnancy recode, and a
  `README.txt` reading only "placeholder".
- County boundaries: geoBoundaries KEN ADM1 (47 counties), downloaded to `data/kenya_adm1.geojson`.

All 1,691 clusters carry valid coordinates; none required exclusion for a missing fix.

## Models

| Model | Specification | Deviance explained |
|-------|---------------|--------------------|
| M1 | `s(X, Y)` | 50.3% |
| M2 | `s(X, Y) + urban/rural` | 55.3% |
| M3 | `s(X, Y) + urban/rural + s(wealth)` | 68.1% |

The spatial smooth stays strongly significant in M3 (edf 207.4, chi-squared 3116, p < 0.001), so
location predicts exclusion beyond settlement type and wealth composition. That residual surface is
Figure 8d and is the substantive answer to the reviewer's point.

## Three deliberate methodological choices

**1. Penalised GAMs, not INLA-SPDE or kriging.** DHS displaces cluster coordinates before release —
up to 2 km urban, 5 km rural, 1% of rural up to 10 km (Burgert et al. 2013). Structure finer than
that is destroyed by design, so a method marketed on sub-kilometre resolution would be fitting
noise. A Duchon spline with REML-selected smoothing recovers structure at the scale the data
support, installs from CRAN, and supplies the standard errors the exceedance map needs. The 5 km
grid is a rendering choice; the surface is interpreted at ~10 km and coarser.

**2. Hotspots are exceedance probabilities, not thresholded point estimates.** Figure 8b maps
P(exclusion > 90%). Thresholding point estimates would flag the most sparsely sampled areas, where
estimates are least certain — exactly backwards for a targeting map.

**3. Predictions are masked to within 50 km of a sampled cluster.** 22,136 of 23,634 land cells
(93.7%) qualify; the rest are drawn grey as "not estimated" rather than left as holes.

## Calibration, and one thing that looks wrong but is not

The model reproduces the national figure: population-weighted 74.1% predicted against 73.9%
observed, with a maximum decile error of 2.2 percentage points.

The mapped surface has an **area-based median of 92.8%**, well above the national 73.9%. These are
different estimands, not a discrepancy. The surface is area-weighted and drawn for rural settlement
at mean wealth; Kenya's land area is dominated by sparsely populated arid counties where coverage is
lowest, while the national figure is population-weighted. `outputs/calibration.txt` records this.

## Results worth knowing

- 55.6% of the sampled land area has at least an 80% probability of exceeding 90% exclusion.
- County estimates span 40.7 percentage points: Tana River 94.7% (92.4-97.0) uninsured, Mandera
  94.0%, Marsabit 93.1%, down to Nairobi 54.0% (47.8-60.2), Nyeri 57.3%, Kiambu 57.8%.
- All 47 counties have design-based 95% intervals narrower than 20 pp, so county reporting is
  supportable directly from this survey — no small-area model is needed to rescue precision.
- Note that `hv024` already carries county in KDHS 2022. The county dimension was available without
  GPS all along; the main analysis collapsed it to eight provinces. The GPS data is what adds the
  continuous surface and the exceedance map.

## Outputs

- `05_figures/Figure8a_Predicted_Uninsured_Surface.*` — predicted surface
- `05_figures/Figure8b_Exclusion_Hotspots.*` — exceedance probability
- `05_figures/Figure8c_Observed_Clusters.*` — observed cluster values
- `05_figures/Figure8d_Residual_Place_Effect.*` — geography net of urbanicity and wealth
- `05_figures/Figure8e_County_Choropleth.*` — 47-county direct estimates
- `04_tables/Table17_County_Insurance_Exclusion.csv`
- `09_geospatial/outputs/model_summary.txt`, `outputs/calibration.txt`
