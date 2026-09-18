# ST09 geospatial — stage 4: county estimates and model calibration
#
# Two jobs.
#
# (1) Calibration. The prediction surface is AREA-based and drawn for rural
#     settlement at mean wealth, so its median (~93% uninsured) is not
#     comparable with the national POPULATION-weighted figure (73.9%). Kenya's
#     land area is dominated by sparsely populated arid counties where coverage
#     is lowest, so an area median above the population mean is expected, not a
#     sign of miscalibration. The honest check is at cluster locations against
#     observed counts, which is what this script does.
#
# (2) County estimates. The main paper reports 8 former provinces. The DHS GPS
#     file and hv024 both carry all 47 counties, so this produces design-based
#     direct estimates per county alongside model-based ones, and flags counties
#     where the direct estimate is too imprecise to stand alone.
#
# Outputs: 04_tables/Table17_County_Insurance_Exclusion.csv
#          09_geospatial/outputs/calibration.txt
#          05_figures/Figure8e_County_Choropleth.png/.tiff

suppressPackageStartupMessages({
  library(mgcv); library(dplyr); library(sf); library(survey)
  library(ggplot2); library(viridis); library(haven)
})

find_root <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  p <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
  while (!dir.exists(file.path(p, "09_geospatial")) && dirname(p) != p) p <- dirname(p)
  p
}
STUDY_ROOT <- find_root()
GEO  <- file.path(STUDY_ROOT, "09_geospatial")
DATA <- file.path(GEO, "data"); OUT <- file.path(GEO, "outputs")
TABS <- file.path(STUDY_ROOT, "04_tables"); FIGS <- file.path(STUDY_ROOT, "05_figures")
for (p in c(OUT, TABS, FIGS)) dir.create(p, showWarnings = FALSE, recursive = TRUE)

fit  <- readRDS(file.path(DATA, "st09_spatial_models.rds"))
surf <- readRDS(file.path(DATA, "st09_surface.rds"))
m2 <- fit$models$M2; d <- fit$data; laea <- fit$laea
adm1 <- surf$adm1

# ---- 1. calibration at cluster locations -------------------------------------
d$fitted_p <- as.numeric(predict(m2, type = "response"))
d$obs_p    <- d$n_uninsured / d$n_total

# population-weighted national figure implied by the model at cluster locations
nat_model <- weighted.mean(d$fitted_p, d$weight_sum)
nat_obs   <- weighted.mean(d$obs_p,    d$weight_sum)

# decile calibration
cal <- d %>%
  mutate(bin = ntile(fitted_p, 10)) %>%
  group_by(bin) %>%
  summarise(n_clusters = n(), n_persons = sum(n_total),
            mean_pred = mean(fitted_p),
            obs = sum(n_uninsured) / sum(n_total), .groups = "drop") %>%
  mutate(diff_pp = 100 * (obs - mean_pred))

sink(file.path(OUT, "calibration.txt"))
cat("ST09 geospatial — calibration of the binomial GAM (M2)\n\n")
cat("Population-weighted uninsured, observed :", sprintf("%.4f", nat_obs), "\n")
cat("Population-weighted uninsured, model    :", sprintf("%.4f", nat_model), "\n")
cat("ST09 main paper (survey-weighted)       : 0.7390\n\n")
cat("Area-based median of the mapped surface : ",
    sprintf("%.4f", median(surf$surface$p_uninsured)), "\n")
cat("  The surface is area-based and drawn for rural settlement at mean wealth.\n")
cat("  Kenya's land area is dominated by sparsely populated arid counties with the\n")
cat("  lowest coverage, so an area median above the population mean is expected.\n")
cat("  These are different estimands, not a discrepancy.\n\n")
cat("Calibration by decile of predicted risk:\n")
print(as.data.frame(cal), row.names = FALSE)
cat("\nmax |observed - predicted| across deciles:",
    sprintf("%.1f pp", max(abs(cal$diff_pp))), "\n")
sink()
cat("national observed:", sprintf("%.4f", nat_obs),
    " model:", sprintf("%.4f", nat_model), "\n")
cat("max decile calibration error:", sprintf("%.1f pp", max(abs(cal$diff_pp))), "\n")

# ---- 2. design-based direct county estimates ---------------------------------
RESEARCH_ROOT <- dirname(dirname(STUDY_ROOT))
pr <- read_dta(file.path(RESEARCH_ROOT, "01_DHS_Data", "KDHS_2022",
                         "PR_Person_Recode", "KEPR8CFL.DTA"),
               col_select = c(hv001, hv005, hv022, hv024, hv103, sh27))
num <- function(x) as.numeric(x)
ind <- pr %>%
  filter(num(hv103) == 1, num(sh27) %in% c(0, 1)) %>%
  transmute(cluster = num(hv001), weight = num(hv005) / 1e6,
            strata = num(hv022), county = num(hv024),
            uninsured = as.integer(num(sh27) != 1))

des <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weight,
                 data = ind, nest = TRUE)
options(survey.lonely.psu = "adjust")
direct <- svyby(~uninsured, ~county, des, svymean, vartype = c("ci", "se")) %>%
  as.data.frame() %>%
  transmute(county, direct = uninsured, direct_se = se,
            direct_lo = ci_l, direct_hi = ci_u)

# county labels from the DHS GPS attributes
lab <- d %>% distinct(county, county_name)

# ---- 3. model-based county estimates -----------------------------------------
gsf <- st_as_sf(surf$surface, coords = c("X", "Y"), crs = laea, remove = FALSE)
idx <- st_intersects(gsf, adm1)
gsf$cty_name <- NA_character_
hit <- lengths(idx) > 0
gsf$cty_name[hit] <- adm1$shapeName[unlist(lapply(idx[hit], `[`, 1))]

model_cty <- st_drop_geometry(gsf) %>%
  filter(!is.na(cty_name)) %>%
  group_by(cty_name) %>%
  summarise(model_area_mean = mean(p_uninsured),
            pct_area_hotspot = mean(p_exceed90 >= 0.80),
            n_cells = n(), .groups = "drop")

cty <- lab %>%
  left_join(direct, by = "county") %>%
  left_join(model_cty, by = c("county_name" = "cty_name")) %>%
  left_join(d %>% group_by(county) %>%
              summarise(n_clusters = n(), n_persons = sum(n_total), .groups = "drop"),
            by = "county") %>%
  mutate(ci_width_pp = 100 * (direct_hi - direct_lo),
         imprecise = ci_width_pp > 20) %>%
  arrange(desc(direct))

write.csv(cty, file.path(TABS, "Table17_County_Insurance_Exclusion.csv"), row.names = FALSE)

cat("\ncounties estimated:", nrow(cty), "\n")
cat("counties with direct 95% CI wider than 20 pp:", sum(cty$imprecise, na.rm = TRUE), "\n")
cat("\nTen counties with highest uninsured prevalence (direct, design-based):\n")
print(cty %>% select(county_name, n_clusters, n_persons, direct, direct_lo, direct_hi,
                     pct_area_hotspot) %>%
        mutate(across(c(direct, direct_lo, direct_hi), ~ round(100 * .x, 1)),
               pct_area_hotspot = round(100 * pct_area_hotspot, 0)) %>%
        head(10), row.names = FALSE)
cat("\nFive counties with lowest uninsured prevalence:\n")
print(cty %>% select(county_name, n_clusters, direct, direct_lo, direct_hi) %>%
        mutate(across(c(direct, direct_lo, direct_hi), ~ round(100 * .x, 1))) %>%
        tail(5), row.names = FALSE)

# ---- 4. county choropleth ----------------------------------------------------
map_df <- adm1 %>% left_join(cty, by = c("shapeName" = "county_name"))
p_e <- ggplot(map_df) +
  geom_sf(aes(fill = 100 * direct), colour = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Uninsured\n(%)",
                       limits = c(0, 100), na.value = "grey85") +
  coord_sf(crs = laea, expand = FALSE) +
  labs(title = "Insurance exclusion by county, Kenya 2022",
       subtitle = "Design-based direct estimates, all 47 counties",
       caption = paste0("KDHS 2022 person recode, de-facto household members. ",
                        "The main analysis reports 8 former provinces;\ncounty detail is ",
                        "available in hv024 and is recovered here. Counties with wide ",
                        "intervals are listed in Table 17.")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 8, colour = "grey30", hjust = 0))
ggsave(file.path(FIGS, "Figure8e_County_Choropleth.png"), p_e, width = 7.2, height = 7.6, dpi = 300)
ggsave(file.path(FIGS, "Figure8e_County_Choropleth.tiff"), p_e, width = 7.2, height = 7.6,
       dpi = 300, compression = "lzw")
cat("\nwrote Figure8e_County_Choropleth\n")
cat("wrote", file.path(TABS, "Table17_County_Insurance_Exclusion.csv"), "\n")
