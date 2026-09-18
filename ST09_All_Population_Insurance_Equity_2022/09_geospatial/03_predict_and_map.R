# ST09 geospatial — stage 3: prediction surface, exceedance probabilities, maps
#
# Produces a 5 km prediction surface of the probability of being uninsured,
# clipped to Kenya and masked to the sampled domain, plus an uncertainty-aware
# "hotspot of exclusion" layer.
#
# A hotspot here is NOT a cell whose point estimate exceeds 90%. It is a cell
# where the posterior probability that coverage exclusion exceeds 90% is itself
# high. Thresholding a point estimate would label the noisiest, least-sampled
# corners of the country as hotspots, which is precisely backwards.
#
# Resolution note: DHS displaces cluster coordinates (up to 2 km urban, 5 km
# rural, 1% of rural up to 10 km). The 5 km grid is a rendering choice; the
# surface must be read at roughly 10 km and coarser, and is interpreted at
# county and sub-regional scale throughout.
#
# Outputs: 05_figures/Figure8_*.png/.tiff, 09_geospatial/data/st09_surface.rds

suppressPackageStartupMessages({
  library(mgcv); library(dplyr); library(sf); library(terra)
  library(ggplot2); library(viridis)
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
FIGS <- file.path(STUDY_ROOT, "05_figures")
dir.create(FIGS, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

fit <- readRDS(file.path(DATA, "st09_spatial_models.rds"))
m2 <- fit$models$M2; m3 <- fit$models$M3
d <- fit$data; laea <- fit$laea

# ---- 1. Kenya boundary -------------------------------------------------------
adm1 <- st_read(file.path(DATA, "kenya_adm1.geojson"), quiet = TRUE) %>%
  st_make_valid() %>% st_transform(laea)
kenya <- st_union(adm1)
cat("counties in boundary file:", nrow(adm1), "\n")

# ---- 2. prediction grid ------------------------------------------------------
RES <- 5  # km
bb <- st_bbox(kenya)
grd <- expand.grid(
  X = seq(floor(bb["xmin"]), ceiling(bb["xmax"]), by = RES),
  Y = seq(floor(bb["ymin"]), ceiling(bb["ymax"]), by = RES)
)
gsf <- st_as_sf(grd, coords = c("X", "Y"), crs = laea, remove = FALSE)
inside <- lengths(st_intersects(gsf, kenya)) > 0
grd <- grd[inside, ]; gsf <- gsf[inside, ]
cat("grid cells inside Kenya:", nrow(grd), "\n")

# Mask cells far from any sampled cluster: the model must not be asked to
# extrapolate into parts of the country the survey did not visit.
cl_sf <- st_as_sf(d, coords = c("X", "Y"), crs = laea, remove = FALSE)
nnd <- as.numeric(st_distance(gsf, cl_sf[st_nearest_feature(gsf, cl_sf), ], by_element = TRUE))
MAXD <- 50  # km
grd$dist_nearest <- nnd
keep <- nnd <= MAXD
cat("cells within", MAXD, "km of a cluster:", sum(keep),
    sprintf(" (%.1f%% of Kenya's land grid)\n", 100 * mean(keep)))

# ---- 3. predict --------------------------------------------------------------
# Surface is for rural settlement at mean wealth, so that the mapped contrast is
# geographic rather than a composition artefact; urban cells are shown separately
# via the cluster overlay.
newd <- grd %>%
  mutate(urban_f = factor("Rural", levels = c("Rural", "Urban")),
         mean_wealth = mean(d$mean_wealth))

p2 <- predict(m2, newdata = newd, type = "link", se.fit = TRUE)
p3 <- predict(m3, newdata = newd, type = "link", se.fit = TRUE)

logit_thr <- qlogis(0.90)
grd$eta        <- as.numeric(p2$fit)
grd$se         <- as.numeric(p2$se.fit)
grd$p_uninsured <- plogis(grd$eta)
grd$p_exceed90 <- 1 - pnorm((logit_thr - grd$eta) / grd$se)
grd$eta_resid  <- as.numeric(p3$fit)
grd$p_resid    <- plogis(grd$eta_resid)
grd$masked     <- !keep

surf <- grd[keep, ]
cat("\nsurface summary (masked to sampled domain):\n")
cat("  predicted uninsured: median", sprintf("%.3f", median(surf$p_uninsured)),
    " range", sprintf("%.3f - %.3f", min(surf$p_uninsured), max(surf$p_uninsured)), "\n")
cat("  cells with P(uninsured > 0.9) >= 0.80:", sum(surf$p_exceed90 >= 0.80),
    sprintf(" (%.1f%% of sampled domain)\n", 100 * mean(surf$p_exceed90 >= 0.80)))
cat("  cells with P(uninsured > 0.9) >= 0.95:", sum(surf$p_exceed90 >= 0.95), "\n")

saveRDS(list(grid = grd, surface = surf, adm1 = adm1, kenya = kenya, laea = laea,
             res_km = RES, max_dist_km = MAXD),
        file.path(DATA, "st09_surface.rds"))

# ---- 4. maps -----------------------------------------------------------------
base_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.key.height = unit(1.1, "cm"),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, colour = "grey25"),
        plot.caption = element_text(size = 7.5, colour = "grey30", hjust = 0))

bnd  <- st_cast(st_boundary(adm1), "MULTILINESTRING")
natl <- st_cast(st_boundary(kenya), "MULTILINESTRING")

# Cells inside Kenya but too far from any cluster to estimate. Drawn in grey so
# they read as "not estimated" rather than as holes in the map.
unsampled <- grd[!keep, ]

# Sequential light-to-dark ramp: darker always means more excluded. Diverging or
# reversed-viridis scales invite the opposite reading on a risk map.
risk_fill <- function(name, limits = c(0, 100)) {
  scale_fill_gradientn(
    colours = c("#fff7ec", "#fee8c8", "#fdbb84", "#fc8d59", "#e34a33", "#b30000", "#6b0000"),
    limits = limits, name = name, na.value = "grey88"
  )
}

grey_layer <- function() {
  list(
    geom_tile(data = unsampled, aes(X, Y), fill = "grey88", width = RES, height = RES),
    geom_sf(data = natl, colour = "grey35", linewidth = 0.3, fill = NA)
  )
}

save_fig <- function(p, name, w = 7.2, h = 7.6) {
  ggsave(file.path(FIGS, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
  ggsave(file.path(FIGS, paste0(name, ".tiff")), p, width = w, height = h, dpi = 300,
         compression = "lzw")
  cat("wrote", name, "\n")
}

# 8a — predicted surface
p_a <- ggplot() +
  grey_layer() +
  geom_tile(data = surf, aes(X, Y, fill = 100 * p_uninsured), width = RES, height = RES) +
  geom_sf(data = bnd, colour = "white", linewidth = 0.12, alpha = 0.7) +
  geom_sf(data = natl, colour = "grey25", linewidth = 0.3) +
  risk_fill("Uninsured\n(%)") +
  coord_sf(crs = laea, expand = FALSE) +
  labs(title = "Predicted probability of being uninsured, Kenya 2022",
       subtitle = "Binomial GAM surface, rural settlement at mean wealth",
       caption = paste0("KDHS 2022, ", nrow(d), " clusters, ", format(sum(d$n_total), big.mark = ","),
                        " de-facto household members. 5 km grid masked to within ", MAXD,
                        " km of a sampled cluster.\nDHS displaces cluster coordinates ",
                        "(<=2 km urban, <=5 km rural); read at ~10 km and coarser.")) +
  base_theme
save_fig(p_a, "Figure8a_Predicted_Uninsured_Surface")

# 8b — exceedance probability (the actual hotspot map)
p_b <- ggplot() +
  grey_layer() +
  geom_tile(data = surf, aes(X, Y, fill = 100 * p_exceed90), width = RES, height = RES) +
  geom_sf(data = bnd, colour = "white", linewidth = 0.12, alpha = 0.7) +
  geom_sf(data = natl, colour = "grey25", linewidth = 0.3) +
  risk_fill("P(exclusion\n> 90%)  (%)") +
  coord_sf(crs = laea, expand = FALSE) +
  labs(title = "Hotspots of insurance exclusion, Kenya 2022",
       subtitle = "Probability that local coverage exclusion exceeds 90%",
       caption = paste0("Uncertainty-aware: shows the probability that exclusion exceeds the 90% ",
                        "threshold, not whether\na point estimate happens to clear it. ",
                        "Sparsely sampled areas are therefore not spuriously flagged.")) +
  base_theme
save_fig(p_b, "Figure8b_Exclusion_Hotspots")

# 8c — observed cluster values
p_c <- ggplot() +
  geom_sf(data = adm1, fill = "grey96", colour = "white", linewidth = 0.15) +
  geom_point(data = d, aes(X, Y, colour = 100 * p_uninsured_w, size = n_total), alpha = 0.85) +
  scale_colour_gradientn(
    colours = c("#fff7ec", "#fdbb84", "#fc8d59", "#e34a33", "#b30000", "#6b0000"),
    limits = c(0, 100), name = "Uninsured
(%)") +
  scale_size_continuous(range = c(0.3, 2.2), name = "Cluster n", guide = "none") +
  coord_sf(crs = laea, expand = FALSE) +
  labs(title = "Observed insurance exclusion by survey cluster",
       subtitle = paste0(nrow(d), " KDHS 2022 clusters, weighted cluster proportions"),
       caption = "Point size is the number of de-facto household members sampled in the cluster.") +
  base_theme
save_fig(p_c, "Figure8c_Observed_Clusters")

# 8d — residual geography after urbanicity and wealth
p_d <- ggplot() +
  grey_layer() +
  geom_tile(data = surf, aes(X, Y, fill = 100 * p_resid), width = RES, height = RES) +
  geom_sf(data = bnd, colour = "white", linewidth = 0.12, alpha = 0.7) +
  geom_sf(data = natl, colour = "grey25", linewidth = 0.3) +
  risk_fill("Uninsured\n(%)") +
  coord_sf(crs = laea, expand = FALSE) +
  labs(title = "Exclusion attributable to place, net of settlement type and wealth",
       subtitle = "Surface from the model adjusting for urban/rural and cluster wealth composition",
       caption = paste0("Structure remaining here is not explained by how urban a place is or how ",
                        "poor its households are.\nIt is the geography a composition-only account ",
                        "of coverage would miss.")) +
  base_theme
save_fig(p_d, "Figure8d_Residual_Place_Effect")

cat("\nfigures written to", FIGS, "\n")
