# ST09 geospatial — stage 2: binomial geostatistical models and prediction surface
#
# Models the cluster-level probability of being uninsured as a smooth function of
# location, using penalised-likelihood binomial GAMs (mgcv). Three nested models
# separate the raw geography of exclusion from the part attributable to
# settlement type and wealth composition:
#
#   M1  uninsured ~ s(X, Y)                       raw geography
#   M2  uninsured ~ s(X, Y) + urban               geography net of settlement type
#   M3  uninsured ~ s(X, Y) + urban + wealth      residual place effect
#
# The M3 smooth answers the question the province-level analysis in the main
# paper cannot: once we know how urban a place is and how poor its households
# are, does *where* it is still predict exclusion?
#
# Why penalised GAMs rather than INLA-SPDE or kriging. DHS displaces cluster
# coordinates for confidentiality -- up to 2 km urban, 5 km rural, with 1% of
# rural clusters moved up to 10 km. Fine-scale structure is therefore destroyed
# by design, and a model whose selling point is sub-kilometre resolution would
# be estimating noise. A thin-plate/Duchon spline with REML-selected smoothing
# recovers structure at the scale the data can actually support, is fully
# reproducible from CRAN, and yields the standard errors needed for exceedance
# probabilities. This is a deliberate choice, not a fallback.
#
# Outputs: 09_geospatial/data/st09_spatial_models.rds
#          09_geospatial/outputs/model_summary.txt

suppressPackageStartupMessages({
  library(mgcv); library(dplyr); library(sf)
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
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

bundle <- readRDS(file.path(DATA, "st09_clusters.rds"))
pts <- bundle$points; laea <- bundle$laea
d <- st_drop_geometry(pts) %>%
  mutate(urban_f = factor(ifelse(urban == 1, "Urban", "Rural"), levels = c("Rural", "Urban")))

cat("clusters:", nrow(d), " persons:", sum(d$n_total), "\n\n")

# ---- 1. models ---------------------------------------------------------------
# k is generous; REML shrinks the smooth to the complexity the data support.
K <- 250
resp <- cbind(d$n_uninsured, d$n_insured)

cat("fitting M1 (geography only) ...\n")
m1 <- gam(resp ~ s(X, Y, bs = "ds", m = c(1, 0.5), k = K),
          family = binomial, data = d, method = "REML")

cat("fitting M2 (+ urban/rural) ...\n")
m2 <- gam(resp ~ s(X, Y, bs = "ds", m = c(1, 0.5), k = K) + urban_f,
          family = binomial, data = d, method = "REML")

cat("fitting M3 (+ wealth composition) ...\n")
m3 <- gam(resp ~ s(X, Y, bs = "ds", m = c(1, 0.5), k = K) + urban_f +
            s(mean_wealth, k = 5),
          family = binomial, data = d, method = "REML")

models <- list(M1 = m1, M2 = m2, M3 = m3)

# ---- 2. summaries ------------------------------------------------------------
sink(file.path(OUT, "model_summary.txt"))
cat("ST09 geospatial models — binomial GAMs of cluster-level uninsured probability\n")
cat("Clusters:", nrow(d), " Persons:", sum(d$n_total), "\n")
cat("Projection: Lambert azimuthal equal-area, km units\n\n")
for (nm in names(models)) {
  cat(strrep("=", 78), "\n", nm, "\n", strrep("=", 78), "\n", sep = "")
  print(summary(models[[nm]]))
  cat("\nAIC:", AIC(models[[nm]]),
      " deviance explained:", sprintf("%.1f%%", 100 * summary(models[[nm]])$dev.expl), "\n\n")
}
cat(strrep("=", 78), "\nModel comparison\n", strrep("=", 78), "\n", sep = "")
print(AIC(m1, m2, m3))
sink()

comp <- data.frame(
  model = names(models),
  edf_spatial = sapply(models, function(m) sum(summary(m)$edf[1])),
  dev_expl = sapply(models, function(m) summary(m)$dev.expl),
  aic = sapply(models, AIC)
)
print(comp)

# ---- 3. scale of spatial structure -------------------------------------------
# Empirical-logit residual variogram describes the distance over which clusters
# remain correlated, i.e. the scale at which the surface should be read.
el <- with(d, log((n_uninsured + 0.5) / (n_insured + 0.5)))
res_m2 <- el - predict(m2, type = "link")
vg <- NULL
if (requireNamespace("gstat", quietly = TRUE)) {
  vdf <- data.frame(X = d$X, Y = d$Y, r = as.numeric(res_m2))
  sp::coordinates(vdf) <- ~X + Y
  vg <- gstat::variogram(r ~ 1, vdf, cutoff = 300, width = 10)
  cat("\nempirical variogram of M2 residuals computed (cutoff 300 km)\n")
}

saveRDS(list(models = models, data = d, laea = laea, comparison = comp, variogram = vg),
        file.path(DATA, "st09_spatial_models.rds"))
cat("\nsaved ->", file.path(DATA, "st09_spatial_models.rds"), "\n")
