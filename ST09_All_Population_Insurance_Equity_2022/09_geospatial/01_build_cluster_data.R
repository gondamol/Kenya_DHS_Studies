# ST09 geospatial — stage 1: build the cluster-level analytic file
#
# Aggregates the KDHS 2022 person recode to DHS cluster level using exactly the
# sample restriction and insurance definition of the main ST09 analysis
# (de-facto household members with a valid sh27 response), then joins the DHS
# GPS cluster coordinates.
#
# Output: 09_geospatial/data/st09_clusters.rds

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(sf)
})

# Resolve the study root from the script path when run with Rscript, else from cwd.
find_root <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  p <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
  while (!dir.exists(file.path(p, "09_geospatial")) && dirname(p) != p) p <- dirname(p)
  p
}
STUDY_ROOT <- find_root()
RESEARCH_ROOT <- dirname(dirname(STUDY_ROOT))  # .../Research
GEO_DIR  <- file.path(STUDY_ROOT, "09_geospatial")
DATA_DIR <- file.path(GEO_DIR, "data")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

num <- function(x) as.numeric(x)

# ---- 1. person recode --------------------------------------------------------
pr_path <- file.path(RESEARCH_ROOT, "01_DHS_Data", "KDHS_2022",
                     "PR_Person_Recode", "KEPR8CFL.DTA")
stopifnot(file.exists(pr_path))

raw <- read_dta(pr_path, col_select = c(hv001, hv005, hv021, hv022, hv103,
                                        hv024, hv025, hv270, sh27))
cat("PR rows read:", nrow(raw), "\n")

dat <- raw %>%
  filter(num(hv103) == 1) %>%            # de-facto: slept there last night
  filter(num(sh27) %in% c(0, 1)) %>%     # valid insurance response
  transmute(
    cluster  = num(hv001),
    weight   = num(hv005) / 1e6,
    county   = num(hv024),
    urban    = as.integer(num(hv025) == 1),
    wealth   = num(hv270),
    insured  = as.integer(num(sh27) == 1)
  )
cat("analytic persons:", nrow(dat), " (ST09 reports 75,561)\n")

# ---- 2. aggregate to cluster -------------------------------------------------
# Unweighted counts drive the binomial likelihood; the weighted proportion is
# retained for descriptive mapping and for weighted county estimates. Within a
# DHS cluster the design weight is essentially constant, so the two agree
# closely; the binomial model additionally carries the urban/rural stratifier.
clus <- dat %>%
  group_by(cluster) %>%
  summarise(
    n_total      = n(),
    n_insured    = sum(insured),
    n_uninsured  = n_total - n_insured,
    p_uninsured  = n_uninsured / n_total,
    p_uninsured_w = weighted.mean(1 - insured, weight),
    mean_wealth  = weighted.mean(wealth, weight),
    poorest2_sh  = weighted.mean(wealth <= 2, weight),
    urban        = as.integer(mean(urban) > 0.5),
    county       = as.integer(names(sort(table(county), decreasing = TRUE))[1]),
    weight_sum   = sum(weight),
    .groups = "drop"
  )
cat("clusters aggregated:", nrow(clus), "\n")

# ---- 3. join GPS -------------------------------------------------------------
gps_path <- file.path(RESEARCH_ROOT, "01_DHS_Data", "gps data",
                      "KEGE8AFL", "KEGE8AFL.shp")
stopifnot(file.exists(gps_path))
gps <- st_read(gps_path, quiet = TRUE) %>%
  select(DHSCLUST, ADM1NAME, DHSREGNA, URBAN_RURA, LATNUM, LONGNUM, ALT_DEM)

# DHS assigns (0,0) to clusters with no fix; none here, but guard anyway.
bad <- gps$LATNUM == 0 & gps$LONGNUM == 0
cat("clusters with missing GPS fix:", sum(bad), "\n")
gps <- gps[!bad, ]

j <- clus %>%
  inner_join(st_drop_geometry(gps), by = c("cluster" = "DHSCLUST")) %>%
  mutate(county_name = ADM1NAME)
cat("clusters after GPS join:", nrow(j),
    " (dropped", nrow(clus) - nrow(j), ")\n")

# sanity: DHS urban/rural flag vs the PR-derived one
cat("urban/rural agreement with DHS flag:",
    sprintf("%.1f%%", 100 * mean(j$urban == as.integer(j$URBAN_RURA == "U"))), "\n")

# ---- 4. project --------------------------------------------------------------
# Lambert azimuthal equal-area centred on Kenya: distances and areas behave
# sensibly across the whole country, which a single UTM zone does not.
laea <- "+proj=laea +lat_0=0.5 +lon_0=37.9 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"
pts <- st_as_sf(j, coords = c("LONGNUM", "LATNUM"), crs = 4326, remove = FALSE) %>%
  st_transform(laea)
xy <- st_coordinates(pts)
pts$X <- xy[, 1]; pts$Y <- xy[, 2]

saveRDS(list(points = pts, laea = laea), file.path(DATA_DIR, "st09_clusters.rds"))

cat("\n--- summary ---\n")
cat("persons:", sum(pts$n_total), " clusters:", nrow(pts), "\n")
cat("crude uninsured proportion:",
    sprintf("%.3f", sum(pts$n_uninsured) / sum(pts$n_total)), "\n")
cat("weighted uninsured (ST09 reports 0.739):",
    sprintf("%.3f", weighted.mean(pts$p_uninsured_w, pts$weight_sum)), "\n")
cat("cluster size: median", median(pts$n_total),
    " range", min(pts$n_total), "-", max(pts$n_total), "\n")
cat("counties represented:", length(unique(pts$county_name)), "\n")
cat("saved ->", file.path(DATA_DIR, "st09_clusters.rds"), "\n")
