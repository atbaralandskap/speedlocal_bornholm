library(sf)
library(dplyr)
library(purrr)
library(mapview)

path <- "C:/gislab/speedlocal/data_test"

gpkg_main <- file.path(path, "geocontext_test_data.gpkg")
gpkg_mi   <- file.path(path, "geocontext_test_data_mapinfo.gpkg")

# --- 1) Läs in "bra" lager från huvud-gpkg -----------------------------
main_layers <- c("telemast", "vindmoelle", "kortlaeg_naturtyper_fl", "bes_naturtyper")

main <- setNames(
  lapply(main_layers, \(lyr) st_read(gpkg_main, layer = lyr, quiet = TRUE)),
  main_layers
)

# Droppa Z om det finns (för att slippa strul i analyser/visualisering)
main <- lapply(main, \(x) if (any(grepl(" Z", class(st_geometry(x))))) st_zm(x, drop = TRUE) else x)

# --- 2) Läs in MapInfo-gpkg -------------------------------------------
mi_layers <- c("fastboendebefolkningmapinfo", "kurverhelebornholm_1m_udtyndet")

mi <- setNames(
  lapply(mi_layers, \(lyr) st_read(gpkg_mi, layer = lyr, quiet = TRUE)),
  mi_layers
)

library(sf)
library(mapview)

fix_geom <- function(x) {
  # 1) drop Z/M
  x <- st_zm(x, drop = TRUE, what = "ZM")
  
  # 2) decide “target” based on what’s inside
  g <- st_geometry_type(x, by_geometry = TRUE)
  g_chr <- as.character(g)
  
  # If collections exist, extract the relevant part
  if (any(g_chr == "GEOMETRYCOLLECTION")) {
    if (any(grepl("POLYGON", g_chr)))      x <- st_collection_extract(x, "POLYGON")
    else if (any(grepl("LINESTRING", g_chr))) x <- st_collection_extract(x, "LINESTRING")
    else                                   x <- st_collection_extract(x, "POINT")
    g_chr <- as.character(st_geometry_type(x, by_geometry = TRUE))
  }
  
  # 3) cast to MULTI-* consistently (this is the usual culprit)
  if (any(grepl("POLYGON", g_chr))) {
    x <- st_cast(x, "MULTIPOLYGON", warn = FALSE)
  } else if (any(grepl("LINESTRING", g_chr))) {
    x <- st_cast(x, "MULTILINESTRING", warn = FALSE)
  } else if (any(grepl("POINT", g_chr))) {
    x <- st_cast(x, "MULTIPOINT", warn = FALSE)
  }
  
  x
}

# Fix all layers you want to view
main$bes_naturtyper        <- fix_geom(main$bes_naturtyper)
main$kortlaeg_naturtyper_fl<- fix_geom(main$kortlaeg_naturtyper_fl)
main$vindmoelle            <- fix_geom(main$vindmoelle)
main$telemast              <- fix_geom(main$telemast)

mi$fastboendebefolkningmapinfo <- fix_geom(mi$fastboendebefolkningmapinfo)
mi$kurverhelebornholm_1m_udtyndet <- fix_geom(mi$kurverhelebornholm_1m_udtyndet)

mv <-
  mapview(main$bes_naturtyper, zcol = "Natyp_navn") +
  mapview(main$kortlaeg_naturtyper_fl) +
  mapview(main$vindmoelle) +
  mapview(main$telemast) +
  mapview(mi$fastboendebefolkningmapinfo) +
  mapview(mi$kurverhelebornholm_1m_udtyndet)

mv



# hex 8 till centroider
library(sf)
library(dplyr)

source("R/geocontext_R.R")  # <-- justera om du lagt funktionen annanstans

con <- connect_pg()

# 1) Läs hexar (gör geom till aktiv geometri)
bornholm_hex <- st_read(
  con,
  query = "
    SELECT
      h3 AS hex_id,
      geometry AS geometry,
      ST_Area(geometry::geography) / 1e6 AS area_km2
    FROM h3.bornholm_r8
  ",
  quiet = TRUE
)

# 2) Centroider + lon/lat (för punkter / join / framtida geocontext)
bornholm_hex_ll <- bornholm_hex |>
  st_transform(4326)

cent <- st_centroid(st_geometry(bornholm_hex_ll))
xy   <- st_coordinates(cent)

points <- bornholm_hex_ll |>
  st_drop_geometry() |>
  transmute(
    point_id = hex_id,
    x = xy[, 1],
    y = xy[, 2]
  )

# höjdkurvor
library(sf)
library(dplyr)

kurvor <- mi$kurverhelebornholm_1m_udtyndet |>
  st_zm(drop = TRUE) |>
  st_transform(st_crs(bornholm_hex))

# matcha via spatial index (snabbare än intersection)
hits <- st_intersects(bornholm_hex, kurvor)

elev <- kurvor$Elevation  # <- du har den kolumnen

stats <- lapply(hits, function(idx) {
  if (length(idx) == 0) return(c(min=NA, max=NA, mean=NA))
  v <- elev[idx]
  c(min = min(v, na.rm=TRUE),
    max = max(v, na.rm=TRUE),
    mean = mean(v, na.rm=TRUE))
})

stats_mat <- do.call(rbind, stats)

hex_elev <- bornholm_hex |>
  mutate(
    elev_min  = stats_mat[, "min"],
    elev_max  = stats_mat[, "max"],
    elev_mean = stats_mat[, "mean"],
    relief    = elev_max - elev_min
  )

# 3) Mapview
mapview(hex_elev, zcol = "relief", layer.name = "Relief (max-min Elevation)") +
  mapview(hex_elev, zcol = "elev_mean", layer.name = "Mean Elevation")+
  mapview(mi$kurverhelebornholm_1m_udtyndet)


# befolkning

library(sf)
library(dplyr)
library(mapview)

sf_use_s2(FALSE)

pop_pts <- mi$fastboendebefolkningmapinfo |>
  st_zm(drop = TRUE) |>
  st_transform(st_crs(bornholm_hex))

# Quick check
stopifnot("Person" %in% names(pop_pts))

# Spatial index join (no geometry output)
hits <- st_intersects(bornholm_hex, pop_pts)

# Sum persons per hex (should match counts if Person==1 always)
persons_sum <- vapply(
  hits,
  function(idx) if (length(idx) == 0) 0 else sum(pop_pts$Person[idx], na.rm = TRUE),
  numeric(1)
)

hex_pop <- bornholm_hex |>
  mutate(
    persons = persons_sum,
    persons_log = log1p(persons)  # helps visualization if skewed
  )

# Map only hexes (safe)
mapview(hex_pop, zcol = "persons_log", layer.name = "Population (log1p persons per hex r8)")


# vindmölla

library(sf)
library(dplyr)
library(mapview)

sf_use_s2(FALSE)

wind <- main$vindmoelle |>
  st_zm(drop = TRUE) |>
  st_transform(st_crs(bornholm_hex))

hits <- st_intersects(bornholm_hex, wind)

n_turbines <- lengths(hits)

hex_wind <- bornholm_hex |>
  mutate(
    n_turbines = n_turbines,
    n_turbines_log = log1p(n_turbines)
  )

nearest_m <- as.numeric(st_distance(st_centroid(bornholm_hex), wind, by_element = FALSE)) # tungt

# bättre: st_nearest_feature + st_distance per hex
nf <- st_nearest_feature(st_centroid(bornholm_hex), wind)
dist_nearest_m <- as.numeric(st_distance(st_centroid(bornholm_hex), wind[nf, ], by_element = TRUE))

hex_wind <- hex_wind |>
  mutate(dist_to_nearest_turbine_m = dist_nearest_m)

mapview(hex_wind, zcol = "n_turbines_log", layer.name = "Wind turbines per hex (log1p)") +
  mapview(hex_wind, zcol = "dist_to_nearest_turbine_m", layer.name = "Distance to nearest turbine (m)")+
  mapview(main$vindmoelle)

# naturtyp
library(sf)
library(dplyr)
library(mapview)

sf_use_s2(FALSE)

# 1) Säker CRS för area (meter)
nat <- main$bes_naturtyper |>
  st_zm(drop = TRUE) |>
  st_make_valid() |>
  st_transform(25832)

hex <- bornholm_hex |>
  st_transform(25832)

# 2) Klipp naturpolygoner mot hex
ix <- suppressWarnings(
  st_intersection(
    nat,                 # inga attribut behövs nu
    hex |> select(hex_id)
  )
)

# 3) Total area per hex (m²)
a <- as.numeric(st_area(st_geometry(ix)))

area_hex <- ix |>
  st_drop_geometry() |>
  mutate(area_m2 = a) |>
  group_by(hex_id) |>
  summarise(protected_m2 = sum(area_m2), .groups = "drop")

# räkna hex-arean som vanlig vektor (m²)
hex_area_m2 <- as.numeric(st_area(st_geometry(hex)))

hex_prot <- hex |>
  left_join(area_hex, by = "hex_id") |>
  mutate(
    protected_m2 = ifelse(is.na(protected_m2), 0, protected_m2),
    hex_area_m2 = hex_area_m2,
    protected_share = protected_m2 / hex_area_m2
  )


# 5) Mapview
mapview(hex_prot, zcol = "protected_share", layer.name = "Protected area share per hex (r8)") +
  mapview(hex_wind, zcol = "dist_to_nearest_turbine_m", layer.name = "Distance to nearest turbine (m)")+
  mapview(hex_pop, zcol = "persons_log", layer.name = "Population (log1p persons per hex r8)")+
  mapview(hex_elev, zcol = "relief", layer.name = "Höjdskillnad")

library(mapview)

# non-linear breaks (quantiles = instant drama)
brks_relief <- quantile(
  hex_elev$relief,
  probs = c(0, .4, .6, .75, .85, .92, .97, 1),
  na.rm = TRUE
)

pal <- hcl.colors(length(brks_relief) - 1, "Spectral")

mv <-
  mapview(
    hex_prot,
    zcol = "protected_share",
    layer.name = "Protected area share",
    lwd = 0.1,
    alpha.regions = 0.6,
    col.regions = rev(hcl.colors(100, "Greens")),
    hide = TRUE
  ) +
  mapview(
    hex_wind,
    zcol = "dist_to_nearest_turbine_m",
    lwd = 0.1,
    layer.name = "Distance to nearest turbine (m)",
    col.regions = hcl.colors(100, "Purple-Orange"),
    alpha.regions = 0.7,
    legend.reverse = TRUE   # close = strong colour
  ) +
  mapview(
    hex_pop,
    zcol = "persons_log",
    layer.name = "Population (log1p)",
    lwd = 0.1,
    alpha.regions = 0.6,
    col.regions = rev(hcl.colors(100, "Reds")),
    hide = TRUE
  ) +
  mapview(
    hex_elev,
    zcol = "relief",
    at = brks_relief,
    col.regions = rev(pal),   # 👈 invert scale
    lwd = 0.1,
    color = "#00000022",
    alpha.regions = 0.6,
    layer.name = "Höjdskillnad (relief)",
    hide = TRUE
  )

mv







# Minimal kod: bygg en enda tabell för geocontext
ctx_features <- bornholm_hex |>
  st_drop_geometry() |>
  select(hex_id) |>
  left_join(hex_prot |> st_drop_geometry() |> select(hex_id, protected_share), by = "hex_id") |>
  left_join(hex_wind |> st_drop_geometry() |> select(hex_id, dist_to_nearest_turbine_m), by = "hex_id") |>
  left_join(hex_pop  |> st_drop_geometry() |> select(hex_id, persons_log), by = "hex_id") |>
  left_join(hex_elev |> st_drop_geometry() |> select(hex_id, elev_mean, relief), by = "hex_id") |>
  mutate(
    dist_turbine_log = log1p(dist_to_nearest_turbine_m),
    relief = ifelse(is.na(relief), 0, relief)  # om du vill fylla tomma
  )

ctx_features <- ctx_features |>
  select(
    hex_id,
    protected_share,
    dist_turbine_log,
    persons_log,
    relief
  )

