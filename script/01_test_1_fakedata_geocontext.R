# examples/example_bornholm_res7.R
# ------------------------------------------------------------
# Example: Bornholm H3 res 7 synthetic test data for geocontext_R()
# Requires: bornholm_poly.rds in project root (or adjust path)
# ------------------------------------------------------------

library(sf)
library(dplyr)
library(h3jsr)
library(readr)
library(DBI)
library(dplyr)

source("R/geocontext_R.R")  # <-- justera om du lagt funktionen annanstans



con <- connect_pg()

bornholm_hex <- st_read(
  con,
  query = "
    SELECT
      h3       AS hex_id,
      geometry AS geom,
      ST_Area(geometry::geography) / 1e6 AS area_km2
    FROM h3.bornholm_r8
  ",
  quiet = TRUE
)



# 3️⃣ Bygg points och pop_locations korrekt
# A. Skapa centroider (en gång)
bornholm_hex <- bornholm_hex |>
  st_transform(4326) |>
  mutate(centroid = st_centroid(geom))

coords <- st_coordinates(bornholm_hex$centroid)

bornholm_hex <- bornholm_hex |>
  mutate(
    x = coords[,1],
    y = coords[,2]
  )

# B. points = analysenheter (hexagoner)
points <- bornholm_hex |>
  st_drop_geometry() |>
  transmute(
    point_id = hex_id,
    x = x,
    y = y
  )

# C. pop_locations = kontextuniversum
set.seed(42)

pop_locations <- bornholm_hex |>
  st_drop_geometry() |>
  transmute(
    loc_id = hex_id,
    x = x,
    y = y,
    
    # population = "massa" som styr radien
    # area_km2 är perfekt för detta
    population = area_km2,
    
    # TEST: syntetiska landskapsandelar
    # (byt mot riktiga kolumner när de finns)
    open_land  = runif(n(), 0, 1),
    forest     = runif(n(), 0, 1),
    settlement = runif(n(), 0, 1)
  )

# 4️⃣ Kör geocontext_R() på riktig SpeedLocal-data
groups      <- c("open_land", "forest", "settlement")
populations <- c("population")
proportions <- c(FALSE, FALSE, FALSE)  # viktat medel + sd
k_values <- c(100, 300, 600)

ctx <- geocontext_R(
  points        = points,
  pop_locations = pop_locations,
  groups        = groups,
  populations   = populations,
  proportions   = proportions,
  k_values      = k_values
)



# -----------------------------
# 6) Sanity checks
# -----------------------------
message("Kolumner i output (urval):")
print(names(ctx)[1:min(25, ncol(ctx))])

# Radier ska finnas och vara monotont ökande
stopifnot(all(!is.na(ctx$radius_k100)))
stopifnot(all(ctx$radius_k300 >= ctx$radius_k100))
stopifnot(all(ctx$radius_k600 >= ctx$radius_k300))

for (k in k_values) {
  stopifnot(paste0("mean_open_land_k", k) %in% names(ctx))
  stopifnot(paste0("sd_open_land_k", k)   %in% names(ctx))
}


# Titta på några rader
ctx |>
  select(
    point_id,
    starts_with("radius_k"),
    starts_with("mean_open_land_k"),
    starts_with("sd_open_land_k")
  ) |>
  head()


library(mapview)


ctx_map <- bornholm_hex |>
  left_join(
    ctx,
    by = c("hex_id" = "point_id")
  )

stopifnot(nrow(ctx_map) == nrow(bornholm_hex))

mapview(ctx_map, zcol = "mean_open_land_k300", layer.name = "Open land (k=300)")+
  mapview(ctx_map, zcol = "mean_open_land_k100") +
  mapview(ctx_map, zcol = "mean_open_land_k600")

mapview(ctx_map, zcol = "radius_k300", layer.name = "Radius k=300 (m)")

