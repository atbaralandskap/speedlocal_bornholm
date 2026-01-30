# ============================================================
# Bornholm AOI -> buffer -> H3 polyfill -> GeoPackage export
# Resolutions: 6, 7, 8, 9, 10
# Notes:
# - Buffer is done in meters (EPSG:25832), then transformed to 4326 for H3.
# - Large outputs (res 10) can be slow/heavy and may fail in GPKG; script
#   writes 6–9 to one GPKG and (optionally) 10 to a separate file.
# ============================================================

library(sf)
library(dplyr)
library(purrr)
library(tibble)
library(mapview)
library(h3jsr)

# ---- Paths ---------------------------------------------------
in_gpkg  <- "C:/gislab/speedlocal/data_raw/Matrikel-MAT2_Bornholm-0400_081224.gpkg"
in_layer <- "lodflade"

out_dir  <- "C:/gislab/speedlocal/data_out"
gpkg_6_9 <- file.path(out_dir, "bornholm_h3_6_9.gpkg")
gpkg_10  <- file.path(out_dir, "bornholm_h3_r10.gpkg")  # optional

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Parameters ---------------------------------------------
crs_meters <- 25832    # ETRS89 / UTM32N (meters)
buffer_m   <- - 5000    # 20 km buffer (change to 5000 for 5 km)
res_main   <- 6:9
res_probe  <- 0:9     # quick calibration table

# ---- 1) Read raw polygons -----------------------------------
bornholm_raw <- st_read(in_gpkg, layer = in_layer, quiet = TRUE)

message("Input CRS: ", st_crs(bornholm_raw)$input)

# Keep only polygon geometries (drop any accidental points/lines)
bornholm_raw <- bornholm_raw |>
  st_make_valid() |>
  st_collection_extract("POLYGON")

# ---- 2) Build AOI (union) -----------------------------------
bornholm_aoi <- bornholm_raw |>
  st_union() |>
  st_make_valid() |>
  st_as_sf() |>
  mutate(aoi = "Bornholm")

message("AOI geometry type: ", paste(unique(st_geometry_type(bornholm_aoi)), collapse = ", "))
message("AOI valid: ", all(st_is_valid(bornholm_aoi)))

# Quick view (optional)
mapview(bornholm_aoi, zcol = "aoi")

# ---- 3) Buffer AOI in meters --------------------------------
bornholm_aoi_m <- bornholm_aoi |>
  st_transform(crs_meters)

bornholm_aoi_buf_m <- bornholm_aoi_m |>
  st_buffer(buffer_m) |>
  st_make_valid()

# For H3 we want lon/lat
aoi_ll <- bornholm_aoi_buf_m |>
  st_transform(4326)

# Visual check (optional)
mapview(bornholm_aoi_buf_m, col.regions = "#74a9cf", alpha.regions = 0.4, layer.name = "AOI + buffer (meters)")

# ---- 4) Calibrate: number of H3 cells by resolution ----------
counts <- tibble(
  res = res_probe,
  n_cells = map_int(res_probe, \(r) length(h3jsr::polygon_to_cells(aoi_ll, res = r)))
)

print(counts)

# ---- 5) Helper: make sf of H3 cells --------------------------
make_h3_sf <- function(aoi_ll_sf, res) {
  cells <- h3jsr::polygon_to_cells(aoi_ll_sf, res = res)
  
  # Convert to polygons; keep as sf; add identifiers
  h3jsr::cell_to_polygon(cells) |>
    st_as_sf() |>
    mutate(
      h3  = as.character(cells),
      res = res
    )
}

# ---- 6) Build H3 layers for res 6–10 -------------------------
h3_list <- map(res_main, ~ make_h3_sf(aoi_ll, res = .x))
names(h3_list) <- paste0("h3_r", res_main)

print(sapply(h3_list, nrow))

# Optional quick mapview for a single res (avoid r10 in mapview)
# mapview(h3_list[["h3_r8"]], alpha.regions = 0.2) + mapview(bornholm_aoi_buf_m, alpha.regions = 0.1)

# ---- 7) Write to GeoPackage ---------------------------------
# # GeoPackage writes can be slow for many features (especially res 10).
# # We'll write res 6–9 into one file (fast + QGIS friendly),
# # and optionally res 10 into its own file.
# 
# # Clean old files
# if (file.exists(gpkg_6_9)) file.remove(gpkg_6_9)
# if (file.exists(gpkg_10))  file.remove(gpkg_10)
# 
# # Write 6–9 (recommended)
# for (nm in c("h3_r6", "h3_r7", "h3_r8", "h3_r9")) {
#   st_write(
#     h3_list[[nm]],
#     gpkg_6_9,
#     layer = nm,
#     append = file.exists(gpkg_6_9),
#     options = "SPATIAL_INDEX=NO",  # faster; QGIS can build index later
#     quiet = TRUE
#   )
# }
# message("Wrote: ", gpkg_6_9)
# 
# # Write 10 separately (optional; can take long / sometimes fail if locked/permission issues)
# # If this fails, keep r10 in PostGIS instead.
# try({
#   st_write(
#     h3_list[["h3_r10"]],
#     gpkg_10,
#     layer = "h3_r10",
#     options = "SPATIAL_INDEX=NO",
#     quiet = TRUE
#   )
#   message("Wrote: ", gpkg_10)
# }, silent = TRUE)
