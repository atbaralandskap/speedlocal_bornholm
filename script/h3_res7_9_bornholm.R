# ============================================================
# Bornholm – H3 energy landscape
#
# AOI (full)
# + core_buffer      = +1000 m  -> r7, r8
# + core_neg_buffer  = -5000 m  -> r9 (lines, web-safe)
#
# Output:
# - Interactive mapview + HTML
# ============================================================

library(sf)
library(dplyr)
library(h3jsr)
library(mapview)
library(htmlwidgets)

# ---- Paths --------------------------------------------------
in_gpkg  <- "C:/gislab/speedlocal/data_raw/Matrikel-MAT2_Bornholm-0400_081224.gpkg"
in_layer <- "lodflade" # byta till ett lager som inkluderar Christians ö



out_html <- "C:/gislab/speedlocal/html_karta/bornholm_h3_r7_r8_r9core.html"
dir.create(dirname(out_html), recursive = TRUE, showWarnings = FALSE)

# ---- Parameters ---------------------------------------------
crs_meters       <- 25832        # ETRS89 / UTM32N
core_buffer      <-  1000        # +1 km (for r7, r8)
core_neg_buffer  <- -5000        # -5 km (for r9 only)

res_r7 <- 7
res_r8 <- 8
res_r9 <- 9

mapviewOptions(fgb = FALSE)

# ============================================================
# 1) Read + build AOI (full)
# ============================================================

bornholm_raw <- st_read(in_gpkg, layer = in_layer, quiet = TRUE) |>
  st_make_valid() |>
  st_collection_extract("POLYGON")

bornholm_aoi <- bornholm_raw |>
  st_union() |>
  st_make_valid() |>
  st_as_sf() |>
  mutate(aoi = "Bornholm")

# ============================================================
# 2) Buffers in meter CRS
# ============================================================

bornholm_aoi_m <- bornholm_aoi |> st_transform(crs_meters)

# +1 km buffer (context for r7 / r8)
bornholm_core_pos_m <- bornholm_aoi_m |>
  st_buffer(core_buffer) |>
  st_make_valid() |>
  st_collection_extract("POLYGON") |>
  st_union() |>
  st_make_valid() |>
  st_as_sf() |>
  mutate(zone = "core_plus_1km")

# -5 km buffer (inner core for r9)
bornholm_core_neg_m <- bornholm_aoi_m |>
  st_buffer(core_neg_buffer) |>
  st_make_valid() |>
  st_collection_extract("POLYGON") |>
  st_union() |>
  st_make_valid() |>
  st_as_sf() |>
  mutate(zone = "core_minus_5km")

# ============================================================
# 3) Lon/lat versions for H3
# ============================================================

aoi_ll        <- st_transform(bornholm_aoi,        4326)
core_pos_ll  <- st_transform(bornholm_core_pos_m,  4326)
core_neg_ll  <- st_transform(bornholm_core_neg_m,  4326)

# ============================================================
# 4) H3 r7 + r8 over +1 km core
# ============================================================

r7_ids <- h3jsr::polygon_to_cells(core_pos_ll, res = res_r7)
h3_r7 <- h3jsr::cell_to_polygon(r7_ids) |>
  st_as_sf() |>
  mutate(h3 = as.character(r7_ids), res = res_r7)

r8_ids <- h3jsr::polygon_to_cells(core_pos_ll, res = res_r8)
h3_r8 <- h3jsr::cell_to_polygon(r8_ids) |>
  st_as_sf() |>
  mutate(h3 = as.character(r8_ids), res = res_r8)

# ============================================================
# 5) H3 r9 only over -5 km core
# ============================================================

r9_ids <- h3jsr::polygon_to_cells(core_neg_ll, res = res_r9)
h3_r9 <- h3jsr::cell_to_polygon(r9_ids) |>
  st_as_sf() |>
  mutate(h3 = as.character(r9_ids), res = res_r9)

# Web-safe: lines instead of polygons
h3_r9_lines <- st_cast(h3_r9, "MULTILINESTRING")

# ============================================================
# 6) Mapview (stable)
# ============================================================

m <- mapview(aoi_ll,
             layer.name = "Bornholm AOI",
             col.regions = "grey40",
             alpha.regions = 0.05, hide = TRUE) +
  mapview(core_pos_ll,
          layer.name = "Core +1 km (r7–r8)",
          col.regions = "#9ecae1",
          alpha.regions = 0.10, hide = TRUE) +
  mapview(core_neg_ll,
          layer.name = "Core -5 km (r9)",
          col.regions = "#3182bd",
          alpha.regions = 0.15,
          lwd = 2, hide = TRUE) +
  mapview(h3_r7,
          layer.name = "H3 r7",
          alpha.regions = 0.08,
          lwd = 0.3) +
  mapview(h3_r8,
          layer.name = "H3 r8",
          alpha.regions = 0.12,
          lwd = 0.25) +
  mapview(h3_r9_lines,
          layer.name = "H3 r9 (lines, core only)",
          alpha = 0.9,
          lwd = 0.4, hide = TRUE)

m

# # ============================================================
# # 7) Save HTML
# # ============================================================
# 
# out_html <- "C:/gislab/speedlocal/html_karta/bornholm_h3_r7_r8_r9core.html"
# 
# dir.create(dirname(out_html), recursive = TRUE, showWarnings = FALSE)
# dir.exists(dirname(out_html))
# 
# 
# htmlwidgets::saveWidget(m@map, out_html, selfcontained = FALSE)
# file.exists(out_html)
# 
# browseURL(out_html)