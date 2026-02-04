library(visNetwork)
library(dplyr)
library(tibble)

# ---- Nodes --------------------------------------------------------

nodes <- tribble(
  ~id,                  ~label,                                                             ~group,
  
  # Core / backbone
  "ingest",             "Data ingestion & harmonisation",                                    "core",
  "postgis",            "PostGIS\n(project database / spatial backbone)",                    "database",
  "gis_analysis",       "GIS analysis\n(overlays, nearest neighbours (Geocontext), buffers,\nH3, indicators)", "core",
  
  # Dashboards / deliveries
  "dash",               "Dashboards & maps\n(Quarto/Shiny, sliders)",                        "output",
  "lablab_method",      "LABLAB\n(metodutveckling)",                                         "output",
  "speedlocal_delivery","SpeedLocal\n(leverans & beslutstöd)",                               "output",
  
  # Social
  "social",             "Social data\n(surveys, acceptance)",                                "input_social",
  "ivl",                "IVL\n(Svenska Miljöinstitutet)",                                    "input_social",
  
  # External umbrella + themes
  "external",           "External geodata\n(OSM, national data)",                            "input_external",
  "landscape_types",    "Landskapstyper",                                                    "input_external",
  "buildings",          "Bebyggelse",                                                        "input_external",
  "urban_areas",        "Tätort",                                                            "input_external",
  "road_network",       "Vägnät",                                                            "input_external",
  "water",              "Vatten",                                                            "input_external",
  "nature_reserves",    "Naturreservat",                                                     "input_external",
  "protected_areas",    "Skyddade områden",                                                  "input_external",
  "elevation_topo",     "Höjddata / topografi",                                              "input_external",
  "grid_elevation",     "Varje grid får en höjddata",                                        "input_external",
  "viewshed",           "Siktanalys / kan vi få med växtlighet?",                            "input_external",
  "grid_network",       "Transmission / distribution",                                       "input_external",
  "classifications",    "Klassificeringar\n(bostadsområde, industri mm)",                    "input_external",
  
  # Energy modelling
  "duckdb",             "DuckDB\n(model output store (upstream))",                           "input_energy",
  "energy_out",         "Energy model outputs\n(TIMES scenarios, results)",                  "input_energy",
  "eml",                "EML\n(Energy Modelling Lab)",                                       "input_energy"
) %>%
  distinct(id, .keep_all = TRUE)

# ---- Edges --------------------------------------------------------

edges <- tribble(
  ~from,                 ~to,
  
  # External themes -> umbrella -> ingestion
  "landscape_types",     "external",
  "buildings",           "external",
  "urban_areas",         "external",
  "road_network",        "external",
  "water",               "external",
  "nature_reserves",     "external",
  "protected_areas",     "external",
  "elevation_topo",      "external",
  "grid_elevation",      "external",
  "viewshed",            "external",
  "grid_network",        "external",
  "classifications",     "external",
  "external",            "ingest",
  
  # Ingestion -> PostGIS
  "ingest",              "postgis",
  
  # Social provenance + shared access via PostGIS
  "ivl",                 "social",
  "social",              "postgis",
  "postgis",             "social",
  
  # GIS analysis loop on PostGIS
  "postgis",             "gis_analysis",
  "gis_analysis",        "postgis",
  
  # Dashboards sit on PostGIS and inform deliveries
  "postgis",             "dash",
  "dash",                "lablab_method",
  "dash",                "speedlocal_delivery",
  
  # Energy model outputs: EML produces outputs; DuckDB stores them (reciprocal)
  "eml",                 "energy_out",
  "duckdb",              "energy_out",
  "energy_out",          "duckdb",
  
  # DuckDB <-> PostGIS (delivery + reuse)
  "duckdb",              "postgis",
  "postgis",             "duckdb"
) %>%
  distinct(from, to, .keep_all = TRUE)

# ---- Visualisation ------------------------------------------------

visNetwork(nodes, edges, width = "100%", height = "700px") |>
  visEdges(arrows = "to") |>
  visGroups(groupname = "core",           color = "#ffd92f") |>
  visGroups(groupname = "database",       color = "#8da0cb") |>
  visGroups(groupname = "input_social",   color = "#66c2a5") |>
  visGroups(groupname = "input_energy",   color = "#fc8d62") |>
  visGroups(groupname = "input_external", color = "#a6d854") |>
  visGroups(groupname = "output",         color = "#e78ac3") |>
  visPhysics(solver = "repulsion",
             repulsion = list(nodeDistance = 180))
