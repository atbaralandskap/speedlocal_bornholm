library(visNetwork)
library(dplyr)
library(tibble)

# ==========================================================
# NODES
# ==========================================================

nodes <- tribble(
  ~id, ~label, ~group,
  
  # Core / backbone
  "ingest",      "Data ingestion & harmonisation", "core",
  "postgis",     "PostGIS\n(project database / spatial backbone)", "database",
  "gis_analysis","GIS analysis\n(overlays, Geocontext, buffers, H3, indicators)", "core",
  "gislab",      "GISLab", "core",
  "github",      "GitHub\n(repo, issues, versionering)", "core",
  
  # Outputs
  # "dash",        "Dashboards & maps\n(Streamlit, sliders)", "output",
  "streamlit",   "Streamlit app / interaktiv karta", "output",
  "kartapplikation",
  "Kartapplikation\n(interaktiv utforskning av scenarier & acceptans)", "output",
  "energilandskap",
  "Energilandskap\n(energi, sociala värden, landskap i relation)", "output",
  "lablab_method","Metodutveckling", "output",
  "speedlocal_delivery","SpeedLocal\n(leverans & beslutstöd)", "output",
  
  # Social
  "social",      "Social data\n(surveys, acceptance)", "input_social",
  "ivl",         "IVL\n(Svenska Miljöinstitutet)", "input_social",
  
  # External geodata
  "external",        "External geodata\n(OSM, national data)", "input_external",
  "landscape_types", "Landskapstyper", "input_external",
  "buildings",       "Bebyggelse", "input_external",
  "urban_areas",     "Tätort", "input_external",
  "road_network",    "Vägnät", "input_external",
  "water",           "Vatten", "input_external",
  "nature_reserves", "Naturreservat", "input_external",
  "protected_areas", "Skyddade områden", "input_external",
  "elevation_topo",  "Höjddata / topografi", "input_external",
  "grid_elevation",  "Varje grid får en höjddata", "input_external",
  "viewshed",        "Siktanalys / vegetation", "input_external",
  "grid_network",    "Transmission / distribution", "input_external",
  "classifications", "Klassificeringar\n(bostadsområde, industri mm)", "input_external",
  
  # Energy modelling
  "duckdb",     "DuckDB\n(model output store)", "input_energy",
  "energy_out", "Energy model outputs\n(TIMES scenarios)", "input_energy",
  "eml",        "EML\n(Energy Modelling Lab)", "input_energy"
)

# ==========================================================
# EDGES
# ==========================================================

edges <- tribble(
  ~from, ~to,
  
  # External structure
  "landscape_types","external",
  "buildings","external",
  "urban_areas","external",
  "road_network","external",
  "water","external",
  "nature_reserves","external",
  "protected_areas","external",
  "elevation_topo","external",
  "grid_elevation","external",
  "viewshed","external",
  "grid_network","external",
  "classifications","external",
  "external","ingest",
  
  # Database flow
  "ingest","postgis",
  "postgis","gis_analysis",
  "gis_analysis","postgis",
  "postgis","kartapplikation",
  "kartapplikation","postgis",
  
  # Social flow (ONLY forward)
  "ivl","social",
  "social","energy_out",
  
  # Energy modelling
  "eml","energy_out",
  "duckdb","energy_out",
  "energy_out","duckdb",
  
  # Database <-> energy store
  "duckdb","postgis",
  "postgis","duckdb",
  
  # Applications
  # "postgis","dash",
  # "dash","kartapplikation",
  "kartapplikation","energilandskap",
  "kartapplikation","lablab_method",
  "kartapplikation","speedlocal_delivery",
  "lablab_method", "gis_analysis",
  "energilandskap", "speedlocal_delivery",
  
  # Streamlit integrations
  "streamlit","postgis",
  "postgis","streamlit",
  "streamlit","duckdb",
  "duckdb","streamlit",
  "streamlit","eml",
  "eml","streamlit",
  
  # Organisational layer
  "gislab","gis_analysis",
  "gis_analysis","gislab",
  # "gislab","streamlit",
  # "streamlit","gislab",
  "github","eml",
  "eml","github",
  "github","gislab",
  "gislab","github"
)

# ==========================================================
# VISUALISATION
# ==========================================================

visNetwork(nodes, edges, width = "100%", height = "100vh") |>
  visEdges(arrows = "to") |>
  visGroups(groupname = "core",           color = "#ffd92f") |>
  visGroups(groupname = "database",       color = "#8da0cb") |>
  visGroups(groupname = "input_social",   color = "#66c2a5") |>
  visGroups(groupname = "input_energy",   color = "#fc8d62") |>
  visGroups(groupname = "input_external", color = "#a6d854") |>
  visGroups(groupname = "output",         color = "#e78ac3") |>
  visPhysics(
    solver = "repulsion",
    repulsion = list(nodeDistance = 180)
  )
