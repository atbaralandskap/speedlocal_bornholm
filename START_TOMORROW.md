# Start Tomorrow

## Current State
- Aggregation complete for Bornholm r8 (`1126` hexes).
- Geocontext tables/views are built in PostGIS.
- QGIS-ready views exist for features, z-scores, and composite score.

## First Commands Tomorrow
```powershell
$env:PIPELINE_ENV_PATH='C:/gislab/databas/generell_databas_setup/.env'
Rscript databas/script/04_build_bornholm_r8_geocontext_from_selection.R
Rscript databas/script/05_finalize_bornholm_r8_geocontext_features.R
Rscript databas/script/06_build_bornholm_r8_geocontext_score.R
Rscript databas/script/07_create_bornholm_r8_qgis_views.R
```

## QGIS Layers To Check
- `h3.v_bornholm_r8_geocontext_features`
- `h3.v_bornholm_r8_geocontext_zscores`
- `h3.v_bornholm_r8_geocontext_score`

## Tuning File
- Edit indicator weights/direction/transform in:
  - `databas/script/config/bornholm_r8_geocontext_scoring.csv`

## Important Git Note
- New scripts are currently in `C:\gislab\databas\script`, which is **not** inside a Git repo.
- Before commit/push, move these files into your target repo (or make `C:\gislab` a repo).

## Files Created Today
- `databas/script/05_finalize_bornholm_r8_geocontext_features.R`
- `databas/script/06_build_bornholm_r8_geocontext_score.R`
- `databas/script/07_create_bornholm_r8_qgis_views.R`
- `databas/script/config/bornholm_r8_geocontext_feature_map.csv`
- `databas/script/config/bornholm_r8_geocontext_scoring.csv`
