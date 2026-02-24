# speedlocal_bornholm

GC4 geocontext workspace for Bornholm, with Quarto reports and a Streamlit app.

## Streamlit app (GC4)

Location:
- `apps/gc4/app.py`
- `apps/gc4/requirements.txt`

Run (PowerShell):

```powershell
cd C:\gislab\speedlocal_bornholm
.\.venv\Scripts\python -m pip install -r apps\gc4\requirements.txt
.\.venv\Scripts\python -m streamlit run apps\gc4\app.py
```

## Data used by app

The app reads:
- `jyp_note_book_geocontext/bornholm_points_with_context_gc4.csv`
- `jyp_note_book_geocontext/bornholm_r8_factor_scores_gc4.csv`
- `jyp_note_book_geocontext/bornholm_r8_factor_loadings_gc4.csv`

## What is interactive

- `k` selector (`10, 50, 100, 250, 1000`)
- weight sliders for four core layers:
  - roads
  - fastboende
  - ecology
  - cultural
- hotspot percentile threshold
- cluster and factor filters
- center-focused map ("diva map") linked to diagnostics and tables
