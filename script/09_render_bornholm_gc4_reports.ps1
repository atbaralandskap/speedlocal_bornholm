param(
  [string]$ProjectRoot = "."
)

$ErrorActionPreference = "Stop"

$root = Resolve-Path $ProjectRoot
$geo = Join-Path $root "docs/geocontext"

if (-not (Test-Path $geo)) {
  throw "Could not find docs/geocontext from root: $root"
}

if (-not (Test-Path "C:\temp")) {
  New-Item -ItemType Directory -Path "C:\temp" | Out-Null
}
$env:TEMP = "C:\temp"
$env:TMP = "C:\temp"

Push-Location $geo
try {
  quarto render bornholm_gc4_F1.qmd --output bornholm_gc4_F1.html
  quarto render bornholm_gc4_F2.qmd --output bornholm_gc4_F2.html
  quarto render bornholm_gc4_F3.qmd --output bornholm_gc4_F3.html
  quarto render bornholm_gc4_F4.qmd --output bornholm_gc4_F4.html
  quarto render bornholm_gc4_F5.qmd --output bornholm_gc4_F5.html
  quarto render bornholm_gc4_report.qmd --output bornholm_gc4_report.html
  quarto render bornholm_gc4_index.qmd --output bornholm_gc4_index.html
}
finally {
  Pop-Location
}

Write-Host "Rendered Bornholm GC4 pages in docs/geocontext"
