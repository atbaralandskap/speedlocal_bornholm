from pathlib import Path

import altair as alt
import numpy as np
import pandas as pd
import streamlit as st


SCENARIO_TOTALS_TWH = {
    "Teknologioptimist": {2030: 190.0, 2040: 320.0, 2050: 370.0},
    "Gjennomgripende omstilling": {2030: 180.0, 2040: 225.0, 2050: 235.0},
    "Litt her og der": {2030: 170.0, 2040: 235.0, 2050: 245.0},
    "Ny hverdag": {2030: 175.0, 2040: 185.0, 2050: 182.0},
}

BASE_MIX_PCT = {
    "Teknologioptimist": {"wind": 52.0, "solar": 16.0, "nuclear": 10.0},
    "Gjennomgripende omstilling": {"wind": 42.0, "solar": 24.0, "nuclear": 7.0},
    "Litt her og der": {"wind": 34.0, "solar": 15.0, "nuclear": 5.0},
    "Ny hverdag": {"wind": 28.0, "solar": 12.0, "nuclear": 4.0},
}

# km2 per TWh (fallback values until AreaDemand mapping is finalized)
DEFAULT_AREA_FACTORS = {"wind": 1.20, "solar": 2.10, "nuclear": 0.12}

TECH_ALIASES = {
    "wind": ["wind", "vind", "onshore", "offshore"],
    "solar": ["solar", "pv", "sol"],
    "nuclear": ["nuclear", "karn", "karnkraft", "atom"],
}


@st.cache_data(show_spinner=False)
def load_bornholm_points(base_path: Path) -> pd.DataFrame:
    points_path = base_path / "jyp_note_book_geocontext" / "bornholm_points_with_context_gc4.csv"
    if points_path.exists():
        df = pd.read_csv(points_path)
        cols = [c for c in ["hex_id", "east", "north"] if c in df.columns]
        df = df[cols].copy()
        if "hex_id" not in df.columns:
            df["hex_id"] = np.arange(1, len(df) + 1)
        return df

    # Fallback synthetic points (rough island blob in local coordinates)
    grid_x = np.linspace(0, 100, 24)
    grid_y = np.linspace(0, 130, 30)
    rows = []
    idx = 1
    for x in grid_x:
        for y in grid_y:
            cx = (x - 52.0) / 42.0
            cy = (y - 66.0) / 56.0
            if (cx * cx + cy * cy) <= 1.0:
                rows.append({"hex_id": idx, "east": x, "north": y})
                idx += 1
    return pd.DataFrame(rows)


def _find_first_col(columns: list[str], tokens: list[str]) -> str | None:
    for c in columns:
        low = c.lower()
        if any(t in low for t in tokens):
            return c
    return None


@st.cache_data(show_spinner=False)
def load_area_factors(repo_root: Path) -> tuple[dict[str, float], pd.DataFrame, str]:
    xlsx_path = repo_root / "eml" / "data" / "raw" / "AreaDemand.xlsx"
    if not xlsx_path.exists():
        return DEFAULT_AREA_FACTORS, pd.DataFrame(), "fallback: AreaDemand.xlsx saknas"

    try:
        raw = pd.read_excel(xlsx_path, sheet_name=0)
    except Exception as exc:
        return DEFAULT_AREA_FACTORS, pd.DataFrame(), f"fallback: kunde inte lasa excel ({exc})"

    if raw.empty:
        return DEFAULT_AREA_FACTORS, raw, "fallback: excel tom"

    df = raw.copy()
    df.columns = [str(c).strip() for c in df.columns]
    cols = list(df.columns)
    tech_col = _find_first_col(cols, ["tech", "technology", "energi", "energy", "type", "slag"])
    area_col = _find_first_col(cols, ["area", "land", "km2", "km^2", "demand", "factor"])

    if tech_col is None or area_col is None:
        return DEFAULT_AREA_FACTORS, df, "fallback: kunde inte identifiera teknik-/areakolumn"

    extracted = {}
    work = df[[tech_col, area_col]].copy()
    work[tech_col] = work[tech_col].astype(str).str.lower().str.strip()
    work[area_col] = pd.to_numeric(work[area_col], errors="coerce")
    work = work.dropna(subset=[area_col])

    for target, aliases in TECH_ALIASES.items():
        match = work[work[tech_col].apply(lambda v: any(a in v for a in aliases))]
        if not match.empty:
            extracted[target] = float(match[area_col].iloc[0])

    factors = DEFAULT_AREA_FACTORS.copy()
    factors.update(extracted)
    return factors, df, "loaded"


def build_spatial_pressure(points: pd.DataFrame, area_by_source: dict[str, float]) -> pd.DataFrame:
    out = points.copy()
    east = out["east"].astype(float)
    north = out["north"].astype(float)
    ex = (east - east.min()) / max(1e-9, east.max() - east.min())
    ny = (north - north.min()) / max(1e-9, north.max() - north.min())

    # Simple synthetic suitability patterns for first prototype.
    wind_pref = 0.5 + 0.5 * (np.abs(ex - 0.5) * 1.6)
    solar_pref = 0.4 + 0.6 * ((1.0 - np.abs(ex - 0.5)) * (1.0 - np.abs(ny - 0.55)))
    nuclear_pref = np.exp(-(((ex - 0.38) ** 2) / 0.01 + ((ny - 0.45) ** 2) / 0.02))
    nuclear_pref = nuclear_pref / max(1e-9, float(nuclear_pref.max()))

    raw = (
        area_by_source["wind"] * wind_pref
        + area_by_source["solar"] * solar_pref
        + area_by_source["nuclear"] * nuclear_pref
    )
    out["land_pressure"] = raw / max(1e-9, float(raw.mean()))
    out["land_pressure"] = out["land_pressure"].round(3)
    return out


st.set_page_config(page_title="Bornholm Energy Land Use Prototype", layout="wide")
st.title("Bornholm Energy Land Use Prototype")
st.caption("Enkel prototyp: 4 scenarier + AreaDemand-koppling + spatial markansprakskarta.")

app_base = Path(__file__).resolve().parent
repo_root = app_base.parents[2]
points = load_bornholm_points(app_base)
area_factors, area_raw, area_status = load_area_factors(repo_root)

st.sidebar.header("Scenario")
scenario = st.sidebar.selectbox("Valj framtidsbild", list(SCENARIO_TOTALS_TWH.keys()), index=0)
year = st.sidebar.select_slider("Ar", options=[2030, 2040, 2050], value=2050)

base_total = SCENARIO_TOTALS_TWH[scenario][year]
base_mix = BASE_MIX_PCT[scenario]

st.sidebar.subheader("Elmix sliders (%)")
wind_pct = st.sidebar.slider("Vind", 0.0, 100.0, float(base_mix["wind"]), 1.0)
solar_pct = st.sidebar.slider("Sol", 0.0, 100.0, float(base_mix["solar"]), 1.0)
nuclear_pct = st.sidebar.slider("Karnkraft", 0.0, 100.0, float(base_mix["nuclear"]), 1.0)
sum_pct = wind_pct + solar_pct + nuclear_pct

if sum_pct > 100.0:
    st.error("Summan av vind + sol + karnkraft kan inte vara over 100%.")
    st.stop()

other_pct = 100.0 - sum_pct
twh_by_source = {
    "wind": base_total * wind_pct / 100.0,
    "solar": base_total * solar_pct / 100.0,
    "nuclear": base_total * nuclear_pct / 100.0,
    "other": base_total * other_pct / 100.0,
}
area_by_source = {
    "wind": twh_by_source["wind"] * area_factors["wind"],
    "solar": twh_by_source["solar"] * area_factors["solar"],
    "nuclear": twh_by_source["nuclear"] * area_factors["nuclear"],
}
total_area = float(sum(area_by_source.values()))

c1, c2, c3, c4 = st.columns(4)
c1.metric("Scenario", scenario)
c2.metric("Ar", str(year))
c3.metric("Total el (TWh)", f"{base_total:.1f}")
c4.metric("Markansprak (km2)", f"{total_area:.1f}")

mix_df = pd.DataFrame(
    [
        {"source": "Vind", "twh": twh_by_source["wind"], "area_km2": area_by_source["wind"]},
        {"source": "Sol", "twh": twh_by_source["solar"], "area_km2": area_by_source["solar"]},
        {"source": "Karnkraft", "twh": twh_by_source["nuclear"], "area_km2": area_by_source["nuclear"]},
        {"source": "Other", "twh": twh_by_source["other"], "area_km2": 0.0},
    ]
)

left, right = st.columns([1.1, 1.9], gap="large")

with left:
    st.subheader("Energimix och markansprak")
    st.dataframe(mix_df.round(2), use_container_width=True, height=210)
    st.caption(
        f"AreaDemand-status: `{area_status}`. Faktorer (km2/TWh): "
        f"vind={area_factors['wind']:.3f}, sol={area_factors['solar']:.3f}, karnkraft={area_factors['nuclear']:.3f}"
    )
    if not area_raw.empty:
        with st.expander("Visa inlast AreaDemand (forsta 20 rader)"):
            st.dataframe(area_raw.head(20), use_container_width=True, height=240)

    st.subheader("Forandring mot scenario-bas")
    base_area = (
        base_total * base_mix["wind"] / 100.0 * area_factors["wind"]
        + base_total * base_mix["solar"] / 100.0 * area_factors["solar"]
        + base_total * base_mix["nuclear"] / 100.0 * area_factors["nuclear"]
    )
    delta = total_area - base_area
    st.metric("Delta markansprak (km2)", f"{delta:+.1f}")

with right:
    st.subheader("Bornholm-karta: relativ markansprakspress")
    spatial = build_spatial_pressure(points, area_by_source)
    chart = (
        alt.Chart(spatial)
        .mark_circle()
        .encode(
            x=alt.X("east:Q", title="East"),
            y=alt.Y("north:Q", title="North"),
            size=alt.Size("land_pressure:Q", scale=alt.Scale(range=[18, 420]), title="Relativ press"),
            color=alt.Color("land_pressure:Q", scale=alt.Scale(scheme="orangered"), title="Relativ press"),
            tooltip=["hex_id", "land_pressure"],
        )
        .properties(height=680)
    )
    st.altair_chart(chart, use_container_width=True)

st.markdown("### Baslinjer (mock, ersatts senare av riktig TIMES-data)")
baseline_df = (
    pd.DataFrame(SCENARIO_TOTALS_TWH)
    .rename_axis("year")
    .reset_index()
    .melt(id_vars="year", var_name="scenario", value_name="total_twh")
)
st.line_chart(baseline_df, x="year", y="total_twh", color="scenario", use_container_width=True)
