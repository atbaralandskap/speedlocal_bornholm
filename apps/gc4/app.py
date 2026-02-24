import re
from pathlib import Path

import altair as alt
import numpy as np
import pandas as pd
import streamlit as st


K_OPTIONS = [10, 50, 100, 250, 1000]
FACTOR_COLS = ["F1", "F2", "F3", "F4", "F5"]
INDICATORS = {
    "roads": "roads_simplified",
    "fastboende": "fastboende",
    "ecology": "ecology_connectivity",
    "cultural": "cultural_and_historical_conservation",
}


@st.cache_data(show_spinner=False)
def load_data(base_path: Path) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    ctx = pd.read_csv(base_path / "jyp_note_book_geocontext" / "bornholm_points_with_context_gc4.csv")
    scores = pd.read_csv(base_path / "jyp_note_book_geocontext" / "bornholm_r8_factor_scores_gc4.csv")
    loadings = pd.read_csv(base_path / "jyp_note_book_geocontext" / "bornholm_r8_factor_loadings_gc4.csv")
    return ctx, scores, loadings


def zscore(series: pd.Series) -> pd.Series:
    std = series.std(ddof=0)
    if std == 0 or np.isnan(std):
        return pd.Series(np.zeros(len(series)), index=series.index)
    return (series - series.mean()) / std


def find_indicator_col(columns: list[str], indicator_token: str, k: int) -> str | None:
    pattern = re.compile(rf"^mean_gc_.*{re.escape(indicator_token)}.*_k{k}$")
    for col in columns:
        if pattern.match(col):
            return col
    return None


def normalize_weights(weights: dict[str, float]) -> dict[str, float]:
    total = sum(weights.values())
    if total <= 0:
        return {k: 0.0 for k in weights}
    return {k: v / total for k, v in weights.items()}


st.set_page_config(page_title="Bornholm GC4 Explorer", layout="wide")
st.title("Bornholm GC4 Explorer")
st.caption("Interaktiv version av Quarto-rapporten med fokus pa de fyra geocontext-lagren.")

base_path = Path(__file__).resolve().parent
ctx, scores, loadings = load_data(base_path)
df = ctx.merge(scores, on="hex_id", how="left")
all_cols = df.columns.tolist()

st.sidebar.header("Kontroller")
k_choice = st.sidebar.select_slider("Kontextstorlek (k)", options=K_OPTIONS, value=100)
normalize = st.sidebar.toggle("Normalisera vikter till 100%", value=True)
hotspot_pct = st.sidebar.slider("Hotspot-percentil", min_value=70, max_value=99, value=90, step=1)

st.sidebar.subheader("Vikter (4 lager)")
w_roads = st.sidebar.slider("Roads", min_value=0.0, max_value=2.0, value=1.0, step=0.05)
w_fast = st.sidebar.slider("Fastboende", min_value=0.0, max_value=2.0, value=1.0, step=0.05)
w_eco = st.sidebar.slider("Ecology", min_value=0.0, max_value=2.0, value=1.0, step=0.05)
w_cult = st.sidebar.slider("Cultural", min_value=0.0, max_value=2.0, value=1.0, step=0.05)

weights = {"roads": w_roads, "fastboende": w_fast, "ecology": w_eco, "cultural": w_cult}
if normalize:
    weights = normalize_weights(weights)

indicator_cols = {
    name: find_indicator_col(all_cols, token, k_choice) for name, token in INDICATORS.items()
}
missing = [k for k, v in indicator_cols.items() if v is None]
if missing:
    st.error(f"Saknar kolumner for valda lager/k: {missing}")
    st.stop()

for ind, col in indicator_cols.items():
    df[f"{ind}_z"] = zscore(df[col])

df["composite_score"] = sum(df[f"{ind}_z"] * w for ind, w in weights.items())

upper_thr = float(np.nanpercentile(df["composite_score"], hotspot_pct))
lower_thr = float(np.nanpercentile(df["composite_score"], 100 - hotspot_pct))
df["priority_zone"] = np.select(
    [df["composite_score"] >= upper_thr, df["composite_score"] <= lower_thr],
    ["hotspot", "coldspot"],
    default="neutral",
)

class_options = sorted(df["class_km"].dropna().unique().tolist())
selected_classes = st.sidebar.multiselect(
    "Klusterfilter (class_km)",
    options=class_options,
    default=class_options,
)

st.sidebar.subheader("Faktorfilter")
factor_ranges: dict[str, tuple[float, float]] = {}
for factor in FACTOR_COLS:
    min_v = float(np.floor(df[factor].min() * 10) / 10)
    max_v = float(np.ceil(df[factor].max() * 10) / 10)
    factor_ranges[factor] = st.sidebar.slider(
        factor, min_value=min_v, max_value=max_v, value=(min_v, max_v), step=0.1
    )

filtered = df[df["class_km"].isin(selected_classes)].copy()
for factor, (lo, hi) in factor_ranges.items():
    filtered = filtered[(filtered[factor] >= lo) & (filtered[factor] <= hi)]

if filtered.empty:
    st.warning("Inga observationer kvar efter filter. Justera kontrollerna i sidpanelen.")
    st.stop()

top_cluster = int(filtered["class_km"].value_counts().idxmax())
factor_sd = filtered[FACTOR_COLS].std(numeric_only=True).sort_values(ascending=False)

c1, c2, c3, c4 = st.columns(4)
c1.metric("Hexagons (filtrerade)", f"{len(filtered):,}")
c2.metric("Andel av total", f"{len(filtered) / len(df):.1%}")
c3.metric("Dominerande kluster", str(top_cluster))
c4.metric("Storsta faktorvariation", factor_sd.index[0])

focus_mode = st.sidebar.radio(
    "Fokus i kartan",
    options=["Ingen", "Top hotspot", "Välj hex_id"],
    horizontal=False,
)
default_focus = None
if focus_mode == "Top hotspot":
    default_focus = filtered.sort_values("composite_score", ascending=False)["hex_id"].iloc[0]
elif focus_mode == "Välj hex_id":
    default_focus = st.sidebar.selectbox(
        "Hexagon-id",
        options=filtered["hex_id"].sort_values().tolist(),
        index=0,
    )

map_df = filtered[["hex_id", "east", "north", "class_km", "priority_zone", "composite_score"] + FACTOR_COLS].copy()
map_df["focus"] = np.where(map_df["hex_id"] == default_focus, "fokus", "normal")
map_df["score_abs"] = np.abs(map_df["composite_score"])

cluster_counts = (
    filtered["class_km"].value_counts().sort_index().rename_axis("class_km").reset_index(name="n_hex")
)
score_bins = np.histogram(filtered["composite_score"], bins=30)
hist_df = pd.DataFrame({"bin_left": score_bins[1][:-1], "count": score_bins[0]})

corr_rows = []
for indicator in INDICATORS:
    col = indicator_cols[indicator]
    for factor in FACTOR_COLS:
        corr = filtered[[col, factor]].corr(numeric_only=True).iloc[0, 1]
        corr_rows.append({"indicator": indicator, "factor": factor, "corr": corr})
corr_df = pd.DataFrame(corr_rows)

profile = (
    filtered.groupby("class_km", as_index=False)[FACTOR_COLS]
    .mean(numeric_only=True)
    .melt(id_vars="class_km", var_name="factor", value_name="mean_value")
)

st.markdown("### GC4-karta i centrum")
left_col, center_col, right_col = st.columns([1.1, 2.6, 1.1], gap="medium")

with left_col:
    st.caption("Scorefordelning")
    st.bar_chart(hist_df, x="bin_left", y="count", use_container_width=True)
    st.caption("Klusterbalans")
    st.bar_chart(cluster_counts, x="class_km", y="n_hex", use_container_width=True)

with center_col:
    st.caption("Diva-kartan: alla kontroller och filter slar igenom direkt.")
    base = alt.Chart(map_df).encode(
        x=alt.X("east:Q", title="East"),
        y=alt.Y("north:Q", title="North"),
        tooltip=["hex_id", "class_km", "priority_zone", "composite_score"] + FACTOR_COLS,
    )
    normal_layer = base.mark_circle().encode(
        color=alt.Color(
            "priority_zone:N",
            scale=alt.Scale(domain=["coldspot", "neutral", "hotspot"], range=["#4575b4", "#bdbdbd", "#d73027"]),
            legend=alt.Legend(title="Zone"),
        ),
        size=alt.Size("score_abs:Q", title="|Composite|", scale=alt.Scale(range=[20, 260])),
        opacity=alt.condition(alt.datum.focus == "fokus", alt.value(1.0), alt.value(0.6)),
    )
    focus_layer = base.transform_filter(alt.datum.focus == "fokus").mark_point(
        shape="diamond", filled=True, size=350, color="#111111", stroke="#ffffff", strokeWidth=1.2
    )
    st.altair_chart((normal_layer + focus_layer).properties(height=680), use_container_width=True)

with right_col:
    st.caption("Korrelation (faktor vs 4 lager)")
    st.dataframe(
        corr_df.pivot(index="indicator", columns="factor", values="corr").round(3),
        use_container_width=True,
        height=250,
    )
    if default_focus is not None:
        focus_row = filtered.loc[filtered["hex_id"] == default_focus, ["hex_id", "class_km", "composite_score"] + FACTOR_COLS]
        st.caption("Fokushexagon")
        st.dataframe(focus_row.round(3), use_container_width=True, height=120)

st.subheader("Genomsnittlig faktorprofil per kluster")
st.line_chart(profile, x="factor", y="mean_value", color="class_km", use_container_width=True)

st.subheader("Topp- och bottenhexagons")
t1, t2 = st.columns(2)
top_n = st.slider("Antal rader i topp/botten-tabeller", 5, 50, 15, 1)
show_cols = ["hex_id", "class_km", "composite_score"] + FACTOR_COLS + list(indicator_cols.values())
t1.dataframe(
    filtered.sort_values("composite_score", ascending=False)[show_cols].head(top_n),
    use_container_width=True,
)
t2.dataframe(
    filtered.sort_values("composite_score", ascending=True)[show_cols].head(top_n),
    use_container_width=True,
)

st.subheader("Loadings-check (fran GC4)")
valid_loading_cols = [c for c in FACTOR_COLS if c in loadings.columns]
if valid_loading_cols:
    loadings_view = loadings[["variable"] + valid_loading_cols].copy()
    st.dataframe(loadings_view, use_container_width=True, height=320)
else:
    st.info("Kunde inte lasa faktorloadings.")
