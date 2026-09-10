# Cave Fauna of Greece — Analysis

Scripts and outputs for the [CFG database](https://database.inspee.gr), which tracks species occurrences across Greek cave sites.

---

## Requirements

- R ≥ 4.6
- System libraries: GDAL, GEOS, PROJ (required by `sf`)

Restore the exact R package environment:

```bash
Rscript -e "renv::restore()"
```

---

## Data

Download the five TSV exports from the CFG database admin panel and place them in `data/`. File names are auto-detected by pattern — no path changes needed in scripts.

| Pattern | Contents |
|---|---|
| `Caves_*.tsv` | Cave locations, coordinates, region, protection |
| `Census_*.tsv` | Species occurrence records |
| `Species_*.tsv` | Taxonomy, classification, conservation status |
| `Census_references_*.tsv` | References for census records |
| `Cave_References_*.tsv` | References for cave records |

---

## Running the analysis

All commands run from the **repo root**. Scripts must be run in order.

For regression checks and a single command to regenerate the corrected outputs,
see [local run instructions](LOCAL_RUN.md).

```bash
# 1. Spatial analysis — run first (produces island areas and richness map)
Rscript scripts/cfg_spatial_analysis.R

# 2. Geology
Rscript scripts/cfg_geology.R

# 3. Thematic questions (any order after steps 1–2)
Rscript scripts/cfg_questions.R
Rscript scripts/cfg_questions_caves.R
Rscript scripts/cfg_questions_islands.R
Rscript scripts/cfg_questions_species.R
Rscript scripts/cfg_questions_temporal.R

# 4. Website figures
Rscript scripts/cfg_website_plots.R

# 5. Composite figures for publication (PNG + TIFF, 300 DPI)
Rscript scripts/cfg_composite_figures.R
```

Outputs are written to `results/` (TSVs), `plots/` (individual figures), and `figures/` (composite publication figures).

---

## Repository layout

```
scripts/        analysis scripts
data/           TSV exports from CFG database (not tracked)
spatial_data/   shapefiles (not tracked)
results/        computed TSVs
plots/          individual figure PNGs
figures/        composite figures (PNG + TIFF)
website_plots/  figures for the database portal
renv.lock       pinned R package versions
```
