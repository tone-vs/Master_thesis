# Norway's Position In The Semiconductor Value Chain During Geopolitical Realignment 

Master's thesis — UC3M, 2025.  
Supervisor: Francisco Villamil

---

## Project overview

This repository analyses the global semiconductor value chain and Norway's structural position in it
using network analysis (SNA) and Exponential Random Graph Models
(ERGMs). The pipeline constructs bilateral trade networks from UN Comtrade data
and Taiwan ITA customs data, computes centrality and community structure, and
estimates ERGM models to explain the probability of trade ties between countries.

The focal country is Norway (NOR). Nordic comparators Sweden, Finland, and
Denmark are included by research design.

---

## Data sources

### Obtained via API (automated on first run, cached locally)

| Source | Script | What it provides |
|--------|--------|-----------------|
| UN Comtrade v2 API | `01_country_selection.R`, `02_comtrade_pull.R` | Bilateral HS6 semiconductor trade flows, 2019 and 2022 |
| World Bank WDI API | `06_geopolitical_attrs.R` | GDP and GDP per capita, 2019 and 2022 |
| `unvotes` R package | `06_geopolitical_attrs.R` | UN General Assembly voting records, 2017–2019 |
| `cepiigeodist` R package | `06_geopolitical_attrs.R` | Bilateral geographic distance (CEPII GeoDist) |

All API results are cached to CSV/RDS on first run. Re-running a script
skips the API call if the output file already exists.

**Comtrade API key required.** Register at <https://comtradeplus.un.org/>.
Add to `.Renviron` (run `usethis::edit_r_environ()` in R):

```
COMTRADE_PRIMARY_KEY=your_key_here
```

Restart R and verify: `Sys.getenv("COMTRADE_PRIMARY_KEY")`.

Free tier: 250 API calls/day, 1 call/second.
Script `02` uses checkpoint/resume logic — safe to interrupt and restart.

### Manual download required

Place these files in `data/raw/` before running the pipeline:

| File | Source | Notes |
|------|--------|-------|
| `frontend_taiwan.csv` | [Taiwan ITA customs portal](https://portal.sw.nat.gov.tw/APGA/GA30E) | Query: Total Exports + Imports, Annual 2022, Partner = World; CCC codes: 381800, 848610-848640, 848690, 903082 |
| `backend_taiwan.csv` | Same portal | Same query; CCC codes: 852351-852359, 854110-854190, 854231-854239, 854290 |
| `oecd_patents_wipo.csv` | [OECD patent statistics](https://stats.oecd.org/) | WIPO patent families by inventor country, 2019-2022 |
| `oecd_rca_taiwan.csv` | [OECD trade data](https://stats.oecd.org/) | Taiwan total exports by year — used as RCA denominator for TWN only |

> **Note:** Taiwan does not report to UN Comtrade. Taiwan's trade data comes
> from the ITA customs portal (2022 only) and is used only for descriptive
> SNA. Taiwan is excluded from all ERGM specifications (no UNGA voting record).

---

## Pipeline run order

All scripts must be run from the **project root** (the folder containing `config.R`).

```bash
Rscript create_data/01_country_selection.R   # Country set selection
Rscript create_data/02_comtrade_pull.R       # Bilateral HS6 trade data
Rscript create_data/03_taiwan_data.R         # Taiwan ITA processing
Rscript create_data/04_patent_data.R         # OECD patent data
Rscript create_data/05_build_network_data.R  # Build igraph objects + RCA
Rscript create_data/06_geopolitical_attrs.R  # GDP, UNGA, distance covariates

# Analysis (can be run independently once create_data/ is complete)
Rscript analyses/07_descriptive_trade.R
Rscript analyses/08_network_summary.R
Rscript analyses/09_centrality.R
Rscript analyses/10_community_detection.R
Rscript analyses/11_multiplex.R
Rscript analyses/12_ergm.R          

# Visualisations (depend on analyses/09 and 10)
Rscript plots/13_trade_plots.R
Rscript plots/14_network_viz.R
Rscript plots/15_centrality_plots.R
```

**Dependency notes:**
- `14_network_viz.R` requires `centrality_all.rds` (from 09) and `communities.rds` (from 10)
- `15_centrality_plots.R` requires `centrality_all.rds` (from 09)
- Scripts 07-15 can be run in any order within their tier once `create_data/` is complete

---

## Output files

Tables are written to `analyses/output/`. Figures are written to `plots/output/`.
Both directories are created automatically on first run.

| Script | Key outputs |
|--------|-------------|
| `01_country_selection.R` | `data/processed/country_selection.csv`, `total_exports.csv` |
| `02_comtrade_pull.R` | `data/raw/semiconductor/semiconductor_network.csv` |
| `03_taiwan_data.R` | `data/processed/taiwan_full.csv`, `taiwan_ita_full.csv` |
| `04_patent_data.R` | `data/processed/patents_avg.csv` |
| `05_build_network_data.R` | `data/processed/graph_frontend_{2019,2022}.rds`, `graph_backend_{2019,2022}.rds`, `node_attributes.csv`, `edges_raw.rds` |
| `06_geopolitical_attrs.R` | `data/processed/node_geopolitical.rds`, `dyad_unga_similarity.csv`, `dist_matrix_log.rds` — also attaches geopolitical attributes to all four `graph_*.rds` |
| `07_descriptive_trade.R` | `analyses/output/table_norway_position.tex`, `table_top_partners.tex`, `table_layer_asymmetry.tex`, `table_hs_exports.tex`, `table_hs_imports.tex` |
| `08_network_summary.R` | `analyses/output/table_network_summary.tex`, `table_network_summary_appendix.tex` |
| `09_centrality.R` | `data/processed/centrality_all.rds`, `analyses/output/table_centrality_*.tex` (x5) |
| `10_community_detection.R` | `data/processed/communities.rds`, `community_alliance_overlap.csv`, `analyses/output/table_community_*.tex` (x4) |
| `11_multiplex.R` | `analyses/output/table_multiplex_cor.tex`, `table_multiplex_change.tex`, `table_multiplex_crosslayer.tex` |
| `12_ergm.R` | `analyses/output/table_ergm_backend.tex`, `table_ergm_layer.tex`, `table_ergm_temporal.tex`, `table_ergm_fe_temporal.tex`; `plots/output/fig_ergm_gof_*.pdf` (x4) |
| `13_trade_plots.R` | `plots/output/fig_norway_hs_combined.pdf`, `fig_norway_partners.pdf` |
| `14_network_viz.R` | `plots/output/net_combined_2022.pdf`, `net_combined_2019.pdf` |
| `15_centrality_plots.R` | `plots/output/cent_scatter_2022.pdf`, `cent_norway_change.pdf`, `cent_ranks_2022.pdf` |

---

## Key design decisions

### Country set
Countries are selected data-driven: ranked by total semiconductor export share
and added iteratively until the sample covers >=99% of world trade (OECD 2025
methodology). Norway, Sweden, Finland, and Denmark are forced inclusions by
research design. This yields approximately 30 countries.

### Two layers
- **Layer 1 — Front-end (layer1_frontend):** upstream/materials HS codes
  (polysilicon, silicon carbide, gallium/germanium, semiconductor equipment)
- **Layer 2 — Back-end (layer2_backend):** downstream HS codes
  (diodes, transistors, integrated circuits, flash memory)

See `config.R` for the full lists of `hs_layer1` and `hs_layer2` codes.

### Trade threshold
Bilateral product-level flows below USD 1,000 are excluded (`MIN_FLOW = 1000` 
in `config.R`), following Ou et al. (2024) who demonstrate this threshold 
retains more than 99.99% of total semiconductor trade value. The threshold 
is applied at the HS6 product level before aggregation to the bilateral dyad 
level in `05_build_network_data.R`.

### Taiwan data handling
Taiwan does not report to UN Comtrade. Its trade data is sourced from the
Taiwan ITA customs portal (2022 only).

| Context | Taiwan included? | Node count |
|---------|-----------------|-----------|
| Descriptive SNA (scripts 07-11, 13-15) | Yes — 2022 networks only | 30 nodes (2022), 29 nodes (2019) |
| ERGM specifications (script 12) | No | 29 nodes (all years) |

The 2019 exclusion is a data limitation (ITA data is 2022-only).
The ERGM exclusion is by design: Taiwan has no UN General Assembly voting
record and cannot be assigned a UNGA similarity score.

### ERGM structure
Four tables are produced:
- **Table A:** Back-end 2022 stepwise (M1: structural, M2: gravity, M3: full)
- **Table B:** Layer comparison — BE M3 vs FE M3 (2022)
- **Table C:** BE temporal — BE 2019 M3 vs BE 2022 M3
- **Table D:** FE temporal — FE 2019 M3 vs FE 2022 M3

All models use `seed = 42` for reproducibility.

### Community detection
Louvain algorithm with `seed = 42` set immediately before each call.
Market-share weights are used so economically stronger ties have more
influence on community assignment.

---

## Requirements

### R version
R >= 4.3 recommended. The native pipe `|>` is used throughout; R >= 4.1 required.

### Packages

```r
install.packages(c(
  # Data retrieval
  "comtradr", "WDI",
  # Data manipulation
  "dplyr", "tidyr", "readr", "purrr", "tibble", "lubridate",
  "scales", "forcats", "stringr",
  # Network analysis
  "igraph", "ggraph",
  # Statistical modelling
  "statnet", "ergm", "texreg",
  # Visualisation
  "ggplot2", "ggrepel", "patchwork",
  # Utilities
  "countrycode", "cepiigeodist", "unvotes", "cli",
  # Tables
  "kableExtra"
))
```

### Environment variables
```
COMTRADE_PRIMARY_KEY=your_key_here
```

### Working directory
All scripts must be run from the project root (the directory containing
`config.R`). The working directory is checked implicitly: all file paths
are relative (e.g., `"data/processed/..."`) and will fail if run from a
subdirectory.
