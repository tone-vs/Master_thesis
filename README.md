# Mapping Norway in the Global Semiconductor Value Chain

Master's thesis — UC3M / NHH, 2025.  
Supervisor: [Francisco Villamil]

## Project overview

This repository analyses Norway's structural position in the global semiconductor
value chain using network analysis (SNA) and Exponential Random Graph Models
(ERGMs). Trade data come from UN Comtrade (v2 API), OECD BTIGE, and Taiwan ITA.

## Repository structure

```
create_data/                  Data preparation scripts (run in order 01–06)
analyses/                     SNA and ERGM analysis scripts
plots/                        Visualisation scripts
data/
  raw/                        Raw input files (not committed — see Data section below)
  processed/                  Pipeline outputs (git-ignored; regenerate via pipeline)
thesis_project/               LaTeX source for the thesis (Overleaf-compatible)
  chapters/                   .tex chapter files
  bibliography/               references.bib
  analyses/output/            ← R scripts write .tex tables HERE (committed)
  plots/output/               ← R scripts write .pdf/.png figures HERE (committed)
  main.tex                    Master LaTeX document
config.R                      Shared constants (YEARS, HS codes, DIRS paths)
thesis_analysis.Rmd           R Markdown notebook (optional; knit after pipeline)
```

All R scripts write their outputs directly into `thesis_project/analyses/output/`
and `thesis_project/plots/output/` using the `DIRS$tables` and `DIRS$figures`
variables defined in `config.R`. This means running any analysis script
automatically updates the LaTeX project.

## Setup

### 1. R packages

```r
install.packages(c(
  "comtradr", "dplyr", "tidyr", "readr", "purrr", "igraph",
  "tidygraph", "ggraph", "ggplot2", "ggrepel", "patchwork",
  "countrycode", "WDI", "unvotes", "lubridate", "scales",
  "statnet", "ergm", "modelsummary", "cli"
))
```

### 2. Comtrade API key

Register at <https://comtradeplus.un.org/> and add your key to `.Renviron`:

```
COMTRADE_PRIMARY_KEY=your_key_here
```

Then restart R (`Sys.getenv("COMTRADE_PRIMARY_KEY")` should return your key).

### 3. Raw data files

Place the following files in `data/raw/` before running the pipeline:

| File | Source |
|------|--------|
| `oecd_btige_taiwan.csv` | OECD BTIGE — CPA C261+C265 |
| `frontend_taiwan.csv` | Taiwan ITA customs portal (rename download) |
| `oecd_patents_wipo.csv` | OECD WIPO patent counts |
| `oecd_rca_taiwan.csv` | OECD total-export data for Taiwan RCA denominator |

## Running the pipeline

### Full pipeline (recommended)

```bash
make
```

### Step by step

```bash
make data    # run create_data/01–06 in order
make plots   # run plots/13_trade_plots.R
```

Or run individual scripts from the **project root**:

```bash
Rscript create_data/01_country_selection.R
Rscript create_data/02_comtrade_pull.R
Rscript create_data/03_taiwan_data.R
Rscript create_data/04_patent_data.R
Rscript create_data/05_build_network_data.R
Rscript create_data/06_geopolitical_attrs.R
Rscript plots/13_trade_plots.R
```

Then knit `final_analysis.Rmd` in RStudio or via:

```bash
Rscript -e "rmarkdown::render('final_analysis.Rmd')"
```

### Script run order and outputs

| Script | Inputs | Key outputs |
|--------|--------|-------------|
| `01_country_selection.R` | Comtrade API | `country_selection.csv`, `total_exports.csv` |
| `02_comtrade_pull.R` | Comtrade API, `country_selection.csv` | `semiconductor_network.csv` |
| `03_taiwan_data.R` | `data/raw/oecd_btige_taiwan.csv`, `frontend_taiwan.csv` | `taiwan_full.csv` |
| `04_patent_data.R` | `data/raw/oecd_patents_wipo.csv` | `patents_avg.csv` |
| `05_build_network_data.R` | All above | `graph_*.rds`, `edges_*.csv`, `node_attributes.csv`, `edges_raw.rds`, `nodes.rds` |
| `06_geopolitical_attrs.R` | `node_attributes.csv`, WDI API, unvotes | `node_geopolitical.csv`, `dyad_unga_similarity.csv`, `unga_similarity_matrix.rds` |
| `13_trade_plots.R` | `edges_raw.rds`, `nodes.rds` | `plots/output/fig0*.pdf` |

## Notes

- Scripts **must be run from the project root** (the folder containing `config.R`).
  The Makefile enforces this automatically.
- API calls in `01` and `02` cost Comtrade quota (free tier: 250 calls/day).
  The script in `02` has checkpoint/resume logic — safe to interrupt and restart.
- Taiwan does not report to UN Comtrade. Backend flows use OECD BTIGE;
  frontend flows use Taiwan ITA (2022 only). The 2019 frontend network
  therefore excludes Taiwan — documented as a limitation in the thesis.
- The BE 2022 network is near-saturated (~88% density). ERGM covariate models
  are unidentifiable at this density; only structural terms (`edges + mutual`)
  are reported for that network. See thesis Section 4.3 for discussion.
