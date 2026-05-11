## Makefile — Semiconductor GVC Pipeline
## Run from the project root: make
## Requires R >= 4.3 and all packages listed in README.md

R = Rscript

# Sentinel files track which scripts have completed successfully.
# They are written to data/processed/ (git-ignored) so a clean checkout
# always re-runs the full pipeline.
SENTINEL_DIR = data/processed

S01 = $(SENTINEL_DIR)/.done_01
S02 = $(SENTINEL_DIR)/.done_02
S03 = $(SENTINEL_DIR)/.done_03
S04 = $(SENTINEL_DIR)/.done_04
S05 = $(SENTINEL_DIR)/.done_05
S06 = $(SENTINEL_DIR)/.done_06
S07 = $(SENTINEL_DIR)/.done_07
S08 = $(SENTINEL_DIR)/.done_08
S09 = $(SENTINEL_DIR)/.done_09
S10 = $(SENTINEL_DIR)/.done_10
S11 = $(SENTINEL_DIR)/.done_11
S12 = $(SENTINEL_DIR)/.done_12

.PHONY: all data analyses plots clean help

## Default target: full pipeline
all: data analyses plots

## ── create_data/ pipeline ─────────────────────────────────────────────────────

data: $(S06)

$(S01):
	@mkdir -p $(SENTINEL_DIR)
	$(R) create_data/01_country_selection.R
	@touch $@

$(S02): $(S01)
	$(R) create_data/02_comtrade_pull.R
	@touch $@

$(S03): $(S01)
	$(R) create_data/03_taiwan_data.R
	@touch $@

$(S04): $(S01)
	$(R) create_data/04_patent_data.R
	@touch $@

$(S05): $(S02) $(S03) $(S04)
	$(R) create_data/05_build_network_data.R
	@touch $@

$(S06): $(S05)
	$(R) create_data/06_geopolitical_attrs.R
	@touch $@

## ── analyses/ pipeline ────────────────────────────────────────────────────────
## 07-11 can run immediately after 06; 09 also produces core-periphery tables;
## 12 (ERGM) requires 09's centrality_all.rds (includes coreness column).

analyses: $(S12)

$(S07): $(S06)
	@mkdir -p analyses/output
	$(R) analyses/07_descriptive_trade.R
	@touch $@

$(S08): $(S06)
	@mkdir -p analyses/output
	$(R) analyses/08_network_summary.R
	@touch $@

$(S09): $(S06)
	@mkdir -p analyses/output
	$(R) analyses/09_centrality.R
	@touch $@

$(S10): $(S09)
	$(R) analyses/10_community_detection.R
	@touch $@

$(S11): $(S09)
	$(R) analyses/11_multiplex.R
	@touch $@

$(S12): $(S07) $(S08) $(S09) $(S10) $(S11)
	$(R) analyses/12_ergm.R
	@touch $@

## ── plots/ pipeline ───────────────────────────────────────────────────────────
## 13 requires only data (S06); 14–16 require analyses (communities, centrality)

plots: $(S06) $(S12)
	@mkdir -p plots/output
	$(R) plots/13_trade_plots.R
	$(R) plots/14_network_viz.R
	$(R) plots/15_centrality_plots.R
	$(R) plots/16_community_plots.R

## ── Utilities ─────────────────────────────────────────────────────────────────

## Remove all generated outputs and sentinel files (forces full re-run)
clean:
	rm -f $(SENTINEL_DIR)/.done_*
	rm -f $(SENTINEL_DIR)/*.csv $(SENTINEL_DIR)/*.rds
	rm -f plots/output/*.pdf
	rm -f analyses/output/*.tex analyses/output/*.html

## Remove only analysis outputs (re-run analyses without re-pulling data)
clean-analyses:
	rm -f $(SENTINEL_DIR)/.done_0[789] $(SENTINEL_DIR)/.done_1[012]
	rm -f analyses/output/*.tex analyses/output/*.html

## Show available targets
help:
	@echo "Available targets:"
	@echo "  make              — full pipeline (data + analyses + plots)"
	@echo "  make data         — create_data/01-06 in order"
	@echo "  make analyses     — analyses/07-12 in order (requires data)"
	@echo "  make plots        — plots/13–16 (requires data + analyses)"
	@echo "  make clean        — remove all generated files"
	@echo "  make clean-analyses — re-run analyses without re-pulling data"
	@echo ""
	@echo "After 'make analyses', knit the notebook:"
	@echo "  Rscript -e \"rmarkdown::render('final_analysis.Rmd')\""
