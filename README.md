# Chapter-1---Recycled-substrates-in-broccolini
Broccolini substrate performance and clubroot resilience — Analysis Code

This repository contains the full data-processing and statistical analysis workflow used in the first chapeter of the thesis titled:

“Developing Novel Technologies to Bolster Plant Resistance to Fungal Pathogens: Mitigating Major Broccolini Diseases”

This in particular evaluates how alternative horticultural substrates influence broccolini growth, nutrient status, yield dynamics, and resilience to Plasmodiophora brassicae (clubroot). All analyses were performed in R (v4.5.0, 2025-04-11) via RStudio 2025.09.2+418, and are fully reproducible using the scripts provided.

Repository contents
/scripts/

R scripts used for all stages of analysis, including:

Biomass analysis

data cleaning, merging, and long-format conversion

linear models for biomass and disease severity

estimated marginal means (EMMs) and treatment-vs-control contrasts

visualisation of dry weight and DSI results

Yield and time-series analysis

aggregation of floret-level harvest data

calculation of total yield, floret number, and mean floret weight

LOESS-smoothed yield curves and temporal comparisons across substrates

integration of rockwool trials and zero-padding for comparable time series

Nutrient analysis

cleaning irregular nutrient formats (“<0.5”, “>10”, decimal commas)

calculation of median nutrient concentrations and % change vs control

Wilcoxon rank-sum tests with BH correction

production of nutrient heatmaps and summary tables

All plots used in the manuscript (Figures 1–X) are generated from these scripts.

/data_clean/

Cleaned datasets used in the manuscript, created from raw data by the processing scripts.
(No raw experimental files are included for storage/privacy reasons.)

/figures/

Figures generated directly by the analysis code, including:

biomass + DSI visualisations

total yield and floret-weight time series

substrate comparison panels

nutrient heatmaps

/tables/

Exported tables used in the manuscript:

EMM contrast outputs

nutrient summary statistics

Wilcoxon test results

yield summaries

Statistical methods (summary)

All analyses were conducted in R 4.5.0 (2025-04-11 ucrt) within RStudio 2025.09.2+418.
Core packages include:

tidyverse (dplyr, tidyr, ggplot2, forcats, stringr, lubridate)

rstatix (Kruskal–Wallis, Wilcoxon, BH correction)

emmeans (treatment-vs-control contrasts)

broom (tidy model outputs)

patchwork (figure assembly)

ggpattern (pattern fills for inoculation status)

janitor (data standardisation)

Biomass and DSI were analysed using linear models with estimated marginal means.
Yield metrics were evaluated using ANOVA and non-parametric tests.
Nutrient data were assessed with Wilcoxon rank-sum tests and expressed as % change relative to controls.

Full statistical workflows are provided in the /scripts/ folder.

Reproducibility

To reproduce the analysis:

Install R ≥ 4.5.0

Install RStudio 2025.09.2+418 (or compatible version)

Install required packages (see sessionInfo() in /metadata/)

Run scripts in the order described in the (optional) master script

All figures and tables will be regenerated automatically.

Citation

If using or adapting this code, please cite:

Vacca, C. (2025). Broccolini substrate performance and clubroot resilience — analysis code. GitHub repository: <link>

Contact

For questions about the analysis, please contact:
Claudia Vacca — Western Sydney University
Email: claudia.vacca97@gmail.com
