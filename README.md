# Embargo Breach: Tracing TenantThread's AI Crisis

This Shiny application investigates the TenantThread Project HarborCrest embargo breach using visual analytics.

## App Tabs

- Landing Page / Overview
- Crisis Timeline
- Agent Network
- Embargo Breach Pathway
- User Guide

## Data Requirement

Place the raw MC1 JSON file at:

```text
data/raw/MC1_final_00.json
```

Processed RDS files are generated from the raw JSON file.

## Data Preparation

In RStudio, run:

```r
source('R/01_data_prep.R')
source('R/02_feature_engineering.R')
```

## Running the App

```r
shiny::runApp()
```

## Main R Packages

The application uses `shiny`, `bslib`, `tidyverse`, `lubridate`, `plotly`, `visNetwork`, `DT`, `scales`, and `jsonlite`.

## Notes

- The app uses linked visualisations and evidence tables to support investigation.
- The causal chain and pathway diagrams are investigative summaries and should be interpreted with the linked message evidence.
