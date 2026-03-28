# Premier League Player Statistics Analysis

End-to-end R project for exploring Premier League player statistics, generating analysis plots, answering project use-case questions, and exporting cleaned/aggregated data for Power BI.

## Dataset Source

Kaggle input reference:
https://www.kaggle.com/code/desalegngeb/english-premier-league-players-statistics/input

Primary dataset file used in this repository:
- `dataset - 2020-09-24.csv`

## Project Status

Current state:
- Analysis pipeline implemented
- Data cleaning and transformation completed
- Visualizations generated through R scripts
- Power BI export files generated in `powerbi_data/`
- Power BI workbook present (`dmdw.pbix`)

## Project Structure

```text
.
|- main.R                     # Orchestrates the full workflow
|- setup/init.R               # Package install + library loading
|- data_prep/                 # Data loading and transformation
|- analysis/                  # Summary, statistics, use-cases, findings
|- visualizations/            # Histograms, pie/bar charts, boxplots, scatterplots
|- export/powerbi.R           # Exports final CSVs for Power BI
|- powerbi_data/              # Output CSVs consumed by Power BI
|- dmdw.pbix                  # Power BI dashboard/report file
|- flake.nix                  # Reproducible Nix dev environment
```

## Quick Start (Beginner Friendly)

Choose one setup path.

### Option A: Recommended (Nix)

Use this if you want a reproducible environment with dependencies preconfigured.

1. Install Nix and enable flakes.
2. Open terminal at project root.
3. Run:

```bash
nix develop . --command Rscript main.R
```

### Option B: Standard R Setup (Without Nix)

Use this if you already have R/RStudio installed.

1. Install R (and optionally RStudio).
2. Open terminal at project root.
3. Run:

```bash
Rscript main.R
```

Notes:
- `setup/init.R` auto-installs missing required packages.
- First run may take longer because packages are downloaded.

## What main.R Does

The pipeline executes these stages in order:

1. Environment setup and package loading
2. Data loading and cleaning
3. Data transformations
4. Summary statistics
5. Visualizations
6. Statistical analysis (correlation, ANOVA, etc.)
7. Use-case/question analysis
8. Findings summary
9. Export processed datasets for Power BI

## Generated Outputs

After a successful run, check `powerbi_data/` for:

- `epl_clean.csv`
- `summary_by_club.csv`
- `summary_by_position.csv`
- `summary_by_nationality.csv`
- `top_performers.csv`
- `goalkeeper_stats.csv`
- `offensive_metrics.csv`
- `defensive_metrics.csv`

Other artifacts:
- `Rplots.pdf` (plot output from R sessions/scripts)

## How to Open Power BI Dashboard

1. Open `dmdw.pbix` in Power BI Desktop.
2. If prompted for data path refresh, point to files in `powerbi_data/`.
3. Refresh the model after rerunning `main.R`.

## Common Beginner Issues

### 1) Command not found: Rscript

Cause:
- R is not installed or not in PATH.

Fix:
- Install R, restart terminal, and run `Rscript --version`.

### 2) Package installation fails

Cause:
- Network/proxy permissions or blocked CRAN mirror.

Fix:
- Retry on stable network.
- Ensure access to `https://cloud.r-project.org/`.

### 3) Nix command fails on Windows

Cause:
- Nix is not fully configured with flakes.

Fix:
- Enable flakes in Nix config, or use Option B (`Rscript main.R`).