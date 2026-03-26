cat("\n    PREMIER LEAGUE PLAYER STATISTICS ANALYSIS   \n")

# 1. Setup and library loading
source("setup/init.R")

# 2. Data loading and preprocessing (Methodology Step 1)
source("data_prep/load_clean.R")

# 3. Data transformations
source("data_prep/transform.R")

# 4. Summary statistics (Methodology Step 2)
source("analysis/summary.R")

# 5. Visualizations - Histograms
source("visualizations/histograms.R")

# 6. Visualizations - Pie charts
source("visualizations/pie_charts.R")

# 7. Visualizations - Bar charts
source("visualizations/bar_charts.R")

# 8. Visualizations - Box plots
source("visualizations/box_plots.R")

# 9. Visualizations - Scatterplots
source("visualizations/scatterplots.R")

# 10. Statistical analysis - Correlations & ANOVA
source("analysis/statistics.R")

# 11. Use Case Questions Analysis
source("analysis/use_cases.R")

# 12. Key Findings Summary
source("analysis/findings.R")

# 13. Export data for Power BI
source("export/powerbi.R")

cat("\n")
