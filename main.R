# Main execution script for Premier League Player Statistics Analysis

# 1. Setup and library loading
source("setup/init.R")

# 2. Data loading and preprocessing
source("data_prep/load_clean.R")

# 3. Data transformations
source("data_prep/transform.R")

# 4. Summary statistics
source("analysis/summary.R")

# 5. Visualizations - Pie charts
source("visualizations/pie_charts.R")

# 6. Visualizations - Bar charts
source("visualizations/bar_charts.R")

# 7. Visualizations - Box plots
source("visualizations/box_plots.R")

# 8. Statistical analysis
source("analysis/statistics.R")

# 9. Summary
source("analysis/findings.R")

# 10. Export data for Power BI
source("export/powerbi.R")
