# Premier League Player Statistics Analysis
# Dataset: EPL Player Statistics (2020-09-24)

# SECTION 1: SETUP AND LIBRARY LOADING

# Install packages if not already installed
required_packages <- c(
  "tidyverse", "plotly", "corrplot", "scales",
  "RColorBrewer", "gridExtra"
)

install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, repos = "https://cloud.r-project.org/")
  }
}

install_if_missing(required_packages)

# Load libraries
library(tidyverse)
library(plotly)
library(corrplot)
library(scales)
library(RColorBrewer)
library(gridExtra)

# Set options
options(scipen = 999) # Disable scientific notation

