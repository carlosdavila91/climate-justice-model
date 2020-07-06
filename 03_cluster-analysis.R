# Prepare workspace -------------------------------------------------------
library(tidyverse)
library(data.table)
library(GGally)
library(AMR)

source("tools.R")

# Run if not shure about data preparation has been performed
# source("00_data-preparation.R")


# Load data ---------------------------------------------------------------

mjc_analysis_prepared_long <- readRDS("data/mjc_analysis_prepared_long.rds")
mjc_analysis_prepared_wide <- readRDS("data/mjc_analysis_prepared_wide.rds")


# Cluster analysis --------------------------------------------------------

# variables_of_interest <- names(mjc_for_pearsons)
# variables_of_interest <- variables_of_interest[4:10]
# variables_of_interest


# ANOVA analysis of variances by factor (e.g. by un_region_name)
# aov(logtime~Fetching.person, data=Datos)

