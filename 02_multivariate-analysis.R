# Prepare workspace -------------------------------------------------------
library(tidyverse)
library(data.table)
library(GGally)
library(AMR)

source("tools.R")

# Run if not shure about data preparation has been performed
source("00_data-preparation.R")


# Load data ---------------------------------------------------------------

# Load raw data
mjc_prepared <- readRDS("data/mjc_prepared.rds")
mjc_prepared %>% head

# input_regexp <- "\\d\\w{1}|MJC.*"
input_regexp <- "[^MJC\\d]"

mjc_analysis <- mjc_prepared %>% filter(str_detect(short_name, input_regexp))


# Raw Data Summary statistics ---------------------------------------------

mjc_analysis %>% summarise_statistics %>% View



# Missing Data ------------------------------------------------------------

# Decision: delete all
missing_data_countries <- mjc_analysis %>% 
  filter(is.na(value)) %>% 
  distinct(country) %>% pull

missing_data_countries

mjc_analysis <- mjc_analysis %>% 
  filter(!country %in% missing_data_countries) %>% 
  arrange(short_name)




# Correlation via Spearman’s r --------------------------------------------

# Matrix of varaibles
all_variables_and_indicator <- mjc_analysis %>% 
  select(-name, -direction) %>% 
  pivot_wider(c("country"), names_from = short_name, values_from = value) %>% 
  select(-country)

# Correlation with Spearman's coefficient
cor_matrix_spearman <- cor(all_variables_and_indicator, 
                           all_variables_and_indicator, 
                           "pairwise.complete.obs", "spearman")
cor_matrix_spearman[upper.tri(cor_matrix_spearman)] <- NA
cor_matrix_spearman %>% View

# Correlation Plot Spearman's
mjc_analysis %>% 
  select(country, short_name, value) %>%
  pivot_wider(names_from = short_name, values_from = value) %>%
  select(matches(input_regexp)) %>%
  ggpairs(
    lower = list(continuous = my_fn),
    upper = list(continuous = wrap("cor", method = "spearman"))
  )


# Data transformation before Pearson's -------------------------------------

# Remove outliers 2.5th percentile
mjc_analysis_capped <- mjc_analysis %>% cap_data %>% arrange(short_name)

# Which countries are outliers for each variable
setdiff(mjc_analysis, mjc_analysis_capped) %>% 
  group_by(name) %>% 
  distinct(country) %>% 
  pivot_wider(
    names_from = name, 
    values_from = country, 
    values_fn = list(country = list)
  ) %>% 
  unnest(cols = c(index_to_be_defined, inverse_global_index_2019, poverty_ratio_320, 
                  population_2019, gdp_per_capita_ppp_2017, cum_emissions_per_capita_1994_2017, 
                  emissions_per_cap_2017, mjc_criteria_ponderated)) %>% 
  # filter(rowSums(is.na(.[-1])) != 2) %>% # when vectors have different lengths
  View
  
# Boxplots
mjc_analysis_capped %>% 
  ggplot(aes(short_name, value)) +
  geom_boxplot() +
  facet_wrap(vars(short_name), scales = "free_y")

# Histrograms
mjc_analysis_capped %>% 
  group_by(short_name) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(vars(short_name), scales = c("free_x"))

# Logtransformation
vars_to_transform <- c("3a", "5a", "6a", "7a", "8a", "MJC9")

mjc_analysis_capped_logtransformed <- mjc_analysis_capped %>%
  filter(short_name %in% vars_to_transform) %>% 
  mutate(value = log(value))

# Join with variables not transformed
mjc_analysis_prepared <- mjc_analysis_capped_logtransformed %>% 
  bind_rows(
    mjc_analysis_capped %>% 
      filter(!short_name %in% vars_to_transform)
  ) %>% 
  arrange(short_name, country)


# Pearson's Correlation ----------------------------------------------------

# Matrix of varaibles for correlation analysis
mjc_for_pearsons <- mjc_analysis_prepared %>% 
  select(-name, -direction) %>% 
  pivot_wider(
    c("country", "id", "un_region_name"), 
    names_from = short_name, 
    values_from = value
  ) %>% drop_na

# Correlation with Pearson's coefficient
cor_matrix_pearson <- cor(mjc_for_pearsons[,4:11], # all vars and indicator 
                          mjc_for_pearsons[,4:11], 
                          "pairwise.complete.obs", "pearson")
cor_matrix_pearson[upper.tri(cor_matrix_pearson)] <- NA
cor_matrix_pearson %>% View

# Correlation Plot Pearson's r
mjc_for_pearsons %>% 
  select(matches(input_regexp)) %>%
  ggpairs(lower = list(continuous = my_fn))


# TODO: fix summarise_statistics.summary_2
# mjc_analysis_prepared %>% summarise_statistics(capped = TRUE) %>% View



# PCA ---------------------------------------------------------------------

mjc.pca <- prcomp(mjc_for_pearsons[,4:10], center = TRUE, scale. = TRUE)

summary(mjc.pca)

str(mjc.pca, max.level = 1)

# PCA plot
ggplot_pca(mjc.pca)

# ggplot_pca(mjc.pca, 
#            labels=mjc_for_pearsons$country, 
#            points_size = 0, 
#            ellipse=TRUE,
#            groups = mjc_for_pearsons$un_region_name)


# Cluster analysis --------------------------------------------------------

# variables_of_interest <- names(mjc_for_pearsons)
# variables_of_interest <- variables_of_interest[4:10]
# variables_of_interest

# Boxplot of vars vs un_region_name
# for (var in variables_of_interest) {
#   par(mfrow=c(2,2))
#   boxplot(sym(var)~un_region_name, data = mjc_for_pearsons, id.method="y")  
# }

# ANOVA analysis of variances by factor (e.g. by un_region_name)
# aov(logtime~Fetching.person, data=Datos)


# Data Normalization ------------------------------------------------------

mjc_normalized <- mjc_analysis_prepared %>% 
  group_by(short_name) %>% 
  mutate(
    normalized_value = ifelse(
      direction > 0, 
      min_max_normalization(value),
      min_max_normalization(value, inverted = T)
    )
  ) %>% 
  ungroup %>% 
  drop_na

# Boxplots
prepared_data %>% 
  ggplot(aes(short_name, normalized_value)) +
  geom_boxplot() +
  facet_wrap(vars(short_name), scales = "free_y")

# Histograms
prepared_data %>% 
  ggplot(aes(normalized_value)) +
  geom_histogram() +
  facet_wrap(vars(short_name), scales = c("free_x"))


# Aggregation -------------------------------------------------------------

prepared_data_wide <- prepared_data %>% 
  select(-short_name, -value, -direction) %>% 
  pivot_wider(names_from = name, values_from = normalized_value) %>% drop_na

non_numeric_vars <- c("country", "id", "un_region_name", "mjc_criteria_ponderated")
criteria_variables <- names(prepared_data_wide)
criteria_variables <- criteria_variables[!criteria_variables %in% non_numeric_vars]

prepared_data_wide %>% 
  mutate(
    mjc_sdr_mild_version = index_to_be_defined, 
    mjc_sdr_medium_version = inverse_global_index_2019,  
    mjc_sdr_heavy_version = poverty_ratio_320, 
    mjc_equality = population_2019,      
    mjc_capacity = gdp_per_capita_ppp_2017, 
    mjc_historical_responsability = cum_emissions_per_capita_1994_2017,
    mjc_present_responsability = emissions_per_cap_2017,
    mjc_aggregated = round(rowsum(
      index_to_be_defined, inverse_global_index_2019,  
      poverty_ratio_320, population_2019,      
      gdp_per_capita_ppp_2017, cum_emissions_per_capita_1994_2017,
      emissions_per_cap_2017            
    ) / 7, digits = 2)
  ) %>% 
  select(
    -index_to_be_defined, -inverse_global_index_2019,  
    -poverty_ratio_320, -population_2019,      
    -gdp_per_capita_ppp_2017, -cum_emissions_per_capita_1994_2017,
    -emissions_per_cap_2017
  ) %>% 
  View
