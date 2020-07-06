# Prepare workspace -------------------------------------------------------
library(tidyverse)
library(data.table)
library(GGally)
library(AMR)

source("tools.R")


# Load data ---------------------------------------------------------------

# Load raw data
mjc_prepared <- readRDS("data/mjc_prepared.rds")
mjc_prepared %>% head

# input_regexp <- "\\d\\w{1}|MJC9"
input_regexp <- "[^MJC\\d]" # All but MJC's

mjc_analysis_long <- mjc_prepared %>% 
  filter(str_detect(short_name, input_regexp)) %>% 
  arrange(short_name)


# Raw Data Summary statistics ---------------------------------------------

mjc_analysis_long %>% summarise_statistics # %>% View



# Missing Data ------------------------------------------------------------

# TODO: test other imputation
# library(mice)
# imputed_data <- mice::mice(mjc_analysis_wide[4:ncol(mjc_analysis_wide)], 
#                      method = "pmm", seed = 42, tol = 1e-21)
#> poverty_ratio_320Error in solve.default(xtx + diag(pen)) : 
#>  system is computationally singular: reciprocal condition number = 1.7014e-20

# Regional mean
var_means_by_region <- mjc_analysis_long %>% 
  select(-country, -id) %>% 
  filter(!is.na(value)) %>% 
  group_by(un_region_name, short_name, name, direction) %>% 
  summarise_all(list(mean = mean))

mjc_missing_data <- mjc_analysis_long %>% 
  filter(is.na(value))

mjc_missing_imputed <- mjc_missing_data %>% 
  left_join(var_means_by_region) %>% 
  select(-value) %>% 
  mutate(value = mean) %>% 
  select(-mean)

mjc_analysis_long_imputed <- mjc_analysis_long %>% 
  left_join(
    mjc_missing_imputed, 
    by = c("country", "id", "un_region_name", "short_name", "name","direction")
  ) %>% 
  mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
  select(-c(value.x, value.y))


# Correlation via Spearman’s r --------------------------------------------

# Put data in wide format
mjc_analysis_wide <- mjc_analysis_long_imputed %>% 
  # filter(str_detect(short_name, input_regexp)) %>% 
  arrange(short_name) %>% 
  select(-name, -direction) %>% 
  pivot_wider(c("country", "id", "un_region_name"), 
              names_from = short_name, values_from = value)

# Correlation with Spearman's coefficient
cor_matrix_spearman <- cor(mjc_analysis_wide[4:ncol(mjc_analysis_wide)], 
                           mjc_analysis_wide[4:ncol(mjc_analysis_wide)], 
                           "pairwise.complete.obs", "spearman")
cor_matrix_spearman[lower.tri(cor_matrix_spearman)] <- NA
cor_matrix_spearman %>% View

# Correlation Heatmap
melted_mjc <- reshape2::melt(cor_matrix_spearman)
ggplot(data = melted_mjc, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


# Data transformation before Pearson's -------------------------------------

# Remove outliers 2.5th percentile
mjc_analysis_capped <- mjc_analysis_long_imputed %>% cap_data %>% arrange(short_name)

# Which countries are outliers for each variable
setdiff(mjc_analysis_long_imputed, mjc_analysis_capped) %>% 
  group_by(name) %>% 
  distinct(country) %>% 
  pivot_wider(
    names_from = name, 
    values_from = country,
    values_fn = list(country = list)
  ) %>% 
  unnest(cols = c(index_to_be_defined, inverse_global_index_2019, poverty_ratio_320, 
                  population_2019, gdp_per_capita_ppp_2017, cum_emissions_per_capita_1994_2017, 
                  emissions_per_cap_2017, global_index_score_2019, undernourishment_prevalence, 
                  wasting_prevalence_in_children_under_5, maternal_mortality_rate_per_100k_births, 
                  neonatal_mortality_rate_1k_births, under_5_mortality_rate_1k_births, 
                  life_expec_at_birth_yrs, univ_health_coverage_index, lower_secondary_completion_rate, 
                  literacy_rate_15_24_yrs_both_sexes, population_rate_using_basic_drinking_water_services, 
                  population_rate_using_basic_sanitation_services, access_to_electricity_pop_rate, 
                  access_to_clean_fuels_and_tech_for_cooking_rate, unemployment_rate, 
                  population_using_the_internet)) %>% 
  filter(rowSums(is.na(.[-1])) != 2) %>% # when vectors have different lengths
  View


# Logtransformation
vars_to_transform <- c("3a", "5a", "6a", "7a", "8a", "MJC9")

mjc_analysis_capped_and_logtransformed <- mjc_analysis_capped %>%
  filter(short_name %in% vars_to_transform) %>% 
  mutate(value = log(value))


# Join with variables not transformed
mjc_analysis_prepared_long <- mjc_analysis_capped_and_logtransformed %>% 
  bind_rows(
    mjc_analysis_capped %>% 
      filter(!short_name %in% vars_to_transform)
  ) %>% 
  arrange(short_name, country)

saveRDS(mjc_analysis_prepared_long, "data/mjc_analysis_prepared_long.rds")

# Boxplots
# mjc_analysis_prepared_long %>% 
#   ggplot(aes(name, value)) +
#   geom_boxplot() +
#   ggtitle("Boxplots After Data Transformation") +
#   facet_wrap(vars(name), scales = "free_y") +
#   xlab(NULL) + 
#   theme(axis.text.x = element_blank())

# Histrograms
# mjc_analysis_prepared_long %>% 
#   group_by(name) %>% 
#   ggplot(aes(value)) +
#   geom_histogram() +
#   ggtitle("Histograms After Data Transformation") +
#   facet_wrap(vars(name), scales = c("free_x")) +
#   xlab(NULL)


# Pearson's Correlation ----------------------------------------------------

# mjc_shapiro <- map(mjc_analysis_wide[4:12], shapiro.test)
# str(mjc_shapiro)

# Matrix of varaibles for correlation analysis
mjc_analysis_prepared_wide <- mjc_analysis_prepared_long %>% 
  select(-name, -direction) %>% 
  pivot_wider(
    c("country", "id", "un_region_name"), 
    names_from = short_name, 
    values_from = value
  ) %>% drop_na


saveRDS(mjc_analysis_prepared_wide, "data/mjc_analysis_prepared_wide.rds")

# Correlation with Pearson's coefficient
cor_matrix_pearson <- cor(mjc_analysis_prepared_wide[,4:ncol(mjc_analysis_prepared_wide)], # all vars and indicator 
                          mjc_analysis_prepared_wide[,4:ncol(mjc_analysis_prepared_wide)], 
                          "pairwise.complete.obs", "pearson")
cor_matrix_pearson[upper.tri(cor_matrix_pearson)] <- NA
cor_matrix_pearson # %>% View

# Correlation Plot Pearson's r
mjc_analysis_prepared_wide %>% 
  select(matches(input_regexp)) %>%
  ggpairs(lower = list(continuous = my_fn))

# TODO: fix summarise_statistics.summary_2
# mjc_analysis_prepared %>% summarise_statistics(capped = TRUE) %>% View



# PCA ---------------------------------------------------------------------

mjc.pca <- prcomp(mjc_analysis_prepared_wide[,4:10], center = TRUE, scale. = TRUE)

summary(mjc.pca)

# str(mjc.pca, max.level = 1)

# PCA plot
ggplot_pca(mjc.pca)

