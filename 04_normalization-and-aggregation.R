# Prepare workspace -------------------------------------------------------
library(tidyverse)
library(data.table)
library(GGally)
library(AMR)

source("tools.R")


# Load data ---------------------------------------------------------------

mjc_analysis_prepared_long <- readRDS("data/mjc_analysis_prepared_long.rds")
mjc_analysis_prepared_wide <- readRDS("data/mjc_analysis_prepared_wide.rds")


# Data Normalization ------------------------------------------------------

mjc_normalized_long <- mjc_analysis_prepared_long %>% 
  group_by(short_name) %>% 
  mutate(
    min_max_normalized_value = ifelse(direction > 0, 
                                      min_max_normalization(value),
                                      min_max_normalization(value, inverted = T)),
    distance_to_reference_norm_value = value / sum(value)
  ) %>% 
  ungroup %>% 
  drop_na

# Min Max Normalized Boxplots
mjc_normalized_long %>% 
  ggplot(aes(short_name, min_max_normalized_value)) +
  geom_boxplot() +
  facet_wrap(vars(short_name), scales = "free_y") +
  ggtitle("Min Max Normalized Boxplots")

# Distance to Reference Boxplots
mjc_normalized_long %>% 
  ggplot(aes(short_name, distance_to_reference_norm_value)) +
  geom_boxplot() +
  facet_wrap(vars(short_name), scales = "free_y") +
  ggtitle("Distance to Reference Boxplots")

# Min-Max Normalized Histograms
mjc_normalized_long %>% 
  ggplot(aes(min_max_normalized_value)) +
  geom_histogram() +
  facet_wrap(vars(short_name), scales = c("free_x")) +
  ggtitle("Min-Max Normalized Histograms")

# Distance to Reference Normalized Histograms
mjc_normalized_long %>% 
  ggplot(aes(distance_to_reference_norm_value)) +
  geom_histogram() +
  facet_wrap(vars(short_name), scales = c("free_x")) +
  ggtitle("Distance to Reference Normalized Histogram")

# Prepare dataset
mjc_min_max_normalized_wide <- mjc_normalized_long %>%
  select(-short_name, -value, -direction, -distance_to_reference_norm_value) %>% 
  pivot_wider(names_from = name, values_from = min_max_normalized_value) %>% drop_na

mjc_dtoreference_normalized_wide <- mjc_normalized_long %>%
  select(-short_name, -value, -direction, -min_max_normalized_value) %>% 
  pivot_wider(names_from = name, values_from = distance_to_reference_norm_value) %>% 
  drop_na

id_vars <- c("country", "id", "un_region_name")

mjc_normalized_wide <- mjc_min_max_normalized_wide %>% 
  left_join(mjc_dtoreference_normalized_wide, 
            by = id_vars, 
            suffix = c("_min_max_norm", "_dist_to_ref_norm"))

# Aggregation -------------------------------------------------------------

criteria_variables <- names(mjc_normalized_wide)
criteria_variables <- criteria_variables[!criteria_variables %in% id_vars]

mjc_min_max_aggregated <- mjc_normalized_wide %>% 
  select(id_vars,
         matches("_min_max_norm"),
         -matches("dist_to_ref_norm|mjc_criteria_ponderated")) %>% 
  mutate(
    aggregated_index_minmax = pmap_dbl(select(., ends_with("_min_max_norm")), 
                                   lift_vd(mean))
  )

mjc_min_max_aggregated %>% View

mjc_dtoref_aggregated <- mjc_normalized_wide %>% 
  select(id_vars,
         matches("_dist_to_ref_norm"),
         -matches("_min_max_norm|mjc_criteria_ponderated")) %>% 
  mutate(
    aggregated_index_dtoref = pmap_dbl(select(., ends_with("_dist_to_ref_norm")), 
                                       lift_vd(mean))
  )

mjc_dtoref_aggregated %>% View
