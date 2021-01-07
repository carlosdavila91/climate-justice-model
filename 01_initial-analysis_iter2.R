

# Work Space preparation --------------------------------------------------

library(tidyverse)
library(data.table)
library(GGally)

source("tools.R")


# Load data ---------------------------------------------------------------

mjc_full_set_prepared <- readRDS("data/mjc_full_set_prepared.rds")

input_regexp <- "MJC"
mjc_analysis <- mjc_full_set_prepared %>% 
  filter(str_detect(short_name, input_regexp))

mjc_analysis_wide <- mjc_analysis %>% 
  select(-name) %>% 
  pivot_wider(c("country", "id", "un_region_name"), 
              values_from = value, names_from = short_name)


# Metadata ----------------------------------------------------------------

summary_table <- mjc_analysis %>% 
  group_by(name) %>% 
  distinct(short_name)

write_csv(summary_table, "share/mjc_summary_table.csv")


# Summary Statistics ------------------------------------------------------

mjc_analysis %>% summarise_statistics %>% select(-name) %>% 
  write_csv("share/mjc_summary_statistics.csv")


# Missing data ------------------------------------------------------------

missing_data_countries <- mjc_analysis %>% 
  filter(is.na(value)) %>% 
  distinct(country) %>% pull

missing_data_countries %>% data.frame %>% write_csv("share/mjc_missing_data.csv")

mjc_analysis_long <- mjc_analysis %>% 
  filter(!country %in% missing_data_countries) %>% 
  arrange(short_name)


# Distributions -----------------------------------------------------------

mjc_analysis %>% 
  select(country, short_name, value) %>%
  pivot_wider(names_from = short_name, values_from = value) %>%
  select(matches(input_regexp)) %>%
  ggpairs(
    upper = NULL,
    lower = list(continuous = my_fn)
  )

ggsave(filename = "image/mjc_distributions.png")



# Normality test ----------------------------------------------------------

mjc_shapiro <- map(mjc_analysis_wide[4:12], shapiro.test)

mjc_shapiro %>% {
  tibble(
    variable = paste0("MJC", seq(1:9)),
    shapiro = map_dbl(., "statistic"),
    p_value = map_dbl(., "p.value"),
    significance_level = .05
  )
} %>% write_csv("share/mjc_shapiro.csv")


# Correlation matrices ----------------------------------------------------

# Pearson's
cor_matrix_pearson <- cor(mjc_analysis_wide[,4:ncol(mjc_analysis_wide)],  
                           mjc_analysis_wide[,4:ncol(mjc_analysis_wide)], 
                           "pairwise.complete.obs", "pearson")

cor_matrix_pearson <- cor_matrix_pearson %>% as.data.frame %>% round_df(2)
cor_matrix_pearson[upper.tri(cor_matrix_pearson)] <- NA

write_csv(cor_matrix_pearson, "share/mjc_pearson.csv")


# Spearman's
cor_matrix_spearman <- cor(mjc_analysis_wide[,4:ncol(mjc_analysis_wide)],  
                           mjc_analysis_wide[,4:ncol(mjc_analysis_wide)], 
                           "pairwise.complete.obs", "spearman")

cor_matrix_spearman <- cor_matrix_spearman %>% as.data.frame %>% round_df(2)
cor_matrix_spearman[upper.tri(cor_matrix_spearman)] <- NA

write_csv(cor_matrix_spearman %>% as.data.frame, "share/mjc_spearman.csv")


# Kendall
cor_matrix_kendall <- cor(mjc_analysis_wide[,4:ncol(mjc_analysis_wide)],  
                          mjc_analysis_wide[,4:ncol(mjc_analysis_wide)], 
                          "pairwise.complete.obs", "kendall")
cor_matrix_kendall <- cor_matrix_kendall %>% as.data.frame %>% round_df(2)
cor_matrix_kendall[upper.tri(cor_matrix_kendall)] <- NA

write_csv(cor_matrix_kendall %>% as.data.frame, "share/mjc_kendall.csv")




# SDR Multivariate Analysis -----------------------------------------------


# Variable selection
selected_data <- c("3a", "B", "N", "O", "P", "Q", "R", "S", "T")

mjc_analysis_2 <- mjc_full_set_prepared %>% 
  filter(short_name %in% selected_data) %>% 
  arrange(short_name)

mjc_analysis_2 %>% 
  group_by(name) %>% 
  distinct(short_name) %>% write_csv("share/01_sdr-corr-analysis_summary_table.csv")

mjc_analysis_wide_2 <- mjc_analysis_2 %>% 
  select(-name) %>% 
  pivot_wider(c("country", "id", "un_region_name"), 
              values_from = value, names_from = short_name)


# Summary Statistics ------------------------------------------------------

mjc_analysis_2 %>% summarise_statistics %>% select(-name) %>% 
  round_df(2) %>% 
  write_csv("share/02_sdr-corr-analysis_summary_statistics.csv")



# Missing Data ------------------------------------------------------------

mjc_analysis_2 %>% 
  filter(is.na(value)) %>% 
  distinct(country) %>% write_csv("share/03_sdr-corr-analysis_missing-data.csv")


# Distributions -----------------------------------------------------------

mjc_analysis_wide_2 %>% 
  select(matches(selected_data), -c(country, un_region_name)) %>%
  ggpairs(
    upper = NULL,
    lower = list(continuous = my_fn)
  )

ggsave(filename = "image/sdr-corr-analysis_distributions.png")



# Normality test ----------------------------------------------------------

mjc_shapiro <- map(mjc_analysis_wide_2[4:12], shapiro.test)

mjc_shapiro %>% {
  tibble(
    variable = selected_data,
    shapiro = map_dbl(., "statistic"),
    p_value = map_dbl(., "p.value"),
    significance_level = .05
  )
} %>% 
  mutate(shapiro = round(shapiro, 2)) %>%
  write_csv("share/04_sdr-corr-analysis_shapiro.csv")


# Correlation Matrices ----------------------------------------------------

# Pearson's
cor_matrix_pearson_2 <- cor(mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)],  
                          mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)], 
                          "pairwise.complete.obs", "pearson")

cor_matrix_pearson_2 <- cor_matrix_pearson_2 %>% as.data.frame %>% round_df(2)
cor_matrix_pearson_2[upper.tri(cor_matrix_pearson_2)] <- NA

write_csv(cor_matrix_pearson_2, "share/05_sdr-corr-analysis_pearson.csv")


# Spearman's
cor_matrix_spearman_2 <- cor(mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)],  
                            mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)], 
                            "pairwise.complete.obs", "spearman")

cor_matrix_spearman_2 <- cor_matrix_spearman_2 %>% as.data.frame %>% round_df(2)
cor_matrix_spearman_2[upper.tri(cor_matrix_spearman_2)] <- NA

write_csv(cor_matrix_spearman_2, "share/06_sdr-corr-analysis_spearman.csv")

# Kendall's
cor_matrix_kendall_2 <- cor(mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)],  
                             mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)], 
                             "pairwise.complete.obs", "kendall")

cor_matrix_kendall_2 <- cor_matrix_kendall_2 %>% as.data.frame %>% round_df(2)
cor_matrix_kendall_2[upper.tri(cor_matrix_kendall_2)] <- NA

write_csv(cor_matrix_kendall_2, "share/07_sdr-corr-analysis_kendall.csv")



# New indicators ----------------------------------------------------------

# Variable selection
selected_data <- c("3a", "B", "NAU", "NAV", "NAW", "NAX", "NAY", "R", "T")

mjc_analysis_2 <- mjc_full_set_prepared %>% 
  filter(short_name %in% selected_data) %>% 
  arrange(short_name)

mjc_analysis_2 %>% 
  group_by(name) %>% 
  distinct(short_name) %>% write_csv("share/08_new-indicators-analysis_summary_table.csv")

mjc_analysis_wide_2 <- mjc_analysis_2 %>% 
  select(-name) %>% 
  pivot_wider(c("country", "id", "un_region_name"), 
              values_from = value, names_from = short_name)


# Summary Statistics ------------------------------------------------------

mjc_analysis_2 %>% summarise_statistics %>% select(-name) %>% 
  round_df(2) %>% 
  write_csv("share/09_new-indicators-analysis_summary_statistics.csv")



# Missing Data ------------------------------------------------------------

mjc_analysis_2 %>% 
  filter(is.na(value)) %>% 
  distinct(country) %>% write_csv("share/10_new-indicators-analysis_missing-data.csv")


# Distributions -----------------------------------------------------------

mjc_analysis_wide_2 %>% 
  select(matches(selected_data), -c(country, un_region_name)) %>%
  ggpairs(
    upper = NULL,
    lower = list(continuous = my_fn)
  )

ggsave(filename = "image/new-indicators-analysis_distributions.png")



# Normality test ----------------------------------------------------------

mjc_shapiro <- map(mjc_analysis_wide_2[4:ncol(mjc_analysis_wide_2)], shapiro.test)

mjc_shapiro %>% {
  tibble(
    variable = selected_data,
    shapiro = map_dbl(., "statistic"),
    p_value = map_dbl(., "p.value"),
    significance_level = .05
  )
} %>% 
  mutate(shapiro = round(shapiro, 2)) %>%
  write_csv("share/11_new-indicators-analysis_shapiro.csv")


# Correlation Matrices ----------------------------------------------------

# Pearson's
cor_matrix_pearson_2 <- cor(mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)],  
                            mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)], 
                            "pairwise.complete.obs", "pearson")

cor_matrix_pearson_2 <- cor_matrix_pearson_2 %>% as.data.frame %>% round_df(2)
cor_matrix_pearson_2[upper.tri(cor_matrix_pearson_2)] <- NA

write_csv(cor_matrix_pearson_2, "share/12_new-indicators-analysis_pearson.csv")


# Spearman's
cor_matrix_spearman_2 <- cor(mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)],  
                             mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)], 
                             "pairwise.complete.obs", "spearman")

cor_matrix_spearman_2 <- cor_matrix_spearman_2 %>% as.data.frame %>% round_df(2)
cor_matrix_spearman_2[upper.tri(cor_matrix_spearman_2)] <- NA

write_csv(cor_matrix_spearman_2, "share/13_new-indicators-analysis_spearman.csv")

# Kendall's
cor_matrix_kendall_2 <- cor(mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)],  
                            mjc_analysis_wide_2[,4:ncol(mjc_analysis_wide_2)], 
                            "pairwise.complete.obs", "kendall")

cor_matrix_kendall_2 <- cor_matrix_kendall_2 %>% as.data.frame %>% round_df(2)
cor_matrix_kendall_2[upper.tri(cor_matrix_kendall_2)] <- NA

write_csv(cor_matrix_kendall_2, "share/14_new-indicators-analysis_kendall.csv")

