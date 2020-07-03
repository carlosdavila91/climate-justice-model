# Prepare workspace -------------------------------------------------------
library(tidyverse)
library(data.table)


# Load data ---------------------------------------------------------------

# Load raw data
mjc <- read_csv("data/2019_global_development_index_v5.csv", 
                trim_ws = TRUE, skip_empty_rows = TRUE) %>% 
  filter(!is.na(country))
# mjc %>% View
mjc %>% names

# Load data classification
data_classification <- read_csv2("data/data_classification_v2.csv", trim_ws = TRUE) %>% 
  as.data.table

# Data preparation --------------------------------------------------------

# Treat data classification -----------------------------------------------

# - Rename columns
(old <- data_classification %>% names)
new <- c("name", "classification", "sub_classification", "direction")
setnames(data_classification, old, new)

# - Extra variables: "Others" >> UPPERCASE LETTERS
data_classification[classification == "Others", `:=`(
  classification = LETTERS[1:nrow(.SD)],
  sub_classification = ""
)]

# - MJC criteria: MJC1, MJC2, ..., MJCn
data_classification[sub_classification == "0", `:=`(
  classification = "MJC",
  sub_classification = classification
)]
# - Rest: Paste classification + subclassification and delete these columns
data_classification[,`:=`(
  short_name = paste0(classification, sub_classification),
  classification = NULL,
  sub_classification = NULL
)]


# Correct directions
data_classification[name == "global_index_score_2019", direction := -1]
data_classification[name == "inverse_global_index_2019", direction := 1]
data_classification[name == "index_to_be_defined", direction := 1]
data_classification[name == "population_2019", direction := 1]
data_classification[name == "gdp_per_capita_ppp_2017", direction := -1]

# Join datasets -----------------------------------------------------------

# - pivot from wide to long format
# - join each criteria, index and variable with its classification
mjc_pivoted <- mjc %>% 
  pivot_longer(-c("country", "id", "un_region_name"), names_to = "name", values_to = "value") %>% 
  left_join(select(
    data_classification, name, short_name, direction
  ), by = "name") %>% 
  select(country, id, un_region_name, short_name, name, value, direction)

mjc_pivoted %>% 
  group_by(short_name) %>% 
  distinct(name) # %>% View


# Save clean data ---------------------------------------------------------

saveRDS(mjc_pivoted, "data/mjc_prepared.rds")
