
# Supuestos de modelización -----------------------------------------------

# ¿Estamos construyendo un indicador compuesto? Sí
# Definición de indicador según CEPAL:
# representación simplificada que busca 
# resumir un concepto multidimensional en un índice simple (unidimensional)

# Construir un indicador compuesto implica realizar los siguientes pasos:
# - Definir el marco teórico
# - Seleccionar los datos correctos
# - [TODO: pendiente de definición] Imputar los valores que falten 
# - Análisis multivariable
# - Normalizar
# - Dar peso y agregar las variables
# - Análisis de sensitividad y de incertidumbre

# Un análisis de correlaciones parte las siguientes supuestos sobre los datos
# - Nivel de medición: todas las variables deben ser continuas
# - Pares relacionados: cada participante debería tener un par de valores
# - Ausencia de valores extremos
# - linealidad de la expectativa: el valor esperado de la suma de variables aleatorias
#   es igual a la suma de sus valores esperados individuales, sin importar si
#   las variables son dependientes o independientes


# Prepare workspace -------------------------------------------------------
library(tidyverse)
library(data.table)

source("tools.R")

# Load data ---------------------------------------------------------------

# Load raw data
mjc <- read_csv("2019_global_development_index_v3.csv", trim_ws = TRUE)
mjc %>% head
mjc %>% names

# Load data classification
data_classification <- read_csv2("data_classification.csv", trim_ws = TRUE) %>% 
  as.data.table
data_classification %>% head

# Data preparation --------------------------------------------------------

# Treat data classification
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


# Join datasets
# - pivot from wide to long format
# - join each criteria, index and variable with its classification
mjc_pivoted <- mjc %>% 
  pivot_longer(-c("country", "id"), names_to = "name", values_to = "value") %>% 
  left_join(select(
    data_classification, name, short_name, direction
  ), by = "name") %>% 
  arrange(short_name) %>% 
  as.data.table


input <- "cirteria"

if (input == "variables") {
  analysis_input <- "\\d\\w{1}" # variables composing each MJCs
} else {
  analysis_input <- "MJC" # MJCs themselves
}

mjc_pivoted %>% 
  filter(str_detect(short_name, analysis_input)) %>% 
  group_by(short_name) %>% 
  summarise(last(name))

mjc_pivoted %>% 
  select(country, short_name, value) %>%
  filter(str_detect(short_name, analysis_input)) %>%
  filter(!is.na(value)) %>% 
  pivot_wider(names_from = short_name, values_from = value) %>% 
  select(-country) %>% 
  GGally::ggpairs()
