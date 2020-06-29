# Missing indicators by country
mjc_pivoted %>% 
  filter(
    str_detect(short_name, "\\d\\w{1}") & !is.na(value)
  ) %>% 
  group_by(country) %>% 
  summarise(n_indicators = n()) %>% 
  mutate(
    missing_indicators_pctg := 100 * round(1 - (n_indicators / max(n_indicators)), 4),
    n_indicators = paste0(n_indicators, " out of ", max(n_indicators))
  ) %>% 
  filter(missing_indicators_pctg > 0) %>% View()