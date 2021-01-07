
summarise_statistics <- function(df, probs = c(.025, .975), capped = FALSE) {
  
  summary_1 <- df %>% 
    group_by(short_name, name) %>% 
    {if(!capped){
      summarise(.,
        number_of_observations = n() - sum(is.na(value)),
        missing_data_pctg = 100*round(sum(is.na(value))/n(), 4)
      )
    } else {
      summarise(.,
        number_of_observations = n()
      )
    }}
  
    n_obs <- max(summary_1$number_of_observations)
    
    summary_1 <- summary_1 %>% 
      mutate(
        missing_data_pctg = 100 * round(1 - (number_of_observations / n_obs), 4)
      )
  
  if(capped){
    summary_2 <- df %>% 
      filter(!is.na(value)) %>% 
      group_by(short_name, name) %>% 
      summarise(
        mean = mean(value),
        skewness = moments::skewness(value),
        kurtosis = moments::kurtosis(value),
        minimum_value = min(value),
        maximum_value = max(value),
        direction = last(direction)
      ) %>% 
      mutate(
        lower_bound = case_when(
          direction > 0 ~ min,
          direction < 0 ~ max
        ),
        upper_bound = case_when(
          direction > 0 ~ max,
          direction < 0 ~ min
        )
      )
  } else {
    summary_2 <- df %>% 
      filter(!is.na(value)) %>%
      group_by(short_name, name) %>% 
      summarise(
        mean = mean(value),
        skewness = moments::skewness(value),
        kurtosis = moments::kurtosis(value),
        minimum_value = min(value),
        maximum_value = max(value),
        direction = last(direction),
        lower_bound = ifelse(direction > 0, 
                             quantile(value, probs[1])[[1]], 
                             quantile(value, probs[2])[[1]]),
        upper_bound = ifelse(direction < 0, 
                             quantile(value, probs[1])[[1]], 
                             quantile(value, probs[2])[[1]])
      )
  }
  
  summary_table <- summary_1 %>% 
    left_join(summary_2) %>% 
    select(short_name, name, number_of_observations, missing_data_pctg, mean, 
           skewness, kurtosis, minimum_value, maximum_value, 
           lower_bound, upper_bound, direction
    )
  
  return(summary_table)
  
}


missing_countries_by_indicator <- function(df){
  return(
    df %>% 
      filter(
        str_detect(short_name, input_regexp) & is.na(value)
      ) %>% 
        group_by(country, short_name) %>% 
        summarise(indicator = n()) %>% 
        group_by(country) %>% 
        mutate(n_indicators = sum(indicator)) %>% 
        select(-indicator, -n_indicators) %>% 
        pivot_wider(
          names_from = short_name, 
          values_from = country, 
          values_fn = list(country = list)
        ) %>% 
        pivot_longer(cols = c(`2a`, `5a`)) %>% 
        mutate(value = lapply(value, `length<-`, max(lengths(value)))) %>% 
        pivot_wider(names_from = name, values_from = value) %>% 
        unnest(cols = c(`2a`, `5a`)) %>% 
        filter(rowSums(is.na(.[-1])) != 2)
  )
}

cap_data <- function(df, 
                     probs = c(.025, .975), 
                     deleting = TRUE){
  
    df %>% 
      {if(deleting) filter(., !is.na(value)) else .} %>% 
      group_by(short_name) %>% 
      mutate(
        lower_bound = quantile(value, probs[1], na.rm = T)[[1]],
        upper_bound = quantile(value, probs[2], na.rm = T)[[1]]
      ) %>% 
      {if(deleting){
        filter(., value > lower_bound, value < upper_bound) %>% 
          select(-lower_bound, -upper_bound)
      } else {
        mutate(.,
               value = case_when(
                 value < lower_bound ~ lower_bound,
                 value > upper_bound ~ upper_bound,
                 TRUE ~ value
               ))
      }
      }
}

min_max_normalization <- function(x, inverted = FALSE){
  if(!inverted){
    result <- 100*(x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
  } else {
    result <- 100*(x - max(x, na.rm = T))/(min(x, na.rm = T) - max(x, na.rm = T))
  }
  return(result)
}

my_fn <- function(data, mapping, reg_lines = FALSE, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(alpha = .5, size = .5)
  
  if(reg_lines){
    p + 
      geom_smooth(method=loess, fill="red", color="red", ...) +
      geom_smooth(method=lm, fill="blue", color="blue", ...)
  } else {
    p
  }
}

round_df <- function(x, digits) {
  numeric_columns <- map_chr(x, is.numeric) %>% unname %>% as.logical
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


ensanchar_indicadores <- function(indicadores_long, id_column){
  indicadores_long %>% 
    select({{ id_column }}, short_name, value) %>% 
    arrange({{ id_column }}, short_name, value) %>% 
    pivot_wider(names_from = short_name, values_from = value)
}

alargar_indicadores <- function(indicadores_wide){
  indicadores_wide %>% 
    pivot_longer(-country, names_to = "short_name", values_to = "value")
}
