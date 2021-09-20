minium_standards <- function(data, ...){

  data %>%  group_by(...) %>%
    dplyr::summarise(., n = n()) %>% mutate(minimum_standards = ifelse(n >= 3, "requirement met", "requirement not met"))

}


minium_standards_prices <- function(data, group_var, items_var){

  data.m <- data %>% select(group_var, items_var) %>% melt()



  data.m %>% group_by(variable, !!sym(group_var)) %>% filter(!is.na(value)) %>%
    dplyr::summarise(., n = n()) %>% mutate(minimum_standards = ifelse(n >= 3, "requirement met", "requirement not met"))

  }


