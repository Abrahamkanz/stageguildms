library(brms)
library(tidyverse)
library(ggridges)
library(ggthemes)
library(viridis)
library(ggpubr)
library(lubridate)
library(cowplot)
library(tidybayes)
library(scales)
library(patchwork)



conditional_posts_fitted <- function(fit, effects, conditions = NULL, re_formula = NA, nsamples = 1000,
                                     resolution = 100){
  library(brms)
  library(tidyverse)
  library(janitor)
  list_of_data <- conditional_effects(fit, effects, conditions, resolution)[[1]]
  new_names <- list_of_data %>% 
    select(-names(fit$data[1])) %>% 
    select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% 
    remove_empty("cols")
  
  as_tibble(t(fitted(fit, newdata = list_of_data, re_formula, summary = F, nsamples = 10))) %>%
    cbind(new_names) %>%
    pivot_longer(cols = contains("V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))
} 