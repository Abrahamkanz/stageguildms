library(tidyverse)
library(janitor)
library(ggridges)
library(bayesplot)
library(rstanarm)
library(ggpubr)
library(viridis)
library(cowplot)


guild_chiro_multi <- readRDS("data/guild_chiro_multi.rds") %>% 
  mutate(total = a + l + p,
         non_cons = l + p)
