library(brms)
library(tidyverse)
library(lubridate)

guild_diet_multi_drymass <- readRDS(file = "data/guild_diet_multi_drymass.rds")


# chiro stages
chiros_only <- guild_diet_multi_drymass %>% 
  filter(grepl("hironomid", prey_family)) %>% 
  mutate(sample_mg_dm01 = sample_mg_dm + 0.01) %>% 
  mutate(day = day(ymd(date)))


gam <- brm(sample_mg_dm01 ~ s(day, by = interaction(fish_guild, prey_stage)) + 
             (1|site) + (1|fish_species),
           data = chiros_only, family = Gamma(link = "log"),
           prior = c(prior(normal(0, 1), class = "Intercept")),
           iter = 2000, chains = 1, cores = 4)

test <- plot(conditional_effects(gam))

test$`day` +
  scale_y_log10()
test <- fitted(gam)
