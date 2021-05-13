library(tidyverse)
library(brms)
library(janitor)
library(readxl)
library(ggridges)
library(lubridate)

# get data
# wrangle data into forms for different models (e.g., adding 0.01 to zeros, grouping by terrestrial/aquatic, etc.)
emergence_2019_formodel <- readRDS("data/emergence_2019_new.rds") %>% 
  mutate(taxa = case_when(name == "chiro" ~ "chironomid", TRUE ~ "other")) %>% 
  group_by(location, julian, id, trap_days, start, collect) %>% 
  summarize(total_emerged = as.integer(sum(value, na.rm = T))) %>% 
  mutate(julian_raw = julian) %>% 
  group_by(location) %>% 
  mutate(julian = julian - min(julian))
  

guild_diet_multi_drymass <- readRDS(file = "data/guild_diet_multi_drymass.rds") %>% 
  mutate(sample_mg_dm01 = sample_mg_dm + 0.01,
         sample_mg_dm01_permm = sample_mg_dm01/parse_number(length_mm)) %>% 
  mutate(prey_ecosystem = case_when(is.na(prey_ecosystem) ~ "unknown",
                                    TRUE ~ prey_ecosystem)) 
total <- guild_diet_multi_drymass %>% 
  group_by(sample_id, fish_species, fish_guild) %>% 
  summarize(total = sum(sample_mg_dm)) %>% 
  mutate(total01 = total + 0.1)

aquatic_terr <- guild_diet_multi_drymass %>% 
  group_by(date, fish_guild, prey_ecosystem, site, fish_species, sample_id) %>% 
  summarize(sample_mg_dm01 = sum(sample_mg_dm, na.rm = T) + 0.01) %>% 
  drop_na(prey_ecosystem)

aquatic_only <- guild_diet_multi_drymass %>% filter(prey_ecosystem == "aquatic") %>% 
  group_by(date, fish_guild, prey_feeding, site, fish_species, sample_id) %>% 
  summarize(sample_mg_dm01 = sum(sample_mg_dm) + 0.01) %>% 
  drop_na(prey_feeding)

chiros_only <- guild_diet_multi_drymass %>% 
  filter(grepl("hironomid", prey_family)) 


# Models by fish guild ----------------------------------------------------------

# prop_terrestrial
brm_aqua_terr_guild <- brm(sample_mg_dm01 ~ date*fish_guild*prey_ecosystem + (1|site) + (1|fish_species),
                              data = aquatic_terr, family = Gamma(link = "log"),
                              prior = c(prior(normal(2, 2), class = "Intercept"),
                                        prior(normal(0, 2), class = "b"),
                                        prior(exponential(1), class = "sd")),
                              chains = 4, iter = 2000)

saveRDS(brm_aqua_terr_guild, file = "models/brm_aqua_terr_guild.rds")


# prop_aquatics_nonconsumers
brm_feeding_nonfeeding <- brm(sample_mg_dm01 ~ date*fish_guild*prey_feeding + (1|site) + (1|fish_species),
                               data = aquatic_only, family = Gamma(link = "log"),
                               prior = c(prior(normal(2, 2), class = "Intercept"),
                                         prior(normal(0, 2), class = "b"),
                                         prior(exponential(1), class = "sd")),
                               chains = 4, iter = 2000)

saveRDS(brm_feeding_nonfeeding, file = "models/brm_feeding_nonfeeding.rds")

brm_feeding_nonfeeding <- update(brm_feeding_nonfeeding, iter = 2000, chains = 4)

# chiro stages
brm_chiros_mg <- brm(sample_mg_dm01 ~ date*fish_guild*prey_stage + (1|site) + (1|fish_species),
                     data = chiros_only, family = Gamma(link = "log"),
                     prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(0, 1), class = "b"),
                               prior(exponential(1), class = "sd")),
                     chains = 4, iter = 2000)

saveRDS(brm_chiros_mg, file = "models/brm_chiros_mg.rds")


# chiro proportions
chiro_nonchiro <- guild_diet_multi_drymass %>% 
  mutate(chiro = case_when(grepl("hironomid", prey_family) ~ "chiro",
                           TRUE ~ "not_chiro")) %>% 
  group_by(chiro, date, fish_guild, site, fish_species, sample_id, length_mm) %>% 
  summarize(sample_mg_dm01 = sum(sample_mg_dm) + 0.01) %>% 
  ungroup() %>% 
  mutate(length_mm_number = parse_number(length_mm),
         sample_mg_dm01_permm = sample_mg_dm01/length_mm_number)

brm_chirononchiro_mg <- brm(sample_mg_dm01 ~ date*fish_guild*chiro + (1|site) + (1|fish_species),
                     data = chiro_nonchiro, family = Gamma(link = "log"),
                     prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(0, 1), class = "b"),
                               prior(exponential(1), class = "sd")),
                     chains = 4, iter = 2000)

saveRDS(brm_chirononchiro_mg, file = "models/brm_chirononchiro_mg.rds")






# Models by fish species --------------------------------------------------------
# total aquatic prey mass
brm_total_fishspecies <-  brm(sample_mg_dm01 ~ fish_species + (1|site),
                              data = aquatic_only, family = Gamma(link = "log"),
                              prior = c(prior(normal(0, 1), class = "Intercept"),
                                        prior(normal(0, 1), class = "b"),
                                        prior(exponential(1), class = "sd")),
                              chains = 4, iter = 2000)

saveRDS(brm_total_fishspecies, file = "models/brm_totalfishspecies.rds")




# proportion terrestrial prey

brm_aquatic_terr <-  brm(sample_mg_dm01 ~ fish_species*prey_ecosystem + (1|site),
                              data = aquatic_terr, family = Gamma(link = "log"),
                              prior = c(prior(normal(0, 1), class = "Intercept"),
                                        prior(normal(0, 1), class = "b"),
                                        prior(exponential(1), class = "sd")),
                              chains = 4, iter = 4000)

saveRDS(brm_aquatic_terr, file = "models/brm_aquatic_terr.rds")


# proportion consumers vs non-consumers total
brm_feeding_nonfeeding_fishspecies <-  brm(sample_mg_dm01 ~ fish_species*prey_feeding + (1|site),
                               data = aquatic_only, family = Gamma(link = "log"),
                               prior = c(prior(normal(0, 1), class = "Intercept"),
                                         prior(normal(0, 1), class = "b"),
                                         prior(exponential(1), class = "sd")),
                               chains = 4, iter = 2000)

saveRDS(brm_feeding_nonfeeding_fishspecies, file = "models/brm_feeding_nonfeeding_fishspecies.rds")


# total chiro prey mass
brm_totalchiro_fishspecies <- brm(sample_mg_dm01 ~ fish_species*prey_stage + (1|site) + (1|fish_species),
                                  data = chiros_only, family = Gamma(link = "log"),
                                  prior = c(prior(normal(0, 1), class = "Intercept"),
                                            prior(normal(0, 1), class = "b"),
                                            prior(exponential(1), class = "sd")),
                                  chains = 4, iter = 2000)

saveRDS(brm_totalchiro_fishspecies, file = "models/brm_totalchiro_fishspecies.rds")

# chiro_stages
brm_chirononchiro_fishspecies <- brm(sample_mg_dm01 ~ fish_species*chiro + (1|site) + (1|fish_species),
                            data = chiro_nonchiro, family = Gamma(link = "log"),
                            prior = c(prior(normal(0, 1), class = "Intercept"),
                                      prior(normal(0, 1), class = "b"),
                                      prior(exponential(1), class = "sd")),
                            chains = 4, iter = 2000)

saveRDS(brm_chirononchiro_fishspecies, file = "models/brm_chirononchiro_fishspecies.rds")







