library(brms)
library(tidyverse)
library(janitor)


#function to extract conditional posteriors
conditional_posts_fitted <- function(fit, effects, conditions = NULL){
  library(brms)
  library(tidyverse)
  library(janitor)
  list_of_data <- conditional_effects(fit, effects, conditions)[[1]]
  new_names <- list_of_data %>% 
    select(-names(fit$data[1])) %>% 
    select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% 
    remove_empty("cols")
  
  as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NA, summary = FALSE))) %>%
    cbind(new_names) %>%
    pivot_longer(cols = contains("V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))
} 


# bring in models
brm_aquatic_terr <- readRDS("models/brm_aquatic_terr.rds")
brm_aqua_terr_guild <- readRDS("models/brm_aqua_terr_guild.rds")
brm_feeding_nonfeeding <- readRDS("models/brm_feeding_nonfeeding.rds")
brm_chiros_mg <- readRDS("models/brm_chiros_mg.rds") #chiro life stages
brm_chirononchiro_mg <- readRDS("models/brm_chirononchiro_mg.rds") #all taxa
brm_feeding_nonfeeding_fishspecies <- readRDS("models/brm_feeding_nonfeeding_fishspecies.rds")
brm_totalchiro_fishspecies <- readRDS("models/brm_totalchiro_fishspecies.rds")
brm_chirononchiro_fishspecies <- readRDS("models/brm_chirononchiro_fishspecies.rds")
stan_emerge_beta <- readRDS("models/stan_emerge_beta.rds")
stan_emergechiro_beta <- readRDS(file = "models/stan_emergechiro_beta.rds")
stan_emergechirodiet_beta <- readRDS(file = "models/stan_emergechirodiet_beta.rds")
brm_emerge_bydate <- readRDS("models/brm_emerge_bydate.rds")

#extract posteriors

# Models by fish guild ----------------------------------------------------------
# proportion terrestrial
conditions_ecosystem = data.frame(prey_ecosystem = unique(brm_aqua_terr_guild$data$prey_ecosystem))
posts_aquatic_terr_guild <-  conditional_posts_fitted(brm_aqua_terr_guild, effects = "date:fish_guild", conditions = conditions_ecosystem)
saveRDS(posts_aquatic_terr_guild, file = "models/posts_aquatic_terr_guild.rds")

# proportion aquatic nonconsumer
conditions_feeding = data.frame(prey_feeding = unique(brm_feeding_nonfeeding$data$prey_feeding))
posts_feeding <- conditional_posts_fitted(brm_feeding_nonfeeding, effects = "date:fish_guild", conditions = conditions_feeding)
saveRDS(posts_feeding, file = "models/posts_feeding.rds")


# chiro life stages
conditions = data.frame(prey_stage = c("a", "l", "p"))
posts_mg_chiro <- conditional_posts_fitted(brm_chiros_mg, effects = "date:fish_guild", conditions = conditions)
saveRDS(posts_mg_chiro, file = "models/posts_mg_chiro.rds")

#all taxa
conditions_nonchiro  <- data.frame(chiro = c("chiro", "not_chiro"))
posts_mg_chirononchiro <- conditional_posts_fitted(brm_chirononchiro_mg, effects = "date:fish_guild", 
                                                   conditions = conditions_nonchiro)
saveRDS(posts_mg_chirononchiro, file = "models/posts_mg_chirononchiro.rds")


# Models by fish species ----------------------------------------------------------
brm_feeding_nonfeeding_fishspecies <- readRDS("models/brm_feeding_nonfeeding_fishspecies.rds")
brm_totalchiro_fishspecies <- readRDS("models/brm_totalchiro_fishspecies.rds")
brm_chirononchiro_fishspecies <- readRDS("models/brm_chirononchiro_fishspecies.rds")

# aquatic terrestrial
posts_aquatic_terr <- conditional_posts_fitted(brm_aquatic_terr, effects = "prey_ecosystem:fish_species")
saveRDS(posts_aquatic_terr, file = "models/posts_aquatic_terr.rds")


# feeding nonfeeding overall
posts_feeding_fishspecies <- conditional_posts_fitted(brm_feeding_nonfeeding_fishspecies, effects = "prey_feeding:fish_species")
saveRDS(posts_feeding_fishspecies, file = "models/posts_feeding_fishspecies.rds")

#all taxa
# ERROR IN FUNCTION...NEED TO EXTRACT BY HAND - conditions_nonchiro  <- data.frame(chiro = c("chiro", "not_chiro"))
# ERROR IN FUNCTION...NEED TO EXTRACT BY HAND - posts_mg_chirononchiro_fishspecies <- conditional_posts_fitted(brm_chirononchiro_fishspecies, effects = "chiro:fish_species", 
                                                   # conditions = conditions_nonchiro)

posts_x <- posterior_samples(brm_chirononchiro_fishspecies, summary = F, samples = 1000) %>% 
  select(starts_with("b_")) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  pivot_longer(cols = c(-b_Intercept, -b_chironot_chiro, -iter)) %>% 
  separate(name, c("name", "stage"), sep = ":") %>% 
  pivot_wider(names_from = stage, values_from = value) %>% 
  rename(b_fishspecies = 'NA',
         b_fishspecies_stage = chironot_chiro) %>% 
  mutate(chiro = b_Intercept + b_fishspecies,
         not_chiro = b_Intercept + b_fishspecies + b_fishspecies_stage) %>% 
  select(name, chiro, not_chiro, iter) %>% 
  rename(fish_species_temp = name)

posts_lowestlevelx <- posterior_samples(brm_chirononchiro_fishspecies, summary = F, samples = 1000) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  select(c(b_Intercept, b_chironot_chiro, iter)) %>% 
  mutate(name = "b_fish_speciesAmeiurusmelas",
         chiro = b_Intercept,
         not_chiro = b_Intercept + b_chironot_chiro) %>% 
  select(name, chiro, not_chiro, iter) %>% 
  rename(fish_species_temp = name)

#fish species to replace with space between genus and species
fish_replace <- posts_feeding_fishspecies %>% distinct(fish_species) %>% mutate(fish_species_temp = str_replace(fish_species, " ", "") )

posts_chirononchiro_fishspecies <- bind_rows(posts_x, posts_lowestlevelx) %>% mutate(fish_species_temp = str_sub(fish_species_temp, 15, 50)) %>% 
  left_join(fish_replace) %>% select(-fish_species_temp)

saveRDS(posts_chirononchiro_fishspecies, file = "models/posts_chirononchiro_fishspecies.rds")


# chiro life stages
# ERROR IN FUNCTION...NEED TO EXTRACT BY HAND - posts_mg_chiro_fishspecies <- conditional_posts_fitted(brm_totalchiro_fishspecies, effects = "fish_species:prey_stage")
posts_a <- posterior_samples(brm_totalchiro_fishspecies, summary = F, samples = 1000) %>% 
  select(starts_with("b_")) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  pivot_longer(cols = c(-b_Intercept, -b_prey_stagel, -b_prey_stagep, -iter)) %>% 
    separate(name, c("name", "stage"), sep = ":") %>% 
    pivot_wider(names_from = stage, values_from = value) %>% 
    rename(b_fishspecies = 'NA',
           b_fishspecies_stagel = prey_stagel,
           b_fishspecies_stagep = prey_stagep) %>% 
  mutate(a = exp(b_Intercept + b_fishspecies),
         l = exp(b_Intercept + b_prey_stagel + b_fishspecies + b_fishspecies_stagel),
         p = exp(b_Intercept + b_prey_stagep + b_fishspecies + b_fishspecies_stagep)) %>% 
  select(name, a, l, p, iter) %>% 
  rename(fish_species_temp = name)

posts_lowestlevel <- posterior_samples(brm_totalchiro_fishspecies, summary = F, samples = 1000) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  select(c(b_Intercept, b_prey_stagel, b_prey_stagep, iter)) %>% 
  mutate(name = "b_fish_speciesAmeiurusmelas",
         a = exp(b_Intercept),
         l = exp(b_Intercept + b_prey_stagel),
         p = exp(b_Intercept + b_prey_stagep)) %>% 
  select(name, a, l, p, iter) %>% 
  rename(fish_species_temp = name)

posts_totalchiro_fishspecies <- bind_rows(posts_a, posts_lowestlevel) %>% mutate(fish_species_temp = str_sub(fish_species_temp, 15, 50)) %>% 
  left_join(fish_replace) %>% select(-fish_species_temp)


saveRDS(posts_totalchiro_fishspecies, file = "models/posts_totalchiro_fishspecies.rds")



# Emergence versus diet --------------

# data
emerge_v_diet <- readRDS("data/emerge_v_diet.rds")

# posts

posts <- posterior_samples(stan_emerge_beta)[,c(1:5, 62)]
colnames(posts) <- c("estimate_scaled", "fish_guildintermediate", "fish_guildsurface",
                     "estimate_scaled:fish_guildintermediate", "estimate_scaled:fish_guildsurface",
                     "Intercept")

emerge_posts <- posts %>% mutate(benthic_int = Intercept,
                                benthic_slope = estimate_scaled,
                                intermediate_int = Intercept + fish_guildintermediate,
                                intermediate_slope = estimate_scaled + `estimate_scaled:fish_guildintermediate`,
                                surface_int = Intercept + fish_guildsurface,
                                surface_slope = estimate_scaled + `estimate_scaled:fish_guildsurface`,
                                iter = 1:nrow(.))

saveRDS(emerge_posts, file = "models/emerge_posts.rds")

emerge_posts_conds <- emerge_posts %>% 
  expand_grid(emergence = emerge_v_diet$estimate_scaled) %>% 
  mutate(benthic = inv_logit_scaled(benthic_int + benthic_slope*emergence),
         intermediate = inv_logit_scaled(intermediate_int + intermediate_slope*emergence),
         surface = inv_logit_scaled(surface_int + surface_slope*emergence))

emerge_posts_long <- emerge_posts_conds[13:17] %>% filter(iter <=1000) %>% pivot_longer(cols = c(benthic, intermediate, surface))
saveRDS(emerge_posts_long, file = "models/emerge_posts_long.rds")
saveRDS(emerge_posts_conds, file = "models/emerge_posts_conds.rds")




# Chiro only emergence versus diet --------------

posts_chiro <- posterior_samples(stan_emergechiro_beta)[,c(1:5, 56)]
colnames(posts_chiro) <- c("estimate_scaled", "fish_guildintermediate", "fish_guildsurface",
                     "estimate_scaled:fish_guildintermediate", "estimate_scaled:fish_guildsurface",
                     "Intercept")

emergechiro_posts <- posts_chiro %>% mutate(benthic_int = Intercept,
                                 benthic_slope = estimate_scaled,
                                 intermediate_int = Intercept + fish_guildintermediate,
                                 intermediate_slope = estimate_scaled + `estimate_scaled:fish_guildintermediate`,
                                 surface_int = Intercept + fish_guildsurface,
                                 surface_slope = estimate_scaled + `estimate_scaled:fish_guildsurface`,
                                 iter = 1:nrow(.))

saveRDS(emergechiro_posts, file = "models/emergechiro_posts.rds")

emergechiro_v_dietall <- readRDS("data/emerge_chiro_v_dietall.rds")

emergechiro_posts_conds <- emergechiro_posts %>% 
  expand_grid(emergence = emergechiro_v_dietall$estimate_scaled) %>% 
  mutate(benthic = inv_logit_scaled(benthic_int + benthic_slope*emergence),
         intermediate = inv_logit_scaled(intermediate_int + intermediate_slope*emergence),
         surface = inv_logit_scaled(surface_int + surface_slope*emergence))

emergechiro_posts_long <- emergechiro_posts_conds[13:17] %>% filter(iter <=1000) %>% pivot_longer(cols = c(benthic, intermediate, surface))
saveRDS(emergechiro_posts_long, file = "models/emergechiro_posts_long.rds")
saveRDS(emergechiro_posts_conds, file = "models/emergechiro_posts_conds.rds")




# Chiro only emergence versus chiro only diet --------------

posts_chiroonly <- posterior_samples(stan_emergechirodiet_beta)[,c(1:5, 56)]
colnames(posts_chiroonly) <- c("estimate_scaled", "fish_guildintermediate", "fish_guildsurface",
                           "estimate_scaled:fish_guildintermediate", "estimate_scaled:fish_guildsurface",
                           "Intercept")

emergechiroonly_posts <- posts_chiroonly %>% mutate(benthic_int = Intercept,
                                            benthic_slope = estimate_scaled,
                                            intermediate_int = Intercept + fish_guildintermediate,
                                            intermediate_slope = estimate_scaled + `estimate_scaled:fish_guildintermediate`,
                                            surface_int = Intercept + fish_guildsurface,
                                            surface_slope = estimate_scaled + `estimate_scaled:fish_guildsurface`,
                                            iter = 1:nrow(.))

saveRDS(emergechiroonly_posts, file = "models/emergechiroonly_posts.rds")

emergechiro_v_dietchiro <- readRDS("data/emergechiro_v_dietchiro.rds")

emergechiroonly_posts_conds <- emergechiroonly_posts %>% 
  expand_grid(emergence = emergechiro_v_dietchiro$estimate_scaled) %>% 
  mutate(benthic = inv_logit_scaled(benthic_int + benthic_slope*emergence),
         intermediate = inv_logit_scaled(intermediate_int + intermediate_slope*emergence),
         surface = inv_logit_scaled(surface_int + surface_slope*emergence))

emergechiroonly_posts_long <- emergechiroonly_posts_conds[13:17] %>% filter(iter <=1000) %>% pivot_longer(cols = c(benthic, intermediate, surface))
saveRDS(emergechiroonly_posts_long, file = "models/emergechiroonly_posts_long.rds")
saveRDS(emergechiroonly_posts_conds, file = "models/emergechiroonly_posts_conds.rds")



