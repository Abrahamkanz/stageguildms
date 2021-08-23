source("code/functions.r") # functions
source("code/make_data.r") # make data and load packages 

# load models
brm_aquatic_terr <- readRDS(file = "models/species_models/brm_aquatic_terr.rds")
brm_cons_noncons <- readRDS(file = "models/species_models/brm_cons_noncons.rds")
brm_chiro_stages <- readRDS(file = "models/species_models/brm_chiro_stages.rds")


# load posteriors (or fit again with code below)
posts_aquatic_terr <- readRDS(file = "posteriors/posts_aquatic_terr.rds") # use for prop terrestrial and for total prey mass
posts_cons_noncons_fishspecies <- readRDS(file = "posteriors/posts_cons_noncons_fishspecies.rds") 

# Extract posteriors 
# aquatic terrestrial ----------------------------------------------------------
fit <- brm_aquatic_terr
list_of_data <- conditional_effects(fit, effects = "date_no:species_aquatic_terr",
                                    re_formula = NULL,
                                    resolution = 30)[[1]]
new_names <- list_of_data %>% 
  select(-names(fit$data[1])) %>% 
  select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% 
  remove_empty("cols")

posts_aquatic_terr <- as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NULL, summary = F, nsamples = 1000))) %>%
  cbind(new_names) %>%
  pivot_longer(cols = contains("V"), names_to = "iter") %>%
  mutate(iter = parse_number(iter)) %>% 
  left_join(dates_correct) %>% 
  mutate(date_no = date_no + unique(total$mean_date_no)) %>% 
  left_join(aquatic_terr %>% distinct(species_aquatic_terr, fish_species, prey_ecosystem)) %>% 
  mutate(date = as_date(date_no, origin = min(ymd(aquatic_only$date)))) %>% 
  select(iter, value, date, prey_ecosystem, fish_species) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial,
         prop_terrestrial = terrestrial/total)

saveRDS(posts_aquatic_terr, file = "posteriors/posts_aquatic_terr.rds")


# consumers v non-consumers aquatic only ----------------------------------------------------------

fit <- brm_cons_noncons

list_of_data <- conditional_effects(fit, effects = "date_no:fish_prey_feeding",
                                    re_formula = NULL,
                                    resolution = 30)[[1]]
new_names <- list_of_data %>% 
  select(-names(fit$data[1])) %>% 
  select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% 
  remove_empty("cols")

posts_cons_noncons_fishspecies <- as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NULL, summary = F, nsamples = 1000))) %>%
  cbind(new_names) %>%
  pivot_longer(cols = contains("V"), names_to = "iter") %>%
  mutate(iter = parse_number(iter)) %>% 
  left_join(dates_correct) %>% 
  mutate(date_no = date_no + unique(total$mean_date_no)) %>% 
  left_join(aquatic_only %>% distinct(fish_prey_feeding, fish_species, prey_feeding)) %>% 
  mutate(date = as_date(date_no, origin = min(ymd(aquatic_only$date)))) %>% 
  select(iter, value, date, prey_feeding, fish_species) %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(total = consumer + non_consumer,
         prop_nonconsumer = non_consumer/total)

saveRDS(posts_cons_noncons_fishspecies, file = "posteriors/posts_cons_noncons_fishspecies.rds")





# chironomids -------------------------------------------------------------

fit <- brm_chiro_stages

list_of_data <- conditional_effects(fit, effects = "date_no:fish_prey_stage",
                                    re_formula = NULL,
                                    resolution = 30)[[1]]
new_names <- list_of_data %>% 
  select(-names(fit$data[1])) %>% 
  select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% 
  remove_empty("cols")

posts_chiros_fishspecies <- as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NULL, summary = F, nsamples = 1000))) %>%
  cbind(new_names) %>%
  pivot_longer(cols = contains("V"), names_to = "iter") %>%
  mutate(iter = parse_number(iter)) %>% 
  left_join(dates_correct) %>% 
  mutate(date_no = date_no + unique(total$mean_date_no)) %>% 
  left_join(chiros_only %>% distinct(fish_prey_stage, fish_species, prey_stage)) %>% 
  mutate(date = as_date(date_no, origin = min(ymd(aquatic_only$date)))) %>% 
  select(iter, value, date, prey_stage, fish_species) %>% 
  pivot_wider(names_from = prey_stage, values_from = value) %>% 
  mutate(total = a + l + p,
         prop_nonconsumer = (a + p)/total)

saveRDS(posts_chiros_fishspecies, file = "posteriors/posts_chiros_fishspecies.rds")

