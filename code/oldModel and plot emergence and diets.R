library(brms)
library(tidyverse)
library(ggridges)
library(ggthemes)
library(viridis)
library(ggpubr)
library(lubridate)
library(cowplot)
library(tidybayes)
library(janitor)
library(rstan)

#load data and models
#read data
guild_diet_multi_drymass <- readRDS(file = "data/guild_diet_multi_drymass.rds")

aquatic_only_means <- guild_diet_multi_drymass %>% filter(prey_ecosystem == "aquatic") %>% 
  group_by(date, fish_guild, prey_feeding, site, fish_species, sample_id) %>% 
  summarize(sample_mg_dm01 = sum(sample_mg_dm) + 0.01) %>% 
  drop_na(prey_feeding) %>% 
  pivot_wider(names_from = prey_feeding, values_from = sample_mg_dm01) %>% 
  mutate(prop_nonconsumer = non_consumer/(consumer + non_consumer))

chiros_only_means <- guild_diet_multi_drymass %>% 
  filter(grepl("hironomid", prey_family)) %>% 
  select(date, site, fish_guild, fish_species, prey_stage, sample_mg_dm, sample_id) %>% 
  pivot_wider(names_from = prey_stage, values_from = sample_mg_dm) %>% 
  mutate(prop_nonconsumer = (a + p)/(a+l+p)) 

emergence_2019_formodel <- readRDS("data/emergence_2019_new.rds") %>% 
  mutate(taxa = case_when(name == "chiro" ~ "chironomid", TRUE ~ "other")) %>%
  group_by(location, julian, id, trap_days, start, collect) %>% 
  summarize(total_emerged = as.integer(sum(value, na.rm = T))) %>% 
  mutate(julian_raw = julian) %>% 
  group_by(location) %>% 
  mutate(julian = julian - min(julian),
         total_perdayperm201 = total_emerged/trap_days/0.36 + 1,
         total_perdayperm201_scaled = total_perdayperm201/max(total_perdayperm201), 
         julian_fixed = as.factor(julian))

emergencechiro_2019_formodel <- readRDS("data/emergence_2019_new.rds") %>% 
  mutate(taxa = case_when(name == "chiro" ~ "chironomid", TRUE ~ "other")) %>%
  filter(taxa == "chironomid") %>% 
  group_by(location, julian, id, trap_days, start, collect) %>% 
  summarize(total_emerged = as.integer(sum(value, na.rm = T))) %>% 
  mutate(julian_raw = julian) %>% 
  group_by(location) %>% 
  mutate(julian = julian - min(julian),
         total_perdayperm201 = total_emerged/trap_days/0.36 + 1,
         total_perdayperm201_scaled = total_perdayperm201/max(total_perdayperm201), 
         julian_fixed = as.factor(julian))

# derive intercept prior as mean individuals expected per day

nps_data <- read_csv("https://raw.githubusercontent.com/jswesner/nps_emergence/master/emerge_data.csv")
nps_data %>%
  summarize(mean_ind_perday = mean(indm2day),
                       max = max(indm2day))

# fit model
# brm_emerge_bydate <- brm(total_perdayperm201 ~ julian_fixed*location,
#                   data = emergence_2019_formodel,
#                   family = Gamma(link = "log"),
#                   prior = c(prior(normal(0, 1), class = "b"),
#                             prior(normal(4,1), class = "Intercept")),
#                   chains = 4, iter = 2000, cores = 4)
# 
# 
# saveRDS(brm_emerge_bydate, file = "models/brm_emerge_bydate.rds")

# fit model for chiros only

# 
# 
# brm_emergechiro_data <- make_standata(total_perdayperm201 ~ julian_fixed*location,
#                   data = emergencechiro_2019_formodel,
#                   family = Gamma(link = "log"),
#                   prior = c(prior(normal(0, 1), class = "b"),
#                             prior(normal(4,1), class = "Intercept")),
#                   chains = 4, iter = 2000, cores = 4)
# 
# brm_emergechiro_code <- make_stancode(total_perdayperm201 ~ julian_fixed*location,
#                                       data = emergencechiro_2019_formodel,
#                                       family = Gamma(link = "log"),
#                                       prior = c(prior(normal(0, 1), class = "b"),
#                                                 prior(normal(4,1), class = "Intercept")),
#                                       chains = 4, iter = 2000, cores = 4)
# 
# brm_emergechiro_bydate <- stan(model_code = brm_emergechiro_code, data = brm_emergechiro_data)
# 
# saveRDS(brm_emergechiro_bydate, file = "models/brm_emergechiro_bydate.rds")

brm_emerge_bydate <- readRDS("models/brm_emerge_bydate.rds")
brm_emergechiro_bydate <- readRDS("models/brm_emergechiro_bydate.rds")

# convert from stan to brms
# fit empty model first
brms_fit_emerge_chiro <- brm(total_perdayperm201 ~ julian_fixed*location,
                data = emergencechiro_2019_formodel,
                family = Gamma(link = "log"),
                prior = c(prior(normal(0, 1), class = "b"),
                          prior(normal(4,1), class = "Intercept")),
                chains = 4, iter = 2000, cores = 4, empty = TRUE) #empty

brms_fit_emerge_chiro$fit <- brm_emergechiro_bydate
brms_fit_emerge_chiro <- rename_pars(brms_fit_emerge_chiro)


#extract posteriors for emergence

post_emerge_means <- fitted(brm_emerge_bydate) %>% as_tibble() %>% 
  mutate(location = emergence_2019_formodel$location,
         julian_fixed = emergence_2019_formodel$julian_fixed,
         julian = emergence_2019_formodel$julian,
         date = emergence_2019_formodel$collect) %>% 
  distinct(location, julian, date, .keep_all = T) %>% 
  clean_names() %>% 
  mutate(estimate_scaled = (estimate - mean(estimate))/sd(estimate)) %>% 
  left_join(emergence_2019_formodel %>% select(julian, julian_raw, start, collect)) %>% 
  mutate(date = ymd(collect)) %>% 
  select(estimate, estimate_scaled, q2_5, q97_5, est_error, location, date) %>% 
  rename(site = location)

saveRDS(post_emerge_means, file = "models/post_merge_means.rds")


post_emergechiro_means <- fitted(brms_fit_emerge_chiro) %>% as_tibble() %>% 
  mutate(location = emergencechiro_2019_formodel$location,
         julian_fixed = emergencechiro_2019_formodel$julian_fixed,
         julian = emergencechiro_2019_formodel$julian,
         date = emergencechiro_2019_formodel$collect) %>% 
  distinct(location, julian, date, .keep_all = T) %>% 
  clean_names() %>% 
  mutate(estimate_scaled = (estimate - mean(estimate))/sd(estimate)) %>% 
  left_join(emergencechiro_2019_formodel %>% select(julian, julian_raw, start, collect)) %>% 
  mutate(date = ymd(collect)) %>% 
  select(estimate, estimate_scaled, location, date) %>% 
  rename(site = location)

saveRDS(post_emergechiro_means, file = "models/post_emergechiro_means.rds")


# combine emergence posterior with raw diet data. Match dates or impute nearest previous date of emergence
emerge_v_diet <- post_emerge_means %>% 
  mutate(site = case_when(site == "littlebridge" ~ "little bridge", 
                          site == "spiritmound" ~ "spirit mound", 
                          TRUE ~ site)) %>% 
  full_join(aquatic_only_means %>% mutate(date = ymd(date))) %>% 
  arrange(date) %>%
  group_by(site) %>% 
  mutate(imputed = case_when(is.na(estimate) ~ "imputed", TRUE ~ "modeled")) %>% 
  fill(estimate, .direction = "down") %>% 
  fill(estimate_scaled, .direction = "down") %>%
  fill(site, .direction = "down") %>% 
  filter(!is.na(estimate)) %>% 
  drop_na(prop_nonconsumer) %>% 
  mutate(prop_nonconsumer01 = case_when(prop_nonconsumer == 0 ~ 0.001, 
                                        prop_nonconsumer == 1 ~ 0.999,
                                        TRUE ~ prop_nonconsumer))

saveRDS(emerge_v_diet, file = "data/emerge_v_diet.rds")

emergechiro_v_dietall <- post_emerge_means %>% 
  mutate(site = case_when(site == "littlebridge" ~ "little bridge", 
                          site == "spiritmound" ~ "spirit mound", 
                          TRUE ~ site)) %>% 
  full_join(chiros_only_means %>% mutate(date = ymd(date))) %>% 
  arrange(date) %>%
  group_by(site) %>% 
  mutate(imputed = case_when(is.na(estimate) ~ "imputed", TRUE ~ "modeled")) %>% 
  fill(estimate, .direction = "down") %>% 
  fill(estimate_scaled, .direction = "down") %>%
  fill(site, .direction = "down") %>% 
  filter(!is.na(estimate)) %>% 
  drop_na(prop_nonconsumer) %>% 
  mutate(prop_nonconsumer01 = case_when(prop_nonconsumer == 0 ~ 0.001, 
                                        prop_nonconsumer == 1 ~ 0.999,
                                        TRUE ~ prop_nonconsumer))

saveRDS(emergechiro_v_dietall, file = "data/emerge_chiro_v_dietall.rds")


emergechiro_v_dietchiro <- post_emergechiro_means %>% 
  mutate(site = case_when(site == "littlebridge" ~ "little bridge", 
                          site == "spiritmound" ~ "spirit mound", 
                          TRUE ~ site)) %>% 
  full_join(chiros_only_means %>% mutate(date = ymd(date))) %>% 
  arrange(date) %>%
  group_by(site) %>% 
  mutate(imputed = case_when(is.na(estimate) ~ "imputed", TRUE ~ "modeled")) %>% 
  fill(estimate, .direction = "down") %>% 
  fill(estimate_scaled, .direction = "down") %>%
  fill(site, .direction = "down") %>% 
  filter(!is.na(estimate)) %>% 
  drop_na(prop_nonconsumer) %>% 
  mutate(prop_nonconsumer01 = case_when(prop_nonconsumer == 0 ~ 0.001, 
                                        prop_nonconsumer == 1 ~ 0.999,
                                        TRUE ~ prop_nonconsumer))

saveRDS(emergechiro_v_dietchiro, file = "data/emergechiro_v_dietchiro.rds")


# sanity check
emerge_v_diet %>% 
  ggplot(aes(x = estimate, y = prop_nonconsumer)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~fish_guild) 

emergechiro_v_dietall %>% 
  ggplot(aes(x = estimate, y = prop_nonconsumer01)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~fish_guild) 

emergechiro_v_dietchiro %>% 
  ggplot(aes(x = estimate, y = prop_nonconsumer01)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~fish_guild) 

# Fit model ---------------------------------------------------------------


# check priors
get_prior(prop_nonconsumer ~ estimate_scaled*site*fish_guild,
              data = emerge_v_diet, 
              family = Beta(link = "logit"))

# simulate priors
prior_sim <- tibble(int = rnorm(100, -3, 2),
               b = rnorm(100, 0, 1)) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  expand_grid(emerge_scaled = unique(emerge_v_diet$estimate_scaled)) %>% 
  mutate(prop_nonconsumer = inv_logit_scaled(int + b*emerge_scaled),
         phi = rexp(nrow(.), 2))

prior_sim %>% 
  ggplot(aes(x = emerge_scaled, y = prop_nonconsumer)) +
  geom_line(aes(group = iter), alpha = 0.3)


# fit model in stan for all taxa --------------------
emerge_stan_code <- make_stancode(prop_nonconsumer01 ~ estimate_scaled*fish_guild + (1|site) + (1|fish_species),
                  data = emerge_v_diet,
                  family = Beta(link = "logit"),
                  prior = c(prior(normal(-3, 2), class = "Intercept"),
                            prior(normal(0, 1), class = "b"),
                            prior(exponential(1), class = "phi")),
                  chains = 4, iter = 2000, cores = 4)

emerge_stan_data <- make_standata(prop_nonconsumer01 ~ estimate_scaled*fish_guild + (1|site) + (1|fish_species),
                  data = emerge_v_diet,
                  family = Beta(link = "logit"),
                  prior = c(prior(normal(-3, 2), class = "Intercept"),
                            prior(normal(0, 1), class = "b"),
                            prior(exponential(1), class = "phi")),
                  chains = 4, iter = 2000, cores = 4)

stan_emerge_beta <- stan(model_code = emerge_stan_code, data = emerge_stan_data)
saveRDS(stan_emerge_beta, file = "models/stan_emerge_beta.rds")


# fit model in stan for chiro emergence and all diet --------------------
emergechiro_stan_code <- make_stancode(prop_nonconsumer01 ~ estimate_scaled*fish_guild + (1|site) + (1|fish_species),
                  data = emergechiro_v_dietall,
                  family = Beta(link = "logit"),
                  prior = c(prior(normal(-3, 2), class = "Intercept"),
                            prior(normal(0, 1), class = "b"),
                            prior(exponential(1), class = "phi")),
                  chains = 4, iter = 2000, cores = 4)

emergechiro_stan_data <- make_standata(prop_nonconsumer01 ~ estimate_scaled*fish_guild + (1|site) + (1|fish_species),
                  data = emergechiro_v_dietall,
                  family = Beta(link = "logit"),
                  prior = c(prior(normal(-3, 2), class = "Intercept"),
                            prior(normal(0, 1), class = "b"),
                            prior(exponential(1), class = "phi")),
                  chains = 4, iter = 2000, cores = 4)

stan_emergechiro_beta <- stan(model_code = emergechiro_stan_code, data = emergechiro_stan_data)
saveRDS(stan_emergechiro_beta, file = "models/stan_emergechiro_beta.rds")

# fit model in stan for chiro emergence and chiro only diet --------------------
emergechirodiet_stan_code <- make_stancode(prop_nonconsumer01 ~ estimate_scaled*fish_guild + (1|site) + (1|fish_species),
                                       data = emergechiro_v_dietchiro,
                                       family = Beta(link = "logit"),
                                       prior = c(prior(normal(-3, 2), class = "Intercept"),
                                                 prior(normal(0, 1), class = "b"),
                                                 prior(exponential(1), class = "phi")),
                                       chains = 4, iter = 2000, cores = 4)

emergechirodiet_stan_data <- make_standata(prop_nonconsumer01 ~ estimate_scaled*fish_guild + (1|site) + (1|fish_species),
                                       data = emergechiro_v_dietchiro,
                                       family = Beta(link = "logit"),
                                       prior = c(prior(normal(-3, 2), class = "Intercept"),
                                                 prior(normal(0, 1), class = "b"),
                                                 prior(exponential(1), class = "phi")),
                                       chains = 4, iter = 2000, cores = 4)

stan_emergechirodiet_beta <- stan(model_code = emergechirodiet_stan_code, data = emergechirodiet_stan_data)
saveRDS(stan_emergechirodiet_beta, file = "models/stan_emergechirodiet_beta.rds")
