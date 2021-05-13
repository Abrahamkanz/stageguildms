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
  mutate(prop_nonconsumer = non_consumer/(consumer + non_consumer)) %>% 
  group_by(date, site, fish_guild) 

chiros_only_means <- guild_diet_multi_drymass %>% 
  filter(grepl("hironomid", prey_family)) %>% 
  pivot_wider(names_from = prey_stage, values_from = sample_mg_dm) %>% 
  mutate(prop_nonconsumer = (a + p)/(a+l+p)) %>% 
group_by(date, site) %>% 
  summarize(mean_propnonconsumer = mean(prop_nonconsumer, na.rm = T),
            sd_propnonconsumer = sd(prop_nonconsumer, na.rm = T))

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

brm_emerge_bydate <- readRDS("models/brm_emerge_bydate.rds")

#extract posteriors for emergence

post_emerge_means <- fitted(brm_emerge_bydate) %>% as_tibble() %>% 
  mutate(location = emergence_2019_formodel$location,
         julian_fixed = emergence_2019_formodel$julian_fixed,
         julian = emergence_2019_formodel$julian,
         date = emergence_2019_formodel$collect) %>% 
  distinct(location, julian, date, .keep_all = T) %>% 
  clean_names() %>% 
  left_join(emergence_2019_formodel %>% select(julian, julian_raw, start, collect)) %>% 
  mutate(date = ymd(collect)) %>% 
  select(estimate, est_error, location, date) %>% 
  rename(site = location)

# combine emergence posterior with raw diet data. Match dates or impute nearest previous date of emergence
emerge_v_diet <- post_emerge_means %>% 
  mutate(site = case_when(site == "littlebridge" ~ "little bridge", 
                          site == "spiritmound" ~ "spirit mound", 
                          TRUE ~ site)) %>% 
  full_join(aquatic_only_means %>% mutate(date = ymd(date)) %>% 
              ungroup() %>% 
              group_by(fish_guild, date, site) %>% 
              summarize(consumer_total = sum(consumer),
                        non_consumer_total = sum(non_consumer)) %>% 
              mutate(prop_nonconsumer = non_consumer_total/(consumer_total + non_consumer_total)) %>% 
              distinct(prop_nonconsumer, site, date, .keep_all = T)) %>% 
  arrange(date) %>%
  group_by(site) %>% 
  mutate(imputed = case_when(is.na(estimate) ~ "imputed", TRUE ~ "modeled")) %>% 
  fill(estimate, .direction = "down") %>% 
  fill(est_error, .direction = "down") %>%
  fill(site, .direction = "down") %>% 
  filter(!is.na(estimate)) %>% 
  drop_na(prop_nonconsumer) %>% 
  ungroup() %>% 
  mutate(estimate_scaled = (estimate - mean(estimate))/sd(estimate)) %>% 
  ungroup()

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


# fit model
# emerge_stan_code <- make_stancode(prop_nonconsumer ~ estimate_scaled*fish_guild + (1|site),
#                   data = emerge_v_diet, 
#                   family = Beta(link = "logit"),
#                   prior = c(prior(normal(-3, 2), class = "Intercept"),
#                             prior(normal(0, 1), class = "b"),
#                             prior(exponential(1), class = "phi")),
#                   chains = 4, iter = 2000, cores = 4)
# 
# emerge_stan_data <- make_standata(prop_nonconsumer ~ estimate_scaled*fish_guild + (1|site),
#                   data = emerge_v_diet, 
#                   family = Beta(link = "logit"),
#                   prior = c(prior(normal(-3, 2), class = "Intercept"),
#                             prior(normal(0, 1), class = "b"),
#                             prior(exponential(1), class = "phi")),
#                   chains = 4, iter = 2000, cores = 4)
# 
# 
# stan_emerge_beta <- stan(model_code = emerge_stan_code, data = emerge_stan_data)
# saveRDS(stan_emerge_beta, file = "models/stan_emerge_beta.rds")
stan_emerge_beta <- readRDS("models/stan_emerge_beta.rds")

stan_emerge_beta@model_pars

names <- as.data.frame(emerge_stan_data$X)

emerge_stan_data$X


posts <- posterior_samples(stan_emerge_beta)[,c(1:5, 17)]
colnames(posts) <- c("estimate_scaled", "fish_guildintermediate", "fish_guildsurface",
                     "estimate_scaled:fish_guildintermediate", "estimate_scaled:fish_guildsurface",
                     "Intercept")

posts_conds <- posts %>% mutate(benthic_int = Intercept,
                                benthic_slope = estimate_scaled,
                                intermediate_int = Intercept + fish_guildintermediate,
                                intermediate_slope = estimate_scaled + `estimate_scaled:fish_guildintermediate`,
                                surface_int = Intercept + fish_guildsurface,
                                surface_slope = estimate_scaled + `estimate_scaled:fish_guildsurface`,
                                iter = 1:nrow(.)) %>% 
  expand_grid(emergence = emerge_v_diet$estimate_scaled) %>% 
  mutate(benthic = inv_logit_scaled(benthic_int + benthic_slope*emergence),
         intermediate = inv_logit_scaled(intermediate_int + intermediate_slope*emergence),
         surface = inv_logit_scaled(surface_int + surface_slope*emergence))

posts_long <- posts_conds[13:17] %>% filter(iter <=1000) %>% pivot_longer(cols = c(benthic, intermediate, surface))


posts_long %>% 
  group_by(name, emergence) %>% 
  summarize(median = median(value),
            sd = sd(value)) %>% 
  ggplot() + 
  geom_line(aes(x = emergence, y = median, ymax = median + sd, ymin = median - sd, color = name)) + 
  geom_ribbon(aes(x = emergence, y = median, ymax = median + sd, ymin = median - sd, color = name),
              alpha = 0.2) +
  geom_point(data = emerge_v_diet %>% mutate(name = fish_guild), aes( x= estimate_scaled, y = prop_nonconsumer, color = fish_guild)) +
  facet_wrap(~name)


ggplot(data = emerge_v_diet %>% mutate(name = fish_guild), aes( x= estimate, y = prop_nonconsumer, color = fish_guild)) +
  facet_grid(~site) +
  geom_point() +
  geom_smooth(method = "lm")


