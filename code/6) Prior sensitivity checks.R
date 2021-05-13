library(brms)
library(tidyverse)
library(janitor)
library(cowplot)


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


# bring in models - just the main feeding models
# brm_aquatic_terr <- readRDS("models/brm_aquatic_terr.rds")
brm_aqua_terr_guild <- readRDS("models/brm_aqua_terr_guild.rds")
brm_feeding_nonfeeding <- readRDS("models/brm_feeding_nonfeeding.rds")
# brm_chiros_mg <- readRDS("models/brm_chiros_mg.rds") #chiro life stages
# brm_chirononchiro_mg <- readRDS("models/brm_chirononchiro_mg.rds") #all taxa
# brm_feeding_nonfeeding_fishspecies <- readRDS("models/brm_feeding_nonfeeding_fishspecies.rds")
# brm_totalchiro_fishspecies <- readRDS("models/brm_totalchiro_fishspecies.rds")
# brm_chirononchiro_fishspecies <- readRDS("models/brm_chirononchiro_fishspecies.rds")
# stan_emerge_beta <- readRDS("models/stan_emerge_beta.rds")
# stan_emergechiro_beta <- readRDS(file = "models/stan_emergechiro_beta.rds")
# stan_emergechirodiet_beta <- readRDS(file = "models/stan_emergechirodiet_beta.rds")
# brm_emerge_bydate <- readRDS("models/brm_emerge_bydate.rds")

# Prior predictive simulation ---------------------------------------------

# refit with priors only
brm_aqua_terr_guild_prior <- update(brm_aqua_terr_guild, iter = 1000, chains = 1, sample_prior = "only")
brm_feeding_nonfeeding_prior <- update(readRDS("models/brm_feeding_nonfeeding.rds"), sample_prior = "only")

saveRDS(brm_aqua_terr_guild_prior, file = "models/brm_aqua_terr_guild_prior.rds")
saveRDS(brm_feeding_nonfeeding_prior, file = "models/brm_feeding_nonfeeding_prior.rds")



#extract prior and posterior conditionals
#aquatic terrestrial
conditions_ecosystem = data.frame(prey_ecosystem = unique(brm_aqua_terr_guild$data$prey_ecosystem))
posts_aquatic_terr_guild <-  conditional_posts_fitted(brm_aqua_terr_guild, effects = "date:fish_guild", conditions = conditions_ecosystem) %>% 
  mutate(model = "posterior")
priors_aquatic_terr_guild <-  conditional_posts_fitted(brm_aqua_terr_guild_prior, effects = "date:fish_guild", conditions = conditions_ecosystem) %>% 
  mutate(model = "prior")


#aquatic feeding/non-feeding
conditions_feeding = data.frame(prey_feeding = unique(brm_feeding_nonfeeding$data$prey_feeding))
posts_feeding <- conditional_posts_fitted(brm_feeding_nonfeeding, effects = "date:fish_guild", conditions = conditions_feeding) %>% 
  mutate(model = "posterior")
priors_feeding <-  conditional_posts_fitted(brm_feeding_nonfeeding_prior, effects = "date:fish_guild", conditions = conditions_feeding) %>% 
  mutate(model = "prior")


# combine posterior and priors
posts_priors_aquatic_terr_guild <- bind_rows(posts_aquatic_terr_guild, priors_aquatic_terr_guild)
posts_priors_feeding <- bind_rows(posts_feeding, priors_feeding)


# wrangle

posts_priors_aquatic_terr_guild_wide <- posts_priors_aquatic_terr_guild %>% 
  group_by(prey_ecosystem, iter, date, fish_guild, model) %>% 
  summarize(value = mean(value)) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown,
         prop_terrestrial = terrestrial/total) 

posts_priors_aquatic_terr_guild_overtime <- posts_priors_aquatic_terr_guild_wide %>% 
  group_by(fish_guild, iter, model) %>% 
  summarize(mean = mean(prop_terrestrial)) %>% 
  mutate(response = "Terrestrial prey")


posts_priors_feeding_guilds <- posts_priors_feeding %>% filter(iter <= 1000) %>%
  group_by(prey_feeding, fish_guild, date, iter, model) %>% 
  mutate(value = mean(value)) %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(prop_nonfeeding_mg = non_consumer/(consumer + non_consumer))

posts_priors_feeding_overtime <- posts_priors_feeding_guilds %>% 
  group_by(fish_guild, iter, model) %>% 
  summarize(mean = mean(prop_nonfeeding_mg)) %>% 
  mutate(response = "Non-consumer aquatic prey")

# combine data and plot  -----------------------

posts_priors_plot_all <- bind_rows(posts_priors_feeding_overtime, posts_priors_aquatic_terr_guild_overtime)

plot_prior_posterior <- posts_priors_plot_all  %>%  
  mutate(model = as.factor(model)) %>%
  mutate(model = fct_relevel(model, "prior")) %>%
  ggplot(aes(x = mean , fill = model, y = ..scaled..)) + 
  geom_density() +
  scale_fill_grey(start = 0.9, end = 0.2) +
  labs(x = "Proportion in diet",
       fill = "") +
  facet_grid(response~fish_guild) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = c(0, .25, 0.5, 0.75, 1),
                     labels = c("0", "0.25", "0.5", "0.75", "1"))

ggsave(plot_prior_posterior, file = "plots/plot_prior_posterior.jpg", dpi = 500, width = 8, height = 4)



# prior predictive checks -----------------------

brm_aqua_terr_guild <- readRDS("models/brm_aqua_terr_guild.rds")
brm_feeding_nonfeeding <- readRDS("models/brm_feeding_nonfeeding.rds")

acheck <- pp_check(brm_aqua_terr_guild, type='stat_grouped', stat= "mean", group = "fish_guild") +
  labs(title = "a) Check mean. Model = terrestrial prey") 
bcheck <- pp_check(brm_feeding_nonfeeding, type='stat_grouped', stat='mean', group = "fish_guild") +
  labs(title = "b) Check mean. Model = non-feeding") 
ccheck <- pp_check(brm_aqua_terr_guild, type = "hist") +
  labs(title = "c) Compare histograms. Model = terrestrial prey") +
  theme(axis.text.x = element_text(size = 8))
dcheck <- pp_check(brm_feeding_nonfeeding, type = "hist") +
  labs(title = "d) Compare histograms. Model = non-feeding") +
  theme(axis.text.x = element_text(size = 8))

ppc_plots <- plot_grid(acheck, bcheck, ccheck, dcheck,
                       ncol = 2) 

ggsave(ppc_plots, file = "plots/ppc_plots.jpg", dpi = 400, width = 13, height = 7)



# prior sensitivity checks -----------------------

brm_aqua_terr_guild_wider2 <- update(brm_aqua_terr_guild,
                                    prior = c(prior(normal(2, 4), class = "Intercept"),
                                              prior(normal(0, 4), class = "b"),
                                              prior(exponential(1), class = "sd")),
                                    iter = 500, chains = 1)

saveRDS(brm_aqua_terr_guild_wider2, file = "models/brm_aqua_terr_guild_wider2.rds")


brm_feeding_nonfeeding_wider <- update(brm_feeding_nonfeeding,
                                       prior = c(prior(normal(2, 4), class = "Intercept"),
                                                 prior(normal(0, 4), class = "b"),
                                                 prior(exponential(1), class = "sd")),
                                       iter = 500, chains = 1)

saveRDS(brm_feeding_nonfeeding_wider, file = "models/brm_feeding_nonfeeding_wider.rds")


brm_aqua_terr_guild_wider2 <- readRDS(file = "models/brm_aqua_terr_guild_wider2.rds")
brm_feeding_nonfeeding_wider <- readRDS(file = "models/brm_feeding_nonfeeding_wider.rds")

#aquatic terrestrial
conditions_ecosystem = data.frame(prey_ecosystem = unique(brm_aqua_terr_guild$data$prey_ecosystem))
posts_aquatic_terr_guild_wider <-  conditional_posts_fitted(brm_aqua_terr_guild_wider2, effects = "date:fish_guild", conditions = conditions_ecosystem) %>% 
  mutate(model = "posterior_wider_priors")


#aquatic feeding/non-feeding
conditions_feeding = data.frame(prey_feeding = unique(brm_feeding_nonfeeding$data$prey_feeding))
posts_feeding_wider <- conditional_posts_fitted(brm_feeding_nonfeeding_wider, effects = "date:fish_guild", conditions = conditions_feeding) %>% 
  mutate(model = "posterior_wider_priors")



# combine posterior
posts_aquatic_terr_guild_all_wider <- bind_rows(posts_aquatic_terr_guild, posts_aquatic_terr_guild_wider)
posts_feeding_all_wider <- bind_rows(posts_feeding, posts_feeding_wider)


# wrangle

posts_priors_aquatic_terr_guild_wide_wider <- posts_aquatic_terr_guild_all_wider %>% 
  group_by(prey_ecosystem, iter, date, fish_guild, model) %>% 
  summarize(value = mean(value)) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown,
         prop_terrestrial = terrestrial/total) 

posts_priors_aquatic_terr_guild_overtime_wider <- posts_priors_aquatic_terr_guild_wide_wider %>% 
  group_by(fish_guild, iter, model) %>% 
  summarize(mean = mean(prop_terrestrial)) %>% 
  mutate(response = "Terrestrial prey")




posts_priors_feeding_guilds_wider <- posts_feeding_all_wider %>% filter(iter <= 1000) %>%
  group_by(prey_feeding, fish_guild, date, iter, model) %>% 
  mutate(value = mean(value)) %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(prop_nonfeeding_mg = non_consumer/(consumer + non_consumer))

posts_priors_feeding_overtime_wider <- posts_priors_feeding_guilds_wider %>% 
  group_by(fish_guild, iter, model) %>% 
  summarize(mean = mean(prop_nonfeeding_mg)) %>% 
  mutate(response = "Non-consumer aquatic prey")


prior_sens_aquatic_terr <- posts_priors_aquatic_terr_guild_overtime_wider %>% 
  ggplot(aes(x = mean, y = ..scaled.., fill = model)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~fish_guild) +
  scale_fill_grey() +
  labs(title = "a) Proportion terrestrial",
       x = "Proportion")

prior_sens_feeding <- posts_priors_feeding_overtime_wider %>% 
  ggplot(aes(x = mean, y = ..scaled.., fill = model)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~fish_guild) +
  scale_fill_grey() +
  labs(title = "b) Proportion aquatic feeding",
       x = "Proportion")

prior_sens <- plot_grid(prior_sens_aquatic_terr, prior_sens_feeding, ncol = 1)
ggsave(prior_sens, file = "plots/prior_sens.jpg", dpi = 500, width = 6, height = 4)
