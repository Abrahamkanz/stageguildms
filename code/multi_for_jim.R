library(tidyverse)
library(brms)
library(janitor)
library(readxl)
library(ggridges)


#load data
#data structure is number of prey items a,l,or p in different fish guilds (finalguild). Will use site and species as random effects
data <- readRDS("data/data_for_jim.rds")

#check what priors are needed
get_prior(mvbind(a,l,p) ~ finalguild + (1|site)  + (1|species), 
          data = data, family = poisson(link = "log"))

#fit model. Priors are similar to what I would use by default in a regular logistic regression.
#basically, a parameter value of 2-3 is really big in a poisson, so I set these to normal(0,1).
guild_brm <- brm(mvbind(a,l,p) ~ finalguild + (1|species), 
                 data = data, family = poisson(link = "log"),
                 prior = c(prior(normal(0,1), class = "Intercept", resp = "a"),
                           prior(normal(0,1), class = "Intercept", resp = "l"),
                           prior(normal(0,1), class = "Intercept", resp = "p"),
                           prior(normal(0,1), class = "b", resp = "a"),
                           prior(normal(0,1), class = "b", resp = "l"),
                           prior(normal(0,1), class = "b", resp = "p")),iter = 1000, chains = 2, 
                 sample_prior = "yes") #this will produce the prior distribution in addition to the posterior

#save the model so we don't have to run again
saveRDS(guild_brm, "models/guild_brm.rds")

#check r-hats
print(guild_brm)

#extract priors (can do all of this before fitting, but it's often easier to just use sample_prior = "yes")
priors <- posterior_samples(guild_brm) %>% 
  select(contains(c("prior"))) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  as_tibble()

#wrangle priors and use inv_logit link to transform to proportions of each prey type
prior_proportions <- priors %>% 
  mutate(benthic_a = prior_Intercept_a,
         benthic_l = prior_Intercept_l,
         benthic_p = prior_Intercept_p,
         intermediate_a = benthic_a + prior_b_a,
         intermediate_l = benthic_l + prior_b_l,
         intermediate_p = benthic_p + prior_b_p,
         surface_a = benthic_a + prior_b_a,
         surface_l = benthic_l + prior_b_l,
         surface_p = benthic_p + prior_b_p) %>% 
  select(iter, tail(names(.), 9)) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -iter) %>% 
  mutate(value = inv_logit_scaled(value), #this is the transformation step
         model = "prior") %>% 
  separate(name, c("fish_type", "prey_type"))

#plot prior distributions 
prior_proportions %>% 
  ggplot(aes(x = value, fill = prey_type)) +
  geom_density() + 
  facet_grid(.~fish_type) 



#If priors look reasonable, then do the exact same thing, just with the posteriors
posts <- posterior_samples(guild_brm) %>% 
  select(contains(c("prior", "b_"))) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  as_tibble()


post_proportions <- posts %>% 
  mutate(benthic_a = b_a_Intercept,
         benthic_l = b_l_Intercept,
         benthic_p = b_p_Intercept,
         intermediate_a = benthic_a + b_a_finalguildintermediate,
         intermediate_l = benthic_l + b_l_finalguildintermediate,
         intermediate_p = benthic_p + b_p_finalguildintermediate,
         surface_a = benthic_a + b_a_finalguildsurface,
         surface_l = benthic_l + b_l_finalguildsurface,
         surface_p = benthic_p + b_p_finalguildsurface) %>% 
  select(iter, tail(names(.), 9)) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -iter) %>% 
  mutate(value = inv_logit_scaled(value),
         model = "posterior") %>% 
  separate(name, c("fish_type", "prey_type"))


post_proportions %>% 
  ggplot(aes(x = value, fill = prey_type)) +
  geom_density() + 
  facet_grid(.~fish_type) 



priors_posts <- bind_rows(prior_proportions, post_proportions)

priors_posts %>% 
  ggplot(aes(x = value, fill = prey_type)) +
  geom_density() + 
  facet_grid(model~fish_type) 



#summarize the posterior
post_proportions %>% 
  group_by(fish_type, prey_type) %>% 
  mutate(mean = mean(value),
         sd = sd(value),
         low95 = quantile(value, probs = 0.025),
         high95 = quantile(value, probs = 0.975))


#probability that prey type "l" is more common than prey type "a", etc?
#calculate differences across the posterior
diffs <- post_proportions %>% 
  pivot_wider(names_from = prey_type, values_from = value) %>% 
  mutate(diff_l_a = l-a,
         diff_l_p = l-p,
         diff_a_p = a-p)

#calculate probabilities
sum(diffs$diff_a_p > 0)/nrow(diffs) #there is a 47% probability that a is more common than p
sum(diffs$diff_l_a > 0)/nrow(diffs) #there is a 96% probability that l is more common than a

#repeat summary stats for any quantity you like.

#fin
  


  