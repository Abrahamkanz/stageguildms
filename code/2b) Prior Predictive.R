# Make data and load packages
source("code/make_data.r")

# Models by fish species 

brm_aquatic_terr <- readRDS(file = "models/species_models/brm_aquatic_terr.rds")
prior_aquatic_terr <- readRDS(file = "models/species_models/prior_aquatic_terr.rds")

# saveRDS(brm_aquatic_terr, file = "models/species_models/brm_aquatic_terr.rds")

# proportion terrestrial prey --------------------------------------------------------
aquatic_terr_priorpred <- aquatic_terr %>% 
  sample_n(200)
###!!!! Takes ~ 2.5 hours

brm_aquatic_terr <-  update(prior_aquatic_terr, newdata = aquatic_terr, sample_prior = "no")
# saveRDS(brm_aquatic_terr, file = "models/species_models/brm_aquatic_terr.rds")

prior_aquatic_terr <- readRDS("models/species_models/prior_aquatic_terr.rds")

prior_aquatic_terr <- update(prior_aquatic_terr, newdata = aquatic_terr_priorpred, 
                             iter = 1000, chains = 1, sample_prior = "only",
                             prior = c(prior(normal(1, 1), class = "Intercept"),
                                       prior(normal(0, 2), class = "b"),
                                       prior(normal(0, 2), class = "sds"),
                                       prior(exponential(2), class = "sd"),
                                       prior(gamma(1, 0.1), class = "shape")))



saveRDS(prior_aquatic_terr, file = "models/species_models/prior_aquatic_terr.rds")

test <- plot(conditional_effects(brm_aquatic_terr, effects = "date_no:species_aquatic_terr",
                                 re_formula = NULL), points = T)


test$`date_no:species_aquatic_terr` + 
  facet_wrap(~species_aquatic_terr) + 
  guides(fill = F, color = F) +
  scale_y_log10()

