# Make data and load packages
source("code/make_data.r")

# Models by fish species 

# proportion terrestrial prey --------------------------------------------------------
aquatic_terr
###!!!! Takes ~ 2.5 hours
# brm_aquatic_terr <-  brm(sample_mg_dm01 ~ s(date_no, by = species_aquatic_terr) + (1|species_aquatic_terr) + (1|site), 
#                          data = aquatic_terr,
#                          family = Gamma(link = "log"),
#                          prior = c(prior(normal(1, 2), class = "Intercept"),
#                                    prior(normal(0, 5), class = "b"),
#                                    prior(normal(0, 5), class = "sds"),
#                                    prior(exponential(2), class = "sd"),
#                                    prior(gamma(1, 0.1), class = "shape")),
#                          iter = 2000, chains = 4)

# saveRDS(brm_aquatic_terr, file = "models/species_models/brm_aquatic_terr.rds")



# proportion of consumers --------------------------------------------------------
aquatic_only
###!!!! Takes ~ 1 hour
# brm_cons_noncons <- brm(sample_mg_dm01 ~ s(date_no, by = fish_prey_feeding) + (1|fish_prey_feeding) + (1|site),
#                            data = aquatic_only,
#                            family = Gamma(link = "log"),
#                            prior = c(prior(normal(1, 2), class = "Intercept"),
#                                      prior(normal(0, 5), class = "b"),
#                                      prior(normal(0, 5), class = "sds"),
#                                      prior(exponential(2), class = "sd"),
#                                      prior(gamma(1, 0.1), class = "shape")),
#                            iter = 2000, chains = 4)

# brm_cons_noncons <- update(readRDS("models/species_models/brm_cons_noncons.rds"), newdata = aquatic_only)

# saveRDS(brm_cons_noncons, file = "models/species_models/brm_cons_noncons_refit.rds")


# chironomid stages -------------------------------------------------------

brm_chiro_stages <- brm(sample_mg_dm01 ~ s(date_no, by = fish_prey_stage) + (1|fish_prey_stage) + (1|site),
                           data = chiros_only,
                           family = Gamma(link = "log"),
                           prior = c(prior(normal(1, 2), class = "Intercept"),
                                     prior(normal(0, 5), class = "b"),
                                     prior(normal(0, 5), class = "sds"),
                                     prior(exponential(2), class = "sd"),
                                     prior(gamma(1, 0.1), class = "shape")),
                           iter = 2000, chains = 4)

# saveRDS(brm_chiro_stages, file = "models/species_models/brm_chiro_stages.rds")
