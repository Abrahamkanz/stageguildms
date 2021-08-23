# Make data and load packages
source("code/make_data.r")

# Fit models
# proportion terrestrial prey --------------------------------------------------------
###!!!! Takes ~ 2.5 hours
brm_aquatic_terr <-  brm(sample_mg_dm01 ~ s(date_no, by = species_aquatic_terr) + (1|species_aquatic_terr) + (1|site),
                         data = aquatic_terr,
                         family = Gamma(link = "log"),
                         prior = c(prior(normal(1, 1), class = "Intercept"),
                                   prior(normal(0, 2), class = "b"),
                                   prior(normal(0, 2), class = "sds"),
                                   prior(exponential(2), class = "sd"),
                                   prior(gamma(1, 0.1), class = "shape")),
                         file = "models/species_models/brm_aquatic_terr.rds",
                         file_refit = "on_change")


# proportion of consumers --------------------------------------------------------

brm_cons_noncons <- update(readRDS("models/species_models/brm_cons_noncons.rds"),
                           prior = c(prior(normal(1, 1), class = "Intercept"),
                                     prior(normal(0, 2), class = "b"),
                                     prior(normal(0, 2), class = "sds"),
                                     prior(exponential(2), class = "sd"),
                                     prior(gamma(1, 0.1), class = "shape")),
                           cores = 4,
                           file = "models/species_models/brm_cons_noncons_refit.rds",
                           file_refit = "on_change")

# chironomid stages -------------------------------------------------------

brm_chiro_stages <- brm(sample_mg_dm01 ~ s(date_no, by = fish_prey_stage) + (1|fish_prey_stage) + (1|site),
                           data = chiros_only,
                           family = Gamma(link = "log"),
                           prior = c(prior(normal(0, 1), class = "Intercept"),
                                     prior(normal(0, 2), class = "b"),
                                     prior(normal(0, 2), class = "sds"),
                                     prior(exponential(2), class = "sd"),
                                     prior(gamma(1, 0.1), class = "shape")),
                           iter = 2000, chains = 4, cores = 4,
                        file = "models/species_models/brm_chiro_stages.rds",
                        file_refit = "on_change")

