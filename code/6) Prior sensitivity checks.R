source("code/functions.r") # functions
source("code/make_data.r") # make data and load packages 

# load models
brm_aquatic_terr <- readRDS(file = "models/species_models/brm_aquatic_terr.rds")
brm_cons_noncons <- readRDS(file = "models/species_models/brm_cons_noncons.rds")
brm_chiro_stages <- readRDS(file = "models/species_models/brm_chiro_stages.rds")

# Prior predictive simulation ---------------------------------------------

# refit with priors only
brm_aquatic_terr_priorpost <- posterior_samples(brm_aquatic_terr) %>% select(contains(c("b_Intercept", "species"))) %>% as_tibble() %>% clean_names() %>% 
  select(!contains("r_site")) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  filter(iter <= 100) %>% 
  pivot_longer(-iter, names_to = "parameter", values_to = "posterior") %>% 
  group_by(parameter, iter) %>% 
  mutate(prior = case_when(grepl("b_inter", parameter) ~ rnorm(1, 1, 2),
                          TRUE ~ rnorm(1, 0, 5))) %>% 
  mutate(model = "brm_aquatic_terr") %>% 
  ungroup() %>% 
  mutate(parameter_no = as.integer(as.factor(parameter)))

brm_cons_noncons_priorpost <- posterior_samples(brm_cons_noncons) %>% select(contains(c("b_Intercept", "fish_prey"))) %>% as_tibble() %>% clean_names() %>% 
  select(!contains("r_site")) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  filter(iter <= 100) %>% 
  pivot_longer(-iter, names_to = "parameter", values_to = "posterior") %>% 
  group_by(parameter, iter) %>% 
  mutate(prior = case_when(grepl("b_inter", parameter) ~ rnorm(1, 1, 2),
                           TRUE ~ rnorm(1, 0, 5))) %>% 
  mutate(model = "brm_cons_noncons") %>% 
  ungroup() %>% 
  mutate(parameter_no = as.integer(as.factor(parameter)))

brm_chiro_stages_priorpost <- posterior_samples(brm_chiro_stages) %>% select(contains(c("b_Intercept", "fish_prey"))) %>% as_tibble() %>% clean_names() %>% 
  select(!contains("r_site")) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  filter(iter <= 100) %>% 
  pivot_longer(-iter, names_to = "parameter", values_to = "posterior") %>% 
  group_by(parameter, iter) %>% 
  mutate(prior = case_when(grepl("b_inter", parameter) ~ rnorm(1, 1, 2),
                           TRUE ~ rnorm(1, 0, 5))) %>% 
  mutate(model = "brm_chiro_stages") %>% 
  ungroup() %>% 
  mutate(parameter_no = as.integer(as.factor(parameter)))


all_priorpost <- bind_rows(brm_aquatic_terr_priorpost, brm_cons_noncons_priorpost, brm_chiro_stages_priorpost)

all_prior_post_summaries <- all_priorpost %>% 
  pivot_longer(cols = c(prior, posterior)) %>% 
  group_by(parameter, parameter_no, model, name) %>% 
  summarize(median = median(value),
            sd = sd(value)) %>% 
  mutate(name = fct_relevel(name, "prior"))


prior_post_plot <- all_prior_post_summaries %>% 
  ggplot(aes(x = parameter_no, y = median, ymin = median - sd, ymax = median + sd, color = name)) + 
  geom_errorbar(size = 0.5, alpha = 0.6) +
  facet_wrap(~model, scales = "free_x", ncol = 1) +
  scale_color_colorblind() +
  theme_bw()

saveRDS(prior_post_plot, file = "plots/prior_post_plot.rds")
ggsave(prior_post_plot, file = "plots/prior_post_plot.jpg", width = 5, height = 6, dpi = 400)




# prior predictive checks -----------------------

acheck <- pp_check(brm_aquatic_terr, type = "boxplot") + scale_y_log10() +
  labs(title = "a) Model = terrestrial prey and total prey") 

bcheck <- pp_check(brm_cons_noncons, type = "boxplot") + scale_y_log10() +
  labs(title = "b) Model = prop consumers") 

ccheck <- pp_check(brm_chiro_stages, type = "boxplot") + scale_y_log10() +
  labs(title = "c) Model = prop consumer chiros") 

ppc_plots <- plot_grid(acheck, bcheck, ccheck, ncol = 1) 




# prior sensitivity checks -----------------------

brm_aquatic_terr_wider2 <- readRDS(file = "models/species_models/brm_aquatic_terr_wider2.rds")
brm_cons_noncons_wider2 <- readRDS(file = "models/species_models/brm_cons_noncons_wider2.rds")
brm_chiro_stages_wider2 <- readRDS(file = "models/species_models/brm_chiro_stages_wider2.rds")

# brm_aquatic_terr_wider2 <- update(brm_aquatic_terr,
#                                prior = c(prior(normal(1, 4), class = "Intercept"),
#                                          prior(normal(0, 10), class = "b"),
#                                          prior(normal(0, 5), class = "sds"),
#                                          prior(exponential(1), class = "sd"),
#                                          prior(gamma(1, 0.1), class = "shape")),
#                                iter = 1000, chains = 1)
# 
# saveRDS(brm_aquatic_terr_wider2, file = "models/species_models/brm_aquatic_terr_wider2.rds")
# 
# 
# brm_cons_noncons_wider2 <- brm(sample_mg_dm01 ~ s(date_no, by = fish_prey_feeding) + (1|fish_prey_feeding) + (1|site),
#                                                         data = aquatic_only,
#                                                         family = Gamma(link = "log"),
#                                                         prior = c(prior(normal(1, 4), class = "Intercept"),
#                                                                   prior(normal(0, 10), class = "b"),
#                                                                   prior(normal(0, 5), class = "sds"),
#                                                                   prior(exponential(1), class = "sd"),
#                                                                   prior(gamma(1, 0.1), class = "shape")),
#                                                         iter = 1000, chains = 1)
# 
# saveRDS(brm_cons_noncons_wider2, file = "models/species_models/brm_cons_noncons_wider2.rds")
# 
# 
# brm_chiro_stages_wider2 <- brm(sample_mg_dm01 ~ s(date_no, by = fish_prey_stage) + (1|fish_prey_stage) + (1|site),
#                         data = chiros_only,
#                         family = Gamma(link = "log"),
#                         prior = c(prior(normal(1, 4), class = "Intercept"),
#                                   prior(normal(0, 10), class = "b"),
#                                   prior(normal(0, 5), class = "sds"),
#                                   prior(exponential(1), class = "sd"),
#                                   prior(gamma(1, 0.1), class = "shape")),
#                         iter = 1000, chains = 1)
# 
# saveRDS(brm_chiro_stages_wider2, file = "models/species_models/brm_chiro_stages_wider2.rds")


# extract aquatic_terrestrial posts
fit <- brm_aquatic_terr_wider2
list_of_data <- conditional_effects(fit, effects = "date_no:species_aquatic_terr",
                                    re_formula = NULL,
                                    resolution = 30)[[1]]
new_names <- list_of_data %>% 
  select(-names(fit$data[1])) %>% 
  select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% 
  remove_empty("cols")

posts_aquatic_terr_wider <- as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NULL, summary = F))) %>%
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

# extract consumer/non_consumer posts
fit <- brm_cons_noncons_wider2

list_of_data <- conditional_effects(fit, effects = "date_no:fish_prey_feeding",
                                    re_formula = NULL,
                                    resolution = 30)[[1]]
new_names <- list_of_data %>% 
  select(-names(fit$data[1])) %>% 
  select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% 
  remove_empty("cols")

posts_cons_noncons_fishspecies_wider <- as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NULL, summary = F))) %>%
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

# extract chironomid posts
fit <- brm_chiro_stages_wider2

list_of_data <- conditional_effects(fit, effects = "date_no:fish_prey_stage",
                                    re_formula = NULL,
                                    resolution = 30)[[1]]
new_names <- list_of_data %>% 
  select(-names(fit$data[1])) %>% 
  select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% 
  remove_empty("cols")

posts_chiros_fishspecies_wider <- as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NULL, summary = F))) %>%
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

rm(posts_chiros_wider_fishspecies)

# combine with main posteriors
posts_aquatic_terr <- readRDS(file = "posteriors/posts_aquatic_terr.rds") 
posts_cons_noncons_fishspecies <- readRDS(file = "posteriors/posts_cons_noncons_fishspecies.rds") 
posts_chiros_fishspecies <- readRDS(file = "posteriors/posts_chiros_fishspecies.rds") 

posts_sens_aquatic_terr <- posts_aquatic_terr %>% mutate(model = "posterior") %>% 
  bind_rows(posts_aquatic_terr_wider %>% mutate(model = "posterior_wider_priors")) %>% 
  pivot_longer(cols = c(total, prop_terrestrial)) %>% select(iter, date, fish_species, model, name, value)

posts_sens_cons <- posts_cons_noncons_fishspecies %>% mutate(model = "posterior") %>% 
  bind_rows(posts_cons_noncons_fishspecies_wider %>% mutate(model = "posterior_wider_priors")) %>% 
  pivot_longer(cols = prop_nonconsumer) %>% select(iter, date, fish_species, model, name, value)

posts_sens_cons_chiro <- posts_chiros_fishspecies %>% mutate(model = "posterior") %>% 
  bind_rows(posts_chiros_fishspecies_wider %>% mutate(model = "posterior_wider_priors")) %>% 
  rename(prop_nonconsumer_chiro = prop_nonconsumer) %>% 
  pivot_longer(cols = prop_nonconsumer_chiro) %>% select(iter, date, fish_species, model, name, value)


all_posts_compare <- bind_rows(posts_sens_aquatic_terr, posts_sens_cons, posts_sens_cons_chiro)


prior_sens_plot <- all_posts_compare %>% 
  # filter(name != "total") %>% 
  group_by(iter, name, model, fish_species) %>% 
  summarize(mean = mean(value)) %>% 
  ggplot(aes(x = mean, y = fish_species, fill = model)) +
  # geom_density(aes(y = ..scaled..), alpha = 0.5) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_colorblind() +
  facet_wrap(~name, ncol = 2, scales = "free_x") +
  theme_bw() +
  scale_x_log10() +
  theme(legend.position = "top") +
  NULL

saveRDS(prior_sens_plot, file = "plots/prior_sens_plot.rds")
ggsave(prior_sens_plot, file = "plots/prior_sens_plot.jpg", dpi = 500, width = 6, height = 7)
