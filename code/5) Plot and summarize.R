source("code/functions.r") # functions
source("code/make_data.r") # make data and load packages

# load posteriors and add average
posts_aquatic_terr <- readRDS(file = "posteriors/posts_aquatic_terr.rds") %>% 
  bind_rows(readRDS(file = "posteriors/posts_aquatic_terr.rds") %>% 
              group_by(iter, date) %>% summarize(prop_terrestrial = mean(prop_terrestrial),
                                                 total = mean(total),
                                                 terrestrial = mean(terrestrial),
                                                 aquatic = mean(aquatic)) %>% mutate(fish_species = "Average")) # add overall average


posts_cons_noncons_fishspecies <- readRDS(file = "models/posts_cons_noncons_fishspecies.rds") %>% 
  bind_rows(readRDS(file = "models/posts_cons_noncons_fishspecies.rds") %>% 
              group_by(iter, date) %>% summarize(prop_nonconsumer = mean(prop_nonconsumer),
                                                 total = mean(total),
                                                 consumer = mean(consumer),
                                                 non_consumer = mean(non_consumer)) %>% mutate(fish_species = "Average")) # add overall average

posts_chiros_fishspecies <- readRDS(file = "models/posts_chiros_fishspecies.rds") %>% 
  bind_rows(readRDS(file = "models/posts_chiros_fishspecies.rds") %>% 
              group_by(iter, date) %>% summarize(prop_nonconsumer = mean(prop_nonconsumer),
                                                 total = mean(total),
                                                 a = mean(a),
                                                 l = mean(l),
                                                 p = mean(p)) %>% mutate(fish_species = "Average"))

# data to sort by
number_of_dates <- aquatic_terr %>% 
  distinct(date, fish_species) %>% 
  group_by(fish_species) %>% 
  tally() %>% 
  arrange(-n) %>% 
  bind_rows(tibble(fish_species = "Average", n = 606))%>% 
  left_join(color_median)

color_median <- posts_cons_noncons_fishspecies %>% 
  group_by(fish_species) %>% 
  summarize(median = median(prop_nonconsumer)) %>% 
  mutate(median = case_when(fish_species == "Average" ~ 0, T ~ median)) # places average at the bottom

# data to plot
prop_consumer_data_plot <- aquatic_only %>% 
  select(prey_feeding, fish_species, sample_mg_dm01, date, sample_id) %>% 
  pivot_wider(names_from = prey_feeding, values_from = sample_mg_dm01) %>% 
  mutate(total = consumer + non_consumer,
         prop_nonconsumer = non_consumer/total) %>% 
  bind_rows(aquatic_only %>% 
              select(prey_feeding, fish_species, sample_mg_dm01, date, sample_id) %>% 
              pivot_wider(names_from = prey_feeding, values_from = sample_mg_dm01) %>% 
              mutate(total = consumer + non_consumer,
                     prop_nonconsumer = non_consumer/total) %>% mutate(fish_species = "Average")) %>% 
  left_join(color_median) %>%
  mutate(date = ymd(date)) %>% 
  left_join(number_of_dates) %>% 
  mutate(fish_species = fct_relevel(fish_species, "Average"))

total_and_propterr_toplot <- aquatic_terr %>% 
  select(prey_ecosystem, fish_species, sample_mg_dm01, date, sample_id) %>% 
  mutate(sample_mg_dm01 = sample_mg_dm01 - 0.01) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = sample_mg_dm01) %>% 
  mutate(total = aquatic + terrestrial + unknown,
         prop_terrestrial = terrestrial/total) %>% 
  bind_rows(aquatic_terr %>% 
              select(prey_ecosystem, fish_species, sample_mg_dm01, date, sample_id) %>% 
              mutate(sample_mg_dm01 = sample_mg_dm01 - 0.01) %>% 
              pivot_wider(names_from = prey_ecosystem, values_from = sample_mg_dm01) %>% 
              mutate(total = aquatic + terrestrial + unknown,
                     prop_terrestrial = terrestrial/total) %>% mutate(fish_species = "Average")) %>% 
  left_join(color_median) %>%
  mutate(date = ymd(date)) %>% 
  left_join(number_of_dates) %>% 
  mutate(fish_species = fct_relevel(fish_species, "Average"))


chiros_toplot <- chiros_only %>% 
  select(prey_stage, fish_species, sample_mg_dm01, date, sample_id) %>% 
  mutate(sample_mg_dm01 = sample_mg_dm01 - 0.01) %>% 
  pivot_wider(names_from = prey_stage, values_from = sample_mg_dm01) %>% 
  mutate(total = a + l + p,
         prop_nonconsumer = (a + p)/total) %>% 
  bind_rows(chiros_only %>% 
              select(prey_stage, fish_species, sample_mg_dm01, date, sample_id) %>% 
              mutate(sample_mg_dm01 = sample_mg_dm01 - 0.01) %>% 
              pivot_wider(names_from = prey_stage, values_from = sample_mg_dm01) %>% 
              mutate(total = a + l + p,
                     prop_nonconsumer = (a + p)/total) %>% mutate(fish_species = "Average")) %>% 
  left_join(color_median) %>%
  mutate(date = ymd(date)) %>% 
  left_join(number_of_dates) %>% 
  mutate(fish_species = fct_relevel(fish_species, "Average"))


# Plot Species Means --------------------------------------------------------------

box_cons <- posts_cons_noncons_fishspecies %>% 
  group_by(iter, fish_species) %>% 
  summarize(prop_nonconsumer = mean(prop_nonconsumer)) %>% 
  left_join(color_median) %>% ungroup() %>% 
  ggplot(aes(y = reorder(fish_species, median) , x = prop_nonconsumer, fill = stat(x))) + 
  geom_point(data = prop_consumer_data_plot, shape = "|", size = 0.8) +
  scale_fill_viridis_c(name = "Proportion", option = "C") +
  stat_density_ridges(geom = "density_ridges_gradient", quantile_lines = T, quantiles = 2) +
  labs(y = "Fish species or family",
       x = "Proportion of non-consumer\naquatic prey",
       subtitle = "c)") +
  theme_classic() +
  theme(axis.title = element_text(size = 9)) + 
  xlim(0, 1) +
  guides(fill = "none") +
  NULL
  
box_total <- posts_aquatic_terr %>% 
  group_by(iter, fish_species) %>% 
  summarize(total = mean(total)) %>% 
  left_join(color_median) %>% 
  ggplot(aes(y = reorder(fish_species, median), x = total, fill = stat(x))) + 
  geom_point(data = total_and_propterr_toplot, shape = "|", size = 0.8) +
  scale_fill_viridis_c(name = "Total", option = "C") +
  stat_density_ridges(geom = "density_ridges_gradient", quantile_lines = T, quantiles = 2) +
  labs(y = "Fish species or family",
       x = "Total prey mass\nper stomach (mgDM)",
       subtitle = "a)") +
  theme_classic() +
  theme(axis.title = element_text(size = 9)) + 
  guides(fill = "none") +
  scale_x_log10() +
  NULL

box_terr <- posts_aquatic_terr %>% 
  group_by(iter, fish_species) %>% 
  summarize(prop_terrestrial = mean(prop_terrestrial)) %>% 
  left_join(color_median) %>% 
  ggplot(aes(y = reorder(fish_species, median), x = prop_terrestrial, fill = stat(x))) + 
  geom_point(data = total_and_propterr_toplot, shape = "|", size = 0.8) +
  scale_fill_viridis_c(option = "C") +
  stat_density_ridges(geom = "density_ridges_gradient", quantile_lines = T, quantiles = 2) +
  labs(y = "Fish species or family",
       x = "Proportion of\nterrestrial prey",
       subtitle = "b)") +
  theme_classic() +
  theme(axis.title = element_text(size = 9)) + 
  guides(fill = "none") +
  xlim(0,1)

box_chiros <- posts_chiros_fishspecies %>% 
  group_by(iter, fish_species) %>% 
  summarize(prop_nonconsumer = mean(prop_nonconsumer)) %>% 
  left_join(color_median) %>% 
  ggplot(aes(y = reorder(fish_species, median), x = prop_nonconsumer, fill = stat(x))) + 
  geom_point(data = chiros_toplot, shape = "|", size = 0.8) +
  scale_fill_viridis_c(option = "C") +
  stat_density_ridges(geom = "density_ridges_gradient", quantile_lines = T, quantiles = 2) +
  labs(y = "Fish species or family",
       x = "Proportion of chironomids\neaten as pupae or adults",
       subtitle = "d)") +
  theme_classic() +
  theme(axis.title = element_text(size = 9)) + 
  guides(fill = "none") +
  xlim(0,1)


fish_species_averages <- box_total + 
  box_terr + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  box_cons + 
  box_chiros + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) 

saveRDS(fish_species_averages, file = "plots/fish_species_averages.rds")
ggsave(fish_species_averages, file = "plots/fish_species_averages.jpg", dpi = 500, width = 5.5, height = 7)


# Plot Time Series --------------------------------------------------------
all_posts <- posts_aquatic_terr %>% 
  pivot_longer(cols = c(total, prop_terrestrial)) %>% 
  select(iter, date, fish_species, name, value) %>% 
  bind_rows(posts_cons_noncons_fishspecies %>% 
  rename(value = prop_nonconsumer) %>% 
  mutate(name = "prop_nonconsumer") %>% 
    select(iter, date, fish_species, name, value)) %>% 
  bind_rows(posts_chiros_fishspecies %>% 
              rename(value = prop_nonconsumer) %>% 
              mutate(name = "prop_nonconsumer_chiros") %>% 
              select(iter, date, fish_species, name, value)) %>% 
  left_join(number_of_dates) %>% 
  mutate(fish_species = fct_relevel(fish_species, "Average"))

terr_total_data <- aquatic_terr2 %>% 
  bind_rows(aquatic_terr2 %>% mutate(fish_species = "Average")) %>% 
  select(date, fish_species, prop_terr, total) %>% 
  rename(prop_terrestrial = prop_terr) %>% 
  pivot_longer(cols = c(-date, -fish_species)) %>% 
  left_join(number_of_dates) %>% 
  mutate(fish_species = fct_relevel(fish_species, "Average"))

all_posts_summaries <- all_posts %>% 
  group_by(fish_species, date, name, n) %>% 
  summarize(median_y = median(value),
            low90 = quantile(value, probs = 0.125),
            high90 = quantile(value, probs = 1-0.125),
            low50 = quantile(value, probs = 0.25),
            high50 = quantile(value, probs = 1-0.25)) 
  

time_ser_noncons <- all_posts_summaries %>% 
  filter(name == "prop_nonconsumer") %>% 
  filter(n >= 8 &
           fish_species != "Average") %>% 
  mutate(panels = case_when(fish_species == "Average" ~ "Average", TRUE ~ "Species")) %>% 
  ggplot(aes(x = date, color = fish_species, fill = fish_species)) + 
  geom_line(aes(y = median_y)) +
  geom_ribbon(aes(ymin = low90, ymax = high90), alpha = 0.4) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.4) +
  # facet_grid(panels ~ .) + 
  geom_point(data = prop_consumer_data_plot %>%
               filter(n >= 8 &
                        fish_species != "Average") %>% 
               mutate(panels = case_when(fish_species == "Average" ~ "Average", TRUE ~ "Species")) , aes(y = prop_nonconsumer),
             position = position_jitter(width = 0.5),
             size = 0.7, alpha = 0.4) + 
  labs(y = "Proportion of prey mass",
       subtitle = "c) Proportion of non-consumer aquatic prey",
       fill = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(size = 10)) + 
  scale_fill_viridis_d(option = "B") +
  scale_color_viridis_d(option = "B")

time_ser_aquatterr <- all_posts_summaries %>% 
  filter(name == "prop_terrestrial") %>% 
  filter(n >= 8 &
           fish_species != "Average") %>% 
  mutate(panels = case_when(fish_species == "Average" ~ "Average", TRUE ~ "Species")) %>% 
  ggplot(aes(x = date, color = fish_species, fill = fish_species)) + 
  geom_line(aes(y = median_y)) +
  geom_ribbon(aes(ymin = low90, ymax = high90), alpha = 0.4) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.4) +
  # facet_grid(panels ~ .) + 
  geom_point(data = terr_total_data %>% 
             filter(name == "prop_terrestrial") %>% 
               filter(n >= 8 &
                        fish_species != "Average") %>% 
               mutate(panels = case_when(fish_species == "Average" ~ "Average", TRUE ~ "Species")) , aes(y = value),
             position = position_jitter(width = 0.5),
             size = 0.7, alpha = 0.4) + 
  labs(y = "Proportion of prey mass",
       subtitle = "b) Proportion of terrestrial prey") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(size = 10)) +  
  scale_fill_viridis_d(option = "B") +
  scale_color_viridis_d(option = "B")

time_ser_total <- all_posts_summaries %>% 
  filter(name == "total") %>% 
  filter(n >= 8 &
           fish_species != "Average") %>% 
  mutate(panels = case_when(fish_species == "Average" ~ "Average", TRUE ~ "Species")) %>% 
  ggplot(aes(x = date, fill = fish_species, color = fish_species)) + 
  geom_point(data = terr_total_data %>%
               filter(name != "prop_terrestrial") %>%
               filter(n >= 8 &
                        fish_species != "Average") %>% 
               mutate(panels = case_when(fish_species == "Average" ~ "Average", TRUE ~ "Species")), aes(y = value),
             position = position_jitter(width = 0.5),
             size = 0.7, alpha = 0.4) +
  geom_line(aes(y = median_y)) +
  geom_ribbon(aes(ymin = low90, ymax = high90), alpha = 0.4) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.4) +
  # facet_grid(. ~ panels) + 
  scale_y_log10() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(size = 10)) + 
  labs(y = "Total prey mass",
       subtitle = "a) Total prey mass per stomach (mgDM)") +
  scale_fill_viridis_d(option = "B") +
  scale_color_viridis_d(option = "B")

time_ser_chiro <- all_posts_summaries %>% 
  filter(name == "prop_nonconsumer_chiros") %>% 
  filter(n >= 8 &
           fish_species != "Average") %>% 
  mutate(panels = case_when(fish_species == "Average" ~ "Average", TRUE ~ "Species")) %>% 
  ggplot(aes(x = date, fill = fish_species, color = fish_species)) + 
  geom_point(data = chiros_toplot %>% 
               filter(n >= 8 & fish_species != "Average") %>% 
               mutate(panels = case_when(fish_species == "Average" ~ "Average", TRUE ~ "Species")), aes(y = prop_nonconsumer),
             position = position_jitter(width = 0.5),
             size = 0.7, alpha = 0.4) +
  geom_line(aes(y = median_y)) +
  geom_ribbon(aes(ymin = low90, ymax = high90), alpha = 0.4) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.4) +
  # facet_grid(. ~ panels) + 
  # scale_y_log10() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(size = 10)) + 
  labs(y = "Proportion of prey mass",
       subtitle = "d) Proportion of chironomids eaten as pupae or adults") +
  scale_fill_viridis_d(option = "B") +
  scale_color_viridis_d(option = "B")

legend_time <- get_legend(time_ser_noncons)
temp_time <- plot_grid(time_ser_total + theme(strip.text = element_blank()) + guides(fill = "none", color = "none"),
          time_ser_aquatterr + theme(strip.text = element_blank()) + scale_y_continuous(breaks = c(0, 0.5, 1)) + guides(fill = "none", color = "none"),
          time_ser_noncons + guides(fill = "none", color = "none") + scale_y_continuous(breaks = c(0, 0.5, 1)), 
            time_ser_chiro + guides(fill = "none", color = "none") + scale_y_continuous(breaks = c(0, 0.5, 1)),
          ncol = 1, align = "v")

plot_time_series <- plot_grid(temp_time, legend_time, ncol = 2, rel_widths = c(.8, 0.4))

saveRDS(plot_time_series, file = "plots/plot_time_series.rds")
ggsave(plot_time_series, file = "plots/plot_time_series.jpg", dpi = 500, width = 5.5, height = 8)


# Correlations ------------------------------------------------------------

all_medians <- all_posts %>% 
  group_by(fish_species, name) %>% 
  summarize(median = median(value)) %>% 
  pivot_wider(names_from = "name", values_from = "median")

a <- ggplot(all_medians, aes(x = total, y = prop_nonconsumer)) + 
  geom_point() +
  stat_cor(method = "pearson", aes(label = ..r.label..)) +
  theme_bw() +
  labs(x = "Total prey mass (mgDM)",
       y = "Proportion of\nnon-consumer aquatic prey",
       subtitle = "a)") +
  NULL

b <- ggplot(all_medians, aes(x = total, y = prop_nonconsumer_chiros)) + 
  geom_point() +
  stat_cor(method = "pearson", aes(label = ..r.label..)) +
  theme_bw() +
  labs(x = "Total prey mass (mgDM)",
       y = "Proportion of\nnon-consumer chironomids",
       subtitle = "b)") +
  NULL

c <- ggplot(all_medians, aes(x = total, y = prop_terrestrial)) + 
  geom_point() +
  stat_cor(method = "pearson", aes(label = ..r.label..)) +
  theme_bw() +
  labs(x = "Total prey mass (mgDM)",
       y = "Proportion of\nterrestrial prey",
       subtitle = "c)") +
  NULL

d <- ggplot(all_medians, aes(x = prop_nonconsumer_chiros, y = prop_terrestrial)) + 
  geom_point() +
  stat_cor(method = "pearson", aes(label = ..r.label..)) +
  theme_bw() +
  labs(x = "Proportion of\nnon-consumer chironomids",
       y = "Proportion of\nterrestrial prey",
       subtitle = "d)") +
  NULL

e <- ggplot(all_medians, aes(x = prop_nonconsumer_chiros, y = prop_nonconsumer)) + 
  geom_point() +
  stat_cor(method = "pearson", aes(label = ..r.label..)) +
  theme_bw() +
  labs(x = "Proportion of\nnon-consumer chironomids",
       y = "Proportion of\nnon-consumer aquatic prey",
       subtitle = "e)") +
  NULL

f <- ggplot(all_medians, aes(x = prop_terrestrial, y = prop_nonconsumer)) + 
  geom_point() +
  stat_cor(method = "pearson", aes(label = ..r.label..)) +
  theme_bw() +
  labs(y = "Proportion of\nnon-consumer aquatic prey",
       x = "Proportion of\nterrestrial prey",
       subtitle = "f)") +
  NULL

correlation_matrix <- plot_grid(a, b, c, d, e, f, ncol = 2, align = "v") +
  theme(axis.title = element_text(size = 10))

ggsave(correlation_matrix, file = "plots/correlation_matrix.jpg", width = 5.5, height = 8, dpi = 500)
saveRDS(correlation_matrix, file = "plots/correlation_matrix.rds")



# Summarize ---------------------------------------------------------------

posts_aquatic_terr %>% 
  filter(fish_species != "Average") %>% 
  pivot_longer(cols = c(total, prop_terrestrial)) %>% 
  group_by(name, fish_species) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  print(n = Inf)


posts_cons_noncons_fishspecies %>% 
  filter(fish_species != "Average") %>% 
  pivot_longer(cols = c(prop_nonconsumer)) %>% 
  group_by(name) %>% 
  summarize(median = median(value),
            sd = sd(value),
            low = quantile(value, probs = 0.125),
            high = quantile(value, probs = 0.875)) 

