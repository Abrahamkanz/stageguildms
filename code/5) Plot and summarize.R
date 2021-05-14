library(brms)
library(tidyverse)
library(ggridges)
library(ggthemes)
library(viridis)
library(ggpubr)
library(lubridate)
library(cowplot)
library(tidybayes)
library(scales)

#read data
guild_diet_multi_drymass <- readRDS(file = "data/guild_diet_multi_drymass.rds")
fish_guilds <- guild_diet_multi_drymass %>% distinct(fish_species, fish_guild)
emerge_v_diet <- readRDS(file = "data/emerge_v_diet.rds")


# load posteriors
posts_totalmasspermm <- readRDS("models/posts_totalmasspermm.rds") %>% mutate(date = ymd(date))
posts_aquatic_terr <- readRDS("models/posts_aquatic_terr.rds")
posts_aquatic_terr_guild <- readRDS("models/posts_aquatic_terr_guild.rds")
posts_feeding <- readRDS("models/posts_feeding.rds") %>% mutate(date = ymd(date),julian = julian(date))
posts_mg_chiro <- readRDS("models/posts_mg_chiro.rds") %>% mutate(date = ymd(date),julian = julian(date))
posts_mg_chirononchiro <- readRDS("models/posts_mg_chirononchiro.rds") %>% mutate(date = ymd(date))
posts_feeding_fishspecies <- readRDS("models/posts_feeding_fishspecies.rds") 
posts_chirononchiro_fishspecies <- readRDS("models/posts_chirononchiro_fishspecies.rds") 
posts_totalchiro_fishspecies <- readRDS("models/posts_totalchiro_fishspecies.rds")
post_emerge_means <- readRDS(file = "models/post_merge_means.rds")
posts_emerge_long <- readRDS("models/emerge_posts_long.rds")
posts_emergechiro_long <- readRDS("models/emergechiro_posts_long.rds")
posts_emergechiroonly_long <- readRDS("models/emergechiroonly_posts_long.rds")

# Plots by guild ------------------

posts_aquatic_terr_guild %>% 
  group_by(fish_guild, prey_ecosystem, iter) %>%
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown,
         prop_terrestrial = terrestrial/total) %>% 
  ggplot(aes(x = prop_terrestrial, y = ..scaled.., color = fish_guild)) + 
  geom_density()


# Proportion of aquatics that are non-consumers

posts_feeding %>% 
  group_by(fish_guild, prey_feeding, iter) %>% 
  summarize(value = mean(value)) %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(total = consumer + non_consumer,
         prop_nonfeeding = non_consumer/total) %>% 
  ggplot(aes(x = prop_nonfeeding, y = ..scaled.., fill = fish_guild)) + 
  geom_density()





# Proportion terrestrial prey by fish guild ------------------

posts_aquatic_terr_guild_wide <- posts_aquatic_terr_guild %>% 
  group_by(prey_ecosystem, iter, date, fish_guild) %>% 
  summarize(value = mean(value)) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown,
         prop_terrestrial = terrestrial/total) 

posts_aquatic_terr_guild_overtime <- posts_aquatic_terr_guild_wide %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean = mean(prop_terrestrial)) %>% 
  mutate(response = "Proportion of terrestrial prey")

posts_aquatic_terr_guild_overtime %>% 
  ggplot(aes(x = mean , fill = fish_guild, y = ..scaled..)) + 
  geom_density() +
  scale_fill_colorblind() + 
  labs(x = "Proportion of terrestrial prey in diet",
       fill = "Fish Domain") +
  # scale_y_log10() +
  theme_classic() +
  theme(axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()) 


# Proportion of life stages -----------------------------------------------

#wrangle posteriors
posts_feeding_guilds <- posts_feeding %>% filter(iter <= 1000) %>%
  group_by(prey_feeding, fish_guild, date, iter) %>% 
  mutate(value = mean(value)) %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(prop_nonfeeding_mg = non_consumer/(consumer + non_consumer))

posts_mg_chiro_guilds <- posts_mg_chiro %>% filter(iter <= 1000) %>% 
  pivot_wider(names_from = prey_stage, values_from = value) %>% 
  mutate(non_feeding_stages = a + p,
         total = a + l + p,
         prop_nonfeeding_mg = non_feeding_stages/total)


# Plot timeseries --------------------------------------------------------
#plot total prey per mm
total_prey_permm <- posts_totalmasspermm %>% 
  filter(iter <= 500) %>% 
  mutate(fish_guild = fct_relevel(fish_guild, "benthic")) %>% 
  ggplot(aes(x = date, y = value, fill = fish_guild)) +
  geom_line(aes(group = interaction(iter, fish_guild), color = fish_guild), alpha = 0.01, 
            position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(group = interaction(date, fish_guild)),
               outlier.shape = NA, 
               position = position_dodge(width = 0.5)) +
  # geom_violin(aes(group = interaction(date, fish_guild))) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(y = "Prey mass (mgDM/mm)",
       x = "Collection Date",
       subtitle = "a) Total prey dry mass in fish diets",
       fill = "Fish Domain") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  # coord_cartesian(ylim = c(0, 20)) +
  theme_classic() +
  annotation_logticks(sides = "l") + 
  theme(legend.position = "right") +
  guides(color = F) +
  NULL


#plot total prey 
total_prey <- posts_aquatic_terr_guild %>%
  group_by(date, iter, fish_guild) %>% 
  summarize(value = sum(value)) %>% 
  filter(iter <= 500) %>% 
  mutate(fish_guild = fct_relevel(fish_guild, "benthic")) %>% 
  ggplot(aes(x = as.Date(date), y = value, fill = fish_guild)) +
  geom_line(aes(group = interaction(iter, fish_guild), color = fish_guild), alpha = 0.01, 
            position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(group = interaction(date, fish_guild)),
               outlier.shape = NA, 
               position = position_dodge(width = 0.5)) +
  # geom_violin(aes(group = interaction(date, fish_guild))) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(y = "Prey mass per stomach (mgDM)",
       x = "Collection Date",
       subtitle = "a) Total prey dry mass in fish diets",
       fill = "Fish Domain") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  # coord_cartesian(ylim = c(0, 20)) +
  theme_classic() +
  annotation_logticks(sides = "l") + 
  theme(legend.position = "right") +
  guides(color = F) +
  NULL


# plot proportion nonfeeding overall
prop_feeding <- posts_feeding_guilds %>% 
  filter(iter <= 500) %>% 
  mutate(fish_guild = fct_relevel(fish_guild, "benthic")) %>% 
  ggplot(aes(x = as.Date(date), y = prop_nonfeeding_mg, fill = fish_guild)) +
  geom_line(aes(group = interaction(iter, fish_guild), color = fish_guild), alpha = 0.01, 
            position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(group = interaction(date, fish_guild)),
               outlier.shape = NA, 
               position = position_dodge(width = 0.5)) +
  # geom_violin(aes(group = interaction(date, fish_guild))) +
  
  labs(y = "Proportion of diet",
       x = "Collection Date",
       subtitle = "c) Proportion of non-consumer aquatic prey in diet",
       fill = "Fish Domain") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  # scale_y_log10() +
  theme_classic() +
  annotation_logticks(sides = "l") + 
  theme(legend.position = "right") +
  guides(color = F) +
  NULL

# plot total chiro mg
total_chiros <- posts_mg_chirononchiro %>% 
  filter(chiro == "chiro") %>% 
  filter(iter <= 500) %>%
  mutate(fish_guild = fct_relevel(fish_guild, "benthic")) %>% 
  ggplot(aes(x = as.Date(date), y = value, fill = fish_guild)) +
  geom_line(aes(group = interaction(iter, fish_guild), color = fish_guild), alpha = 0.01, 
            position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(group = interaction(date, fish_guild)),
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) +
  # geom_violin(aes(group = interaction(date, fish_guild))) +
  # scale_y_log10() +
  labs(y = "Chironomid mass per stomach (mgDM)",
       x = "Collection Date",
       subtitle = "b) Chironomid dry mass in fish diets",
       fill = "Fish Domain") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_classic() +
  annotation_logticks(sides = "l") + 
  theme(legend.position = "right") +
  guides(color = F) +
  NULL 
  

# plot proportion life stages for chiros only
prop_lifestages <- posts_mg_chiro_guilds %>% 
  filter(iter <= 500) %>%
  mutate(fish_guild = fct_relevel(fish_guild, "benthic")) %>% 
  ggplot(aes(x = as.Date(date), y = prop_nonfeeding_mg, fill = fish_guild)) +
  geom_line(aes(group = interaction(iter, fish_guild), color = fish_guild), alpha = 0.01, 
            position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(group = interaction(date, fish_guild)),
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) +
  # geom_violin(aes(group = interaction(date, fish_guild))) +
  # scale_y_log10() +
  labs(y = "Proportion of diet",
       x = "Collection Date",
       subtitle = "d) Proportion of chironomids eaten as pupae or adults",
       fill = "Fish Domain") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  # scale_y_log10(
  #   breaks = scales::trans_breaks("log10", function(x) 10^x),
  #   labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_classic() +
  annotation_logticks(sides = "l") + 
  theme(legend.position = "right") +
  guides(color = F) +
  NULL

# Combine timeseries plots -----------------------------------------------------------

legend_timeseries <- get_legend(total_prey + theme(legend.position = "top"))

plot_timeseriesnolegend <- plot_grid(total_prey + guides(fill = F, color = F),
                             total_chiros + guides(fill = F, color = F),
                             prop_feeding + guides(fill = F, color = F),
                             prop_lifestages + guides(fill = F, color = F),
                             ncol = 2, align = "h")


plot_timeseries <- plot_grid(legend_timeseries, plot_timeseriesnolegend, ncol = 1,
                             rel_heights = c(0.2, 1))

saveRDS(plot_timeseries, file = "plots/plot_timeseries.rds")
ggsave(plot_timeseries, file = "plots/plot_timeseries.jpg", dpi = 400, width = 9, height = 7)


# Plot averages across time -----------------------------------------------

posts_total_preymass_averaged_over_time <- posts_mg_chirononchiro %>% 
  group_by(fish_guild, iter, date) %>% 
  summarize(total_mg = sum(value)) %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean = mean(total_mg)) %>% 
  mutate(response = "Prey Mass")  

posts_feeding_guilds_overtime <- posts_feeding_guilds %>% 
              group_by(fish_guild, iter) %>% 
              summarize(mean = mean(prop_nonfeeding_mg)) %>% 
              mutate(response = "Proportion of non-consumer aquatic prey")



# Proportion of chironomids -----------------------------------------------

#wrangle posteriors

posts_mg_chirononchiro_guilds <- posts_mg_chirononchiro %>% filter(iter <= 1000) %>% 
  pivot_wider(names_from = chiro, values_from = value) %>% 
  mutate(prop_chiro = chiro/(chiro + not_chiro)) %>% 
  mutate(fish_guild = str_to_sentence(fish_guild),
         fish_guild = fct_relevel(fish_guild, "Benthic", "Intermediate", "Surface"))

# plot proportion chiro
prop_chiros <- posts_mg_chirononchiro_guilds %>% 
  filter(iter <= 500) %>% 
  ggplot(aes(x = date, y = prop_chiro, fill = fish_guild)) +
  geom_line(aes(group = interaction(iter, fish_guild), color = fish_guild), alpha = 0.01) +
  geom_boxplot(aes(group = interaction(date, fish_guild)),
               outlier.shape = NA) +
  # geom_violin(aes(group = interaction(date, fish_guild))) +
  # scale_y_log10() +
  labs(y = "Proportion",
       subtitle = "b) Proportion of chironomids in diet (dry mass)",
       x = "Collection Date") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_classic() +
  annotation_logticks(sides = "l") + 
  theme(legend.position = "right") +
  guides(color = F) +
  NULL



#plot total prey 
posts_feeding %>% 
  filter(iter <= 500) %>% 
  group_by(fish_guild, date, iter) %>% 
  summarize(mean = mean(value)) %>% 
  ggplot(aes(x = date, y = mean, fill = fish_guild)) +
  geom_line(aes(group = interaction(fish_guild, iter), color = fish_guild), alpha = 0.01) +
  geom_boxplot(aes(group = interaction(fish_guild, date))) +
  scale_y_log10()


#plot across dates
posts_feeding_guilds %>% 
  filter(fish_guild != "All fish") %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean = mean(prop_nonfeeding_mg)) %>% 
  ggplot(aes(x = mean, fill = fish_guild)) + 
  geom_density() +
  scale_fill_colorblind() +
  coord_cartesian(xlim = c(0, 1)) +
  theme_pubclean()

posts_mg_chiro_everything %>% 
  filter(fish_guild != "All fish") %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean = mean(prop_nonfeeding_mg)) %>% 
  ggplot(aes(x = mean, fill = fish_guild)) + 
  geom_density() +
  scale_fill_colorblind() +
  coord_cartesian(xlim = c(0, 1)) +
  theme_pubclean()


posts_mg_chirononchiro_guilds %>% 
  filter(fish_guild != "All fish") %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean_prop_across_dates = mean(prop_chiro)) %>% 
  ggplot(aes(x = mean_prop_across_dates, fill = fish_guild)) + 
  geom_density(alpha = 0.6) +
  scale_fill_colorblind() +
  coord_cartesian(xlim = c(0, 1)) +
  theme_pubclean()


#plot across dates and guild
posts_feeding_guilds %>% 
  filter(fish_guild != "All fish") %>% 
  group_by(iter) %>% 
  summarize(mean = mean(prop_nonfeeding_mg)) %>% 
  ggplot(aes(x = mean)) + 
  geom_density() +
  # scale_fill_colorblind() +
  coord_cartesian(xlim = c(0, 1)) +
  theme_pubclean()

posts_mg_chiro_everything %>% 
  filter(fish_guild != "All fish") %>% 
  group_by(iter) %>% 
  summarize(mean = mean(prop_nonfeeding_mg)) %>% 
  ggplot(aes(x = mean)) + 
  geom_density() +
  # scale_fill_colorblind() +
  coord_cartesian(xlim = c(0, 1)) +
  theme_pubclean()


posts_mg_chirononchiro_guilds %>% 
  filter(fish_guild != "All fish") %>% 
  group_by(iter) %>% 
  summarize(mean_prop_across_dates = mean(prop_chiro)) %>% 
  ggplot(aes(x = mean_prop_across_dates)) + 
  geom_density(alpha = 0.6) +
  # scale_fill_colorblind() +
  coord_cartesian(xlim = c(0, 1)) +
  theme_pubclean()




# Plot for individual fish species ----------------------------------------

posts_total_preymass_averaged_over_time <- posts_mg_chirononchiro %>% 
  group_by(fish_guild, iter, date) %>% 
  summarize(total_mg = sum(value)) %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean = mean(total_mg)) %>% 
  mutate(response = "Prey Mass")  

posts_feeding_guilds_overtime <- posts_feeding_guilds %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean = mean(prop_nonfeeding_mg)) %>% 
  mutate(response = "Proportion of non-consumer aquatic prey")


#medians to sort by
species_medians = posts_feeding_fishspecies %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(prop_nonfeeding = non_consumer/(consumer + non_consumer)) %>% 
  ungroup() %>% 
  group_by(fish_species) %>% 
  summarize(median = median(prop_nonfeeding))

# total prey mass 
total_species <- posts_aquatic_terr %>% 
  filter(iter <= 1000) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown) %>% 
  left_join(species_medians) %>% 
  left_join(fish_guilds) %>% 
  ggplot(aes(y = reorder(fish_species, median), x = total, fill = fish_guild)) +
  geom_boxplot(aes(group = fish_species), outlier.shape = NA) +
  # coord_flip(ylim = c(NA, 150)) +
  scale_fill_colorblind() + 
  labs(x = "Total prey mass per fish stomach (mg DM)",
       y = "Fish species or family",
       fill = "Fish Domain",
       subtitle = "a)") +
  scale_x_log10() +
  coord_cartesian(xlim = c(0.5, 250)) +
  theme_classic()

a_overtime2 <- posts_total_preymass_averaged_over_time %>% 
  mutate(fish_guild = fct_relevel(fish_guild, "benthic")) %>% 
  ggplot(aes(x = mean, y = ..scaled.., fill = fish_guild)) + 
  geom_density(alpha = 0.7) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  scale_x_log10() +
  scale_y_continuous() +
  theme_classic() + 
  theme(legend.position = "right") +
  guides(color = F) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Total prey mass\nper stomach (mgDM)",
       fill = "Fish Domain") +
  coord_cartesian(xlim = c(0.5, 250)) +
  NULL

plot_prey_mass_species <- plot_grid(total_species + guides(fill = F, color = F) +
            theme(axis.title.x = element_blank()), 
          a_overtime2 + guides(fill = F), 
          ncol = 1, align = "v", rel_heights = c(1, 0.5))


#prop_terrestrial
prop_terr <- posts_aquatic_terr %>% 
  filter(iter <= 1000) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown,
         total = terrestrial/total) %>% 
  left_join(species_medians) %>% 
  left_join(fish_guilds) %>% 
  ggplot(aes(y = reorder(fish_species, median), x = total, fill = fish_guild)) +
  geom_boxplot(aes(group = fish_species), outlier.shape = NA) +
  # coord_flip(ylim = c(NA, 150)) +
  scale_fill_colorblind() + 
  labs(x = "Proportion of terrestrial prey",
       y = "Fish species or family",
       fill = "Fish Domain",
       subtitle = "b)") +
  # scale_x_log10() +
  # coord_cartesian(xlim = c(0.5, 250)) +
  theme_classic() +
  xlim(0, 0.75) +
  NULL

posts_aquatic_terr_guild_overtime <- posts_aquatic_terr_guild %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = terrestrial/(aquatic + terrestrial + unknown)) %>% 
  group_by(fish_guild, iter) %>% 
  summarize(total = mean(total))


c_overtime2 <- posts_aquatic_terr_guild_overtime %>% 
  ggplot(aes(x = total, y = ..scaled.., fill = fish_guild)) + 
  geom_density(alpha = 0.7) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  # scale_x_log10() +
  scale_y_continuous() +
  theme_classic() + 
  theme(legend.position = "right") +
  guides(color = F) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Proportion of\nterrestrial prey",
       fill = "Fish Domain") +
  xlim(0, 0.75) +
  NULL

plot_prop_terr_species <- plot_grid(prop_terr + guides(fill = F, color = F) +
                                      theme(axis.title.x = element_blank(),
                                            axis.title.y = element_blank(),
                                            axis.text.y = element_blank()), 
          c_overtime2 + guides(fill = F), 
          ncol = 1, align = "v", rel_heights = c(1, 0.5))



# prop_nonconsumer
prop_cons_species <- posts_feeding_fishspecies %>% 
  filter(iter <= 1000) %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(prop_nonfeeding = non_consumer/(consumer + non_consumer)) %>% 
  group_by(fish_species) %>% 
  mutate(median = median(prop_nonfeeding)) %>% 
  left_join(fish_guilds) %>% 
  ggplot(aes(x = reorder(fish_species, median), y = prop_nonfeeding, fill = fish_guild)) +
  geom_boxplot(aes(group = fish_species), outlier.shape = NA) +
  coord_flip() +
  scale_fill_colorblind() + 
  labs(y = "Proportion of non-consumer aquatic prey in diet",
       x = "Fish species or family",
       fill = "Fish Domain",
       subtitle = "c)") +
  theme_classic() +
  ylim(0,  0.75)
  

d_overtime2 <- posts_feeding_guilds_overtime %>% 
  mutate(fish_guild = fct_relevel(fish_guild, "benthic")) %>% 
  ggplot(aes(x = mean, y = ..scaled.., fill = fish_guild)) +
  # ggdist::stat_halfeye(alpha = 0.7, size = 8) +
  geom_density(alpha = 0.7) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  # scale_x_log10() +
  scale_y_continuous() +
  theme_classic() + 
  theme(legend.position = "right") +
  guides(color = F) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Proportion of non-consumer\naquatic prey in diet",
       fill = "Fish Domain") +
  coord_cartesian(x = c(0, 0.75)) +
  NULL


# Combine averages across time --------------------------------------------



legend_propcons <- get_legend(prop_cons_species + theme(legend.position = "top"))

plot_prop_nonconsumer_species <- plot_grid(prop_cons_species + guides(fill = F, color = F) +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank()), 
          d_overtime2 + guides(fill = F, color = F),
          ncol = 1, align = "v", rel_heights = c(1, 0.5))


# combine
plotall <- plot_grid(plot_prey_mass_species, plot_prop_terr_species, 
          plot_prop_nonconsumer_species, ncol = 3,
          rel_widths = c(0.9, 0.5, 0.5))


plotall_final <- plot_grid(legend_propcons, plotall, ncol = 1,
          rel_heights = c(0.1, 1))

saveRDS(plotall_final, file = "plots/plotall_final.rds")
ggsave(plotall_final, file = "plots/plot_all_final.jpg", width = 7.5, height = 5)



# summarize ----------------

#prey mass
posts_total_preymass_averaged_over_time %>% 
  pivot_wider(names_from = fish_guild, values_from = mean) %>% 
  mutate(mean_all = (benthic + intermediate + surface)/3) %>% 
  pivot_longer(cols = c(benthic, intermediate, surface, mean_all)) %>% 
  rename(total = value, fish_guild = name) %>% 
  group_by(fish_guild) %>% 
  summarize(mean = mean(total),
            sd = sd(total),
            low95 = quantile(total, probs = 0.025),
            median = quantile(total, probs = 0.5),
            high95 = quantile(total, probs = 0.975))

posts_aquatic_terr %>% 
  filter(iter <= 1000) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown) %>% 
  group_by(fish_species) %>% 
  summarize(mean = mean(total),
            median = median(total),
            sd = sd(total),
            lower = quantile(total, probs = 0.025),
            upper = quantile(total, probs = 0.975)) %>% 
  arrange(-mean) %>% 
  mutate(response = "prey mg per fish")



#proportion of terrestrials
posts_aquatic_terr_guild_overtime %>% 
  group_by(fish_guild) %>% 
  summarize(mean = mean(total),
            sd = sd(total),
            low95 = quantile(total, probs = 0.025),
            median = quantile(total, probs = 0.5),
            high95 = quantile(total, probs = 0.975))

posts_aquatic_terr_guild_overtime %>% 
  pivot_wider(names_from = fish_guild, values_from = total) %>% 
  mutate(diff_ib = intermediate - benthic,
         diff_sb = surface - benthic,
         diff_si = surface - intermediate) %>%
  pivot_longer(cols = contains("diff")) %>% 
  group_by(name) %>% 
  summarize(prob_diff = sum(value>0)/4000)



prop_terr <- posts_aquatic_terr %>% 
  filter(iter <= 1000) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown,
         total = terrestrial/total) %>% 
  group_by(fish_species) %>% 
  summarize(mean = mean(total),
            median = median(total),
            sd = sd(total),
            lower = quantile(total, probs = 0.025),
            upper = quantile(total, probs = 0.975)) %>% 
  arrange(-mean) %>% 
  mutate(response = "prop_terrestrial")

# proportion of non_consumers

prop_non <- posts_feeding_fishspecies %>% 
  filter(iter <= 1000) %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(total = non_consumer/(consumer + non_consumer)) %>% 
  group_by(fish_species) %>% 
  summarize(mean = mean(total),
            median = median(total),
            sd = sd(total),
            lower = quantile(total, probs = 0.025),
            upper = quantile(total, probs = 0.975)) %>% 
  arrange(mean) %>% 
  mutate(response = "prop_nonconsumer")


prop_terr %>% select(fish_species, median) %>% 
  rename(prop_terrestrial = median) %>% 
  left_join(prop_non %>% select(fish_species, median) %>% 
              rename(prop_nonconsumer = median)) %>% 
  mutate(total_nonconsumer = prop_terrestrial + prop_nonconsumer) %>% 
  arrange(-total_nonconsumer)

posts_aquatic_terr_guild %>% 
  group_by(fish_guild, iter, date) %>% 
  summarize(total = sum(value)) %>% 
  group_by(fish_guild, iter) %>% 
  summarize(total = mean(total)) %>% 
  group_by(fish_guild) %>% 
  summarize(mean = mean(total),
            median = median(total),
            sd = sd(total),
            lower = quantile(total, probs = 0.025),
            upper = quantile(total, probs = 0.975)) %>% 
  arrange(-mean) %>% 
  mutate(response = "prey mg per fish")


posts_aquatic_terr_guild %>% 
  group_by(fish_guild, iter, date) %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = value) %>% 
  mutate(total = aquatic + terrestrial + unknown,
         total = terrestrial/total) %>% 
  group_by(fish_guild, iter) %>% 
  summarize(total = mean(total)) %>% 
  group_by(fish_guild) %>% 
  summarize(mean = mean(total),
            median = median(total),
            sd = sd(total),
            lower = quantile(total, probs = 0.025),
            upper = quantile(total, probs = 0.975)) %>% 
  arrange(-mean) %>% 
  mutate(response = "prop_terrestrial")


posts_feeding_guilds %>% 
  # mutate(fish_guild = fct_relevel(fish_guild, "benthic")) %>% 
  group_by(fish_guild, iter) %>% 
  summarize(total = mean(prop_nonfeeding_mg)) %>% 
  group_by(fish_guild) %>% 
  summarize(mean = mean(total),
            median = median(total),
            sd = sd(total),
            lower = quantile(total, probs = 0.025),
            upper = quantile(total, probs = 0.975)) %>% 
  arrange(-mean) %>% 
  mutate(response = "prop_nonconsumer")


#total chiro
posts_mg_chiro_guilds %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean_chiro = mean(total)) %>% 
  # group_by(fish_guild) %>% 
  ggplot(aes(x = mean_chiro, y = ..scaled.., fill = fish_guild)) +
  geom_density()
  # ungroup() %>% 
  summarize(mean = mean(mean_chiro),
            sd = sd(mean_chiro),
            low95 = quantile(mean_chiro, probs = 0.025),
            median = median(mean_chiro),
            high95 = quantile(mean_chiro, probs = 0.975))


total_chiro_species <- posts_totalchiro_fishspecies %>% 
  mutate(total_chiro = a + l + p,
         prop_nonfeeding = (a + p)/total_chiro) %>% 
  group_by(fish_species) %>% 
  mutate(median = median(prop_nonfeeding)) %>% 
  left_join(fish_guilds) %>% 
  ggplot(aes(x = reorder(fish_species, median), y = total_chiro, fill = fish_guild)) +
  geom_boxplot(aes(group = fish_species), outlier.shape = NA) +
  coord_flip(ylim = c(0.1, 150)) +
  scale_y_log10() +
  scale_fill_colorblind() + 
  labs(y = "Total chironomid prey mass per fish stomach (mg DM)",
       x = "Fish species or family",
       fill = "Fish Domain") +
  theme_classic()

d_overtime3 <- posts_mg_chiro %>% 
  pivot_wider(names_from = prey_stage, values_from = value) %>% 
  mutate(total = a + l + p) %>% 
  group_by(fish_guild, iter) %>% 
  summarize(mean = mean(total)) %>% 
  ggplot(aes(x = mean, y = ..scaled.., fill = fish_guild, color = fish_guild)) +
  annotation_logticks(sides = "b") + 
  # ggdist::stat_halfeye(alpha = 0.7, size = 8) +
  geom_density(alpha = 0.7) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  scale_x_log10() +
  scale_y_continuous() +
  # coord_cartesian(ylim = c(0.1, 150)) +
  theme_classic() + 
  theme(legend.position = "right") +
  guides(color = F) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Total Chironomids in diet",
       fill = "Fish Domain") +
  NULL
 

legend_propcons <- get_legend(prop_cons_species + theme(legend.position = "top"))

plot_total_chiro <- plot_grid(total_chiro_species + guides(fill = F, color = F) +
                                             theme(axis.title.x = element_blank()), 
                                           d_overtime3 + guides(fill = F, color = F),
                                           ncol = 1, align = "v", rel_heights = c(1, 0.3))




#proportion larvae or pupae chiro
prop_chiroap_species <- posts_totalchiro_fishspecies %>% 
  mutate(total_chiro = a + l + p,
         prop_nonfeeding = (a + p)/total_chiro) %>% 
  group_by(fish_species) %>% 
  mutate(median = median(prop_nonfeeding)) %>% 
  left_join(fish_guilds) %>% 
  ggplot(aes(x = reorder(fish_species, median), y = prop_nonfeeding, fill = fish_guild)) +
  geom_boxplot(aes(group = fish_species), outlier.shape = NA) +
  coord_flip() +
  scale_fill_colorblind() + 
  labs(y = "Proportion of chironomids eaten as pupae or adults",
       x = "Fish species or family",
       fill = "Fish Domain") +
  theme_classic()
  

legend_species <- get_legend(prop_chiroap_species + theme(legend.position = "top"))

p1 <- plot_grid(total_species + guides(color = F, fill = F)+
                  labs(subtitle = "a)"),
                prop_cons_species + guides(color = F, fill = F)+
                  labs(subtitle = "c)"),
                ncol = 1, align = "vh")

p2 <- plot_grid(total_chiro_species + guides(color = F, fill = F) + theme(axis.title.y = element_blank(),
                                                                          axis.text.y = element_blank()) +
                  labs(subtitle = "b)"),
                prop_chiroap_species + guides(color = F, fill = F) + theme(axis.title.y = element_blank(),
                                                                          axis.text.y = element_blank())+
                  labs(subtitle = "d)"),
                ncol = 1, align = "vh")

p3 <- plot_grid(p1, p2,
                ncol = 2, align = "vh",
                rel_widths = c(0.43, 0.3))


p4 <- plot_grid(legend_species, p3, ncol = 1,
                rel_heights = c(0.1, 1))


ggsave(p4, file = "plots/totals_by_fishspecies.jpg", width = 10, height = 9)



posts_feeding_fishspecies %>% 
  filter(iter <= 1000) %>% 
  pivot_wider(names_from = prey_feeding, values_from = value) %>% 
  mutate(prop_nonfeeding = non_consumer/(consumer + non_consumer)) %>% 
  group_by(fish_species) %>% 
  summarize(median = median(prop_nonfeeding),
            low = quantile(prop_nonfeeding, probs = 0.025),
            high = quantile(prop_nonfeeding, probs = 0.975)) %>% 
  arrange(-median)



# Plot emergence  ---------------------------------------------------------
plot_emergencepersite <- post_emerge_means %>%
  mutate(date = ymd(date),
         site = case_when(site == "burbank" ~ "a) Burbank (backwater)",
                          site == "gunderson" ~ "b) Gunderson (backwater)",
                          site == "littlebridge" ~ "c) Little Bridge (stream)",
                          TRUE ~ "d) Spirit Mound (stream)")) %>% 
  ggplot(aes(x = date, y = estimate, ymin = q2_5, ymax = q97_5)) + 
  geom_pointrange() +
  facet_wrap(~site, scales = "free_y") +
  labs(x = "Collection Date",
       y = expression ("Aquatic insect emergence"~(number/m^2/d))) + 
  theme_bw()

saveRDS(plot_emergencepersite, file = "plots/emergencepersite.rds")
ggsave(plot_emergencepersite, file = "plots/emergencepersite.jpg", dpi = 500, width = 6, height = 6)

# Plot emergence versus diet -----------

scale_convert <- readRDS(file = "models/post_emerge_means.rds") %>% distinct(estimate, estimate_scaled) 

all_emerge_diet <- posts_emerge_long %>% 
  group_by(name, emergence) %>% 
  summarize(median = median(value),
            sd = sd(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>% 
  mutate(emergence = (emergence*sd(scale_convert$estimate)) + mean(scale_convert$estimate),
         name = case_when(name == "benthic" ~ "a) Benthic",
                                       name == "intermediate" ~ "b) Intermediate",
                                       name == "surface" ~ "c) Surface")) %>% 
  ggplot() + 
  geom_line(aes(x = emergence, y = median)) + 
  geom_ribbon(aes(x = emergence, y = median, ymax = upper, ymin = lower),
              alpha = 0.2) +
  geom_point(data = emerge_v_diet, aes( x= estimate,
                                                                      y = prop_nonconsumer),
             position = position_jitter(height = 0.0091, width = 0.06),
             shape = 21, size = 0.7) +
  facet_wrap(~name) + 
  labs(y = "Proportion of non-consumer\n aquatic prey in diet",
       x = expression ("Aquatic insect emergence"~(number/m^2/d))) +
  # scale_color_colorblind() + 
  theme_classic() +
  ylim(0,1)


# summary stats
emerge_posts <- readRDS("models/emerge_posts.rds")

emerge_posts %>% as_tibble() %>% 
  select(contains(c("_int", "_slope"))) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  mutate(value = exp(value)) %>% # summaries of odds
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975),
            prob_positive = sum(value>1)/4000) # high probability that insect emergence increases the odds of containing a non-consumer prey


emerge_posts %>% as_tibble() %>% 
  select(contains(c("_int", "_slope"))) %>% 
  mutate(diff_s_b = surface_slope - benthic_slope,
         diff_s_i = surface_slope - intermediate_slope,
         diff_i_b = intermediate_slope - benthic_slope) %>% 
  pivot_longer(cols = contains("diff")) %>% 
  group_by(name) %>% 
  mutate(value = exp(value)) %>% # summaries of odds
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975),
            prob_positive = sum(value>1)/4000)



# Plot emergence chiro versus chiro diet -----------
emergechiro_v_dietchiro <- readRDS("data/emergechiro_v_dietchiro.rds")
scale_convertchiro <- readRDS(file = "models/post_emergechiro_means.rds") %>% distinct(estimate, estimate_scaled) 

chiro_emerge_diet <- posts_emergechiroonly_long %>% 
  group_by(name, emergence) %>% 
  summarize(median = median(value),
            sd = sd(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>% 
mutate(emergence = (emergence*sd(scale_convert$estimate)) + mean(scale_convert$estimate),
       name = case_when(name == "benthic" ~ "d) Benthic",
                        name == "intermediate" ~ "e) Intermediate",
                        name == "surface" ~ "f) Surface")) %>%
  ggplot() + 
  geom_line(aes(x = emergence, y = median)) + 
  geom_ribbon(aes(x = emergence, y = median, ymax = upper, ymin = lower),
              alpha = 0.2) +
  geom_point(data = emergechiro_v_dietchiro, aes( x= estimate,
                                                                      y = prop_nonconsumer),
             position = position_jitter(height = 0.0091, width = 0.06),
             shape = 21, size = 0.7) +
  facet_wrap(~name) + 
  labs(y = "Proportion of chironomids\neaten as pupae or adults ",
       x = expression ("Chironomid emergence"~(number/m^2/d))) +
  # scale_color_colorblind() + 
  theme_classic() +
  ylim(0,1)


# summary stats
emergechiroonly_posts <- readRDS("models/emergechiroonly_posts.rds")

emergechiroonly_posts %>% as_tibble() %>% 
  select(contains(c("_int", "_slope"))) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  mutate(value = exp(value)) %>% # summaries of odds
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975),
            prob_positive = sum(value>1)/4000) # high probability that insect emergence increases the odds of containing a non-consumer prey

emergechiroonly_posts %>% as_tibble() %>% 
  select(contains(c("_int", "_slope"))) %>% 
  mutate(diff_s_b = surface_slope - benthic_slope,
         diff_s_i = surface_slope - intermediate_slope,
         diff_i_b = intermediate_slope - benthic_slope) %>% 
  pivot_longer(cols = contains("diff")) %>% 
  group_by(name) %>% 
  mutate(value = exp(value)) %>% # summaries of odds
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975),
            prob_positive = sum(value>1)/4000)



# Combine emerge plots ----------------------------------------------------

plot_emerge_both <- plot_grid(all_emerge_diet, chiro_emerge_diet, ncol = 1, align = "vh")

saveRDS(plot_emerge_both, file = "plots/plot_emerge_both.rds")
ggsave(plot_emerge_both, file = "plots/plot_emerge_both.jpg", width = 9, height = 7, dpi = 500)


# Combine emerge model table ----------------------------------

emerge_slopes <- emerge_posts %>% as_tibble() %>% 
  select(contains(c("_int", "_slope"))) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  mutate(value = exp(value)) %>% # summaries of odds
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975),
            prob_positive = sum(value>1)/4000) %>% 
  separate(name, c("fish_domain", "parameter")) %>% 
  mutate(model = "Everything") %>% 
  bind_rows(emergechiroonly_posts %>% as_tibble() %>% 
  select(contains(c("_int", "_slope"))) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  mutate(value = exp(value)) %>% # summaries of odds
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975),
            prob_positive = sum(value>1)/4000) %>% 
  separate(name, c("fish_domain", "parameter")) %>% 
  mutate(model = "Chironomids Only")) %>% 
  select(-median, -lower, -upper, -prob_positive) %>% 
  pivot_longer(cols = c(mean, sd)) %>% 
  unite("par_name", c(parameter,name)) %>% 
  mutate(value = round(value, 2)) %>% 
  pivot_wider(names_from = par_name, values_from = value)

write.csv(emerge_slopes, file = "tables/emerge_slopes.csv", row.names = F)
