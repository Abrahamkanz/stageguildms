library(tidyverse)
library(janitor)
library(ggridges)
library(bayesplot)
library(rstanarm)
library(ggpubr)
library(viridis)
library(cowplot)

# load data - Note: value_corrected replaces guild-date combos with zero if those combos weren't present in the model (i.e. if diets were collected from a benthic feeder on June6 but not from surface feeders, then we don't want to plot estimates from surface feeders on June 6)

props_guild_corrected_use <- read_csv("data/props_guild_corrected_rev_use_f.csv") %>% 
  ungroup() %>% 
  mutate(guild = case_when(guild == "benthic" ~ "Benthic",
                           guild == "intermediate" ~ "Intermediate",
                           guild == "surfacewatercolumn" ~ "Surface")) %>% 
  mutate(guild = fct_relevel(guild, "Intermediate","Surface"),
         value = case_when(correction == 1 ~ value, TRUE ~ NA_real_)) 



# Temporal variation ------------------------------------------------------

mean_props_overdate <- props_guild_corrected_use %>% 
  group_by(iter, stage, date, julian) %>% 
  summarize(mean_prop = mean(value, na.rm = T))

all_posts_guild <- props_guild_corrected_use %>% 
  bind_rows(mean_props_overdate %>% mutate(guild = "All Fish", guild = as.factor(guild)) %>% rename(value = mean_prop)) %>% 
  filter(stage == "prop_l") %>% 
  group_by(date, guild, julian) %>% 
  mutate(median = median(value)) %>% 
  mutate(guild = fct_relevel(guild, "Benthic", "Intermediate", "Surface", "Overall"),
         date = as.character(date))

all_posts_time_averaged <- props_guild_corrected_use %>% 
  bind_rows(mean_props_overdate %>% mutate(guild = "All Fish", guild = as.factor(guild)) %>% rename(value = mean_prop)) %>% 
  filter(stage == "prop_l") %>% 
  select(guild, iter, value) %>% 
  group_by(guild, iter) %>% 
  summarize(value = mean(value, na.rm = T)) %>% 
  mutate(julian = 1000,
         date = "Average",
         correction = 1)

all_posts <- bind_rows(all_posts_guild, all_posts_time_averaged)

time_plot <- all_posts %>% 
  ggplot(aes(y = reorder(date, -julian), x = 1-value, fill = stat(x))) +
  geom_density_ridges_gradient(aes(alpha = correction),quantile_lines = T, quantiles = 2, vline_size = 1, alpha = 1) +
  scale_fill_viridis(option = "E", direction = 1, name = "Proportion of non-feeding prey stages in diet",
                     breaks = c(0, 0.5, 1)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  facet_grid(.~guild) +
  theme_pubclean() +
  xlab("Proportion of non-feeding prey stages in diet\n(by abundance)") +
  ylab("Collection Date") +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
        text = element_text(size = 15))

saveRDS(all_posts, file = "models/all_posts.rds")
ggsave(time_plot, file = "plots/time_plot.tiff", dpi = 600, width = 6, height = 8)




# Summary Tables ----------------------------------------------------------

# Proportion of each life-stage

(all_stages <- props_guild_corrected_use %>% 
    group_by(iter, stage, guild) %>% 
    summarize(mean_prop = mean(value, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(stage, guild) %>% 
    summarize(mean = mean(mean_prop),
              sd = sd(mean_prop),
              low95 = quantile(mean_prop, probs = 0.025),
              median = quantile(mean_prop, probs = 0.5),
              high95 = quantile(mean_prop, probs = 0.975)) %>% 
   ungroup() %>% 
   mutate(stage = fct_relevel(stage, "prop_l", "prop_p", "prop_a")) %>% 
  mutate_if(is.numeric, round , 4) %>% 
   arrange(stage, guild) %>% 
   select(stage, guild, everything()))

write.csv(all_stages, file = "tables/all_stages.csv", row.names = F)

# Proportion of non-feeding prey

prop_nonfeeding <- all_posts %>% 
  group_by(date, guild) %>% 
  summarize(mean = mean(1-value,na.rm = T),
            sd = sd(value, na.rm = T),
            low95 = quantile(1-value, probs = 0.025, na.rm = T),
            median = quantile(1-value, probs = 0.5, na.rm = T),
            high95 = quantile(1-value, probs = 0.975, na.rm = T)) %>% 
  filter(date == "Average") %>% 
  mutate_if(is.numeric, round , 2) %>% 
  ungroup() %>% 
  select(-date)

write.csv(prop_nonfeeding, file = "tables/prop_nonfeeding.csv", row.names = F)


# Fish and traits ---------------------------------------------------------

table <- read_csv("table.csv") %>% 
  rename(guild = domain)

fish_plot <- table %>% 
  ggplot(aes(x = reorder(species, sampled), y = sampled))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ylab("Number of fish sampled")+
  xlab("Fish species")+
  theme_pubr()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 15))

ggsave(fish_plot, file = "fish_plot.svg", dpi =500, width = 22, height = 15)

fish_plot_trait <- table %>% 
  ggplot(aes(x = reorder(species, sampled), y = sampled, fill = guild))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(direction = -1)+
  coord_flip()+
  ylab("Number of fish sampled")+
  xlab("Fish species")+
  theme_pubr()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 15))

ggsave(fish_plot_trait, file = "fish_plot_trait.svg", dpi =500, width = 22, height = 15)


fish_plot_trait_benthic <- table %>% 
  filter(guild == "benthic") %>% 
  ggplot(aes(x = reorder(species, sampled), y = sampled, fill = guild))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = "#3183bd")+
  coord_flip()+
  ylab("Number of fish sampled")+
  xlab("Fish species")+
  theme_pubr()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10))+
  ylim(c(0,100))


fish_plot_trait_intermediate <- table %>% 
  filter(guild == "intermediate") %>% 
  ggplot(aes(x = reorder(species, sampled), y = sampled, fill = guild))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = "#9ecae1")+
  coord_flip()+
  ylab("Number of fish sampled")+
  xlab("Fish species")+
  theme_pubr()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10))+
  ylim(c(0,100))


fish_plot_trait_surface <- table %>% 
  filter(guild == "surface/watercolumn") %>% 
  ggplot(aes(x = reorder(species, sampled), y = sampled, fill = guild))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = "#deebf7")+
  coord_flip()+
  ylab("Number of fish sampled")+
  xlab("Fish species")+
  theme_pubr()+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10))+
  ylim(c(0,100))


all_fish_plot <- plot_grid(fish_plot_trait_benthic, fish_plot_trait_intermediate, fish_plot_trait_surface,
          ncol = 1, align = "v")


ggsave(all_fish_plot, file = "all_fish_plot.svg", dpi =500, width = 15, height = 20)



