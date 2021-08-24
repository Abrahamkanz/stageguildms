library(tidyverse)
library(here)
library(ggthemes)
library(ggridges)

props_guild_corrected_use <- readRDS(here("data/props_guild_corrected_use.rds"))

props_mean <- props_guild_corrected_use %>% group_by(finalguild, stage, iter) %>% 
  summarize(avg_across_dates = mean(value_corrected))



props_mean %>% 
  filter(stage == "prop_l") %>% 
  ggplot(aes(x = 1-avg_across_dates, y = finalguild)) +
  geom_density_ridges()

props_guild_corrected_use %>% 
  ggplot(aes(x = value_corrected, y = reorder(date, -julian), fill = stage)) + 
  geom_density_ridges() + 
  scale_fill_colorblind() +
  facet_grid(.~finalguild)

summaries <- props_guild_corrected_use %>% 
  filter(stage == "prop_l") %>% 
  mutate(prop_nonfeeding = 1-value_corrected) %>% 
  group_by(guild) %>% 
  summarize(median = median(prop_nonfeeding),
            low = quantile(prop_nonfeeding, prob = 0.025),
            high = quantile(prop_nonfeeding, prob = 0.975))


props_guild_corrected_use %>% 
  filter(stage == "prop_l") %>% 
  group_by(guild, iter) %>% 
  summarize(avg_across_dates)
  ggplot(aes(x = 1-, y = median, ymin = low, ymax = high)) +
  geom_pointrange(position = position_dodge(width = 0.5)) 
