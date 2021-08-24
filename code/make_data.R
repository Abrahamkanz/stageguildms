library(tidyverse)
library(brms)
library(janitor)
library(readxl)
library(ggridges)
library(lubridate)
library(GGally)
library(patchwork)
library(ggpubr)
library(ggthemes)
library(cowplot)
library(egg)


guild_diet_multi_drymass <- readRDS(file = "data/guild_diet_multi_drymass.rds") %>% 
  mutate(sample_mg_dm01 = sample_mg_dm + 0.01,
         sample_mg_dm01_permm = sample_mg_dm01/parse_number(length_mm)) %>% 
  mutate(prey_ecosystem = case_when(is.na(prey_ecosystem) ~ "unknown",
                                    TRUE ~ prey_ecosystem),
         date_formatted = ymd(date),
         date_no = yday(date_formatted) - min(yday(date_formatted)) + 1) 

fish_guilds <- guild_diet_multi_drymass %>% distinct(fish_species, fish_guild)
emergence_2019_formodel <- readRDS("data/emergence_2019_new.rds") %>% 
  mutate(taxa = case_when(name == "chiro" ~ "chironomid", TRUE ~ "other")) %>% 
  group_by(location, julian, id, trap_days, start, collect) %>% 
  summarize(total_emerged = as.integer(sum(value, na.rm = T))) %>% 
  mutate(julian_raw = julian) %>% 
  group_by(location) %>% 
  mutate(julian = julian - min(julian),
         total_perdayperm201 = total_emerged/trap_days/0.36 + 1,
         total_perdayperm201_scaled = total_perdayperm201/max(total_perdayperm201), 
         julian_fixed = as.factor(julian))

# wrangle data into forms for different models (e.g., adding 0.01 to zeros, grouping by terrestrial/aquatic, etc.)
total_date <- guild_diet_multi_drymass %>% 
  group_by(fish_species, date, date_no) %>% 
  summarize(total = sum(sample_mg_dm))


total <- guild_diet_multi_drymass %>% 
  group_by(sample_id, fish_species, fish_guild, date, date_no, site) %>% 
  summarize(total = sum(sample_mg_dm)) %>% 
  mutate(total01 = total + 0.1) %>% 
  ungroup() %>% 
  mutate(mean_date_no = mean(date_no),
         original_date_no = date_no,
         date_no = date_no - mean(date_no))

data_centered <- total

aquatic_terr <- guild_diet_multi_drymass %>% 
  group_by(date, fish_guild, prey_ecosystem, site, fish_species, sample_id, date_no) %>% 
  summarize(sample_mg_dm01 = sum(sample_mg_dm, na.rm = T) + 0.01) %>% 
  ungroup() %>% 
  mutate(mean_date_no = mean(date_no),
         original_date_no = date_no,
         date_no = date_no - mean(date_no),
         species_aquatic_terr = paste0(fish_species, "_", prey_ecosystem)) %>% 
  drop_na(prey_ecosystem)

aquatic_only <- guild_diet_multi_drymass %>% filter(prey_ecosystem == "aquatic") %>% 
  group_by(date, fish_guild, prey_feeding, site, fish_species, sample_id, date_no) %>% 
  summarize(sample_mg_dm01 = sum(sample_mg_dm) + 0.01) %>% 
  ungroup() %>% 
  mutate(mean_date_no = mean(date_no),
         original_date_no = date_no,
         date_no = date_no - mean(date_no)) %>% 
  drop_na(prey_feeding) %>% 
  mutate(fish_prey_feeding = paste0(fish_species, prey_feeding))

chiros_only <- guild_diet_multi_drymass %>% 
  filter(grepl("hironomid", prey_family)) %>% 
  ungroup() %>% 
  mutate(mean_date_no = mean(date_no),
         original_date_no = date_no,
         date_no = date_no - mean(date_no)) %>%
  mutate(fish_prey_stage = paste0(fish_species,"_", prey_stage)) %>% 
  select(date, mean_date_no, original_date_no, date_no, 
         sample_id, fish_prey_stage, sample_mg_dm, sample_mg_dm01, site, fish_species, prey_stage)

chiro_nonchiro <- guild_diet_multi_drymass %>% 
  mutate(chiro = case_when(grepl("hironomid", prey_family) ~ "chiro",
                           TRUE ~ "not_chiro")) %>% 
  group_by(chiro, date, fish_guild, site, fish_species, sample_id, length_mm) %>% 
  summarize(sample_mg_dm01 = sum(sample_mg_dm) + 0.01) %>% 
  ungroup() %>% 
  mutate(length_mm_number = parse_number(length_mm),
         sample_mg_dm01_permm = sample_mg_dm01/length_mm_number)

dates_correct <- total %>% distinct(date_no, date, original_date_no)


# create a multiplier to exclude posteriors before the first collection of a species and after the last collection of a species ------------
min_date <- total_date %>% ungroup() %>% 
  distinct(fish_species) %>% 
  expand_grid(date_no = unique(total$date_no)) %>% 
  left_join(total_date %>% group_by(fish_species, date_no) %>% summarize(total = sum(total))) %>% 
  group_by(fish_species, date_no) %>% 
  mutate(isna = case_when(is.na(total) ~ 'yes', TRUE ~ 'no')) %>% 
  filter(isna == 'no') %>% 
  group_by(fish_species) %>% 
  filter(date_no == min(date_no)) %>% 
  mutate(min_date = date_no) %>% 
  select(fish_species, min_date)

max_date <- total_date %>% ungroup() %>% 
  distinct(fish_species) %>% 
  expand_grid(date_no = unique(total$date_no)) %>% 
  left_join(total_date %>% group_by(fish_species, date_no) %>% summarize(total = sum(total))) %>% 
  group_by(fish_species, date_no) %>% 
  mutate(isna = case_when(is.na(total) ~ 'yes', TRUE ~ 'no')) %>% 
  filter(isna != "yes") %>% 
  group_by(fish_species) %>% 
  filter(date_no == max(date_no)) %>% 
  mutate(max_date = date_no) %>% 
  distinct(fish_species, max_date)


aquatic_terr2 <- guild_diet_multi_drymass %>% 
  group_by(date, fish_guild, prey_ecosystem, site, fish_species, sample_id, date_no) %>% 
  summarize(sample_mg_dm = sum(sample_mg_dm, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mean_date_no = mean(date_no),
         original_date_no = date_no,
         date_no = date_no - mean(date_no),
         species_aquatic_terr = paste0(fish_species, "_", prey_ecosystem)) %>% 
  drop_na(prey_ecosystem) %>% 
  as_tibble() %>% 
  mutate(date = as_date(original_date_no, origin = min(ymd(aquatic_only$date)) - 1)) %>%
  separate(species_aquatic_terr, c("species", "prey_ecosystem"), sep = "_") %>% 
  pivot_wider(names_from = prey_ecosystem, values_from = sample_mg_dm) %>% 
  mutate(total = aquatic + terrestrial,
         prop_terr = terrestrial/total) %>% 
  filter(total > 0) %>% 
  mutate(aquatic = aquatic,
         total = total,
         terrestrial = terrestrial,
         prop_terr_int = terrestrial/total,
         prop_aquatic_int = aquatic/total)
