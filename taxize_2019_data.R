#add taxonomic information for prey using taxize package
library(taxize)
library(tidyverse)
names(diet_2019)

diet_2019_tidy <- NEW_diet2019_fixed %>% 
  as_tibble() %>% 
  gather(group, number, "amphipoda_NA_aquatic":"zygoptera_l_aquatic") %>% 
  separate(group, c("prey_taxa","prey_stage","prey_habitat")) %>% 
  mutate(number = as.numeric(number))


#make a list of unique prey names (otherwise taxize takes forever because i searches the same name over and over)

prey_taxa <- unique(diet_2019_tidy$prey_taxa)

prey_taxa_taxized <- classification(prey_taxa, db = 'itis')

prey_taxa_taxized_noNA <- prey_taxa_taxized[!is.na(prey_taxa_taxized)]

#make a list of prey that taxize couldn't find. Add their names separately and bind them back in
#non_taxized_prey <- tidy_df_taxized_prey %>% 
# distinct(prey_taxa, .keep_all = T) %>% 
#filter(is.na(prey_family))

#write.csv(non_taxized_prey, file = "non_taxized_prey.csv")

non_taxized_prey_to_add <- read_csv("non_taxized_prey.csv") %>% 
  select(prey_taxa, prey_kingdom, prey_class, prey_order, prey_family, prey_species)


prey_taxa_use <- bind_rows(unclass(prey_taxa_taxized_noNA),.id = 'prey_taxon') %>% 
  select(-id) %>% 
  as_tibble() %>% 
  filter(rank=="kingdom"|rank=="class"|rank=="order"|rank=="family"|rank=="species") %>% 
  spread(rank, name) %>% 
  rename("prey_class" = class,
         "prey_kingdom" = kingdom,
         "prey_order" = order,
         "prey_family" = family,
         "prey_species" = species,
         "prey_taxa" = prey_taxon)

#fix typos in prey_taxon

prey_taxa_full <- rbind(prey_taxa_use, non_taxized_prey_to_add) %>% 
  distinct(prey_taxa, .keep_all = T)


diet_2019_tidy_tax <- diet_2019_tidy %>% 
  left_join(prey_taxa_full)








