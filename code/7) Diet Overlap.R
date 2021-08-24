source("code/make_data.r")

library(RInSp)

prey_taxa_diet <- guild_diet_multi_drymass %>% 
  select(sample_id, site, date, fish_species, sample_mg_dm, prey_taxon, prey_feeding) %>% 
  group_by(sample_id, site, date, fish_species, prey_taxon) %>% 
  summarize(sample_mg_dm = sum(sample_mg_dm)) %>% 
  pivot_wider(names_from = prey_taxon, values_from = sample_mg_dm)

# Use RInSp to calculate overlap for individuals from Bolnick et al. (2002)
prey_taxa_diet_rinsp <- import.RInSp(prey_taxa_diet, row.names = 1, info.cols = c(1:4))
similarity <- overlap(prey_taxa_diet_rinsp)

similarity$meanindividualoverlap
diet.similarity <- as.dist(similarity$overlapmatrix)
plot(diet.similarity)

simil <- similarity$meanindividualoverlap %>% as_tibble()

overlap_taxaonly <- prey_taxa_diet_rinsp$info %>% as_tibble() %>% 
  mutate(overlap = simil$V1) %>% clean_names()

prey_taxacons_diet <- guild_diet_multi_drymass %>% 
  select(sample_id, site, date, fish_species, sample_mg_dm, prey_taxon, prey_feeding) %>% 
  group_by(sample_id, site, date, fish_species, prey_taxon, prey_feeding) %>% 
  summarize(sample_mg_dm = sum(sample_mg_dm)) %>% 
  unite("prey_taxa_feeding", c(prey_taxon, prey_feeding), sep = "_") %>% 
  pivot_wider(names_from = prey_taxa_feeding, values_from = sample_mg_dm)

prey_taxafeeding_diet_rinsp <- import.RInSp(prey_taxacons_diet, row.names = 1, info.cols = c(1:4))

similarity_feeding <- overlap(prey_taxafeeding_diet_rinsp)

simil_feeding <- similarity_feeding$meanindividualoverlap %>% as_tibble()

overlap_taxafeeding <- prey_taxafeeding_diet_rinsp$info %>% as_tibble() %>% 
  mutate(overlap = simil_feeding$V1) %>% clean_names()

#overlap by stage

prey_taxastage_diet <- guild_diet_multi_drymass %>% 
  select(sample_id, site, date, fish_species, sample_mg_dm, prey_taxon, prey_stage) %>% 
  group_by(sample_id, site, date, fish_species, prey_taxon, prey_stage) %>% 
  summarize(sample_mg_dm = sum(sample_mg_dm)) %>% 
  unite("prey_taxa_stage", c(prey_taxon, prey_stage), sep = "_") %>% 
  pivot_wider(names_from = prey_taxa_stage, values_from = sample_mg_dm)

prey_taxastage_diet_rinsp <- import.RInSp(prey_taxastage_diet, row.names = 1, info.cols = c(1:4))

similarity_stage <- overlap(prey_taxastage_diet_rinsp)

simil_stage <- similarity_stage$meanindividualoverlap %>% as_tibble()

overlap_taxastage <- prey_taxastage_diet_rinsp$info %>% as_tibble() %>% 
  mutate(overlap = simil_stage$V1) %>% clean_names()

overlap_taxafeeding %>% 
  mutate(aggregation = "prey_taxa_feeding") %>% 
  bind_rows(overlap_taxaonly %>% mutate(aggregation = "prey_taxa_only")) %>% 
  bind_rows(overlap_taxastage %>% mutate(aggregation = "prey_taxa_stage")) %>% 
  filter(aggregation != "prey_taxa_feeding") %>% 
  ggplot(aes(x = reorder(fish_species, overlap), y = overlap, fill = aggregation)) + 
  geom_boxplot() +
  coord_flip()


overlap_taxafeeding %>% 
  mutate(aggregation = "prey_taxa_feeding") %>% 
  bind_rows(overlap_taxaonly %>% mutate(aggregation = "prey_taxa_only")) %>% 
  bind_rows(overlap_taxastage %>% mutate(aggregation = "prey_taxa_stage")) %>% 
  filter(aggregation != "prey_taxa_feeding") %>% 
  pivot_wider(names_from = aggregation, values_from = overlap) %>% 
  mutate(diff_stage_minus_taxa = prey_taxa_stage - prey_taxa_only) %>% 
  ggplot(aes(y = reorder(fish_species, diff_stage_minus_taxa), x = diff_stage_minus_taxa,
             fill = stat(x))) + 
  stat_density_ridges(geom = "density_ridges_gradient", quantile_lines = T, quantiles = 2)

prop_nonconsumer <- guild_diet_multi_drymass %>% 
  select(sample_id, site, date, fish_species, sample_mg_dm, prey_feeding)  %>% 
  group_by(sample_id, site, date, fish_species, prey_feeding) %>% 
  summarize(total = sum(sample_mg_dm)) %>% 
  pivot_wider(names_from = prey_feeding, values_from = total) %>% 
  mutate(total = consumer + non_consumer,
         prop_nonconsumer = non_consumer/total)
  
prop_nonlarval <- guild_diet_multi_drymass %>% 
  select(sample_id, site, date, fish_species, sample_mg_dm, prey_stage)  %>% 
  group_by(sample_id, site, date, fish_species, prey_stage) %>% 
  mutate(prey_stage = case_when(is.na(prey_stage) ~ "unknown", TRUE ~ prey_stage)) %>% 
  summarize(total = sum(sample_mg_dm)) %>% 
  pivot_wider(names_from = prey_stage, values_from = total) %>% 
  mutate(total = a + l + p + unknown,
         prop_nonlarval = (a + p)/total)

diffs <- overlap_taxafeeding %>% 
  mutate(aggregation = "overlap_prey_taxa_feeding") %>% 
  bind_rows(overlap_taxaonly %>% mutate(aggregation = "overlap_prey_taxa_only")) %>% 
  bind_rows(overlap_taxastage %>% mutate(aggregation = "overlap_prey_taxa_stage")) %>% 
  filter(aggregation != "overlap_prey_taxa_feeding") %>% 
  pivot_wider(names_from = aggregation, values_from = overlap) %>% 
  mutate(diff_stage_minus_taxa = overlap_prey_taxa_stage - overlap_prey_taxa_only) %>% 
  left_join(prop_nonconsumer %>% select(sample_id, prop_nonconsumer)) %>% 
  left_join(prop_nonlarval %>% select(sample_id, prop_nonlarval)) %>% 
  ungroup() %>% 
  mutate(prop_nonlarval_z = (prop_nonlarval - mean(prop_nonlarval, na.rm = T))/sd(prop_nonlarval, na.rm = T))


diff_plot <- diffs %>% 
  arrange(desc(overlap_prey_taxa_only)) %>% 
  mutate(rank = 1:nrow(.)) %>% 
  pivot_longer(cols = contains("overlap")) %>% 
  mutate(name_yn = case_when(grepl("only", name) ~ "Without life-stage", TRUE ~ "With life-stage"),
         name_yn = fct_relevel(name_yn, "Without life-stage")) %>% 
  ggplot(aes(x = rank, y = value)) + 
  geom_point(aes(color = name_yn), size = 0.7) +
  labs(y = "Individual Diet Overlap",
       x = "Rank",
       color = "",
       subtitle = "a)") +
  scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = c(0.75, 0.9),
        panel.grid = element_blank(),
        legend.backgroun = element_rect(fill = NA)) +
  guides(color = guide_legend(override.aes = list(size=1))) +
  NULL


# Model diet overlap ------------------------------------------------------
get_prior(diff_stage_minus_taxa ~ prop_nonlarval_z + (1|date) + (1|site) + (1|fish_species),
          data = diffs , family = gaussian())

brm_dietoverlap <- brm(bf(diff_stage_minus_taxa ~ prop_nonlarval_z + (1|date) + (1|site) + (1|fish_species),
                          sigma ~ prop_nonlarval_z),
                       data = diffs , family = gaussian(),
                       prior = c(prior(normal(0, 1), class = "Intercept"),
                                 prior(normal(0, 1), class = "b"),
                                 prior(exponential(2), class = "sd")),
                       file = "models/species_models/brm_dietoverlap.rds",
                       file_refit = "on_change", cores = 4, chains = 4, iter = 2000)

overlap_conds <- conditional_effects(brm_dietoverlap)

diff_regression_plot <- overlap_conds$prop_nonlarval_z %>% 
  ggplot(aes(x = prop_nonlarval_z, y = estimate__)) + 
  geom_point(data = brm_dietoverlap$data, aes(y = diff_stage_minus_taxa),
             size = 1, shape = 21) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3) +
  theme_bw() +
  labs(x = "Proportion of non-larval prey in diet\n(standardized)",
       y = "Overlap differences\n('with life-stage' - 'without life-stage'",
       subtitle = "b)")


overlap_plot <- diff_plot/diff_regression_plot
saveRDS(overlap_plot, file = "plots/overlap_plot.rds")
ggsave(overlap_plot, file = "plots/overlap_plot.jpg", dpi = 500, width = 4, height = 7)
