
guild_diet_multi <- diet_2019_tidy_tax_multi %>% 
  left_join(feeding_guilds_r)



guild_brm <- brm(mvbind(a,l,p) ~ date*finalguild + (1|site + species), 
                 data = guild_diet_multi, family = poisson(link = "log"),
              prior = c(prior(normal(0,3), class = "Intercept"),
                        prior(normal(0,2), class = "b")),iter = 1000, chains = 2)


print(guild_brm)

guild_brm_marg <- marginal_effects(guild_brm)


date <- unique(guild_diet_multi$date)
finalguild <- unique(guild_diet_multi$finalguild)
stages <- c("a","l","p")

new_data_guild <- expand_grid(date, finalguild)


post_names_guild <- expand_grid(finalguild, date, stages) %>% 
  arrange(stages, date) %>%
  unite(names, c("finalguild","date", "stages")) 



posts_guild <- as.data.frame(fitted(guild_brm, newdata = new_data_guild, 
                                    summary = F, re_formula = NA))

colnames(posts_guild) <- post_names_guild$names

post_summary_guild <- posts_guild %>% 
  mutate(iter = 1:1000) %>% 
  gather(key, value, -iter) %>%
  separate(key, c("guild","date","stage"), sep = "_") %>% 
  as_tibble() %>% 
  pivot_wider(names_from = "stage",
              values_from = "value")




props_guild <- post_summary_guild %>% 
  mutate(total_alp = a+l+p,
         prop_a = a/total_alp,
         prop_l = l/total_alp,
         prop_p = p/total_alp) %>% 
  gather(stage, value, prop_a:prop_p) %>% 
  select(iter, guild, date, stage, value)


ggplot(data = props_guild, aes(x = date, y = value, color = stage, group = interaction(stage,date)))+
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~guild)+
  NULL

correction_data_guild <- diet_2019_tidy_tax_multi %>% 
  mutate(tot = a+l+p) %>%
  left_join(feeding_guilds_r) %>% 
  select(finalguild, date, tot) %>% 
  group_by(finalguild, date) %>% 
  summarize(tot = sum(tot)) %>% 
  mutate(correction = ifelse(tot >= 1, 1, 0)) %>% 
  select(-tot)

correction_data_guild <- correction_data_guild %>% 
  mutate(guild = finalguild)

guild_combined <- props_guild %>% 
  left_join(correction_data_guild) %>% 
  group_by(date, stage, guild) %>% 
  mutate(correction = ifelse(is.na(correction),0,1))

props_guild_corrected <- guild_combined %>% 
  mutate(value_corrected = correction*value)

ggplot(data = props_guild_corrected, aes(x = date, y = value, color = stage, group = interaction(stage,date)))+
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(.~guild)+
  NULL



benthic_diet_props <- ggplot(data = subset(props_guild_corrected, 
                                           props_guild_corrected$guild =="benthic"), 
                             aes(x = date, y = value, fill = stage,
                                 group = interaction(stage,date)))+
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(.~guild)+
  ylab("Proportion of Diet")+
  xlab(element_blank())+
  theme_classic()+
  labs(fill = "Gut Item Stage")+
  theme(strip.text = element_blank())+
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        text = element_text(size = 30))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(breaks = c("prop_a","prop_l", "prop_p"),
                    values = c("grey40", "white", "darkgrey"),
                    labels = c("Adults", "Larvae", "Pupae"))+
  NULL

ggsave(benthic_diet_props, file = "benthic_diet_props.jpg", 
       dpi=600, width=20, height=10, units="in")


intermediate_diet_props <- ggplot(data = subset(props_guild_corrected, 
                                                props_guild_corrected$guild =="intermediate"), aes(x = date, y = value, fill = stage, group = interaction(stage,date)))+
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(.~guild)+
  ylab("Proportion of Diet")+
  xlab(element_blank())+
  theme_classic()+
  labs(fill = "Gut Item Stage")+
  theme(strip.text = element_blank())+
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        text = element_text(size = 30))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(breaks = c("prop_a","prop_l", "prop_p"),
                    values = c("grey40", "white", "darkgrey"),
                    labels = c("Adults", "Larvae", "Pupae"))+
  NULL

ggsave(intermediate_diet_props, file = "intermediate_diet_props.jpg", dpi=600, width=20, height=10, units="in")


surface_diet_props <- ggplot(data = subset(props_guild_corrected_use, props_guild_corrected$guild =="surfacewatercolumn"), aes(x = date, y = value, fill = stage, group = interaction(stage,date)))+
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(.~guild)+
  ylab("Proportion of Diet")+
  xlab(element_blank())+
  theme_classic()+
  labs(fill = "Gut Item Stage")+
  theme(strip.text = element_blank())+
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        text = element_text(size = 8))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(breaks = c("prop_a","prop_l", "prop_p"),
                    values = c("grey40", "white", "darkgrey"),
                    labels = c("Adults", "Larvae", "Pupae"))+
  NULL

ggsave(surface_diet_props, file = "surface_diet_props.jpg", dpi=600, width=20, height=10, units="in")


test <- props_guild_corrected %>% 
  filter(guild == "benthic") %>% 
  summarize(med = median(value_corrected),
            low95 = quantile(value_corrected, probs = 0.025),
            high95 = quantile(value_corrected, probs = 0.975),
            mean = mean(value_corrected))

test2 <- subset(test, test$stage == "prop_a")


  
guild_multi_grid <- plot_grid(benthic_diet_props, intermediate_diet_props, surface_diet_props,
          labels = c("A", "B", "C"), ncol = 1, nrow = 3)

save_plot("guild_multi_grid.jpeg", guild_multi_grid, base_aspect_ratio = 1.3)










