#raw proportion of pupae

raw_prop_p <- guild_diet_multi %>% 
  mutate(julian_date = yday(date)) %>% 
  left_join(site_loc) %>% 
  mutate(total_alp = a+l+p,
         prop_a = a/total_alp,
         prop_l = l/total_alp,
         prop_p = p/total_alp) %>%
  select(sample_id, prop_a, prop_l, prop_p,
         finalguild, julian_date, location) %>% 
  gather(stage, prop, prop_a:prop_p) %>% 
  filter(stage == "prop_p")

#GLMM posterior for proportion of pupae

posterior_prop_p <- props_guild_corrected_use %>% 
  filter(stage == "prop_p")


d <- guild_diet_multi %>% 
  mutate(julian_date = yday(date)) %>% 
  left_join(site_loc) %>% select(date, sample_id, species, a, l, p,
                                 finalguild, julian_date, location) %>% 
  mutate(total = a+l+p) %>% 
  left_join(gam_mod_post_all_sum_new2) %>% 
  filter(total > 0)

diet_emerge_comp_model <- brm(data = d, p|trials(total)~ 1 + Estimate*location*finalguild + (1|species),
                              family = binomial(link = "logit"),
                              prior = c(prior(normal(0,3), class = "Intercept"),
                                        prior(normal(0,2), class = "b")),iter = 1000, chains = 2)

pp_check(diet_emerge_comp_model, type = "boxplot")

ggplot(marg_diet_df_new, aes(x = Estimate, y = estimate__, ymin = lower__, ymax = upper__,
                         fill = finalguild))+
  geom_ribbon(alpha = 0.5)+
  geom_line()+
  facet_grid(finalguild~location, scales = "free")+
  geom_point(data =d, aes(x = Estimate, y = p/total, 
                          color = finalguild, 
                          ymin = p/total, ymax = p/total)) +
  scale_color_grey() +
  scale_fill_grey()

post_diet <- posterior_samples(diet_emerge_comp_model) %>% as_tibble()

mean(exp(post_diet$b_Estimate))
post_diet %>% 
  summarize(median = median(exp(b_Estimate +
                              `b_Estimate:finalguildintermediate` +
                              `b_Estimate:locationspiritmound`+
                              `b_Estimate:locationspiritmound:finalguildintermediate`)),
            upper = quantile(exp(b_Estimate + 
                                   `b_Estimate:finalguildintermediate` +
                                   `b_Estimate:locationspiritmound`+
                                   `b_Estimate:locationspiritmound:finalguildintermediate`), probs = 0.975),
            lower = quantile(exp(b_Estimate + 
                                   `b_Estimate:finalguildintermediate` +
                                   `b_Estimate:locationspiritmound`+
                                   `b_Estimate:locationspiritmound:finalguildintermediate`), probs = 0.025))

median(exp(post_diet$b_Estimate + 
          post_diet$`b_Estimate:finalguildintermediate` +
          post_diet$`b_Estimate:locationspiritmound`+
          post_diet$`b_Estimate:locationspiritmound:finalguildintermediate`))

marg_diet_df_new <- marg_diet_df %>% as_tibble() %>% 
  mutate(finalguild = as.character(finalguild))

marg_diet_df %>% filter(location == "burbank", finalguild == "benthic") %>% as_tibble()



marg_diet_df_new %>% 
  group_by(location, finalguild)
  

median(exp(post_diet$b_Estimate +
          post_diet$`b_Estimate:finalguildintermediate` +
          post_diet$`b_Estimate:locationgunderson`+
          post_diet$`b_Estimate:locationgunderson:finalguildintermediate`))


median(exp(post_diet$b_Estimate +
          post_diet$`b_Estimate:finalguildsurfacewatercolumn` +
          post_diet$`b_Estimate:locationlittlebridge`+
          post_diet$`b_Estimate:locationlittlebridge:finalguildsurfacewatercolumn`))





