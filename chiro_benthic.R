
chiro_benthic_mean_drymass <- benthics_mean_drymass %>% 
  subset(taxa_stage == "chiro_l"|taxa_stage == "chiro_p")

chiro_benthic_use <- chiro_benthic_mean_drymass %>% 
  mutate(date = as.factor(date)) %>% 
  group_by(date,tank,trt)%>%
  summarize(total_drymass_mg = sum(dry_mass_mg))



chiro_benthic_brm <- brm(total_drymass_mg~date*trt+(1|tank),data = chiro_benthic_use, family=Gamma(link="log"),chains=4,
                   prior = c(prior(normal(0,3),class="Intercept"),
                             prior(normal(0,2),class="b")))


marginal_effects(chiro_benthic_brm)


chiro_marg_benthic_prior_mod <-marginal_effects(chiro_benthic_brm,method="fitted",effects="date:trt")

chiro_marg_benthic_prior_post <- fitted(chiro_benthic_brm, newdata = chiro_marg_benthic_prior_mod$`date:trt`,summary=F)



chiro_columns_benthic <- paste(chiro_marg_benthic_prior_mod$`date:trt`$date,"_",chiro_marg_benthic_prior_mod$`date:trt`$trt)



colnames(chiro_marg_benthic_prior_post) <- chiro_columns_benthic

chiro_marg_benthic_prior_post <- as.data.frame(chiro_marg_benthic_prior_post)

chiro_marg_benthic_prior_post$iter <- 1:nrow(chiro_marg_benthic_prior_post)



chiro_marg_benthic_prior_plot <- gather(chiro_marg_benthic_prior_post, trt, mg_dm, -iter)

chiro_marg_benthic_prior_plot <- chiro_marg_benthic_prior_plot%>%
  separate(col = trt, into = c("date", "trt"), sep = "_")

ggplot(marg_benthic_prior_plot, aes(x=date, y=mg_dm, fill=trt))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0,600))


chiro_marg_benthic_prior_plot <- chiro_marg_benthic_prior_plot %>%
  mutate_if(is.character, str_trim)

chiro_tot_benthic_ctl <- chiro_marg_benthic_prior_plot%>%
  filter(trt=="ctl")%>%
  filter(date=="2018-06-26")%>%
  mutate(iteration = 1:4000)%>%
  mutate(mg_dm_ctrl = mg_dm)%>%
  select(iteration:mg_dm_ctrl)

chiro_tot_benthic_cc <- chiro_marg_benthic_prior_plot%>%
  filter(trt=="cc")%>%
  filter(date=="2018-07-11")%>%
  mutate(iteration = 1:4000)%>%
  mutate(mg_dm_carp = mg_dm)%>%
  select(iteration:mg_dm_carp)

chiro_tot_benthic_sp <- chiro_marg_benthic_prior_plot%>%
  filter(trt=="sp")%>%
  filter(date=="2018-06-26")%>%
  mutate(iteration = 1:4000)%>%
  mutate(mg_dm_spot = mg_dm)%>%
  select(iteration:mg_dm_spot)

chiro_tot_benthic_all <- merge(chiro_tot_benthic_ctl, chiro_tot_benthic_sp)
chiro_tot_benthic_all <- merge(chiro_tot_benthic_all, chiro_tot_benthic_cc)


chiro_tot_benthic_all_perc <- chiro_tot_benthic_all%>%
  mutate(perc_spot = mg_dm_spot/mg_dm_ctrl,
         perc_carp = mg_dm_carp/mg_dm_ctrl)%>%
  mutate(e_or_b = "benthic")%>%
  gather(compare, value, perc_spot:perc_carp)%>%
  select(e_or_b:value)


chiro_tot_mg_emerge_benthic <- rbind(tot_chiro_mg_all_perc, chiro_tot_benthic_all_perc)


chiro_benthic_emerge_comp <- ggplot(data=chiro_tot_mg_emerge_benthic,aes(x = compare, y = value, fill=e_or_b))+
  geom_boxplot(outlier.shape = NA)+
  theme_classic()+
  xlab("Fish Treatment")+
  ylab("Treatment Biomass / Control Biomass")+
  ylim(0,2)+
  scale_fill_grey(start = 0.4)+
  labs(fill = "Community")+
  scale_x_discrete(labels=c("perc_carp" = "Carp", "perc_spot" = "Spotfin"))+
  theme(axis.title = element_text(size=30),
        text = element_text(size=35))


ggsave(chiro_benthic_emerge_comp, file = "chiro_benthic_emerge_comp.jpeg", dpi=600, width=20, height=10, units="in")


chiro_tot_benthic_all %>% 
  mutate(diff_carp_ctrl = mg_dm_carp - mg_dm_ctrl)%>%
  mutate(tf_carp_ctrl = diff_carp_ctrl<0)%>%
  mutate(diff_carp_spot = mg_dm_carp - mg_dm_spot)%>%
  mutate(tf_carp_spot = diff_carp_spot<0)%>%
  mutate(diff_spot_ctrl = mg_dm_spot - mg_dm_ctrl)%>%
  mutate(tf_spot_ctrl = diff_spot_ctrl<0)%>%
  summarize(carp_ctrl_percent_diff = sum(tf_carp_ctrl/4000),
            carp_spot_percent_diff = sum(tf_carp_spot/4000),
            spot_ctrl_percent_diff = sum(tf_spot_ctrl/4000))



chiro_tot_benthic_all %>% 
  summarize(mean = mean(mg_dm_ctrl),
          low95 = quantile(mg_dm_ctrl, probs = 0.025),
          high95 = quantile(mg_dm_ctrl, probs = 0.975),
          mean_carp = mean(mg_dm_carp),
          low95_carp = quantile(mg_dm_carp, probs = 0.025),
          high95_carp = quantile(mg_dm_carp, probs = 0.975),
          mean_spot = mean(mg_dm_spot),
          low95_spot = quantile(mg_dm_spot, probs = 0.025),
          high95_spot = quantile(mg_dm_spot, probs = 0.975))

