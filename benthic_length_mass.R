
library(tidyverse)
library(brms)

benthic_merge<-merge(benthic_lengths2018,benthic_lm_data)


test<- subset(benthic_merge, taxa=="anisoptera")

benthic_merge%>%
  group_by(taxa) %>%
  summarize(mean = mean(length_mm))

#note from diet3 analysis
#dragonfly_m_day<-dragonfly_emerge_new %>%
#  mutate(alpha = mean^2/sd^2,
#         beta = mean/sd^2,
#         draw_mg = rgamma(nrow(dragonfly_emerge_new), shape=alpha, rate=beta)


anisoptera_sub<-subset(benthic_merge,taxa=="anisoptera")
mean(anisoptera_sub$length_mm)
sd(anisoptera_sub$length_mm)

chiro_sub<-subset(benthic_merge,taxa=="chiro")
mean(chiro_sub$length_mm)
sd(chiro_sub$length_mm)

cerat_sub<-subset(benthic_merge,taxa=="cerat")
mean(cerat_sub$length_mm)
sd(cerat_sub$length_mm)

hydro_sub<-subset(benthic_merge,taxa=="hydro")
mean(hydro_sub$length_mm)
sd(hydro_sub$length_mm)

ephem_sub<-subset(benthic_merge,taxa=="ephem")
mean(ephem_sub$length_mm)
sd(ephem_sub$length_mm)

dytisc_sub<-subset(benthic_merge,taxa=="dytisc")
mean(dytisc_sub$length_mm)
sd(dytisc_sub$length_mm)

benthics_2018_gather<-benthics_2018%>%
  gather(taxa, abundance, 3:13)




###Jeff's stuff
benthic_merge <- benthic_merge %>% 
  mutate(dry_mass_mg = a*length_mm^b) 

benthic_mean_table <- benthic_merge %>% 
  unite(taxa_stage, c(taxa,stage),remove = F)%>%
  group_by(taxa_stage, date) %>% 
  summarize(mean = mean(dry_mass_mg),
            sd = sd(dry_mass_mg)) 

  ggplot(aes(x=date, y = mean, ymin = mean-sd, ymax = mean+sd, color=trt))+
  geom_pointrange(position=position_dodge(width=1))+
  facet_wrap(~taxa, scales="free")



ggplot(benthic_merge, aes(x=date, y = dry_mass_mg, color=taxa, group=taxa))+
  geom_point(position=position_dodge(width=100))

#########End

benthics_2018_gather$taxa_stage <- benthics_2018_gather$taxa

benthics_2018_gather_new <- benthics_2018_gather%>%
  select(-taxa)

benthics_with_means <- merge(benthics_2018_gather_new, benthic_mean_table)

benthics_mean_drymass <- benthics_with_means%>%
  mutate(dry_mass_mg = abundance*mean)%>%
  merge(assigned_trts_2018)

benthics_brm_data <- benthics_mean_drymass%>%
  mutate(date = as.factor(date)) %>% 
  group_by(date,tank,trt)%>%
  summarize(total_drymass_mg = sum(dry_mass_mg))

#rerun model before use of data

benthic_brm <- brm(total_drymass_mg~date*trt+(1|tank),data=benthics_brm_data, family=Gamma(link="log"),chains=4,
    prior = c(prior(normal(0,3),class="Intercept"),
              prior(normal(0,2),class="b")))

test_brm <- brm(total_drymass_mg~date*trt+(1|tank),data=benthics_brm_data, family=Gamma(link="log"),chains=4,
    prior = c(prior(normal(0,3),class="Intercept"),
              prior(normal(0,2),class="b")),sample_prior = "only")

marginal_effects(benthic_brm)

marginal_effects(test_brm)


marg_benthic_prior_mod <-marginal_effects(benthic_brm,method="fitted",effects="date:trt")

marg_benthic_prior_post <- fitted(benthic_brm, newdata=marg_benthic_prior_mod$`date:trt`,summary=F)



columns_benthic <- paste(marg_benthic_prior_mod$`date:trt`$date,"_",marg_benthic_prior_mod$`date:trt`$trt)



colnames(marg_benthic_prior_post) <- columns_benthic

marg_benthic_prior_post <- as.data.frame(marg_benthic_prior_post)

marg_benthic_prior_post$iter <- 1:nrow(marg_benthic_prior_post)



marg_benthic_prior_plot <- gather(marg_benthic_prior_post, trt, mg_dm, -iter)

marg_benthic_prior_plot <- marg_benthic_prior_plot%>%
  separate(col = trt, into = c("date", "trt"), sep = "_")

ggplot(marg_benthic_prior_plot, aes(x=date, y=mg_dm, fill=trt))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0,750))


#6_12

marg_benthic_prior_post%>%
  mutate(ctl_cc_diff_6_12 = (`2018-06-12 _ ctl`- `2018-06-12 _ cc`))%>%
  mutate(total_diff = ctl_cc_diff_6_12>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(ctl_sp_diff_6_12 = (`2018-06-12 _ ctl`- `2018-06-12 _ sp`))%>%
  mutate(total_diff = ctl_sp_diff_6_12>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(cc_sp_diff_6_12 = (`2018-06-12 _ cc`- `2018-06-12 _ sp`))%>%
  mutate(total_diff = cc_sp_diff_6_12>0)%>%
  summarize(sum_t = sum(total_diff))/4000



#6_26

marg_benthic_prior_post%>%
  mutate(ctl_cc_diff_6_26 = (`2018-06-26 _ ctl`- `2018-06-26 _ cc`))%>%
  mutate(total_diff = ctl_cc_diff_6_26>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(ctl_sp_diff_6_26 = (`2018-06-26 _ ctl`- `2018-06-26 _ sp`))%>%
  mutate(total_diff = ctl_sp_diff_6_26>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(cc_sp_diff_6_26 = (`2018-06-26 _ cc`- `2018-06-26 _ sp`))%>%
  mutate(total_diff = cc_sp_diff_6_26>0)%>%
  summarize(sum_t = sum(total_diff))/4000

#7_11

marg_benthic_prior_post%>%
  mutate(ctl_cc_diff_7_11 = (`2018-07-11 _ ctl`- `2018-07-11 _ cc`))%>%
  mutate(total_diff = ctl_cc_diff_7_11>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(ctl_sp_diff_7_11 = (`2018-07-11 _ ctl`- `2018-07-11 _ sp`))%>%
  mutate(total_diff = ctl_sp_diff_7_11>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(cc_sp_diff_7_11 = (`2018-07-11 _ cc`- `2018-07-11 _ sp`))%>%
  mutate(total_diff = cc_sp_diff_7_11>0)%>%
  summarize(sum_t = sum(total_diff))/4000

#7_24

marg_benthic_prior_post%>%
  mutate(ctl_cc_diff_7_24 = (`2018-07-24 _ ctl`- `2018-07-24 _ cc`))%>%
  mutate(total_diff = ctl_cc_diff_7_24>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(ctl_sp_diff_7_24 = (`2018-07-24 _ ctl`- `2018-07-24 _ sp`))%>%
  mutate(total_diff = ctl_sp_diff_7_24>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(cc_sp_diff_7_24 = (`2018-07-24 _ cc`- `2018-07-24 _ sp`))%>%
  mutate(total_diff = cc_sp_diff_7_24>0)%>%
  summarize(sum_t = sum(total_diff))/4000


#8_06

marg_benthic_prior_post%>%
  mutate(ctl_cc_diff_8_06 = (`2018-08-06 _ ctl`- `2018-08-06 _ cc`))%>%
  mutate(total_diff = ctl_cc_diff_8_06>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(ctl_sp_diff_8_06 = (`2018-08-06 _ ctl`- `2018-08-06 _ sp`))%>%
  mutate(total_diff = ctl_sp_diff_8_06>0)%>%
  summarize(sum_t = sum(total_diff))/4000

marg_benthic_prior_post%>%
  mutate(cc_sp_diff_8_06 = (`2018-08-06 _ cc`- `2018-08-06 _ sp`))%>%
  mutate(total_diff = cc_sp_diff_8_06>0)%>%
  summarize(sum_t = sum(total_diff))/4000


benthics_brm_data$day <- lubridate::yday(benthics_brm_data$date)

benthics_brm_summary <- benthics_brm_data%>%
  group_by(day, trt)%>%
  summarize(benthic_mg = sum(total_drymass_mg))
  

library(janitor)
marg_benthic_prior_plot <-clean_names(marg_benthic_prior_plot)

marg_benthic_prior_plot %>% 
  spread(trt, mg_dm) %>% 
  clean_names() %>% 
  mutate(perc_cc = cc/ctl,
         perc_sp = sp/ctl) %>% 
  gather(trt_perc, percent_of_ctl , perc_cc:perc_sp) %>% 
  group_by(date, trt_perc) %>% 
  summarize(mean = mean(percent_of_ctl),
            low95 = quantile(percent_of_ctl, probs = 0.025),
            high95 = quantile(percent_of_ctl, probs = 0.975))





testdata2<-data.frame(day_n=seq(134,141,length=7), trt = "ctrl")

model_new_predicts <- data.frame(fitted(benthic_brm,type="response", newdata=testdata2,re_formula=~(1|loc/year),
                            allow_new_levels = TRUE,summary=FALSE,sample_new_levels = "gaussian"))






