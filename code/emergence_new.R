emergence_2019_summary <- emergence_2019_new %>% 
  mutate_at(c(4:30), ~replace(., is.na(.), 0)) %>% 
  gather(taxa, abun, anisoptera:zygoptera) %>% 
  group_by(taxa) %>% 
  summarise(tot = sum(abun))

emerge_2019_chiro_new <- emergence_2019_new %>%
  mutate_at(c(4:30), ~replace(., is.na(.), 0)) %>% 
  gather(taxa, abun, anisoptera:zygoptera) %>% 
  filter(taxa == "chiro") %>% 
  mutate(jstart = yday(start),
         jend = yday(collect),
         days = jend - jstart,
         corrected_abun = abun/days)

emerge_2019_chiro_new %>% 
  summarise(tot = sum(abun))

emerge_2019_chiro_new$corrected_abun <- as.numeric(emerge_2019_chiro_new$corrected_abun)

emerge_2019_chiro_new$corrected_abun[emerge_2019_chiro_new$corrected_abun == 0] <- 0.00001
  
ggplot(data = emerge_2019_chiro_new, aes(x = jend, y = corrected_abun, color = location))+
  geom_point()+
  facet_grid(location~., scales = "free_y")


emerge_new_2019_gam <- brm(corrected_abun~s(jend, by=location) + location,
                           data=emerge_2019_chiro_new, 
                           family=Gamma(link="log"),
                           chains=4, cores=4)

print(emerge_new_2019_gam)
marginal_effects(emerge_new_2019_gam)

test <- emerge_2019_chiro_new %>% 
  filter(location == "spiritmound")

unique(test$jend)

#lost early emergence samples for burbank (sent for Katy's study)

nd_gun_new = data.frame(jend = seq(from = 161, to = 207, length.out = 47), location = "gunderson")

nd_bur_new = data.frame(jend = seq(from = 161, to = 206, length.out = 46), location = "burbank")

nd_lb_new = data.frame(jend = seq(from = 155, to = 206, length.out = 52), location = "littlebridge")

nd_sm_new = data.frame(jend = seq(from = 161, to = 207, length.out = 47), location = "spiritmound")


gam_mod_post_gun_sum_new <- data.frame(fitted(emerge_new_2019_gam, newdata=nd_gun_new, summary=T, re_formula=NA,nsamples=1000))

gam_mod_post_bur_sum_new <- data.frame(fitted(emerge_new_2019_gam, newdata=nd_bur_new, summary=T, re_formula=NA,nsamples=1000))

gam_mod_post_lb_sum_new <- data.frame(fitted(emerge_new_2019_gam, newdata=nd_lb_new, summary=T, re_formula=NA,nsamples=1000))

gam_mod_post_sm_sum_new <- data.frame(fitted(emerge_new_2019_gam, newdata=nd_sm_new, summary=T, re_formula=NA,nsamples=1000))




gam_mod_post_gun_sum_new <- gam_mod_post_gun_sum_new %>% 
  mutate(location = "gunderson",
         julian_date = seq(from = 161, to = 207, length.out = 47))

gam_mod_post_bur_sum_new <- gam_mod_post_bur_sum_new %>% 
  mutate(location = "burbank",
         julian_date = seq(from = 161, to = 206, length.out = 46))

gam_mod_post_lb_sum_new <- gam_mod_post_lb_sum_new %>% 
  mutate(location = "littlebridge",
         julian_date = seq(from = 155, to = 206, length.out = 52))

gam_mod_post_sm_sum_new <- gam_mod_post_sm_sum_new %>% 
  mutate(location = "spiritmound",
         julian_date = seq(from = 161, to = 207, length.out = 47))


gam_mod_post_all_sum_new <- rbind(gam_mod_post_gun_sum_new, gam_mod_post_bur_sum_new,
                                  gam_mod_post_lb_sum_new, gam_mod_post_sm_sum_new)


emerge_2019_new_fig <- ggplot()+
  geom_ribbon(data = gam_mod_post_all_sum_new, 
              aes(x = julian_date, y = Estimate, ymax=Q97.5, ymin=Q2.5, fill = location))+
  geom_line(data = gam_mod_post_all_sum_new, 
            aes(x = julian_date, y = Estimate, group = location), alpha = 0.4)+
  geom_point(data = emerge_2019_chiro_new, aes(x = jend, y = corrected_abun, fill = location),
             size = 1)+
  facet_grid(location~., scales = "free_y")+
  xlab("Julian Date")+
  ylab("Estimate (emergent chironomid abundance/day)")+
  scale_fill_grey(labels = c("Burbank", "Gunderson", "Little Bridge", "Spirit Mound"),
                  start = 0.4)+
  labs(fill = "Location")+
  theme_classic()+
  theme(strip.text = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        text = element_text(size = 8))

ggsave(emerge_2019_new_fig, file = "emerge_2019_new_fig.jpeg", 
       dpi=600, width=6.5, height=3, units="in" )

emerge_2019_new_fig_use <- tag_facet2(emerge_2019_new_fig, size = 2, hjust = -0.1)

ggsave(emerge_2019_new_fig_use, file = "emerge_2019_new_fig_use.jpeg", 
       dpi=600, width=6.5, height=3, units="in" )

emerge_2019_new_fig_use

ggplot()+
  geom_ribbon(data = gam_mod_post_gun_sum_new, aes(x = julian_date, y = Estimate, ymax=Q97.5, ymin=Q2.5))+
  geom_line(data = gam_mod_post_gun_sum_new, aes(x = julian_date, y = Estimate), alpha = 0.4)+
  xlab("Julian Date")+
  ylab("Estimate (emergent chironomid abundance/day)")+
  theme(strip.text = element_blank())

ggplot()+
  geom_ribbon(data = gam_mod_post_bur_sum_new, aes(x = julian_date, y = Estimate, ymax=Q97.5, ymin=Q2.5))+
  geom_line(data = gam_mod_post_bur_sum_new, aes(x = julian_date, y = Estimate), alpha = 0.4)+
  xlab("Julian Date")+
  ylab("Estimate (emergent chironomid abundance/day)")+
  theme(strip.text = element_blank())

ggplot()+
  geom_ribbon(data = gam_mod_post_lb_sum_new, aes(x = julian_date, y = Estimate, ymax=Q97.5, ymin=Q2.5))+
  geom_line(data = gam_mod_post_lb_sum_new, aes(x = julian_date, y = Estimate), alpha = 0.4)+
  xlab("Julian Date")+
  ylab("Estimate (emergent chironomid abundance/day)")+
  theme(strip.text = element_blank())

ggplot()+
  geom_ribbon(data = gam_mod_post_sm_sum_new, aes(x = julian_date, y = Estimate, ymax=Q97.5, ymin=Q2.5))+
  geom_line(data = gam_mod_post_sm_sum_new, aes(x = julian_date, y = Estimate), alpha = 0.4)+
  xlab("Julian Date")+
  ylab("Estimate (emergent chironomid abundance/day)")+
  theme(strip.text = element_blank())

#peaks
#gunderson: 1st- day 161 (start), 2nd- day 207 (end)
#burbank: 1st- day 169, 2nd- none, decreases after first peak
#littlbridge: 1st- day 166, 2nd- day 206 (slight increase at end, not really a "peak")
#spiritmound: 1st- day 161 (starts a bit higher and decreases), 2nd- day 195

emerge_2019_new_figtag <- tag_facet(emerge_2019_new_fig, x = 200, size = 20)

ggsave(emerge_2019_new_figtag, file = "emerge_2019_new_figtag.jpeg", dpi=600, width=22, height=22, units="in" )


gam_mod_post_all_sum_new2 <- gam_mod_post_all_sum_new %>% 
  left_join(site_type)

backwater_emergence <- ggplot()+
  geom_ribbon(data = subset(gam_mod_post_all_sum_new2, type == "backwater"), 
              aes(x = julian_date, y = Estimate, ymax=Q97.5, ymin=Q2.5, fill = location))+
  geom_line(data = subset(gam_mod_post_all_sum_new2, type == "backwater"),
            aes(x = julian_date, y = Estimate, group = location), alpha = 0.4)+
  facet_grid(.~location, scales = "free_y")+
  xlab("Julian Date")+
  ylab("Estimate (emergent chironomid abundance/day)")+
  scale_fill_grey(labels = c("Burbank", "Gunderson"), start = 0.5)+
  labs(fill = "Location")+
  theme_classic()+
  theme(strip.text = element_blank(),
        axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        text = element_text(size = 35))


ggsave(backwater_emergence, file = "backwater_emergence.jpeg", dpi=600, width=30, height=12, units="in" )

backwater_emergence_use <- tag_facet2(backwater_emergence, size = 20, hjust = -10)

ggsave(backwater_emergence_use, file = "backwater_emergence_use.jpeg", dpi=72, width=30, height=12, units="in" )


stream_emergence <- ggplot()+
  geom_ribbon(data = subset(gam_mod_post_all_sum_new2, type == "stream"), 
              aes(x = julian_date, y = Estimate, ymax=Q97.5, ymin=Q2.5, fill = location))+
  geom_line(data = subset(gam_mod_post_all_sum_new2, type == "stream"),
            aes(x = julian_date, y = Estimate, group = location), alpha = 0.4)+
  facet_grid(.~location, scales = "free_y")+
  xlab("Julian Date")+
  ylab("Estimate (emergent chironomid abundance/day)")+
  scale_fill_grey(labels = c("Little Bridge", "Spirit Mound"), start = 0.5)+
  labs(fill = "Location")+
  theme_classic()+
  theme(strip.text = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        text = element_text(size = 8))

ggsave(stream_emergence, file = "stream_emergence.jpeg", 
       dpi=600, width=6.5, height=4, units="in" )

stream_emergence_use <- tag_facet2(stream_emergence, size = 2, hjust = -10)

ggsave(stream_emergence_use, file = "stream_emergence_use.jpeg", 
       dpi=600, width=6.5, height=4, units="in" )



