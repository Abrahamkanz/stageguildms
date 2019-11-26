
chiro_only_gam_mod2 <- brm(mg_dm_m~s(day, by=trt) + (1|tank) + trt, data=chiro_only2, family=Gamma(link="log"),
                          chains=4, cores=4)





print(chiro_only_gam_mod2)
plot(marginal_effects(chiro_only_gam_mod2),points=T)




nd_chiro2 = data.frame(day = seq(from=159, to =218, length.out=60),
                      trt = "ctrl")
nd_s_chiro2 = data.frame(day = seq(from=159, to =218, length.out=60),
                        trt = "spot")
nd_c_chiro2 = data.frame(day = seq(from=159, to =218, length.out=60),
                        trt = "carp")




chiro_mod_fit_ctrl2 <- data.frame(fitted(chiro_only_gam_mod2, newdata=nd_chiro2, summary=T, re_formula=NA),
                                 trt="ctrl", day=seq(from=159, to =218, length.out=60))

chiro_mod_fit_carp2 <- data.frame(fitted(chiro_only_gam_mod2, newdata = nd_c_chiro2, summary=T, re_formula=NA), trt="carp", day=seq(from=159, to =218, length.out=60))

chiro_mod_fit_spot2 <- data.frame(fitted(chiro_only_gam_mod2, newdata=nd_s_chiro2, summary=T, re_formula=NA), trt="spot", day=seq(from=159, to =218, length.out=60))




chiro_nd_full2 <-rbind(nd_chiro2, nd_c_chiro2, nd_s_chiro2)

chiro_mod_fitted2 <- fitted(chiro_only_gam_mod2, new_data=chiro_nd_full2, summary=T, re_formula=NA)

chiro_only_gam_mod_fit_all2 <- rbind(chiro_mod_fit_ctrl2, chiro_mod_fit_carp2, chiro_mod_fit_spot2)

chiro_only2_mod$mg_dm_m <- chiro_only2$mg_dm_m

chiro_only_gam_mod_fit_all2$trt_new <- factor(chiro_only_gam_mod_fit_all2$trt, levels = c("ctrl", "carp", "spot"))

chiro_only2_mod$trt_new <- factor(chiro_only2_mod$trt, levels = c("ctrl", "carp", "spot"))

chiro_only_plot2 <- ggplot()+
  geom_ribbon(data=chiro_only_gam_mod_fit_all2, aes(x=day, y=Estimate, ymax=Q97.5, ymin=Q2.5, fill=trt_new),alpha=0.4)+
  geom_line(data=chiro_only_gam_mod_fit_all2, aes(x=day, y=Estimate, group=trt_new), alpha=0.4)+
  xlab("Julian Date (day of year)")+
  ylab("Emergence (mg dry mass/m^2/day)")+
  scale_fill_grey(name = "Treatment\n(Model)")+
  scale_color_grey(name = "Treatment\n(Raw Data)", breaks = c("ctrl", "carp", "spot"))+
  theme_classic()+
  theme(axis.title = element_text(size=30),
        text = element_text(size=30))+
  geom_point(data = chiro_only2_mod, aes(x = day, y = mg_dm_m, color = trt_new))+
  facet_wrap(.~trt_new)+
  NULL


ggsave(chiro_only_plot2, file="chiro_only_plot2.jpg", dpi=600, width=20, height=8, units="in")


chiro_only_gam_mod_fit_all2 %>% 
  group_by(trt) %>% 
  summarize(med = median(Estimate),
            low95 = quantile(Estimate, probs = 0.025),
            high95 = quantile(Estimate, probs= 0.975), 
            tot = sum(Estimate),
            tothigh95 = sum(Q97.5),
            totlow95 = sum(Q2.5))







