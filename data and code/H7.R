# Hypothesis 7: Effect of vomitting anf drug action ----

#### data frame ----
SEvomitingdata_BUG <- BugotoSE2004 %>% 
  dplyr::select(vomit) %>% 
  rename(vomiting = vomit) %>% 
  mutate(CID = BugotoAll$CID)
SEvomitingdata_BUG$vomiting[which(SEvomitingdata_BUG$vomiting==2)] <- 0

Hypo7_BUG <- ClearProb1BG %>% 
  mutate(mean = EggT1meanBG$mean, vomiting = SEvomitingdata_BUG$vomiting, School = "Bugoto LV",
         TF_Clear = ((Clear*93)+0.5)/94)

SEvomitingdata <- MusubiSE2004 %>% 
  dplyr::select(vomit) %>% 
  rename(vomiting = vomit) %>% 
  mutate(CID = MusubiAll$CID)
SEvomitingdata$vomiting[which(SEvomitingdata$vomiting==2)] <- 0

Hypo7_MUS <- ClearProb1 %>% 
  mutate(mean = EggT1mean$mean, vomiting = SEvomitingdata$vomiting, School = "Musubi",
         TF_Clear = ((Clear*67)+0.5)/68)

Hypo7_2schools <- bind_rows(Hypo7_MUS, Hypo7_BUG)

#### statistics ----
vomiting_clearance_beta_BUG <- glmmTMB(data = Hypo7_BUG,
                                       formula = TF_Clear ~ vomiting, 
                                       family=beta_family("logit"))

summary(vomiting_clearance_beta_BUG)
testDispersion(vomiting_clearance_beta_BUG)
simulationH7beta_BUG <- simulateResiduals(fittedModel = vomiting_clearance_beta_BUG, plot = T)

vomiting_clearance_beta_MUS <- glmmTMB(data = Hypo7_MUS,
                                       formula = TF_Clear ~ vomiting, 
                                       family=beta_family("logit"))

summary(vomiting_clearance_beta_MUS)
testDispersion(vomiting_clearance_beta_MUS)
simulationH7beta_MUS <- simulateResiduals(fittedModel = vomiting_clearance_beta_MUS, plot = T)

vomiting_mean_BUG <- glm(data = Hypo7_BUG,
                         vomiting ~ mean, 
                         family = binomial(link='logit'))

summary(vomiting_mean_BUG)

testDispersion(vomiting_mean_BUG)
simulationH702_BUG <- simulateResiduals(fittedModel = vomiting_mean_BUG, plot = T)


vomiting_mean_MUS <- glm(data = Hypo7_MUS,
                         vomiting ~ mean, 
                         family = binomial(link='logit'))

summary(vomiting_mean_MUS)

testDispersion(vomiting_mean_MUS)
simulationH702_MUS <- simulateResiduals(fittedModel = vomiting_mean_MUS, plot = T)

#### Plot ----
vomit_clear_plot_BUG <- SEvomitingdata_BUG %>% 
  mutate(Clearance = ClearProb1BG$Clear, School = "Bugoto LV")
vomit_clear_plot_BUG = melt(vomit_clear_plot_BUG,
                            id.vars = c("CID", "Clearance", "School"),
                            variable.name = "Side_effect",value.name = "status")
vomit_clear_plot_BUG$status[vomit_clear_plot_BUG$status==1]<- "YES"
vomit_clear_plot_BUG$status[vomit_clear_plot_BUG$status==0]<- "NO"
vomit_clear_plot_BUG <- na.omit(vomit_clear_plot_BUG)

vomit_clear_plot_MUS <- SEvomitingdata %>% 
  mutate(Clearance = ClearProb1$Clear, School = "Musubi")
vomit_clear_plot_MUS = melt(vomit_clear_plot_MUS,
                         id.vars = c("CID", "Clearance", "School"),
                         variable.name = "Side_effect",value.name = "status")
vomit_clear_plot_MUS$status[vomit_clear_plot_MUS$status==1]<- "YES"
vomit_clear_plot_MUS$status[vomit_clear_plot_MUS$status==0]<- "NO"
vomit_clear_plot_MUS <- na.omit(vomit_clear_plot_MUS)

PlotH7A <- bind_rows(vomit_clear_plot_BUG, vomit_clear_plot_MUS)

PlotH7A_size <- PlotH7A %>% 
  group_by(Clearance, School, Side_effect, status) %>% 
  tally()

PlotH7A_clear <- ggplot() +
  geom_point(data = PlotH7A_size,
             aes(x = status, y = Clearance, colour = School, size = n), stroke = 1.5, position = position_jitterdodge(0.4), alpha = .4)+
  theme_bw()+
  scale_colour_manual(values = col_2School)+
  scale_fill_manual(values = col_2School)+
  ylab("Clearance Probability")+
  xlab("Vomiting Status")+
  theme(text=element_text(size=13), legend.position = "left")+
  scale_size_continuous(name="Number of Student")

mean_vomiting_MUS <- EggT1mean %>% 
  mutate(vomiting = MusubiSE2004$vomit, School = "Musubi")
mean_vomiting_MUS$vomiting[mean_vomiting_MUS$vomiting==1]<- "YES"
mean_vomiting_MUS$vomiting[mean_vomiting_MUS$vomiting==0]<- "NO"
mean_vomiting_MUS <- na.omit(mean_vomiting_MUS)

mean_vomiting_BUG <- EggT1meanBG %>% 
  mutate(vomiting = SEvomitingdata_BUG$vomiting, School = "Bugoto LV")
mean_vomiting_BUG$vomiting[mean_vomiting_BUG$vomiting==1]<- "YES"
mean_vomiting_BUG$vomiting[mean_vomiting_BUG$vomiting==0]<- "NO"
mean_vomiting_BUG <- na.omit(mean_vomiting_BUG)

mean_vomiting <- bind_rows(mean_vomiting_MUS, mean_vomiting_BUG)

PlotH7B_mean <- ggplot() +
  geom_boxplot(data = mean_vomiting,
               aes(x = vomiting, y = mean, fill = School), alpha=.5, position=position_dodge(1))+
  geom_point(data = mean_vomiting,
             aes(x = vomiting, y = mean, colour = School), alpha=.7, position = position_jitterdodge(0.3))+
  theme_bw()+
  scale_colour_manual(values = col_2School)+
  scale_fill_manual(values = col_2School)+
  ylab("Mean Infection Intensity")+
  xlab("Vomiting Status")+
  theme(text=element_text(size=13), legend.position = "right")

PlotH7 <- (PlotH7A_clear + PlotH7B_mean)

ggsave("PlotH7.pdf")
ggsave("PlotH7.png")
