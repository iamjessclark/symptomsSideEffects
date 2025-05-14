# Hypothesis 3: Effect of S. mansoni infection probability on the reporting of post-treatment side effects ----

# Bugoto LV ----

#### data frame for GLM ----

infection_sta_se_BUG <- BugotoSE2004 %>% 
  dplyr::select(CID, abdom_pain, headache, diarrhoea, rash, urine_pain, nausea, blood_stool) %>% 
  rename(itching = rash) %>% 
  mutate(status = StatusT1BG$ProbInf, TF_Status = ((status*93)+0.5)/94, School = 'Bugoto LV')

infection_sta_se_BUG$urine_pain <- as.factor(infection_sta_se_BUG$urine_pain)
infection_sta_se_BUG$headache <- as.factor(infection_sta_se_BUG$headache)
infection_sta_se_BUG$itching <- as.factor(infection_sta_se_BUG$itching)
infection_sta_se_BUG$diarrhoea <- as.factor(infection_sta_se_BUG$diarrhoea)
infection_sta_se_BUG$nausea <- as.factor(infection_sta_se_BUG$nausea)
infection_sta_se_BUG$blood_stool <- as.factor(infection_sta_se_BUG$blood_stool)
infection_sta_se_BUG$abdom_pain <- as.factor(infection_sta_se_BUG$abdom_pain)

#### GLM run ----

# abdominal pain AP
Hypo2AP_BUG <- glmmTMB(data = infection_sta_se_BUG,
                       formula = TF_Status ~ abdom_pain, 
                       family=beta_family("logit"))
summary(Hypo2AP_BUG)

testDispersion(Hypo2AP_BUG)
simulationH2AP_BUG <- simulateResiduals(fittedModel = Hypo2AP_BUG, plot = T)

# headache HA
Hypo2HA_BUG <- glmmTMB(data = infection_sta_se_BUG,
                       formula = TF_Status ~ headache, 
                       family=beta_family("logit"))
summary(Hypo2HA_BUG)

testDispersion(Hypo2HA_BUG)
simulationH2HA_BUG <- simulateResiduals(fittedModel = Hypo2HA_BUG, plot = T)

# diarrhoea DH
Hypo2DH_BUG <- glmmTMB(data = infection_sta_se_BUG,
                       formula = TF_Status ~ diarrhoea, 
                       family=beta_family("logit"))
summary(Hypo2DH_BUG)

testDispersion(Hypo2DH_BUG)
simulationH2DH_BUG <- simulateResiduals(fittedModel = Hypo2DH_BUG, plot = T)

##### side effect ~ status -----
# abdominal pain AP
Hypo2AP_BUG_bi <- glm(data = infection_sta_se_BUG,
                      formula = abdom_pain ~ status, 
                      family=binomial)
summary(Hypo2AP_BUG_bi)

testDispersion(Hypo2AP_BUG_bi)
simulationH1AP_BUG_bi <- simulateResiduals(fittedModel = Hypo2AP_BUG_bi, plot = T)

# headache HA
Hypo2HA_BUG_bi <- glm(data = infection_sta_se_BUG,
                      formula = headache ~ status, 
                      family=binomial)
summary(Hypo2HA_BUG_bi)

testDispersion(Hypo2HA_BUG_bi)
simulationH1HA_BUG_bi <- simulateResiduals(fittedModel = Hypo2HA_BUG_bi, plot = T)

# diarrhoea DH
Hypo2DH_BUG_bi <- glm(data = infection_sta_se_BUG,
                      formula = diarrhoea ~ status, 
                      family=binomial)
summary(Hypo2DH_BUG_bi)

testDispersion(Hypo2DH_BUG_bi)
simulationH1DH_BUG_bi <- simulateResiduals(fittedModel = Hypo2DH_BUG_bi, plot = T)

# Musubi ----

#### data frame for GLM ----

infection_sta_se <- MusubiSE2004 %>% 
  dplyr::select(CID, abdom_pain, headache, diarrhoea, rash, urine_pain, nausea, blood_stool) %>% 
  rename(itching = rash) %>% 
  mutate(status = StatusT1$ProbInf, TF_Status = ((status*67)+0.5)/68,  School = 'Musubi')

infection_sta_se$urine_pain <- as.factor(infection_sta_se$urine_pain)
infection_sta_se$headache <- as.factor(infection_sta_se$headache)
infection_sta_se$itching <- as.factor(infection_sta_se$itching)
infection_sta_se$diarrhoea <- as.factor(infection_sta_se$diarrhoea)
infection_sta_se$nausea <- as.factor(infection_sta_se$nausea)
infection_sta_se$blood_stool <- as.factor(infection_sta_se$blood_stool)
infection_sta_se$abdom_pain <- as.factor(infection_sta_se$abdom_pain)

#### GLM run ----

# abdominal pain AP
Hypo2AP_MUS <- glmmTMB(data = infection_sta_se,
                       formula = TF_Status ~ abdom_pain, 
                       family=beta_family("logit"))
summary(Hypo2AP_MUS)


testDispersion(Hypo2AP_MUS)
simulationH2AP_MUS <- simulateResiduals(fittedModel = Hypo2AP_MUS, plot = T)

# headache HA
Hypo2HA_MUS <- glmmTMB(data = infection_sta_se,
                       formula = TF_Status ~ headache, 
                       family=beta_family("logit"))
summary(Hypo2HA_MUS)


testDispersion(Hypo2HA_MUS)
simulationH2HA_MUS <- simulateResiduals(fittedModel = Hypo2HA_MUS, plot = T)

# diarrhoea DH
Hypo2DH_MUS <- glmmTMB(data = infection_sta_se,
                       formula = TF_Status ~ diarrhoea, 
                       family=beta_family("logit"))
summary(Hypo2DH_MUS)


testDispersion(Hypo2DH_MUS)
simulationH2DH_MUS <- simulateResiduals(fittedModel = Hypo2DH_MUS, plot = T)

##### side effect ~ status -----
# abdominal pain AP
Hypo2AP_MUS_bi <- glm(data = infection_sta_se,
                      formula = abdom_pain ~ status, 
                      family=binomial)
summary(Hypo2AP_MUS_bi)

testDispersion(Hypo2AP_MUS_bi)
simulationH1AP_MUS_bi <- simulateResiduals(fittedModel = Hypo2AP_MUS_bi, plot = T)

# headache HA
Hypo2HA_MUS_bi <- glm(data = infection_sta_se,
                      formula = headache ~ status, 
                      family=binomial)
summary(Hypo2HA_MUS_bi)

testDispersion(Hypo2HA_MUS_bi)
simulationH1HA_MUS_bi <- simulateResiduals(fittedModel = Hypo2HA_MUS_bi, plot = T)

# diarrhoea DH
Hypo2DH_MUS_bi <- glm(data = infection_sta_se,
                      formula = diarrhoea ~ status, 
                      family=binomial)
summary(Hypo2DH_MUS_bi)

testDispersion(Hypo2DH_MUS_bi)
simulationH1DH_MUS_bi <- simulateResiduals(fittedModel = Hypo2DH_MUS_bi, plot = T)

#### plot ----

PlotH2BUG <- as.data.frame(infection_sta_se_BUG) %>% 
  rename(IP = "status", TFIP = "TF_Status", 'abdominal pain' = abdom_pain,
         'urinate pain' = urine_pain, 'blood stool' = blood_stool)
PlotH2BUG = melt(PlotH2BUG,
                 id.vars = c("School", "CID", "IP", "TFIP"),
                 variable.name = "Side_Effects", value.name = "status")
PlotH2BUG <- na.omit(PlotH2BUG)
PlotH2BUG$status[PlotH2BUG$status==1]<- "yes"
PlotH2BUG$status[PlotH2BUG$status==0]<- "no"

PlotH2MUS <- as.data.frame(infection_sta_se) %>% 
  rename(IP = "status", TFIP = "TF_Status", 'abdominal pain' = abdom_pain,
         'urinate pain' = urine_pain, 'blood stool' = blood_stool)
PlotH2MUS = melt(PlotH2MUS,
                 id.vars = c("School", "CID", "IP", "TFIP"),
                 variable.name = "Side_Effects", value.name = "status")
PlotH2MUS <- na.omit(PlotH2MUS)
PlotH2MUS$status[PlotH2MUS$status==1]<- "yes"
PlotH2MUS$status[PlotH2MUS$status==0]<- "no"

PlotH2 <- bind_rows(PlotH2BUG, PlotH2MUS)
PlotH2$status <- factor(PlotH2$status, levels = c("yes", "no"))

PlotH2$Side_Effects <- factor(PlotH2$Side_Effects, levels = c("abdominal pain", "blood stool", "diarrhoea", "headache", "itching", "nausea", "urinate pain"))

ggplot()+
  geom_point(data = PlotH2,
             aes(x = Side_Effects, y = IP, colour = status), position = position_jitter(width = 0.3, height = 0.01), alpha = 0.3)+
  theme_bw()+
  scale_colour_manual(values=col_status, name="Report side effect")+
  ylab("Infection Probability")+
  xlab("Side Effects")+
  facet_grid(School~., labeller = labeller_musubi)

ggsave("PlotH2.pdf")
ggsave("PlotH2.png")
