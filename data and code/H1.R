# Hypothesis 1: Effect of S. mansoni infection probability on the reporting of pre-treatment symptoms ----

# Bugoto LV ----

#### data frame for GLM ----

infection_sta_sym_Bugoto <- BugotoSymp2004 %>% 
  mutate(status = StatusT1BG$ProbInf, TF_Status = ((status*93)+0.5)/94)
infection_sta_sym_Bugoto[infection_sta_sym_Bugoto==2]<-0

infection_sta_sym_Bugoto$urine_pain <- as.factor(infection_sta_sym_Bugoto$urine_pain)
infection_sta_sym_Bugoto$headache <- as.factor(infection_sta_sym_Bugoto$headache)
infection_sta_sym_Bugoto$itching <- as.factor(infection_sta_sym_Bugoto$itching)
infection_sta_sym_Bugoto$diarrhoea <- as.factor(infection_sta_sym_Bugoto$diarrhoea)
infection_sta_sym_Bugoto$nausea <- as.factor(infection_sta_sym_Bugoto$nausea)
infection_sta_sym_Bugoto$blood_stool <- as.factor(infection_sta_sym_Bugoto$blood_stool)
infection_sta_sym_Bugoto$abdom_pain <- as.factor(infection_sta_sym_Bugoto$abdom_pain)

#### GLM run ----

# urine_pain UP
Hypo1UP_BUG <- glmmTMB(data = infection_sta_sym_Bugoto,
                       formula = TF_Status ~ urine_pain, 
                              family=beta_family("logit"))
summary(Hypo1UP_BUG)

testDispersion(Hypo1UP_BUG)
simulationH1UP_BUG <- simulateResiduals(fittedModel = Hypo1UP_BUG, plot = T)

# abdominal pain AP
Hypo1AP_BUG <- glmmTMB(data = infection_sta_sym_Bugoto,
                       formula = TF_Status ~ abdom_pain, 
                       family=beta_family("logit"))
summary(Hypo1AP_BUG)

testDispersion(Hypo1AP_BUG)
simulationH1AP_BUG <- simulateResiduals(fittedModel = Hypo1AP_BUG, plot = T)

# headache HA
Hypo1HA_BUG <- glmmTMB(data = infection_sta_sym_Bugoto,
                       formula = TF_Status ~ headache, 
                       family=beta_family("logit"))
summary(Hypo1HA_BUG)

testDispersion(Hypo1HA_BUG)
simulationH1HA_BUG <- simulateResiduals(fittedModel = Hypo1HA_BUG, plot = T)

# itching IR
Hypo1IR_BUG <- glmmTMB(data = infection_sta_sym_Bugoto,
                       formula = TF_Status ~ itching, 
                       family=beta_family("logit"))
summary(Hypo1IR_BUG)

testDispersion(Hypo1IR_BUG)
simulationH1IR_BUG <- simulateResiduals(fittedModel = Hypo1IR_BUG, plot = T)

# diarrhoea DH
Hypo1DH_BUG <- glmmTMB(data = infection_sta_sym_Bugoto,
                       formula = TF_Status ~ diarrhoea, 
                       family=beta_family("logit"))
summary(Hypo1DH_BUG)

testDispersion(Hypo1DH_BUG)
simulationH1DH_BUG <- simulateResiduals(fittedModel = Hypo1DH_BUG, plot = T)

# nausea NS
Hypo1NS_BUG <- glmmTMB(data = infection_sta_sym_Bugoto,
                       formula = TF_Status ~ nausea, 
                       family=beta_family("logit"))
summary(Hypo1NS_BUG)

testDispersion(Hypo1NS_BUG)
simulationH1NS_BUG <- simulateResiduals(fittedModel = Hypo1NS_BUG, plot = T)

# blood stool BS
Hypo1BS_BUG <- glmmTMB(data = infection_sta_sym_Bugoto,
                       formula = TF_Status ~ blood_stool, 
                       family=beta_family("logit"))
summary(Hypo1BS_BUG)

testDispersion(Hypo1BS_BUG)
simulationH1BS_BUG <- simulateResiduals(fittedModel = Hypo1BS_BUG, plot = T)

##### symptom ~ status -----
# urine_pain UP
Hypo1UP_BUG_bi <- glm(data = infection_sta_sym_Bugoto,
                       formula = urine_pain ~ status, 
                       family=binomial)
summary(Hypo1UP_BUG_bi)

testDispersion(Hypo1UP_BUG_bi)
simulationH1UP_BUG_bi <- simulateResiduals(fittedModel = Hypo1UP_BUG_bi, plot = T)

# abdominal pain AP
Hypo1AP_BUG_bi <- glm(data = infection_sta_sym_Bugoto,
                      formula = abdom_pain ~ status, 
                      family=binomial)
summary(Hypo1AP_BUG_bi)

testDispersion(Hypo1AP_BUG_bi)
simulationH1AP_BUG_bi <- simulateResiduals(fittedModel = Hypo1AP_BUG_bi, plot = T)

# headache HA
Hypo1HA_BUG_bi <- glm(data = infection_sta_sym_Bugoto,
                      formula = headache ~ status, 
                      family=binomial)
summary(Hypo1HA_BUG_bi)

testDispersion(Hypo1HA_BUG_bi)
simulationH1HA_BUG_bi <- simulateResiduals(fittedModel = Hypo1HA_BUG_bi, plot = T)

# itching IR
Hypo1IR_BUG_bi <- glm(data = infection_sta_sym_Bugoto,
                      formula = itching ~ status, 
                      family=binomial)
summary(Hypo1IR_BUG_bi)

testDispersion(Hypo1IR_BUG_bi)
simulationH1IR_BUG_bi <- simulateResiduals(fittedModel = Hypo1IR_BUG_bi, plot = T)

# diarrhoea DH
Hypo1DH_BUG_bi <- glm(data = infection_sta_sym_Bugoto,
                      formula = diarrhoea ~ status, 
                      family=binomial)
summary(Hypo1DH_BUG_bi)

testDispersion(Hypo1DH_BUG_bi)
simulationH1DH_BUG_bi <- simulateResiduals(fittedModel = Hypo1DH_BUG_bi, plot = T)

# nausea NS
Hypo1NS_BUG_bi <- glm(data = infection_sta_sym_Bugoto,
                      formula = nausea ~ status, 
                      family=binomial)
summary(Hypo1NS_BUG_bi)

testDispersion(Hypo1NS_BUG_bi)
simulationH1NS_BUG_bi <- simulateResiduals(fittedModel = Hypo1NS_BUG_bi, plot = T)

# blood stool BS
Hypo1BS_BUG_bi <- glm(data = infection_sta_sym_Bugoto,
                      formula = blood_stool ~ status, 
                      family=binomial)
summary(Hypo1BS_BUG_bi)

testDispersion(Hypo1BS_BUG_bi)
simulationH1BS_BUG_bi <- simulateResiduals(fittedModel = Hypo1BS_BUG_bi, plot = T)


# Musubi ----

#### data frame for GLM ----

infection_sta_sym <- MusubiSymp2004 %>% 
  mutate(status = StatusT1$ProbInf, TF_Status = ((status*67)+0.5)/68)
infection_sta_sym[infection_sta_sym==2]<-0
infection_sta_sym$urine_pain <- as.factor(infection_sta_sym$urine_pain)
infection_sta_sym$headache <- as.factor(infection_sta_sym$headache)
infection_sta_sym$itching <- as.factor(infection_sta_sym$itching)
infection_sta_sym$diarrhoea <- as.factor(infection_sta_sym$diarrhoea)
infection_sta_sym$nausea <- as.factor(infection_sta_sym$nausea)
infection_sta_sym$blood_stool <- as.factor(infection_sta_sym$blood_stool)
infection_sta_sym$abdom_pain <- as.factor(infection_sta_sym$abdom_pain)

#### GLM run ----

# urine_pain UP
Hypo1UP_MUS <- glmmTMB(data = infection_sta_sym,
                       formula = TF_Status ~ urine_pain, 
                       family=beta_family("logit"))
summary(Hypo1UP_MUS)

testDispersion(Hypo1UP_MUS)
simulationH1UP_MUS <- simulateResiduals(fittedModel = Hypo1UP_MUS, plot = T)

# abdominal pain AP
Hypo1AP_MUS <- glmmTMB(data = infection_sta_sym,
                       formula = TF_Status ~ abdom_pain, 
                       family=beta_family("logit"))
summary(Hypo1AP_MUS)

testDispersion(Hypo1AP_MUS)
simulationH1AP_MUS <- simulateResiduals(fittedModel = Hypo1AP_MUS, plot = T)

# headache HA
Hypo1HA_MUS <- glmmTMB(data = infection_sta_sym,
                       formula = TF_Status ~ headache, 
                              family=beta_family("logit"))
summary(Hypo1HA_MUS)

testDispersion(Hypo1HA_MUS)
simulationH1HA_MUS <- simulateResiduals(fittedModel = Hypo1HA_MUS, plot = T)

# itching IR
Hypo1IR_MUS <- glmmTMB(data = infection_sta_sym,
                       formula = TF_Status ~ itching, 
                       family=beta_family("logit"))
summary(Hypo1IR_MUS)

testDispersion(Hypo1IR_MUS)
simulationH1IR_MUS <- simulateResiduals(fittedModel = Hypo1IR_MUS, plot = T)

# diarrhoea DH
Hypo1DH_MUS <- glmmTMB(data = infection_sta_sym,
                       formula = TF_Status ~ diarrhoea, 
                       family=beta_family("logit"))
summary(Hypo1DH_MUS)

testDispersion(Hypo1DH_MUS)
simulationH1DH_MUS <- simulateResiduals(fittedModel = Hypo1DH_MUS, plot = T)

# nausea NS
Hypo1NS_MUS <- glmmTMB(data = infection_sta_sym,
                       formula = TF_Status ~ nausea, 
                       family=beta_family("logit"))
summary(Hypo1NS_MUS)

testDispersion(Hypo1NS_MUS)
simulationH1NS_MUS <- simulateResiduals(fittedModel = Hypo1NS_MUS, plot = T)

# blood stool BS
Hypo1BS_MUS <- glmmTMB(data = infection_sta_sym,
                       formula = TF_Status ~ blood_stool, 
                       family=beta_family("logit"))
summary(Hypo1BS_MUS)

testDispersion(Hypo1BS_MUS)
simulationH1BS_MUS <- simulateResiduals(fittedModel = Hypo1BS_MUS, plot = T)

##### symptom ~ status -----
# urine_pain UP
Hypo1UP_MUS_bi <- glm(data = infection_sta_sym,
                      formula = urine_pain ~ status, 
                      family=binomial)
summary(Hypo1UP_MUS_bi)

testDispersion(Hypo1UP_MUS_bi)
simulationH1UP_MUS_bi <- simulateResiduals(fittedModel = Hypo1UP_MUS_bi, plot = T)

# abdominal pain AP
Hypo1AP_MUS_bi <- glm(data = infection_sta_sym,
                      formula = abdom_pain ~ status, 
                      family=binomial)
summary(Hypo1AP_MUS_bi)

testDispersion(Hypo1AP_MUS_bi)
simulationH1AP_MUS_bi <- simulateResiduals(fittedModel = Hypo1AP_MUS_bi, plot = T)

# headache HA
Hypo1HA_MUS_bi <- glm(data = infection_sta_sym,
                      formula = headache ~ status, 
                      family=binomial)
summary(Hypo1HA_MUS_bi)

testDispersion(Hypo1HA_MUS_bi)
simulationH1HA_MUS_bi <- simulateResiduals(fittedModel = Hypo1HA_MUS_bi, plot = T)

# itching IR
Hypo1IR_MUS_bi <- glm(data = infection_sta_sym,
                      formula = itching ~ status, 
                      family=binomial)
summary(Hypo1IR_MUS_bi)

testDispersion(Hypo1IR_MUS_bi)
simulationH1IR_MUS_bi <- simulateResiduals(fittedModel = Hypo1IR_MUS_bi, plot = T)

# diarrhoea DH
Hypo1DH_MUS_bi <- glm(data = infection_sta_sym,
                      formula = diarrhoea ~ status, 
                      family=binomial)
summary(Hypo1DH_MUS_bi)

testDispersion(Hypo1DH_MUS_bi)
simulationH1DH_MUS_bi <- simulateResiduals(fittedModel = Hypo1DH_MUS_bi, plot = T)

# nausea NS
Hypo1NS_MUS_bi <- glm(data = infection_sta_sym,
                      formula = nausea ~ status, 
                      family=binomial)
summary(Hypo1NS_MUS_bi)

testDispersion(Hypo1NS_MUS_bi)
simulationH1NS_MUS_bi <- simulateResiduals(fittedModel = Hypo1NS_MUS_bi, plot = T)

# blood stool BS
Hypo1BS_MUS_bi <- glm(data = infection_sta_sym,
                      formula = blood_stool ~ status, 
                      family=binomial)
summary(Hypo1BS_MUS_bi)

testDispersion(Hypo1BS_MUS_bi)
simulationH1BS_MUS_bi <- simulateResiduals(fittedModel = Hypo1BS_MUS_bi, plot = T)

#### plot ----

PlotH1BUG <- as.data.frame(infection_sta_sym_Bugoto) %>% 
  rename(IP = "status", TFIP = "TF_Status", 'abdominal pain' = abdom_pain,
         'urinate pain' = urine_pain, 'blood stool' = blood_stool)
PlotH1BUG = melt(PlotH1BUG,
              id.vars = c("School", "CID", "IP", "TFIP"),
              variable.name = "Symptoms", value.name = "status")
PlotH1BUG <- na.omit(PlotH1BUG)
PlotH1BUG$status[PlotH1BUG$status==1]<- "yes"
PlotH1BUG$status[PlotH1BUG$status==0]<- "no"

PlotH1MUS <- as.data.frame(infection_sta_sym) %>% 
  rename(IP = "status", TFIP = "TF_Status", 'abdominal pain' = abdom_pain,
         'urinate pain' = urine_pain, 'blood stool' = blood_stool)
PlotH1MUS = melt(PlotH1MUS,
                 id.vars = c("School", "CID", "IP", "TFIP"),
                 variable.name = "Symptoms", value.name = "status")
PlotH1MUS <- na.omit(PlotH1MUS)
PlotH1MUS$status[PlotH1MUS$status==1]<- "yes"
PlotH1MUS$status[PlotH1MUS$status==0]<- "no"

PlotH1 <- bind_rows(PlotH1BUG, PlotH1MUS)
PlotH1$status <- factor(PlotH1$status, levels = c("yes", "no"))

PlotH1$Symptoms <- factor(PlotH1$Symptoms, levels = c("abdominal pain", "blood stool", "diarrhoea", "headache", "itching", "nausea", "urinate pain"))

ggplot()+
  geom_point(data = PlotH1,
             aes(x = Symptoms, y = IP, colour = status), position = position_jitter(width = 0.3, height = 0.01), alpha = 0.3)+
    theme_bw()+
    scale_colour_manual(values=col_status, name="Report symptoms")+
    ylab("Infection Probability")+
    xlab("Symptoms")+
  facet_grid(School~., labeller = labeller_musubi)

ggsave("PlotH1.pdf")
ggsave("PlotH1.png")
