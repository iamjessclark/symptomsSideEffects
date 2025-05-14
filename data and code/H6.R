# Question 6: Effect of all the other side effects (expect vomiting) on drug efficacy ----

# Bugoto LV ----

#### Dataframes ----

SEdata_BUG <- BugotoSE2004 %>% 
  dplyr::select(abdom_pain, headache, diarrhoea, rash, urine_pain, nausea, blood_stool, vomit) %>% 
  mutate(CID = BugotoSE2004$CID)
SEdata_BUG$abdom_pain[which(SEdata_BUG$abdom_pain==2)] <- 0
SEdata_BUG$headache[which(SEdata_BUG$headache==2)] <- 0
SEdata_BUG$diarrhoea[which(SEdata_BUG$diarrhoea==2)] <- 0
SEdata_BUG$rash[which(SEdata_BUG$rash==2)] <- 0
SEdata_BUG$urine_pain[which(SEdata_BUG$urine_pain==2)] <- 0
SEdata_BUG$nausea[which(SEdata_BUG$nausea==2)] <- 0
SEdata_BUG$blood_stool[which(SEdata_BUG$blood_stool==2)] <- 0
SEdata_BUG$vomit[which(SEdata_BUG$vomit==2)] <- 0

H6_se_clr_BUG <- ClearProb1BG %>% 
  mutate(abdom_pain = SEdata_BUG$abdom_pain, headache = SEdata_BUG$headache, 
         diarrhoea = SEdata_BUG$diarrhoea, rash = SEdata_BUG$rash,
         urine_pain = SEdata_BUG$urine_pain, nausea = SEdata_BUG$nausea,
         blood_stool = SEdata_BUG$blood_stool, vomit = SEdata_BUG$vomit, School = "Bugoto LV",
         number = c(abdom_pain+headache+diarrhoea+rash+urine_pain+nausea+blood_stool+vomit),
         TF_Clear = ((Clear*93)+0.5)/94)

H6_se_clr_BUG$number <- as.factor(H6_se_clr_BUG$number)

SMSEdata_BUG <- BugotoSE2004 %>% 
  dplyr::select(abdom_pain, headache, diarrhoea, rash, urine_pain, nausea, blood_stool, vomit, dizzy, breathing,
                swelling, weakness) %>% 
  mutate(CID = BugotoSE2004$CID)
SMSEdata_BUG$abdom_pain[which(SMSEdata_BUG$abdom_pain==2)] <- 0
SMSEdata_BUG$headache[which(SMSEdata_BUG$headache==2)] <- 0
SMSEdata_BUG$diarrhoea[which(SMSEdata_BUG$diarrhoea==2)] <- 0
SMSEdata_BUG$rash[which(SMSEdata_BUG$rash==2)] <- 0
SMSEdata_BUG$urine_pain[which(SMSEdata_BUG$urine_pain==2)] <- 0
SMSEdata_BUG$nausea[which(SMSEdata_BUG$nausea==2)] <- 0
SMSEdata_BUG$blood_stool[which(SMSEdata_BUG$blood_stool==2)] <- 0
SMSEdata_BUG$vomit[which(SMSEdata_BUG$vomit==2)] <- 0
SMSEdata_BUG$dizzy[which(SMSEdata_BUG$dizzy==2)] <- 0
SMSEdata_BUG$breathing[which(SMSEdata_BUG$breathing==2)] <- 0
SMSEdata_BUG$swelling[which(SMSEdata_BUG$swelling==2)] <- 0
SMSEdata_BUG$weakness[which(SMSEdata_BUG$weakness==2)] <- 0

SMH6_se_clr_BUG <- ClearProb1BG %>% 
  mutate(abdom_pain = SMSEdata_BUG$abdom_pain, headache = SMSEdata_BUG$headache, 
         diarrhoea = SMSEdata_BUG$diarrhoea, rash = SMSEdata_BUG$rash,
         urine_pain = SMSEdata_BUG$urine_pain, nausea = SMSEdata_BUG$nausea,
         blood_stool = SMSEdata_BUG$blood_stool, vomit = SMSEdata_BUG$vomit, 
         dizzy = SMSEdata_BUG$dizzy, breathing = SMSEdata_BUG$breathing,
         swelling = SMSEdata_BUG$swelling, weakness = SMSEdata_BUG$weakness, School = "Bugoto LV",
         number = c(abdom_pain+headache+diarrhoea+rash+urine_pain+nausea+blood_stool+vomit+dizzy+breathing+swelling+weakness), 
         TF_Clear = ((Clear*93)+0.5)/94)

SMH6_se_clr_BUG$number <- as.factor(SMH6_se_clr_BUG$number)

#### Statistics ----

###### Bugoto se clearance ----

# Abdominal Pain
abdpain_clearance_beta_BUG <- glmmTMB(data = SMH6_se_clr_BUG,
                                formula = TF_Clear ~ abdom_pain, 
                                family=beta_family("logit"))

summary(abdpain_clearance_beta_BUG)
testDispersion(abdpain_clearance_beta_BUG)
simulationH6beta_abdpain_BUG <- simulateResiduals(fittedModel = abdpain_clearance_beta_BUG, plot = T)

# Headache
headache_clearance_beta_BUG <- glmmTMB(data = SMH6_se_clr_BUG,
                                      formula = TF_Clear ~ headache, 
                                      family=beta_family("logit"))

summary(headache_clearance_beta_BUG)
testDispersion(headache_clearance_beta_BUG)
simulationH6beta_headache_BUG <- simulateResiduals(fittedModel = headache_clearance_beta_BUG, plot = T)

# Diarrhoea
diarrhoea_clearance_beta_BUG <- glmmTMB(data = SMH6_se_clr_BUG,
                                       formula = TF_Clear ~ diarrhoea, 
                                       family=beta_family("logit"))

summary(diarrhoea_clearance_beta_BUG)
testDispersion(diarrhoea_clearance_beta_BUG)
simulationH6beta_diarrhoea_BUG <- simulateResiduals(fittedModel = diarrhoea_clearance_beta_BUG, plot = T)

# Dizzy <- go to SM
dizzy_clearance_beta_BUG <- glmmTMB(data = SMH6_se_clr_BUG,
                                    formula = TF_Clear ~ dizzy, 
                                    family=beta_family("logit"))

summary(dizzy_clearance_beta_BUG)
testDispersion(dizzy_clearance_beta_BUG)
simulationH6beta_dizzy_BUG <- simulateResiduals(fittedModel = dizzy_clearance_beta_BUG, plot = T)

# breathing <- go to SM
breathing_clearance_beta_BUG <- glmmTMB(data = SMH6_se_clr_BUG,
                                    formula = TF_Clear ~ breathing, 
                                    family=beta_family("logit"))

summary(breathing_clearance_beta_BUG)
testDispersion(breathing_clearance_beta_BUG)
simulationH6beta_breathing_BUG <- simulateResiduals(fittedModel = breathing_clearance_beta_BUG, plot = T)

###### Bugoto num clearance (vomiting considered) ---- 
SE_num_clearance_BUG <- glmmTMB(data = H6_se_clr_BUG,
                                        formula = TF_Clear ~ number, 
                                        family=beta_family("logit"))

summary(SE_num_clearance_BUG)
testDispersion(SE_num_clearance_BUG)
simulationH6se_num_BUG <- simulateResiduals(fittedModel = SE_num_clearance_BUG, plot = T)

# Musubi ----

#### data frame ----

SEdata_MUS <- MusubiSE2004 %>% 
  dplyr::select(abdom_pain, headache, diarrhoea, rash, urine_pain, nausea, blood_stool, vomit) %>% 
  mutate(CID = MusubiAll$CID)
SEdata_MUS$abdom_pain[which(SEdata_MUS$abdom_pain==2)] <- 0
SEdata_MUS$headache[which(SEdata_MUS$headache==2)] <- 0
SEdata_MUS$diarrhoea[which(SEdata_MUS$diarrhoea==2)] <- 0
SEdata_MUS$rash[which(SEdata_MUS$rash==2)] <- 0
SEdata_MUS$urine_pain[which(SEdata_MUS$urine_pain==2)] <- 0
SEdata_MUS$nausea[which(SEdata_MUS$nausea==2)] <- 0
SEdata_MUS$blood_stool[which(SEdata_MUS$blood_stool==2)] <- 0
SEdata_MUS$vomit[which(SEdata_MUS$vomit==2)] <- 0

H6_se_clr_MUS <- ClearProb1 %>% 
  mutate(abdom_pain = SEdata_MUS$abdom_pain, headache = SEdata_MUS$headache, 
         diarrhoea = SEdata_MUS$diarrhoea, rash = SEdata_MUS$rash,
         urine_pain = SEdata_MUS$urine_pain, nausea = SEdata_MUS$nausea,
         blood_stool = SEdata_MUS$blood_stool, vomit = SEdata_MUS$vomit, School = "Musubi",
         number = c(abdom_pain+headache+diarrhoea+rash+urine_pain+nausea+blood_stool+vomit),
         TF_Clear = ((Clear*67)+0.5)/68)

H6_se_clr_MUS$number <- as.factor(H6_se_clr_MUS$number)

H6_se_clr <- bind_rows(H6_se_clr_BUG, H6_se_clr_MUS)

SMSEdata_MUS <- MusubiSE2004 %>% 
  dplyr::select(abdom_pain, headache, diarrhoea, rash, urine_pain, nausea, blood_stool, vomit, dizzy, breathing,
                swelling, weakness) %>% 
  mutate(CID = MusubiAll$CID)
SMSEdata_MUS$abdom_pain[which(SMSEdata_MUS$abdom_pain==2)] <- 0
SMSEdata_MUS$headache[which(SMSEdata_MUS$headache==2)] <- 0
SMSEdata_MUS$diarrhoea[which(SMSEdata_MUS$diarrhoea==2)] <- 0
SMSEdata_MUS$rash[which(SMSEdata_MUS$rash==2)] <- 0
SMSEdata_MUS$urine_pain[which(SMSEdata_MUS$urine_pain==2)] <- 0
SMSEdata_MUS$nausea[which(SMSEdata_MUS$nausea==2)] <- 0
SMSEdata_MUS$blood_stool[which(SMSEdata_MUS$blood_stool==2)] <- 0
SMSEdata_MUS$vomit[which(SMSEdata_MUS$vomit==2)] <- 0
SMSEdata_MUS$dizzy[which(SMSEdata_MUS$dizzy==2)] <- 0
SMSEdata_MUS$breathing[which(SMSEdata_MUS$breathing==2)] <- 0
SMSEdata_MUS$swelling[which(SMSEdata_MUS$swelling==2)] <- 0
SMSEdata_MUS$weakness[which(SMSEdata_MUS$weakness==2)] <- 0

SMH6_se_clr_MUS <- ClearProb1 %>% 
  mutate(abdom_pain = SMSEdata_MUS$abdom_pain, headache = SMSEdata_MUS$headache, 
         diarrhoea = SMSEdata_MUS$diarrhoea, rash = SMSEdata_MUS$rash,
         urine_pain = SMSEdata_MUS$urine_pain, nausea = SMSEdata_MUS$nausea,
         blood_stool = SMSEdata_MUS$blood_stool, vomit = SMSEdata_MUS$vomit, 
         dizzy = SMSEdata_MUS$dizzy, breathing = SMSEdata_MUS$breathing,
         swelling = SMSEdata_MUS$swelling, weakness = SMSEdata_MUS$weakness,School = "Musubi",
         number = c(abdom_pain+headache+diarrhoea+rash+urine_pain+nausea+blood_stool+vomit+dizzy+breathing+swelling+weakness),
         TF_Clear = ((Clear*67)+0.5)/68)

SMH6_se_clr_MUS$number <- as.factor(SMH6_se_clr_MUS$number)

SMH6_se_clr <- bind_rows(SMH6_se_clr_MUS, SMH6_se_clr_BUG)

#### Statistics ----

###### Musubi se clearance ----
# Abdominal Pain
abdpain_clearance_beta_MUS <- glmmTMB(data = SMH6_se_clr_MUS,
                                      formula = TF_Clear ~ abdom_pain, 
                                      family=beta_family("logit"))


summary(abdpain_clearance_beta_MUS)
testDispersion(abdpain_clearance_beta_MUS)
simulationH6beta_abdpain_MUS <- simulateResiduals(fittedModel = abdpain_clearance_beta_MUS, plot = T)

# Headache
headache_clearance_beta_MUS <- glmmTMB(data = SMH6_se_clr_MUS,
                                      formula = TF_Clear ~ headache, 
                                      family=beta_family("logit"))

summary(headache_clearance_beta_MUS)
testDispersion(headache_clearance_beta_MUS)
simulationH6beta_headache_MUS <- simulateResiduals(fittedModel = headache_clearance_beta_MUS, plot = T)

# Diarrhoea
diarrhoea_clearance_beta_MUS <- glmmTMB(data = SMH6_se_clr_MUS,
                                       formula = TF_Clear ~ diarrhoea, 
                                       family=beta_family("logit"))

summary(diarrhoea_clearance_beta_MUS)
testDispersion(diarrhoea_clearance_beta_MUS)
simulationH6beta_diarrhoea_MUS <- simulateResiduals(fittedModel = diarrhoea_clearance_beta_MUS, plot = T)


# Dizzy <- go to SM
dizzy_clearance_beta_MUS <- glmmTMB(data = SMH6_se_clr_MUS,
                                        formula = TF_Clear ~ dizzy, 
                                        family=beta_family("logit"))

summary(dizzy_clearance_beta_MUS)
testDispersion(dizzy_clearance_beta_MUS)
simulationH6beta_dizzy_MUS <- simulateResiduals(fittedModel = dizzy_clearance_beta_MUS, plot = T)


# Breathing <- go to SM
breathing_clearance_beta_MUS <- glmmTMB(data = SMH6_se_clr_MUS,
                                    formula = TF_Clear ~ breathing, 
                                    family=beta_family("logit"))

summary(breathing_clearance_beta_MUS)
testDispersion(breathing_clearance_beta_MUS)
simulationH6beta_breathing_MUS <- simulateResiduals(fittedModel = breathing_clearance_beta_MUS, plot = T)

# Weakness <- go to SM
weakness_clearance_beta_MUS <- glmmTMB(data = SMH6_se_clr_MUS,
                                        formula = TF_Clear ~ weakness, 
                                        family=beta_family("logit"))

summary(weakness_clearance_beta_MUS)
testDispersion(weakness_clearance_beta_MUS)
simulationH6beta_weakness_MUS <- simulateResiduals(fittedModel = weakness_clearance_beta_MUS, plot = T)

###### Musubi num clearance (vomiting considered) ----
SE_num_clearance_MUS <- glmmTMB(data = H6_se_clr_MUS,
                                formula = TF_Clear ~ number, 
                                family=beta_family("logit"))

summary(SE_num_clearance_MUS)
testDispersion(SE_num_clearance_MUS)
simulationH6se_num_MUS <- simulateResiduals(fittedModel = SE_num_clearance_MUS, plot = T)

# Plot ----

#### every side effects and clearance ####
H6_se_clr_plot_MUS <- MusubiSE2004 %>% 
  dplyr::select(CID, abdom_pain, headache, diarrhoea, vomit) %>% 
  mutate(Clearance = ClearProb1$Clear, School = "Musubi") %>% 
  rename('Abdominal pain' = abdom_pain, Headache = headache, Diarrhoea = diarrhoea, Vomiting = vomit) 
  
H6_se_clr_plot_MUS = melt(H6_se_clr_plot_MUS,
                         id.vars = c("CID", "Clearance", "School"),
                         variable.name = "Side_effect",value.name = "status")
H6_se_clr_plot_MUS$status[H6_se_clr_plot_MUS$status==1]<- "YES"
H6_se_clr_plot_MUS$status[H6_se_clr_plot_MUS$status==0]<- "NO"
H6_se_clr_plot_MUS <- na.omit(H6_se_clr_plot_MUS)

H6_se_clr_plot_BUG <- BugotoSE2004 %>% 
  dplyr::select(CID, abdom_pain, headache, diarrhoea, vomit) %>% 
  mutate(Clearance = ClearProb1BG$Clear, School = "Bugoto LV") %>% 
  rename('Abdominal pain' = abdom_pain, Headache = headache, Diarrhoea = diarrhoea, Vomiting = vomit) 

H6_se_clr_plot_BUG = melt(H6_se_clr_plot_BUG,
                          id.vars = c("CID", "Clearance", "School"),
                          variable.name = "Side_effect",value.name = "status")
H6_se_clr_plot_BUG$status[H6_se_clr_plot_BUG$status==1]<- "YES"
H6_se_clr_plot_BUG$status[H6_se_clr_plot_BUG$status==0]<- "NO"
H6_se_clr_plot_BUG <- na.omit(H6_se_clr_plot_BUG)

H6_se_clr_plot <- bind_rows(H6_se_clr_plot_MUS, H6_se_clr_plot_BUG)

PlotH6_se <- H6_se_clr_plot %>% 
  group_by(Clearance, School, Side_effect, status) %>% 
  tally()

PlotH6_se$Side_effect <- factor(PlotH6_se$Side_effect, levels = c("Abdominal pain", "Diarrhoea", "Headache", "Vomiting"))

ggplot() +
  geom_point(data = PlotH6_se,
           aes(x = status, y = Clearance, colour = School, size = n), stroke = 1, position = position_jitterdodge(0.4), alpha = .4)+
  theme_bw()+
  scale_colour_manual(values = col_2School,
                      labels = c("Bugoto LV" = "Bugoto LV",
                                 "Musubi" = "Musubi CoG"))+
  scale_fill_manual(values = col_2School)+
  ylab("Clearance Probability")+
  xlab("Status")+
  theme(text=element_text(size=13))+
  scale_size_continuous(name="Number of Student")+
  labs(size = "Number of students")+
  facet_grid(.~Side_effect)

ggsave("PlotH6A.pdf")
ggsave("PlotH6A.png")

#### number of side effects and clearance ####
PlotH6_num <- H6_se_clr %>% 
  group_by(Clear, School, number) %>% 
  tally()

PlotH6_num <- na.omit(PlotH6_num)

ggplot() +
  geom_histogram(data = PlotH6_num,
                 aes(x = Clear),
                 position = "dodge", bins = 5, alpha = .8)+
  theme_bw()+
  xlab("Clearance Probability")+
  ylab("Number of Students")+
  theme(text=element_text(size=10))+
  facet_grid(School~number, labeller = labeller_musubi)+
  scale_x_continuous(breaks = seq(0, 1, by = 0.2))


ggsave("PlotH6B.pdf")
ggsave("PlotH6B.png")


# Question 5 SM: Effect of all the other side effects (include vomiting) on drug efficacy ----

#### Statistics num ----

###### Bugoto ----
SMSE_num_clearance_BUG <- glmmTMB(data = SMH6_se_clr_BUG,
                                formula = TF_Clear ~ number, 
                                family=beta_family("logit"))

summary(SMSE_num_clearance_BUG)
testDispersion(SMSE_num_clearance_BUG)
SMsimulationH6se_num_BUG <- simulateResiduals(fittedModel = SMSE_num_clearance_BUG, plot = T)

###### Musubi ----
SMSE_num_clearance_MUS <- glmmTMB(data = SMH6_se_clr_MUS,
                                  formula = TF_Clear ~ number, 
                                  family=beta_family("logit"))

summary(SMSE_num_clearance_MUS)
testDispersion(SMSE_num_clearance_MUS)
SMsimulationH6se_num_MUS <- simulateResiduals(fittedModel = SMSE_num_clearance_MUS, plot = T)

# Plot ----

#### every side effects and clearance ####
SMH6_se_clr_plot_MUS <- MusubiSE2004 %>% 
  dplyr::select(CID, abdom_pain, headache, diarrhoea, dizzy, breathing, weakness) %>% 
  mutate(Clearance = ClearProb1$Clear, School = "Musubi") %>% 
  rename('Abdominal pain' = abdom_pain, Headache = headache, Diarrhoea = diarrhoea, Dizzy = dizzy,
         Breathing = breathing, Weakness = weakness) 

SMH6_se_clr_plot_MUS = melt(SMH6_se_clr_plot_MUS,
                          id.vars = c("CID", "Clearance", "School"),
                          variable.name = "Side_effect",value.name = "status")
SMH6_se_clr_plot_MUS$status[SMH6_se_clr_plot_MUS$status==1]<- "YES"
SMH6_se_clr_plot_MUS$status[SMH6_se_clr_plot_MUS$status==0]<- "NO"
SMH6_se_clr_plot_MUS <- na.omit(SMH6_se_clr_plot_MUS)

SMH6_se_clr_plot_BUG <- BugotoSE2004 %>% 
  dplyr::select(CID, abdom_pain, headache, diarrhoea, dizzy, breathing, weakness) %>% 
  mutate(Clearance = ClearProb1BG$Clear, School = "Bugoto LV") %>% 
  rename('Abdominal pain' = abdom_pain, Headache = headache, Diarrhoea = diarrhoea, Dizzy = dizzy,
         Breathing = breathing, Weakness = weakness) 

SMH6_se_clr_plot_BUG = melt(SMH6_se_clr_plot_BUG,
                          id.vars = c("CID", "Clearance", "School"),
                          variable.name = "Side_effect",value.name = "status")
SMH6_se_clr_plot_BUG$status[SMH6_se_clr_plot_BUG$status==1]<- "YES"
SMH6_se_clr_plot_BUG$status[SMH6_se_clr_plot_BUG$status==0]<- "NO"
SMH6_se_clr_plot_BUG <- na.omit(SMH6_se_clr_plot_BUG)

SMH6_se_clr_plot <- bind_rows(SMH6_se_clr_plot_MUS, SMH6_se_clr_plot_BUG)

SMPlotH6 <- SMH6_se_clr_plot %>% 
  group_by(Clearance, School, Side_effect, status) %>% 
  tally()

ggplot() +
  geom_point(data = SMPlotH6,
             aes(x = status, y = Clearance, colour = School, size = n),
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             stroke = 1, alpha = .5)+
  theme_bw()+
  scale_colour_manual(values = col_2School,
                      labels = c("Bugoto LV" = "Bugoto LV",
                                 "Musubi" = "Musubi CoG"))+
  scale_fill_manual(values = col_2School)+
  ylab("Clearance Probability")+
  xlab("Status")+
  theme(text=element_text(size=13))+
  scale_size_continuous(name="Number of Student")+
  labs(size = "Number of students")+
  facet_grid(.~Side_effect)

ggsave("PlotH6_se_SM.pdf")
ggsave("PlotH6_se_SM.png")

#### number of side effects and clearance ####
SMPlotH6_num <- SMH6_se_clr %>% 
  group_by(Clear, School, number) %>% 
  tally()
SMPlotH6_num <- na.omit(SMPlotH6_num)
SMPlotH6_num$number <- factor(SMPlotH6_num$number, levels = c("0","1","2","3","4","5","6"))

ggplot() +
  geom_histogram(data = SMPlotH6_num,
                 aes(x = Clear),
                 position = "dodge", bins = 10, alpha = .8)+
  theme_bw()+
  xlab("Clearance Probability")+
  ylab("Number of Students")+
  theme(text=element_text(size=11))+
  facet_grid(School~number, labeller = labeller_musubi)

ggsave("PlotH6_num_SM.pdf")
ggsave("PlotH6_num_SM.png")


# extra calculation ----
mean(ClearProb1$Clear)
mean(ClearProb1BG$Clear)
wilcox.test(ClearProb1$Clear, ClearProb1BG$Clear)
