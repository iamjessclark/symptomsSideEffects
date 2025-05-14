# Hypothesis 5: Effect of reporting pre-treatment symptoms on reporting that as a post-treatment side effects

# Bugoto LV ----

#### data frame ----
sym_se_BUG <- BugotoSymp2004 %>% 
  rename(sym_urine_pain = urine_pain, sym_abdom_pain = abdom_pain, sym_headache = headache,
         sym_itching = itching, sym_diarrhoea = diarrhoea, sym_nausea = nausea, sym_blood_stool = blood_stool) %>% 
  mutate(se_urine_pain = BugotoSE2004$urine_pain, se_abdom_pain = BugotoSE2004$abdom_pain, 
         se_headache = BugotoSE2004$headache, se_itching = BugotoSE2004$rash, se_diarrhoea = BugotoSE2004$diarrhoea, 
         se_nausea = BugotoSE2004$nausea, se_blood_stool = BugotoSE2004$blood_stool)
sym_se_BUG[sym_se_BUG==2]<-0

sym_se_BUG$se_abdom_pain <- as.factor(sym_se_BUG$se_abdom_pain)
sym_se_BUG$sym_abdom_pain <- as.factor(sym_se_BUG$sym_abdom_pain)
sym_se_BUG$se_headache <- as.factor(sym_se_BUG$se_headache)
sym_se_BUG$sym_headache <- as.factor(sym_se_BUG$sym_headache)
sym_se_BUG$se_diarrhoea <- as.factor(sym_se_BUG$se_diarrhoea)
sym_se_BUG$sym_diarrhoea <- as.factor(sym_se_BUG$sym_diarrhoea)
sym_se_BUG$se_nausea <- as.factor(sym_se_BUG$se_nausea)
sym_se_BUG$sym_nausea <- as.factor(sym_se_BUG$sym_nausea)
sym_se_BUG$se_itching <- as.factor(sym_se_BUG$se_itching)
sym_se_BUG$sym_itching <- as.factor(sym_se_BUG$sym_itching)
sym_se_BUG$se_urine_pain <- as.factor(sym_se_BUG$se_urine_pain)
sym_se_BUG$sym_urine_pain <- as.factor(sym_se_BUG$sym_urine_pain)
sym_se_BUG$se_blood_stool <- as.factor(sym_se_BUG$se_blood_stool)
sym_se_BUG$sym_blood_stool <- as.factor(sym_se_BUG$sym_blood_stool)

#### GLM run ----

# abdominal pain AP_BUG
Hypo5AP_BUG_plus <- glm(data = sym_se_BUG, 
                         formula = se_abdom_pain ~ sym_urine_pain+sym_abdom_pain+sym_headache+sym_itching+sym_diarrhoea+sym_nausea+sym_blood_stool,
                         family = binomial(link='logit'))
summary(Hypo5AP_BUG_plus)

testDispersion(Hypo5AP_BUG_plus)
simulationH5AP_BUG_plus <- simulateResiduals(fittedModel = Hypo5AP_BUG_plus, plot = T)

Hypo5AP_BUG_multi <- glm(data = sym_se_BUG, 
                   formula = se_abdom_pain ~ sym_urine_pain*sym_abdom_pain*sym_headache*sym_itching*sym_diarrhoea*sym_nausea*sym_blood_stool,
                   family = binomial(link='logit'))
summary(Hypo5AP_BUG_multi)

testDispersion(Hypo5AP_BUG_multi)
simulationH5AP_BUG_multi <- simulateResiduals(fittedModel = Hypo5AP_BUG_multi, plot = T)

Hypo5AP_BUG_mix <- glm(data = sym_se_BUG, 
                         formula = se_abdom_pain ~ sym_abdom_pain*(sym_urine_pain+sym_abdom_pain+sym_headache+sym_itching+sym_diarrhoea+sym_nausea+sym_blood_stool),
                         family = binomial(link='logit'))
summary(Hypo5AP_BUG_mix)

testDispersion(Hypo5AP_BUG_mix)
simulationH5AP_BUG_mix <- simulateResiduals(fittedModel = Hypo5AP_BUG_mix, plot = T)

Hypo5AP_BUG <- glm(data = sym_se_BUG, 
                   formula = se_abdom_pain ~ sym_abdom_pain,
                   family = binomial(link='logit'))
summary(Hypo5AP_BUG)

testDispersion(Hypo5AP_BUG)
simulationH5AP_BUG <- simulateResiduals(fittedModel = Hypo5AP_BUG, plot = T)

# headache HA_BUG
Hypo5HA_BUG <- glm(data = sym_se_BUG, 
                         formula = se_headache ~ sym_headache, 
                         family = binomial(link='logit'))

summary(Hypo5HA_BUG)

testDispersion(Hypo5HA_BUG)
simulationH5HA_BUG <- simulateResiduals(fittedModel = Hypo5HA_BUG, plot = T)

# diarrhoea DH_BUG
Hypo5DH_BUG <- glm(data = sym_se_BUG, 
                       formula = se_diarrhoea ~ sym_diarrhoea,
                       family = binomial(link='logit'))
summary(Hypo5DH_BUG)

testDispersion(Hypo5DH_BUG)
simulationH5DH_BUG <- simulateResiduals(fittedModel = Hypo5DH_BUG, plot = T)

# Musubi ----

#### data frame ----
sym_se <- MusubiSymp2004 %>% 
  rename(sym_urine_pain = urine_pain, sym_abdom_pain = abdom_pain, sym_headache = headache,
         sym_itching = itching, sym_diarrhoea = diarrhoea, sym_nausea = nausea, sym_blood_stool = blood_stool) %>% 
  mutate(se_urine_pain = MusubiSE2004$urine_pain, se_abdom_pain = MusubiSE2004$abdom_pain, 
         se_headache = MusubiSE2004$headache, se_itching = MusubiSE2004$rash, se_diarrhoea = MusubiSE2004$diarrhoea, 
         se_nausea = MusubiSE2004$nausea, se_blood_stool = MusubiSE2004$blood_stool)
sym_se[sym_se==2]<-0

sym_se$se_abdom_pain <- as.factor(sym_se$se_abdom_pain)
sym_se$sym_abdom_pain <- as.factor(sym_se$sym_abdom_pain)
sym_se$se_headache <- as.factor(sym_se$se_headache)
sym_se$sym_headache <- as.factor(sym_se$sym_headache)
sym_se$se_diarrhoea <- as.factor(sym_se$se_diarrhoea)
sym_se$sym_diarrhoea <- as.factor(sym_se$sym_diarrhoea)
sym_se$se_nausea <- as.factor(sym_se$se_nausea)
sym_se$sym_nausea <- as.factor(sym_se$sym_nausea)
sym_se$se_itching <- as.factor(sym_se$se_itching)
sym_se$sym_itching <- as.factor(sym_se$sym_itching)
sym_se$se_urine_pain <- as.factor(sym_se$se_urine_pain)
sym_se$sym_urine_pain <- as.factor(sym_se$sym_urine_pain)
sym_se$se_blood_stool <- as.factor(sym_se$se_blood_stool)
sym_se$sym_blood_stool <- as.factor(sym_se$sym_blood_stool)

#### GLM run ----

# abdominal pain AP
Hypo5AP_MUS <- glm(data = sym_se, 
                   formula = se_abdom_pain ~ sym_abdom_pain,
                   family = binomial(link='logit'))
summary(Hypo5AP_MUS)

testDispersion(Hypo5AP_MUS)
simulationH5AP_MUS <- simulateResiduals(fittedModel = Hypo5AP_MUS, plot = T)

# headache HA
Hypo5HA_MUS <- glm(data = sym_se, 
                   formula = se_headache ~ sym_headache, 
                   family = binomial(link='logit'))
summary(Hypo5HA_MUS)

testDispersion(Hypo5HA_MUS)
simulationH5HA_MUS <- simulateResiduals(fittedModel = Hypo5HA_MUS, plot = T)

# diarrhoea DH
Hypo5DH_MUS <- glm(data = sym_se, 
                   formula = se_diarrhoea ~ sym_diarrhoea, 
                   family = binomial(link='logit'))

summary(Hypo5DH_MUS)

testDispersion(Hypo5DH_MUS)
simulationH5DH_MUS <- simulateResiduals(fittedModel = Hypo5DH_MUS, plot = T)

# Bwondha ----

#### data frame ----
sym_se_BWD <- read_csv("Baseline 2004 + six months.csv") %>% 
  dplyr::select(School, CID,seabdpn0, sehead0, sediar0, serash0, sepnur0, senausea0, sebldst0, 
                pnurE0, abdpnE0, headE0, itchE0, diaE0, nauE0, bldstlE0)%>%
  rename(se_abdom_pain=seabdpn0, se_headache=sehead0, 
         se_diarrhoea=sediar0, se_itching=serash0, 
         se_urine_pain=sepnur0, se_nausea=senausea0, se_blood_stool=sebldst0, 
         sym_urine_pain = pnurE0, sym_abdom_pain = abdpnE0, 
         sym_headache = headE0, sym_itching = itchE0, 
         sym_diarrhoea = diaE0, sym_nausea = nauE0, sym_blood_stool = bldstlE0) %>% 
  filter(School == "Bwondha")
sym_se_BWD[sym_se_BWD==2]<-0

sym_se_BWD$se_abdom_pain <- as.factor(sym_se_BWD$se_abdom_pain)
sym_se_BWD$sym_abdom_pain <- as.factor(sym_se_BWD$sym_abdom_pain)
sym_se_BWD$se_headache <- as.factor(sym_se_BWD$se_headache)
sym_se_BWD$sym_headache <- as.factor(sym_se_BWD$sym_headache)
sym_se_BWD$se_diarrhoea <- as.factor(sym_se_BWD$se_diarrhoea)
sym_se_BWD$sym_diarrhoea <- as.factor(sym_se_BWD$sym_diarrhoea)
sym_se_BWD$se_nausea <- as.factor(sym_se_BWD$se_nausea)
sym_se_BWD$sym_nausea <- as.factor(sym_se_BWD$sym_nausea)
sym_se_BWD$se_itching <- as.factor(sym_se_BWD$se_itching)
sym_se_BWD$sym_itching <- as.factor(sym_se_BWD$sym_itching)
sym_se_BWD$se_urine_pain <- as.factor(sym_se_BWD$se_urine_pain)
sym_se_BWD$sym_urine_pain <- as.factor(sym_se_BWD$sym_urine_pain)
sym_se_BWD$se_blood_stool <- as.factor(sym_se_BWD$se_blood_stool)
sym_se_BWD$sym_blood_stool <- as.factor(sym_se_BWD$sym_blood_stool)

#### GLM run ----

# abdominal pain AP
Hypo5AP_bwd <- glm(data = sym_se_BWD, 
                       formula = se_abdom_pain ~ sym_abdom_pain,
                       family = binomial(link='logit'))
summary(Hypo5AP_bwd)

testDispersion(Hypo5AP_bwd)
simulationH5AP_bwd <- simulateResiduals(fittedModel = Hypo5AP_bwd, plot = T)

# headache HA

Hypo5HA_bwd <- glm(data = sym_se_BWD, 
                       formula = se_headache ~ sym_headache, 
                       family = binomial(link='logit'))
summary(Hypo5HA_bwd)

testDispersion(Hypo5HA_bwd)
simulationH5HA_bwd <- simulateResiduals(fittedModel = Hypo5HA_bwd, plot = T)

# diarrhoea DH
Hypo5DH_BWD <- glm(data = sym_se_BWD, 
                       formula = se_diarrhoea ~ sym_diarrhoea, 
                       family = binomial(link='logit'))

summary(Hypo5DH_BWD)

testDispersion(Hypo5DH_BWD)
simulationH5DH_BWD <- simulateResiduals(fittedModel = Hypo5DH_BWD, plot = T)

# Nausea NS
Hypo5NS_BWD <- glm(data = sym_se_BWD, 
                       formula = se_nausea ~ sym_nausea, 
                       family = binomial(link='logit'))

summary(Hypo5NS_BWD)

testDispersion(Hypo5NS_BWD)
simulationH5NS_BWD <- simulateResiduals(fittedModel = Hypo5NS_BWD, plot = T)

# itching IR
Hypo5IR_BWD <- glm(data = sym_se_BWD, 
                       formula = se_itching ~ sym_itching, 
                       family = binomial(link='logit'))

summary(Hypo5IR_BWD)

testDispersion(Hypo5IR_BWD)
simulationH5IR_BWD <- simulateResiduals(fittedModel = Hypo5IR_BWD, plot = T)

# GLM run for trying interaction with schools ----

#### data frame ----

sym_se_3sch <- bind_rows(sym_se, sym_se_BUG, sym_se_BWD)

#### models ----

# Abdonimal Pain AP
Hypo5AP_3s <- glm(data = sym_se_3sch, 
                   formula = se_abdom_pain ~ sym_abdom_pain*School, 
                   family = binomial(link='logit'))

summary(Hypo5AP_3s)

testDispersion(Hypo5AP_3s)
simulationH5AP_3s <- simulateResiduals(fittedModel = Hypo5AP_3s, plot = T)

# Headache HA
Hypo5HA_3s <- glm(data = sym_se_3sch, 
                  formula = se_headache ~ sym_headache*School, 
                  family = binomial(link='logit'))

summary(Hypo5HA_3s)

testDispersion(Hypo5HA_3s)
simulationH5HA_3s <- simulateResiduals(fittedModel = Hypo5HA_3s, plot = T)

# diarrhoea DH
Hypo5DH_3s <- glm(data = sym_se_3sch, 
                  formula = se_diarrhoea ~ sym_diarrhoea*School, 
                  family = binomial(link='logit'))

summary(Hypo5DH_3s)

testDispersion(Hypo5DH_3s)
simulationH5DH_3s <- simulateResiduals(fittedModel = Hypo5DH_3s, plot = T)

# Itching IR
Hypo5IR_3s <- glm(data = sym_se_3sch, 
                  formula = se_itching ~ sym_itching*School, 
                  family = binomial(link='logit'))

summary(Hypo5IR_3s)

testDispersion(Hypo5IR_3s)
simulationH5IR_3s <- simulateResiduals(fittedModel = Hypo5IR_3s, plot = T)

# urinate pain UP <- not a side effect
Hypo5UP_3s <- glm(data = sym_se_3sch, 
                  formula = se_urine_pain ~ sym_urine_pain*School, 
                  family = binomial(link='logit'))

summary(Hypo5UP_3s)

testDispersion(Hypo5UP_3s)
simulationH5UP_3s <- simulateResiduals(fittedModel = Hypo5UP_3s, plot = T)

# nausea NS
Hypo5NS_3s <- glm(data = sym_se_3sch, 
                  formula = se_nausea ~ sym_nausea*School, 
                  family = binomial(link='logit'))

summary(Hypo5NS_3s)

testDispersion(Hypo5NS_3s)
simulationH5NS_3s <- simulateResiduals(fittedModel = Hypo5NS_3s, plot = T)

# blood stool BS <- not a side effect
Hypo5BS_3s <- glm(data = sym_se_3sch, 
                  formula = se_blood_stool ~ sym_blood_stool*School, 
                  family = binomial(link='logit'))

summary(Hypo5BS_3s)

testDispersion(Hypo5BS_3s)
simulationH5BS_3s <- simulateResiduals(fittedModel = Hypo5BS_3s, plot = T)

# PLOT ----
sym_se_abdominalpain <- sym_se_3sch %>% 
  dplyr::select(School, sym_abdom_pain, se_abdom_pain) %>% 
  group_by(School, sym_abdom_pain, se_abdom_pain) %>% 
  tally() %>% 
  mutate(Symptom = "abdominal pain") %>% 
  rename(pre_treatment = sym_abdom_pain, post_treatment = se_abdom_pain)
sym_se_abdominalpain <- na.omit(sym_se_abdominalpain)

sym_se_headache <- sym_se_3sch %>% 
  dplyr::select(School, sym_headache, se_headache) %>% 
  group_by(School, sym_headache, se_headache) %>% 
  tally() %>% 
  mutate(Symptom = "headache") %>% 
  rename(pre_treatment = sym_headache, post_treatment = se_headache)
sym_se_headache <- na.omit(sym_se_headache)

sym_se_diarrhoea <- sym_se_3sch %>% 
  dplyr::select(School, sym_diarrhoea, se_diarrhoea) %>% 
  group_by(School, sym_diarrhoea, se_diarrhoea) %>% 
  tally() %>% 
  mutate(Symptom = "diarrhoea") %>% 
  rename(pre_treatment = sym_diarrhoea, post_treatment = se_diarrhoea)
sym_se_diarrhoea <- na.omit(sym_se_diarrhoea)

sym_se_itching <- sym_se_3sch %>% 
  dplyr::select(School, sym_itching, se_itching) %>% 
  group_by(School, sym_itching, se_itching) %>% 
  tally() %>% 
  mutate(Symptom = "itching") %>% 
  rename(pre_treatment = sym_itching, post_treatment = se_itching)
sym_se_itching <- na.omit(sym_se_itching)

sym_se_nausea <- sym_se_3sch %>% 
  dplyr::select(School, sym_nausea, se_nausea) %>% 
  group_by(School, sym_nausea, se_nausea) %>% 
  tally() %>% 
  mutate(Symptom = "nausea") %>% 
  rename(pre_treatment = sym_nausea, post_treatment = se_nausea)
sym_se_nausea <- na.omit(sym_se_nausea)

sym_se_blood_stool <- sym_se_3sch %>% 
  dplyr::select(School, sym_blood_stool, se_blood_stool) %>% 
  group_by(School, sym_blood_stool, se_blood_stool) %>% 
  tally() %>% 
  mutate(Symptom = "blood stool") %>% 
  rename(pre_treatment = sym_blood_stool, post_treatment = se_blood_stool)
sym_se_blood_stool <- na.omit(sym_se_blood_stool)

sym_se_urine_pain <- sym_se_3sch %>% 
  dplyr::select(School, sym_urine_pain, se_urine_pain) %>% 
  group_by(School, sym_urine_pain, se_urine_pain) %>% 
  tally() %>% 
  mutate(Symptom = "urinate pain") %>% 
  rename(pre_treatment = sym_urine_pain, post_treatment = se_urine_pain)
sym_se_urine_pain <- na.omit(sym_se_urine_pain)

PlotH5 <- bind_rows(sym_se_abdominalpain, sym_se_headache, sym_se_diarrhoea,
                         sym_se_itching, sym_se_nausea, sym_se_blood_stool, sym_se_urine_pain) %>% 
  mutate(Status = "as symptom, as side effects")

PlotH5$Status[which(PlotH5$pre_treatment=="1" & PlotH5$post_treatment=="0")] <- "as symptom, but not side effects"
PlotH5$Status[which(PlotH5$pre_treatment=="0" & PlotH5$post_treatment=="1")] <- "not as symptom, but side effects"
PlotH5$Status[which(PlotH5$pre_treatment=="0" & PlotH5$post_treatment=="0")] <- "not as symptom, not as side effects"

PlotH5$pre_treatment <- as.character(PlotH5$pre_treatment)
PlotH5$post_treatment <- as.character(PlotH5$post_treatment)

PlotH5$pre_treatment[PlotH5$pre_treatment == 1] <- "YES"
PlotH5$pre_treatment[PlotH5$pre_treatment == 0] <- "NO"
PlotH5$post_treatment[PlotH5$post_treatment == 1] <- "YES"
PlotH5$post_treatment[PlotH5$post_treatment == 0] <- "NO"

PlotH5$pre_treatment <- as.factor(PlotH5$pre_treatment)
PlotH5$post_treatment <- as.factor(PlotH5$post_treatment)

sym_se_musubi <- PlotH5 %>% 
  filter(School == "Musubi") %>% 
  mutate(Freq = n/68)

sym_se_bugotoLV <- PlotH5 %>% 
  filter(School == "Bugoto LV") %>% 
  mutate(Freq = n/94)

sym_se_bwondha <- PlotH5 %>% 
  filter(School == "Bwondha") %>% 
  mutate(Freq = n/173)

PlotH5 <- bind_rows(sym_se_musubi, sym_se_bugotoLV, sym_se_bwondha)
PlotH5$Symptom <- factor(PlotH5$Symptom, levels = c("abdominal pain", "headache", "diarrhoea", "itching", "urinate pain", "nausea", "blood stool"))

ggplot()+
  geom_bar(data =PlotH5, 
           aes(x = Symptom, y = n, fill = Status), position = "fill", stat = "identity")+
  scale_y_continuous(labels = scales::percent)+
  facet_grid(.~School)+
  theme_bw()+
  xlab("Symptoms")+
  ylab("Proportion of Children")+
  labs(fill = "Status")+
  scale_fill_manual(values=col_4_status)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

ggsave("sym_se_3sch.pdf")
ggsave("sym_se_3sch.png")

ggplot(as.data.frame(PlotH5),
       aes(y = Freq,
           axis1 = pre_treatment, axis2 = post_treatment)) +
  geom_alluvium(aes(fill = Status),
                curve_type = "sigmoid",
                width = 0, knot.pos = 0, reverse = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:2, labels = c("Symptom", "Side effect")) +
  facet_grid(Symptom~School)+
  theme_bw()+
  ylab("Proportion")+
  scale_fill_discrete(guide = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom", 
        legend.box = "vertical", 
        axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1), text=element_text(size=12))

ggsave("sym_se_allivium.pdf", height = 9, width = 12)
ggsave("sym_se_allivium.png", height = 9, width = 12)
