#### SUMMARY STATISTIC ----
##### prepared the dataframe -----
columns_to_check <- c("sm0d1s1", "sm0d12", "sm0d2s1", "sm0d2s2", "sm0d3s1", "sm0d3s2")

sum_2school <- para2004 %>%
  rowwise() %>%
  mutate(
    inf_sta = ifelse(any(c_across(all_of(columns_to_check)) > 0, na.rm = TRUE), 1, 0),
    inf_int = mean(c_across(all_of(columns_to_check)), na.rm = TRUE)
  ) %>%
  ungroup()

##### population in study -----
pp_c <- sum_2school %>% 
  group_by(School) %>% 
  tally()

print(pp_c)

pp_c_sex <- sum_2school %>% 
  group_by(School, SEX) %>% 
  tally()

print(pp_c_sex)

##### infection prevalence by school -----
pp_c_previnf <- sum_2school %>% 
  group_by(School, inf_sta) %>% 
  tally()

print(pp_c_previnf)

##### intensity range by school -----
# BUGOTO LV
BUGintran <- sum_2school %>% 
  filter(School == "Bugoto LV")

BUGintmean <- mean(BUGintran$inf_int, na.rm = TRUE)
print(BUGintmean)
BUGintsd <- sd(BUGintran$inf_int, na.rm = TRUE)
print(BUGintsd)

# BWONDHA
BWDintran <- sum_2school %>% 
  filter(School == "Bwondha")

BWDintmean <- mean(BWDintran$inf_int, na.rm = TRUE)
print(BWDintmean)
BWDintsd <- sd(BWDintran$inf_int, na.rm = TRUE)
print(BWDintsd)

# MUSUBI
MUSintran <- sum_2school %>% 
  filter(School == "Musubi")

MUSintmean <- mean(MUSintran$inf_int, na.rm = TRUE)
print(MUSintmean)
MUSintsd <- sd(MUSintran$inf_int, na.rm = TRUE)
print(MUSintsd)

intmean <- mean(sum_2school$inf_int, na.rm = TRUE)
print(intmean)
intsd <- sd(sum_2school$inf_int, na.rm = TRUE)
print(intsd)

intran_2 <- sum_2school %>% 
  filter(!School == "Bwondha")

intmean_2 <- mean(intran_2$inf_int, na.rm = TRUE)
print(intmean_2)
intsd_2 <- sd(intran_2$inf_int, na.rm = TRUE)
print(intsd_2)

##### prevalance sym/se Overall and by School -----
sym_se_3school_overall <- read_csv("Baseline 2004 + six months.csv") %>% 
  dplyr::select(School, CID,seabdpn0, sehead0, sediar0, serash0, sepnur0, senausea0, sebldst0, 
                pnurE0, abdpnE0, headE0, itchE0, diaE0, nauE0, bldstlE0)%>%
  rename(se_abdom_pain=seabdpn0, se_headache=sehead0, 
         se_diarrhoea=sediar0, se_itching=serash0, 
         se_urine_pain=sepnur0, se_nausea=senausea0, se_blood_stool=sebldst0, 
         sym_urine_pain = pnurE0, sym_abdom_pain = abdpnE0, 
         sym_headache = headE0, sym_itching = itchE0, 
         sym_diarrhoea = diaE0, sym_nausea = nauE0, sym_blood_stool = bldstlE0)

sym_se_3school_overall[sym_se_3school_overall==2]<-0

sym_se_3school_overall$se_abdom_pain <- as.factor(sym_se_3school_overall$se_abdom_pain)
sym_se_3school_overall$sym_abdom_pain <- as.factor(sym_se_3school_overall$sym_abdom_pain)
sym_se_3school_overall$se_headache <- as.factor(sym_se_3school_overall$se_headache)
sym_se_3school_overall$sym_headache <- as.factor(sym_se_3school_overall$sym_headache)
sym_se_3school_overall$se_diarrhoea <- as.factor(sym_se_3school_overall$se_diarrhoea)
sym_se_3school_overall$sym_diarrhoea <- as.factor(sym_se_3school_overall$sym_diarrhoea)
sym_se_3school_overall$se_nausea <- as.factor(sym_se_3school_overall$se_nausea)
sym_se_3school_overall$sym_nausea <- as.factor(sym_se_3school_overall$sym_nausea)
sym_se_3school_overall$se_itching <- as.factor(sym_se_3school_overall$se_itching)
sym_se_3school_overall$sym_itching <- as.factor(sym_se_3school_overall$sym_itching)
sym_se_3school_overall$se_urine_pain <- as.factor(sym_se_3school_overall$se_urine_pain)
sym_se_3school_overall$sym_urine_pain <- as.factor(sym_se_3school_overall$sym_urine_pain)
sym_se_3school_overall$se_blood_stool <- as.factor(sym_se_3school_overall$se_blood_stool)
sym_se_3school_overall$sym_blood_stool <- as.factor(sym_se_3school_overall$sym_blood_stool)

sym_se_3school_overall <- sym_se_3school_overall[-(365:1766),]

sym_se_3school_overall <- sym_se_3school_overall %>%
  left_join(sum_2school %>% 
              dplyr::select(CID, inf_sta, inf_int), by = "CID")

APsym_prev_over <- sym_se_3school_overall %>% 
  group_by(School, sym_abdom_pain) %>% 
  tally()
print(APsym_prev_over)

APse_prev_over <- sym_se_3school_overall %>% 
  group_by(School, se_abdom_pain) %>% 
  tally()
print(APse_prev_over)

BSsym_prev_over <- sym_se_3school_overall %>% 
  group_by(School, sym_blood_stool) %>% 
  tally()
print(BSsym_prev_over)

BSse_prev_over <- sym_se_3school_overall %>% 
  group_by(School, se_blood_stool) %>% 
  tally()
print(BSse_prev_over)

DHsym_prev_over <- sym_se_3school_overall %>% 
  group_by(School, sym_diarrhoea) %>% 
  tally()
print(DHsym_prev_over)

DHse_prev_over <- sym_se_3school_overall %>% 
  group_by(School, se_diarrhoea) %>% 
  tally()
print(DHse_prev_over)

HAsym_prev_over <- sym_se_3school_overall %>% 
  group_by(School, sym_headache) %>% 
  tally()
print(HAsym_prev_over)

HAse_prev_over <- sym_se_3school_overall %>% 
  group_by(School, se_headache) %>% 
  tally()
print(HAse_prev_over)

IRsym_prev_over <- sym_se_3school_overall %>% 
  group_by(School, sym_itching) %>% 
  tally()
print(IRsym_prev_over)

IRse_prev_over <- sym_se_3school_overall %>% 
  group_by(School, se_itching) %>% 
  tally()
print(IRse_prev_over)

NSsym_prev_over <- sym_se_3school_overall %>% 
  group_by(School, sym_nausea) %>% 
  tally()
print(NSsym_prev_over)

NSse_prev_over <- sym_se_3school_overall %>% 
  group_by(School, se_nausea) %>% 
  tally()
print(NSse_prev_over)

UPsym_prev_over <- sym_se_3school_overall %>% 
  group_by(School, sym_urine_pain) %>% 
  tally()
print(UPsym_prev_over)

UPse_prev_over <- sym_se_3school_overall %>% 
  group_by(School, se_urine_pain) %>% 
  tally()
print(UPse_prev_over)

##### PLOT - INTENSITY & SYM/SE -----

# prepare the dataframe - 3schools
side_effect_columns <- c("se_abdom_pain", "se_headache", "se_diarrhoea", "se_itching", 
                         "se_urine_pain", "se_nausea", "se_blood_stool")
symptom_columns <- c("sym_urine_pain", "sym_abdom_pain", "sym_headache", "sym_itching", 
                     "sym_diarrhoea", "sym_nausea", "sym_blood_stool")

side_effect_long <- sym_se_3school_overall %>%
  dplyr::select(School, CID, inf_int, all_of(side_effect_columns)) %>%
  pivot_longer(cols = all_of(side_effect_columns), names_to = "side_effect", values_to = "se_status")

symptom_long <- sym_se_3school_overall %>%
  dplyr::select(School, CID, inf_int, all_of(symptom_columns)) %>%
  pivot_longer(cols = all_of(symptom_columns), names_to = "symptom", values_to = "sym_status")

side_effect_long <- side_effect_long %>%
  mutate(side_effect = recode(side_effect, 
                              se_abdom_pain = "abdominal pain", 
                              se_headache = "headache",
                              se_diarrhoea = "diarrhoea",
                              se_itching = "itching",
                              se_urine_pain = "urinate pain",
                              se_nausea = "nausea",
                              se_blood_stool = "blood stool"))

symptom_long <- symptom_long %>% 
  mutate(symptom = recode(symptom,
                          sym_urine_pain = "urinate pain",
                          sym_abdom_pain = "abdominal pain",
                          sym_headache = "headache",
                          sym_itching = "itching",
                          sym_diarrhoea = "diarrhoea",
                          sym_nausea = "nausea",
                          sym_blood_stool = "blood stool"))

symptom_long$symptom <- factor(symptom_long$symptom, levels = c("abdominal pain", "blood stool", "diarrhoea", "headache", "itching", "nausea", "urinate pain"))
side_effect_long$side_effect <- factor(side_effect_long$side_effect, levels = c("abdominal pain", "blood stool", "diarrhoea", "headache", "itching", "nausea", "urinate pain"))
symptom_long <- na.omit(symptom_long)
side_effect_long <- na.omit(side_effect_long)


symptom_long$sym_status <- as.character(symptom_long$sym_status)
symptom_long$sym_status[symptom_long$sym_status==1]<- "yes"
symptom_long$sym_status[symptom_long$sym_status==0]<- "no"

symptom_long$sym_status <- factor(symptom_long$sym_status, levels = c("yes", "no"))


side_effect_long$se_status <- as.character(side_effect_long$se_status)
side_effect_long$se_status[side_effect_long$se_status==1]<- "yes"
side_effect_long$se_status[side_effect_long$se_status==0]<- "no"

side_effect_long$se_status <- factor(side_effect_long$se_status, levels = c("yes", "no"))

# PLOTS:

labeller_musubi <- as_labeller(function(x){
  ifelse(x == "Musubi", "Musubi CoG", x)
})

# by 3 schools

plot_3s_p_sym <- ggplot()+
  geom_point(data = symptom_long,
             aes(x = symptom, y = inf_int, colour = sym_status), position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3)+
  theme_bw()+
  scale_colour_manual(values=col_status, name="Symptom reported")+
  ylab("Infection Intensity (EPG)")+
  xlab("Symptoms")+
  theme(text = element_text(size = 14))+
  facet_grid(School~., labeller = labeller_musubi)

plot_3s_pb_sym <- ggplot() +
  geom_point(data = symptom_long,
             aes(x = symptom, y = inf_int, colour = sym_status), 
             position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3) +
  geom_boxplot(data = symptom_long, 
               aes(x = symptom, y = inf_int, fill = sym_status), 
               position = position_dodge(width = 0.75), alpha = 0.5, outlier.shape = NA) +
  theme_bw() +
  scale_colour_manual(values = col_status, name = "Symptom reported") +
  scale_fill_manual(values = col_status, name = "Symptom reported") +
  ylab("Infection Intensity (EPG)") +
  xlab("Symptoms") +
  facet_grid(School~., labeller = labeller_musubi)

plot_3s_p_se <- ggplot()+
  geom_point(data = side_effect_long,
             aes(x = side_effect, y = inf_int, colour = se_status), position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3)+
  theme_bw()+
  scale_colour_manual(values=col_status, name="Side effect reported")+
  ylab("Infection Intensity (EPG)")+
  xlab("Side effects")+
  theme(text = element_text(size = 14))+
  facet_grid(School~., labeller = labeller_musubi)

plot_3s_pb_se <- ggplot() +
  geom_point(data = side_effect_long,
             aes(x = side_effect, y = inf_int, colour = se_status), 
             position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3) +
  geom_boxplot(data = side_effect_long, 
               aes(x = side_effect, y = inf_int, fill = se_status), 
               position = position_dodge(width = 0.75), alpha = 0.5, outlier.shape = NA) +
  theme_bw() +
  scale_colour_manual(values = col_status, name = "Side effect reported") +
  scale_fill_manual(values = col_status, name = "Side effect reported") +
  ylab("Infection Intensity (EPG)") +
  xlab("Side effects") +
  facet_grid(School~., labeller = labeller_musubi)

plot_3s_p <- plot_3s_p_sym / plot_3s_p_se
print(plot_3s_p)
ggsave("plot_3s_p.pdf")
ggsave("plot_3s_p.png")

plot_3s_pb <- plot_3s_pb_sym / plot_3s_pb_se
print(plot_3s_pb)
ggsave("plot_3s_pb.pdf")
ggsave("plot_3s_pb.png")

# by together

plot_p_sym <- ggplot()+
  geom_point(data = symptom_long,
             aes(x = symptom, y = inf_int, colour = sym_status), position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3)+
  theme_bw()+
  scale_colour_manual(values=col_status, name="Symptom reported")+
  ylab("Infection Intensity (EPG)")+
  xlab("Symptoms")

plot_pb_sym <- ggplot() +
  geom_point(data = symptom_long,
             aes(x = symptom, y = inf_int, colour = sym_status), 
             position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3) +
  geom_boxplot(data = symptom_long, 
               aes(x = symptom, y = inf_int, fill = sym_status), 
               position = position_dodge(width = 0.75), alpha = 0.5, outlier.shape = NA) +
  theme_bw() +
  scale_colour_manual(values = col_status, name = "Symptom reported") +
  scale_fill_manual(values = col_status, name = "Symptom reported") +
  ylab("Infection Intensity (EPG)") +
  xlab("Symptoms")

plot_p_se <- ggplot()+
  geom_point(data = side_effect_long,
             aes(x = side_effect, y = inf_int, colour = se_status), position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3)+
  theme_bw()+
  scale_colour_manual(values=col_status, name="Side effect reported")+
  ylab("Infection Intensity (EPG)")+
  xlab("Side effects")

plot_pb_se <- ggplot() +
  geom_point(data = side_effect_long,
             aes(x = side_effect, y = inf_int, colour = se_status), 
             position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3) +
  geom_boxplot(data = side_effect_long, 
               aes(x = side_effect, y = inf_int, fill = se_status), 
               position = position_dodge(width = 0.75), alpha = 0.5, outlier.shape = NA) +
  theme_bw() +
  scale_colour_manual(values = col_status, name = "Side effect reported") +
  scale_fill_manual(values = col_status, name = "Side effect reported") +
  ylab("Infection Intensity (EPG)") +
  xlab("Side effects")

plot_p <- plot_p_sym / plot_p_se
print(plot_p)
ggsave("plot_p.pdf")
ggsave("plot_p.png")

plot_pb <- plot_pb_sym / plot_pb_se
print(plot_pb)
ggsave("plot_pb.pdf")
ggsave("plot_pb.png")

# by 2 schools
symptom_long_2s <- symptom_long %>% 
  filter(!School == "Bwondha")

side_effect_long_2s <- side_effect_long %>% 
  filter(!School == "Bwondha")

plot_2s_p_sym <- ggplot()+
  geom_point(data = symptom_long_2s,
             aes(x = symptom, y = inf_int, colour = sym_status), position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3)+
  theme_bw()+
  scale_colour_manual(values=col_status, name="Symptom reported")+
  ylab("Infection Intensity (EPG)")+
  xlab("Symptoms")+
  facet_grid(School~., labeller = labeller_musubi)

plot_2s_pb_sym <- ggplot() +
  geom_point(data = symptom_long_2s,
             aes(x = symptom, y = inf_int, colour = sym_status), 
             position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3) +
  geom_boxplot(data = symptom_long_2s, 
               aes(x = symptom, y = inf_int, fill = sym_status), 
               position = position_dodge(width = 0.75), alpha = 0.5, outlier.shape = NA) +
  theme_bw() +
  scale_colour_manual(values = col_status, name = "Symptom reported") +
  scale_fill_manual(values = col_status, name = "Symptom reported") +
  ylab("Infection Intensity (EPG)") +
  xlab("Symptoms") +
  facet_grid(School~., labeller = labeller_musubi)

plot_2s_p_se <- ggplot()+
  geom_point(data = side_effect_long_2s,
             aes(x = side_effect, y = inf_int, colour = se_status), position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3)+
  theme_bw()+
  scale_colour_manual(values=col_status, name="Side effect reported")+
  ylab("Infection Intensity (EPG)")+
  xlab("Side effects")+
  facet_grid(School~., labeller = labeller_musubi)

plot_2s_pb_se <- ggplot() +
  geom_point(data = side_effect_long_2s,
             aes(x = side_effect, y = inf_int, colour = se_status), 
             position = position_jitter(width = 0.35, height = 0.01), alpha = 0.3) +
  geom_boxplot(data = side_effect_long_2s, 
               aes(x = side_effect, y = inf_int, fill = se_status), 
               position = position_dodge(width = 0.75), alpha = 0.5, outlier.shape = NA) +
  theme_bw() +
  scale_colour_manual(values = col_status, name = "Side effect reported") +
  scale_fill_manual(values = col_status, name = "Side effect reported") +
  ylab("Infection Intensity (EPG)") +
  xlab("Side effects") +
  facet_grid(School~., labeller = labeller_musubi)

plot_2s_p <- plot_2s_p_sym / plot_2s_p_se
print(plot_2s_p)
ggsave("plot_2s_p.pdf")
ggsave("plot_2s_p.png")

plot_2s_pb <- plot_2s_pb_sym / plot_2s_pb_se
print(plot_2s_pb)
ggsave("plot_2s_pb.pdf")
ggsave("plot_2s_pb.png")


