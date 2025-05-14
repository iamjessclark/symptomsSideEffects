# Hypothesis 2: Effect of S. masoni infection intnesity on the reporting of pre-treatment symptoms

# Bugoto LV ----

## tKKDist T1 ----
tKKDistT1_BUG <- data.frame(matrix(nrow=40000, ncol=26))

Index <- seq(120, 3000, 120) # from 0 to the highest estimated egg count in blocks of 24 because one slide is multiplied by 24 to give eggs per gram
Index1 <- seq(1, 2977, 120)
cats <-vector()

for(i in 1:25){
  cats[i]<- paste(Index1[i], Index[i],sep="-")
}

cats <- c(0,cats)

colnames(tKKDistT1_BUG) <- cats

tKKDistT1_BUG$`0`<-rowSums(EstEggT1BG==0)

#this tells me who has 0 egg countglmms
whoTKK_BUG <- list()

for(i in 1:nrow(EstEggT1BG)){
  whoTKK_BUG[[i]] <- list()
  for(j in 2:ncol(tKKDistT1_BUG)){
    tKKDistT1_BUG[i,j] <- length(which(EstEggT1BG[i,]>=Index1[[j-1]] & EstEggT1BG[i,]<=Index[[j-1]]))
    whoTKK_BUG[[i]][[1]] <- which(EstEggT1BG[i,]==0)
    whoTKK_BUG[[i]][[j]] <-  which(EstEggT1BG[i,]>=Index1[[j-1]] & EstEggT1BG[i,]<=Index[[j-1]])
  }
}
tKKDistT1_BUG <- as.data.frame(tKKDistT1_BUG)

## headache_sym ----
EggT1dataHA_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(HeadacheSymp = BugotoSymp2004$headache)
EggT1dataHA_BUG$HeadacheSymp[which(EggT1dataHA_BUG$HeadacheSymp=="2")] <- 0

EggT1dataHA_BUG <- EggT1dataHA_BUG %>% 
  filter(EggT1dataHA_BUG$HeadacheSymp=="1")
EggT1dataHA_BUG <- EggT1dataHA_BUG[,-40001]
EggT1dataHA_BUG <- round(as.matrix(t(EggT1dataHA_BUG)))

HAtkkcat_Symp_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(HAtkkcat_Symp_BUG) <- cats
HAtkkcat_Symp_BUG$`0`<-rowSums(EggT1dataHA_BUG==0)

HATKK_Symp_BUG <- list()

for(i in 1:nrow(EggT1dataHA_BUG)){
  HATKK_Symp_BUG[[i]] <- list()
  for(j in 2:ncol(HAtkkcat_Symp_BUG)){
    HAtkkcat_Symp_BUG[i,j] <- length(which(EggT1dataHA_BUG[i,]>=Index1[[j-1]] & EggT1dataHA_BUG[i,]<=Index[[j-1]]))
    HATKK_Symp_BUG[[i]][[1]] <- which(EggT1dataHA_BUG[i,]==0)
    HATKK_Symp_BUG[[i]][[j]] <-  which(EggT1dataHA_BUG[i,]>=Index1[[j-1]] & EggT1dataHA_BUG[i,]<=Index[[j-1]])
  }
}

HAtkkcat_Symp_BUG <- as.data.frame(HAtkkcat_Symp_BUG)

eggcountscats <- as.data.frame(cats)
eggcountscats$cats <- as.factor(eggcountscats$cats)

Proportion_HATKKT1_BUG <- as.data.frame(HAtkkcat_Symp_BUG/tKKDistT1_BUG)

# 0/0 is NaN (no NA) - NaN means "not actual number" 
# lets use the quantile function again to get at the proper distribution of proportions

# make into a matrix - cannot remove NaNs until you have done this
# I have saved as a new object so I don't break all of your code. 
Proportion_HATKKT1_BUG2 <- as.matrix(Proportion_HATKKT1_BUG)

# remove the NaNs
Proportion_HATKKT1_BUG2[is.nan(Proportion_HATKKT1_BUG2)] <- 0
Proportion_HATKKT1_BUG2 <- as.data.frame(Proportion_HATKKT1_BUG2)

# check no remaining NaNs 
str(Proportion_HATKKT1_BUG2)

# this gives you the min (2.5%), mid (50%), and max (97.5%) quantiles for the proportions in each category. 
quantProportions_HA_sym_BUG <- sapply(Proportion_HATKKT1_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

# turn it around so that it is the shape of a sensible dataframe you can use to run your models 
quantProportions_HA_sym_BUG <- as.data.frame(t(quantProportions_HA_sym_BUG))
quantProportions_HA_sym_BUG$categories <- rownames(quantProportions_HA_sym_BUG)
colnames(quantProportions_HA_sym_BUG) <- c("min", "mid", "upper", "categories")

# this looks very different to the original (Proportion_HATKKT1_BUG)
# but its actually not, the upper quantile still shows a high proportion of children
# with headaches in the 9th egg count category, but these don't show up in the 
# mid quantile, because actually most of the proportions are zero. 

# now we want to run three models, so that we obtain three curves, you will plot the "mid" 
# curve in a colour, and the min and upper curves in light grey. 
quantProportions_HA_sym_BUG$dummyNum <- 1:nrow(quantProportions_HA_sym_BUG)

quantProportions_HA_sym_BUG$categories <- factor(quantProportions_HA_sym_BUG$categories,levels = quantProportions_HA_sym_BUG$categories)

ggplot(quantProportions_HA_sym_BUG, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_HA_sym_BUG <- drm(min ~ dummyNum, data = quantProportions_HA_sym_BUG, fct = CRS.6())
model_mid_HA_sym_BUG <- drm(mid ~ dummyNum, data = quantProportions_HA_sym_BUG, fct = W1.3())
model_upper_HA_sym_BUG <- drm(upper ~ dummyNum, data = quantProportions_HA_sym_BUG, fct = BC.4())

pred_min_HA_sym_BUG <- predict(model_min_HA_sym_BUG, newdata = data.frame(dummyNum = quantProportions_HA_sym_BUG$dummyNum))
pred_mid_HA_sym_BUG <- predict(model_mid_HA_sym_BUG, newdata = data.frame(dummyNum = quantProportions_HA_sym_BUG$dummyNum))
pred_upper_HA_sym_BUG <- predict(model_upper_HA_sym_BUG, newdata = data.frame(dummyNum = quantProportions_HA_sym_BUG$dummyNum))

quantProportions_HA_sym_BUG <- quantProportions_HA_sym_BUG[-(16:26),]

pred_min_HA_sym_BUG <- pred_min_HA_sym_BUG[-(16:26)]
pred_mid_HA_sym_BUG <- pred_mid_HA_sym_BUG[-(16:26)]
pred_upper_HA_sym_BUG <- pred_upper_HA_sym_BUG[-(16:26)]

pred_df_HA_sym_BUG <- data.frame(
  categories = rep(quantProportions_HA_sym_BUG$categories, times = 3),
  proportion = c(pred_min_HA_sym_BUG, pred_mid_HA_sym_BUG, pred_upper_HA_sym_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_HA_sym_BUG)))

pred_df_HA_sym_BUG$group <- factor(pred_df_HA_sym_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_HA_sym_BUG <- pred_df_HA_sym_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_HA_sym_BUG$categories <- factor(ribbon_df_HA_sym_BUG$categories, levels = quantProportions_HA_sym_BUG$categories)

pred_df_HA_sym_BUG$legend <- ifelse(pred_df_HA_sym_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 
  
ggplot() +
  geom_ribbon(data = ribbon_df_HA_sym_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_HA_sym_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_HA_sym_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("HeadacheproportionT1_fitted_BUG.pdf", height = 15, width = 15)
ggsave("HeadacheproportionT1_fitted_BUG.jpg", height = 15, width = 15)

## abdom_pain_sym ----
EggT1dataAP_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(Abdom_painSymp = BugotoSymp2004$abdom_pain)
EggT1dataAP_BUG$Abdom_painSymp[which(EggT1dataAP_BUG$Abdom_painSymp=="2")] <- 0

EggT1dataAP_BUG <- EggT1dataAP_BUG %>% 
  filter(EggT1dataAP_BUG$Abdom_painSymp=="1")
EggT1dataAP_BUG <- EggT1dataAP_BUG[,-40001]
EggT1dataAP_BUG <- round(as.matrix(t(EggT1dataAP_BUG)))

APtkkcat_Symp_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(APtkkcat_Symp_BUG) <- cats
APtkkcat_Symp_BUG$`0`<-rowSums(EggT1dataAP_BUG==0)

APTKK_Symp_BUG <- list()

for(i in 1:nrow(EggT1dataAP_BUG)){
  APTKK_Symp_BUG[[i]] <- list()
  for(j in 2:ncol(APtkkcat_Symp_BUG)){
    APtkkcat_Symp_BUG[i,j] <- length(which(EggT1dataAP_BUG[i,]>=Index1[[j-1]] & EggT1dataAP_BUG[i,]<=Index[[j-1]]))
    APTKK_Symp_BUG[[i]][[1]] <- which(EggT1dataAP_BUG[i,]==0)
    APTKK_Symp_BUG[[i]][[j]] <-  which(EggT1dataAP_BUG[i,]>=Index1[[j-1]] & EggT1dataAP_BUG[i,]<=Index[[j-1]])
  }
}

APtkkcat_Symp_BUG <- as.data.frame(APtkkcat_Symp_BUG)

Proportion_APTKKT1_BUG <- as.data.frame(APtkkcat_Symp_BUG/tKKDistT1_BUG)

Proportion_APTKKT1_BUG2 <- as.matrix(Proportion_APTKKT1_BUG)

# remove the NaNs
Proportion_APTKKT1_BUG2[is.nan(Proportion_APTKKT1_BUG2)] <- 0
Proportion_APTKKT1_BUG2 <- as.data.frame(Proportion_APTKKT1_BUG2)

# check no remaining NaNs 
str(Proportion_APTKKT1_BUG2)

quantProportions_AP_sym_BUG <- sapply(Proportion_APTKKT1_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_AP_sym_BUG <- as.data.frame(t(quantProportions_AP_sym_BUG))
quantProportions_AP_sym_BUG$categories <- rownames(quantProportions_AP_sym_BUG)
colnames(quantProportions_AP_sym_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_AP_sym_BUG$dummyNum <- 1:nrow(quantProportions_AP_sym_BUG)

quantProportions_AP_sym_BUG$categories <- factor(quantProportions_AP_sym_BUG$categories,levels = quantProportions_AP_sym_BUG$categories)

ggplot(quantProportions_AP_sym_BUG, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_AP_sym_BUG <- drm(min ~ dummyNum, data = quantProportions_AP_sym_BUG, fct = CRS.6())
model_mid_AP_sym_BUG <- drm(mid ~ dummyNum, data = quantProportions_AP_sym_BUG, fct = CRS.6())
model_upper_AP_sym_BUG <- drm(upper ~ dummyNum, data = quantProportions_AP_sym_BUG, fct = CRS.6())

pred_min_AP_sym_BUG <- predict(model_min_AP_sym_BUG, newdata = data.frame(dummyNum = quantProportions_AP_sym_BUG$dummyNum))
pred_mid_AP_sym_BUG <- predict(model_mid_AP_sym_BUG, newdata = data.frame(dummyNum = quantProportions_AP_sym_BUG$dummyNum))
pred_upper_AP_sym_BUG <- predict(model_upper_AP_sym_BUG, newdata = data.frame(dummyNum = quantProportions_AP_sym_BUG$dummyNum))

quantProportions_AP_sym_BUG <- quantProportions_AP_sym_BUG[-(16:26),]
pred_min_AP_sym_BUG <- pred_min_AP_sym_BUG[-(16:26)]
pred_mid_AP_sym_BUG <- pred_mid_AP_sym_BUG[-(16:26)]
pred_upper_AP_sym_BUG <- pred_upper_AP_sym_BUG[-(16:26)]

pred_df_AP_sym_BUG <- data.frame(
  categories = rep(quantProportions_AP_sym_BUG$categories, times = 3),
  proportion = c(pred_min_AP_sym_BUG, pred_mid_AP_sym_BUG, pred_upper_AP_sym_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_AP_sym_BUG)))

pred_df_AP_sym_BUG$group <- factor(pred_df_AP_sym_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_AP_sym_BUG <- pred_df_AP_sym_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_AP_sym_BUG$categories <- factor(ribbon_df_AP_sym_BUG$categories, levels = quantProportions_AP_sym_BUG$categories)

pred_df_AP_sym_BUG$legend <- ifelse(pred_df_AP_sym_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_AP_sym_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_AP_sym_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_AP_sym_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("AbdompainproportionT1_fitted_BUG.pdf", height = 15, width = 15)
ggsave("AbdompainproportionT1_fitted_BUG.jpg", height = 15, width = 15)

## urine_pain_sym ----
EggT1dataUP_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(Urine_painSymp = BugotoSymp2004$urine_pain)
EggT1dataUP_BUG$Urine_painSymp[which(EggT1dataUP_BUG$Urine_painSymp=="2")] <- 0

EggT1dataUP_BUG <- EggT1dataUP_BUG %>% 
  filter(EggT1dataUP_BUG$Urine_painSymp=="1")
EggT1dataUP_BUG <- EggT1dataUP_BUG[,-40001]
EggT1dataUP_BUG <- round(as.matrix(t(EggT1dataUP_BUG)))

UPtkkcat_Symp_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(UPtkkcat_Symp_BUG) <- cats
UPtkkcat_Symp_BUG$`0`<-rowSums(EggT1dataUP_BUG==0)

UPTKK_Symp_BUG <- list()

for(i in 1:nrow(EggT1dataUP_BUG)){
  UPTKK_Symp_BUG[[i]] <- list()
  for(j in 2:ncol(UPtkkcat_Symp_BUG)){
    UPtkkcat_Symp_BUG[i,j] <- length(which(EggT1dataUP_BUG[i,]>=Index1[[j-1]] & EggT1dataUP_BUG[i,]<=Index[[j-1]]))
    UPTKK_Symp_BUG[[i]][[1]] <- which(EggT1dataUP_BUG[i,]==0)
    UPTKK_Symp_BUG[[i]][[j]] <-  which(EggT1dataUP_BUG[i,]>=Index1[[j-1]] & EggT1dataUP_BUG[i,]<=Index[[j-1]])
  }
}

UPtkkcat_Symp_BUG <- as.data.frame(UPtkkcat_Symp_BUG)

Proportion_UPTKKT1_BUG <- as.data.frame(UPtkkcat_Symp_BUG/tKKDistT1_BUG)
Proportion_UPTKKT1_BUG2 <- as.matrix(Proportion_UPTKKT1_BUG)

# remove the NaNs
Proportion_UPTKKT1_BUG2[is.nan(Proportion_UPTKKT1_BUG2)] <- 0
Proportion_UPTKKT1_BUG2 <- as.data.frame(Proportion_UPTKKT1_BUG2)

# check no remaining NaNs 
str(Proportion_UPTKKT1_BUG2)

quantProportions_UP_sym_BUG <- sapply(Proportion_UPTKKT1_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_UP_sym_BUG <- as.data.frame(t(quantProportions_UP_sym_BUG))
quantProportions_UP_sym_BUG$categories <- rownames(quantProportions_UP_sym_BUG)
colnames(quantProportions_UP_sym_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_UP_sym_BUG$dummyNum <- 1:nrow(quantProportions_UP_sym_BUG)

quantProportions_UP_sym_BUG$categories <- factor(quantProportions_UP_sym_BUG$categories,levels = quantProportions_UP_sym_BUG$categories)

ggplot(quantProportions_UP_sym_BUG, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_UP_sym_BUG <- drm(min ~ dummyNum, data = quantProportions_UP_sym_BUG, fct = BC.5())
model_mid_UP_sym_BUG <- drm(mid ~ dummyNum, data = quantProportions_UP_sym_BUG, fct = W1.3())
model_upper_UP_sym_BUG <- drm(upper ~ dummyNum, data = quantProportions_UP_sym_BUG, fct = BC.5())

pred_min_UP_sym_BUG <- predict(model_min_UP_sym_BUG, newdata = data.frame(dummyNum = quantProportions_UP_sym_BUG$dummyNum))
pred_mid_UP_sym_BUG <- predict(model_mid_UP_sym_BUG, newdata = data.frame(dummyNum = quantProportions_UP_sym_BUG$dummyNum))
pred_upper_UP_sym_BUG <- predict(model_upper_UP_sym_BUG, newdata = data.frame(dummyNum = quantProportions_UP_sym_BUG$dummyNum))

quantProportions_UP_sym_BUG <- quantProportions_UP_sym_BUG[-(16:26),]

pred_min_UP_sym_BUG <- pred_min_UP_sym_BUG[-(16:26)]
model_mid_UP_sym_BUG <- model_mid_UP_sym_BUG[-(16:26)]
model_upper_UP_sym_BUG <- model_upper_UP_sym_BUG[-(16:26)]

pred_df_UP_sym_BUG <- data.frame(
  categories = rep(quantProportions_UP_sym_BUG$categories, times = 3),
  proportion = c(pred_min_UP_sym_BUG, pred_mid_UP_sym_BUG, pred_upper_UP_sym_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_UP_sym_BUG)))

pred_df_UP_sym_BUG$group <- factor(pred_df_UP_sym_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_UP_sym_BUG <- pred_df_UP_sym_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_UP_sym_BUG$categories <- factor(ribbon_df_UP_sym_BUG$categories, levels = quantProportions_UP_sym_BUG$categories)

pred_df_UP_sym_BUG$legend <- ifelse(pred_df_UP_sym_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_UP_sym_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_UP_sym_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_UP_sym_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Urine_painproportionT1_fitted_BUG.pdf", height = 15, width = 15)
ggsave("Urine_painproportionT1_fitted_BUG.jpg", height = 15, width = 15)

## itching ----
EggT1dataIR_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(ItchingSymp = BugotoSymp2004$itching)
EggT1dataIR_BUG$ItchingSymp[which(EggT1dataIR_BUG$ItchingSymp=="2")] <- 0

EggT1dataIR_BUG <- EggT1dataIR_BUG %>% 
  filter(EggT1dataIR_BUG$ItchingSymp=="1")
EggT1dataIR_BUG <- EggT1dataIR_BUG[,-40001]
EggT1dataIR_BUG <- round(as.matrix(t(EggT1dataIR_BUG)))

IRtkkcat_Symp_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(IRtkkcat_Symp_BUG) <- cats
IRtkkcat_Symp_BUG$`0`<-rowSums(EggT1dataIR_BUG==0)

IRTKK_Symp_BUG <- list()

for(i in 1:nrow(EggT1dataIR_BUG)){
  IRTKK_Symp_BUG[[i]] <- list()
  for(j in 2:ncol(IRtkkcat_Symp_BUG)){
    IRtkkcat_Symp_BUG[i,j] <- length(which(EggT1dataIR_BUG[i,]>=Index1[[j-1]] & EggT1dataIR_BUG[i,]<=Index[[j-1]]))
    IRTKK_Symp_BUG[[i]][[1]] <- which(EggT1dataIR_BUG[i,]==0)
    IRTKK_Symp_BUG[[i]][[j]] <-  which(EggT1dataIR_BUG[i,]>=Index1[[j-1]] & EggT1dataIR_BUG[i,]<=Index[[j-1]])
  }
}

IRtkkcat_Symp_BUG <- as.data.frame(IRtkkcat_Symp_BUG)

Proportion_IRTKKT1_BUG <- as.data.frame(IRtkkcat_Symp_BUG/tKKDistT1_BUG)

Proportion_IRTKKT1_BUG2 <- as.matrix(Proportion_IRTKKT1_BUG)

# remove the NaNs
Proportion_IRTKKT1_BUG2[is.nan(Proportion_IRTKKT1_BUG2)] <- 0
Proportion_IRTKKT1_BUG2 <- as.data.frame(Proportion_IRTKKT1_BUG2)

# check no remaining NaNs 
str(Proportion_IRTKKT1_BUG2)

quantProportions_IR_sym_BUG <- sapply(Proportion_IRTKKT1_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_IR_sym_BUG <- as.data.frame(t(quantProportions_IR_sym_BUG))
quantProportions_IR_sym_BUG$categories <- rownames(quantProportions_IR_sym_BUG)
colnames(quantProportions_IR_sym_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_IR_sym_BUG$dummyNum <- 1:nrow(quantProportions_IR_sym_BUG)

quantProportions_IR_sym_BUG$categories <- factor(quantProportions_IR_sym_BUG$categories,levels = quantProportions_IR_sym_BUG$categories)

ggplot(quantProportions_IR_sym_BUG, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_IR_sym_BUG <- drm(min ~ dummyNum, data = quantProportions_IR_sym_BUG, fct = BC.5())
model_mid_IR_sym_BUG <- drm(mid ~ dummyNum, data = quantProportions_IR_sym_BUG, fct = BC.5())
model_upper_IR_sym_BUG <- drm(upper ~ dummyNum, data = quantProportions_IR_sym_BUG, fct = BC.5())

pred_min_IR_sym_BUG <- predict(model_min_IR_sym_BUG, newdata = data.frame(dummyNum = quantProportions_IR_sym_BUG$dummyNum))
pred_mid_IR_sym_BUG <- predict(model_mid_IR_sym_BUG, newdata = data.frame(dummyNum = quantProportions_IR_sym_BUG$dummyNum))
pred_upper_IR_sym_BUG <- predict(model_upper_IR_sym_BUG, newdata = data.frame(dummyNum = quantProportions_IR_sym_BUG$dummyNum))

quantProportions_IR_sym_BUG <- quantProportions_IR_sym_BUG[-(16:26),]

pred_min_IR_sym_BUG <- pred_min_IR_sym_BUG[-(16:26)]
pred_mid_IR_sym_BUG <- pred_mid_IR_sym_BUG[-(16:26)]
pred_upper_IR_sym_BUG <- pred_upper_IR_sym_BUG[-(16:26)]

pred_df_IR_sym_BUG <- data.frame(
  categories = rep(quantProportions_IR_sym_BUG$categories, times = 3),
  proportion = c(pred_min_IR_sym_BUG, pred_mid_IR_sym_BUG, pred_upper_IR_sym_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_IR_sym_BUG)))

pred_df_IR_sym_BUG$group <- factor(pred_df_IR_sym_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_IR_sym_BUG <- pred_df_IR_sym_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_IR_sym_BUG$categories <- factor(ribbon_df_IR_sym_BUG$categories, levels = quantProportions_IR_sym_BUG$categories)

pred_df_IR_sym_BUG$legend <- ifelse(pred_df_IR_sym_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ribbon_df_IR_sym_BUG <- pred_df_IR_sym_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper) %>%
  left_join(pred_df_IR_sym_BUG %>% 
              filter(group == "mid") %>% 
              rename(mid = proportion) %>% 
              dplyr::select(categories, mid), by = "categories")

ribbon_df_IR_sym_BUG <- ribbon_df_IR_sym_BUG %>%
  mutate(ymin = pmin(ymin, mid, ymax),
         ymax = pmax(ymin, mid, ymax))

ggplot() +
  geom_ribbon(data = ribbon_df_IR_sym_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_IR_sym_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_IR_sym_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("ItchingproportionT1_fitted_BUG.pdf", height = 15, width = 15)
ggsave("ItchingproportionT1_fitted_BUG.jpg", height = 15, width = 15)

ribbon_df_IR_sym_BUG <- ribbon_df_IR_sym_BUG[, -c(2, 5)]

## diarrhoea ---- 
EggT1dataDH_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(DiarrhoeaSymp = BugotoSymp2004$diarrhoea)
EggT1dataDH_BUG$DiarrhoeaSymp[which(EggT1dataDH_BUG$DiarrhoeaSymp=="2")] <- 0

EggT1dataDH_BUG <- EggT1dataDH_BUG %>% 
  filter(EggT1dataDH_BUG$DiarrhoeaSymp=="1")
EggT1dataDH_BUG <- EggT1dataDH_BUG[,-40001]
EggT1dataDH_BUG <- round(as.matrix(t(EggT1dataDH_BUG)))

DHtkkcat_Symp_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(DHtkkcat_Symp_BUG) <- cats
DHtkkcat_Symp_BUG$`0`<-rowSums(EggT1dataDH_BUG==0)

DHTKK_Symp_BUG <- list()

for(i in 1:nrow(EggT1dataDH_BUG)){
  DHTKK_Symp_BUG[[i]] <- list()
  for(j in 2:ncol(DHtkkcat_Symp_BUG)){
    DHtkkcat_Symp_BUG[i,j] <- length(which(EggT1dataDH_BUG[i,]>=Index1[[j-1]] & EggT1dataDH_BUG[i,]<=Index[[j-1]]))
    DHTKK_Symp_BUG[[i]][[1]] <- which(EggT1dataDH_BUG[i,]==0)
    DHTKK_Symp_BUG[[i]][[j]] <-  which(EggT1dataDH_BUG[i,]>=Index1[[j-1]] & EggT1dataDH_BUG[i,]<=Index[[j-1]])
  }
}

DHtkkcat_Symp_BUG <- as.data.frame(DHtkkcat_Symp_BUG)

Proportion_DHTKKT1_BUG <- as.data.frame(DHtkkcat_Symp_BUG/tKKDistT1_BUG)

Proportion_DHTKKT1_BUG2 <- as.matrix(Proportion_DHTKKT1_BUG)

# remove the NaNs
Proportion_DHTKKT1_BUG2[is.nan(Proportion_DHTKKT1_BUG2)] <- 0
Proportion_DHTKKT1_BUG2 <- as.data.frame(Proportion_DHTKKT1_BUG2)

# check no remaining NaNs 
str(Proportion_DHTKKT1_BUG2)

quantProportions_DH_sym_BUG <- sapply(Proportion_DHTKKT1_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_DH_sym_BUG <- as.data.frame(t(quantProportions_DH_sym_BUG))
quantProportions_DH_sym_BUG$categories <- rownames(quantProportions_DH_sym_BUG)
colnames(quantProportions_DH_sym_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_DH_sym_BUG$dummyNum <- 1:nrow(quantProportions_DH_sym_BUG)

quantProportions_DH_sym_BUG$categories <- factor(quantProportions_DH_sym_BUG$categories,levels = quantProportions_DH_sym_BUG$categories)

ggplot(quantProportions_DH_sym_BUG, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_DH_sym_BUG <- drm(min ~ dummyNum, data = quantProportions_DH_sym_BUG, fct = BC.5())
model_mid_DH_sym_BUG <- drm(mid ~ dummyNum, data = quantProportions_DH_sym_BUG, fct = LL.5())
model_upper_DH_sym_BUG <- drm(upper ~ dummyNum, data = quantProportions_DH_sym_BUG, fct = LL.5())

pred_min_DH_sym_BUG <- predict(model_min_DH_sym_BUG, newdata = data.frame(dummyNum = quantProportions_DH_sym_BUG$dummyNum))
pred_mid_DH_sym_BUG <- predict(model_mid_DH_sym_BUG, newdata = data.frame(dummyNum = quantProportions_DH_sym_BUG$dummyNum))
pred_upper_DH_sym_BUG <- predict(model_upper_DH_sym_BUG, newdata = data.frame(dummyNum = quantProportions_DH_sym_BUG$dummyNum))

quantProportions_DH_sym_BUG <- quantProportions_DH_sym_BUG[-(16:26),]

pred_min_DH_sym_BUG <- pred_min_DH_sym_BUG[-(16:26)]
pred_mid_DH_sym_BUG <- pred_mid_DH_sym_BUG[-(16:26)]
pred_upper_DH_sym_BUG <- pred_upper_DH_sym_BUG[-(16:26)]

pred_df_DH_sym_BUG <- data.frame(
  categories = rep(quantProportions_DH_sym_BUG$categories, times = 3),
  proportion = c(pred_min_DH_sym_BUG, pred_mid_DH_sym_BUG, pred_upper_DH_sym_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_DH_sym_BUG)))

pred_df_DH_sym_BUG$group <- factor(pred_df_DH_sym_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_DH_sym_BUG <- pred_df_DH_sym_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_DH_sym_BUG$categories <- factor(ribbon_df_DH_sym_BUG$categories, levels = quantProportions_DH_sym_BUG$categories)

pred_df_DH_sym_BUG$legend <- ifelse(pred_df_DH_sym_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_DH_sym_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_DH_sym_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_DH_sym_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("DiarrhoeaproportionT1_fitted_BUG.pdf", height = 15, width = 15)
ggsave("DiarrhoeaproportionT1_fitted_BUG.jpg", height = 15, width = 15)

## nausea_sym ----
EggT1dataNS_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(NauseaSymp = BugotoSymp2004$nausea)
EggT1dataNS_BUG$NauseaSymp[which(EggT1dataNS_BUG$NauseaSymp=="2")] <- 0

EggT1dataNS_BUG <- EggT1dataNS_BUG %>% 
  filter(EggT1dataNS_BUG$NauseaSymp=="1")
EggT1dataNS_BUG <- EggT1dataNS_BUG[,-40001]
EggT1dataNS_BUG <- round(as.matrix(t(EggT1dataNS_BUG)))

NStkkcat_Symp_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(NStkkcat_Symp_BUG) <- cats
NStkkcat_Symp_BUG$`0`<-rowSums(EggT1dataNS_BUG==0)

NSTKK_Symp_BUG <- list()

for(i in 1:nrow(EggT1dataNS_BUG)){
  NSTKK_Symp_BUG[[i]] <- list()
  for(j in 2:ncol(NStkkcat_Symp_BUG)){
    NStkkcat_Symp_BUG[i,j] <- length(which(EggT1dataNS_BUG[i,]>=Index1[[j-1]] & EggT1dataNS_BUG[i,]<=Index[[j-1]]))
    NSTKK_Symp_BUG[[i]][[1]] <- which(EggT1dataNS_BUG[i,]==0)
    NSTKK_Symp_BUG[[i]][[j]] <-  which(EggT1dataNS_BUG[i,]>=Index1[[j-1]] & EggT1dataNS_BUG[i,]<=Index[[j-1]])
  }
}

NStkkcat_Symp_BUG <- as.data.frame(NStkkcat_Symp_BUG)

Proportion_NSTKKT1_BUG <- as.data.frame(NStkkcat_Symp_BUG/tKKDistT1_BUG)

Proportion_NSTKKT1_BUG2 <- as.matrix(Proportion_NSTKKT1_BUG)

# remove the NaNs
Proportion_NSTKKT1_BUG2[is.nan(Proportion_NSTKKT1_BUG2)] <- 0
Proportion_NSTKKT1_BUG2 <- as.data.frame(Proportion_NSTKKT1_BUG2)

# check no remaining NaNs 
str(Proportion_NSTKKT1_BUG2)

quantProportions_NS_sym_BUG <- sapply(Proportion_NSTKKT1_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_NS_sym_BUG <- as.data.frame(t(quantProportions_NS_sym_BUG))
quantProportions_NS_sym_BUG$categories <- rownames(quantProportions_NS_sym_BUG)
colnames(quantProportions_NS_sym_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_NS_sym_BUG$dummyNum <- 1:nrow(quantProportions_NS_sym_BUG)

quantProportions_NS_sym_BUG$categories <- factor(quantProportions_NS_sym_BUG$categories,levels = quantProportions_NS_sym_BUG$categories)

ggplot(quantProportions_NS_sym_BUG, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_NS_sym_BUG <- drm(min ~ dummyNum, data = quantProportions_NS_sym_BUG, fct = BC.5())
model_mid_NS_sym_BUG <- drm(mid ~ dummyNum, data = quantProportions_NS_sym_BUG, fct = LL.5())
model_upper_NS_sym_BUG <- drm(upper ~ dummyNum, data = quantProportions_NS_sym_BUG, fct = BC.5())

pred_min_NS_sym_BUG <- predict(model_min_NS_sym_BUG, newdata = data.frame(dummyNum = quantProportions_NS_sym_BUG$dummyNum))
pred_mid_NS_sym_BUG <- predict(model_mid_NS_sym_BUG, newdata = data.frame(dummyNum = quantProportions_NS_sym_BUG$dummyNum))
pred_upper_NS_sym_BUG <- predict(model_upper_NS_sym_BUG, newdata = data.frame(dummyNum = quantProportions_NS_sym_BUG$dummyNum))

quantProportions_NS_sym_BUG <- quantProportions_NS_sym_BUG[-(16:26),]

pred_min_NS_sym_BUG <- pred_min_NS_sym_BUG[-(16:26)]
pred_mid_NS_sym_BUG <- pred_mid_NS_sym_BUG[-(16:26)]
pred_upper_NS_sym_BUG <- pred_upper_NS_sym_BUG[-(16:26)]

pred_df_NS_sym_BUG <- data.frame(
  categories = rep(quantProportions_NS_sym_BUG$categories, times = 3),
  proportion = c(pred_min_NS_sym_BUG, pred_mid_NS_sym_BUG, pred_upper_NS_sym_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_NS_sym_BUG)))

pred_df_NS_sym_BUG$group <- factor(pred_df_NS_sym_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_NS_sym_BUG <- pred_df_NS_sym_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_NS_sym_BUG$categories <- factor(ribbon_df_NS_sym_BUG$categories, levels = quantProportions_NS_sym_BUG$categories)

pred_df_NS_sym_BUG$legend <- ifelse(pred_df_NS_sym_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_NS_sym_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_NS_sym_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_NS_sym_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("NauseaproportionT1_fitted_BUG.pdf", height = 15, width = 15)
ggsave("NauseaproportionT1_fitted_BUG.jpg", height = 15, width = 15)

## blood_stool_sym ----
EggT1dataBS_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(Blood_stoolSymp = BugotoSymp2004$blood_stool)
EggT1dataBS_BUG$Blood_stoolSymp[which(EggT1dataBS_BUG$Blood_stoolSymp=="2")] <- 0

EggT1dataBS_BUG <- EggT1dataBS_BUG %>% 
  filter(EggT1dataBS_BUG$Blood_stoolSymp=="1")
EggT1dataBS_BUG <- EggT1dataBS_BUG[,-40001]
EggT1dataBS_BUG <- round(as.matrix(t(EggT1dataBS_BUG)))

BStkkcat_Symp_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(BStkkcat_Symp_BUG) <- cats
BStkkcat_Symp_BUG$`0`<-rowSums(EggT1dataBS_BUG==0)
BSTKK_Symp_BUG <- list()

for(i in 1:nrow(EggT1dataBS_BUG)){
  BSTKK_Symp_BUG[[i]] <- list()
  for(j in 2:ncol(BStkkcat_Symp_BUG)){
    BStkkcat_Symp_BUG[i,j] <- length(which(EggT1dataBS_BUG[i,]>=Index1[[j-1]] & EggT1dataBS_BUG[i,]<=Index[[j-1]]))
    BSTKK_Symp_BUG[[i]][[1]] <- which(EggT1dataBS_BUG[i,]==0)
    BSTKK_Symp_BUG[[i]][[j]] <-  which(EggT1dataBS_BUG[i,]>=Index1[[j-1]] & EggT1dataBS_BUG[i,]<=Index[[j-1]])
  }
}

BStkkcat_Symp_BUG <- as.data.frame(BStkkcat_Symp_BUG)

Proportion_BSTKKT1_BUG <- as.data.frame(BStkkcat_Symp_BUG/tKKDistT1_BUG)

Proportion_BSTKKT1_BUG2 <- as.matrix(Proportion_BSTKKT1_BUG)

# remove the NaNs
Proportion_BSTKKT1_BUG2[is.nan(Proportion_BSTKKT1_BUG2)] <- 0
Proportion_BSTKKT1_BUG2 <- as.data.frame(Proportion_BSTKKT1_BUG2)

# check no remaining NaNs 
str(Proportion_BSTKKT1_BUG2)

quantProportions_BS_sym_BUG <- sapply(Proportion_BSTKKT1_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_BS_sym_BUG <- as.data.frame(t(quantProportions_BS_sym_BUG))
quantProportions_BS_sym_BUG$categories <- rownames(quantProportions_BS_sym_BUG)
colnames(quantProportions_BS_sym_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_BS_sym_BUG$dummyNum <- 1:nrow(quantProportions_BS_sym_BUG)

quantProportions_BS_sym_BUG$categories <- factor(quantProportions_BS_sym_BUG$categories,levels = quantProportions_BS_sym_BUG$categories)

ggplot(quantProportions_BS_sym_BUG, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_BS_sym_BUG <- drm(min ~ dummyNum, data = quantProportions_BS_sym_BUG, fct = BC.5())
model_mid_BS_sym_BUG <- drm(mid ~ dummyNum, data = quantProportions_BS_sym_BUG, fct = BC.5())
model_upper_BS_sym_BUG <- drm(upper ~ dummyNum, data = quantProportions_BS_sym_BUG, fct = BC.5())

pred_min_BS_sym_BUG <- predict(model_min_BS_sym_BUG, newdata = data.frame(dummyNum = quantProportions_BS_sym_BUG$dummyNum))
pred_mid_BS_sym_BUG <- predict(model_mid_BS_sym_BUG, newdata = data.frame(dummyNum = quantProportions_BS_sym_BUG$dummyNum))
pred_upper_BS_sym_BUG <- predict(model_upper_BS_sym_BUG, newdata = data.frame(dummyNum = quantProportions_BS_sym_BUG$dummyNum))

quantProportions_BS_sym_BUG <- quantProportions_BS_sym_BUG[-(16:26),]

pred_min_BS_sym_BUG <- pred_min_BS_sym_BUG[-(16:26)]
pred_mid_BS_sym_BUG <- pred_mid_BS_sym_BUG[-(16:26)]
pred_upper_BS_sym_BUG <- pred_upper_BS_sym_BUG[-(16:26)]

pred_df_BS_sym_BUG <- data.frame(
  categories = rep(quantProportions_BS_sym_BUG$categories, times = 3),
  proportion = c(pred_min_BS_sym_BUG, pred_mid_BS_sym_BUG, pred_upper_BS_sym_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_BS_sym_BUG)))

pred_df_BS_sym_BUG$group <- factor(pred_df_BS_sym_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_BS_sym_BUG <- pred_df_BS_sym_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_BS_sym_BUG$categories <- factor(ribbon_df_BS_sym_BUG$categories, levels = quantProportions_BS_sym_BUG$categories)

pred_df_BS_sym_BUG$legend <- ifelse(pred_df_BS_sym_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_BS_sym_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_BS_sym_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_BS_sym_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Blood_stoolproportionT1_fitted_BUG.pdf", height = 15, width = 15)
ggsave("Blood_stoolproportionT1_fitted_BUG.jpg", height = 15, width = 15)


# Musubi ----

## tKKDist T1 ----
tKKDistT1 <- data.frame(matrix(nrow=40000, ncol=26))

Index <- seq(120, 3000, 120)
Index1 <- seq(1, 2977, 120)
cats <-vector()

for(i in 1:25){
  cats[i]<- paste(Index1[i], Index[i],sep="-")
}

cats <- c(0,cats)

colnames(tKKDistT1) <- cats

tKKDistT1$`0`<-rowSums(EstEggT1==0)

#this tells me who has 0 egg counts
whoTKK <- list()

for(i in 1:nrow(EstEggT1)){
  whoTKK[[i]] <- list()
  for(j in 2:ncol(tKKDistT1)){
    tKKDistT1[i,j] <- length(which(EstEggT1[i,]>=Index1[[j-1]] & EstEggT1[i,]<=Index[[j-1]]))
    whoTKK[[i]][[1]] <- which(EstEggT1[i,]==0)
    whoTKK[[i]][[j]] <-  which(EstEggT1[i,]>=Index1[[j-1]] & EstEggT1[i,]<=Index[[j-1]])
  }
}
tKKDistT1 <- as.data.frame(tKKDistT1)

## headache_sym ----
EggT1dataHA <- as.data.frame(t(EstEggT1)) %>% 
  mutate(HeadacheSymp = MusubiSymp2004$headache)
EggT1dataHA$HeadacheSymp[which(EggT1dataHA$HeadacheSymp=="2")] <- 0

EggT1dataHA <- EggT1dataHA %>% 
  filter(EggT1dataHA$HeadacheSymp=="1")
EggT1dataHA <- EggT1dataHA[,-40001]
EggT1dataHA <- round(as.matrix(t(EggT1dataHA)))

HAtkkcat_Symp <- data.frame(matrix(nrow=40000, ncol=26))
colnames(HAtkkcat_Symp) <- cats
HAtkkcat_Symp$`0`<-rowSums(EggT1dataHA==0)

HATKK_Symp <- list()

for(i in 1:nrow(EggT1dataHA)){
  HATKK_Symp[[i]] <- list()
  for(j in 2:ncol(HAtkkcat_Symp)){
    HAtkkcat_Symp[i,j] <- length(which(EggT1dataHA[i,]>=Index1[[j-1]] & EggT1dataHA[i,]<=Index[[j-1]]))
    HATKK_Symp[[i]][[1]] <- which(EggT1dataHA[i,]==0)
    HATKK_Symp[[i]][[j]] <-  which(EggT1dataHA[i,]>=Index1[[j-1]] & EggT1dataHA[i,]<=Index[[j-1]])
  }
}

HAtkkcat_Symp <- as.data.frame(HAtkkcat_Symp)

Proportion_HATKKT1 <- as.data.frame(HAtkkcat_Symp/tKKDistT1)
Proportion_HATKKT1_MUS2 <- as.matrix(Proportion_HATKKT1)

# remove the NaNs
Proportion_HATKKT1_MUS2[is.nan(Proportion_HATKKT1_MUS2)] <- 0
Proportion_HATKKT1_MUS2 <- as.data.frame(Proportion_HATKKT1_MUS2)

# check no remaining NaNs 
str(Proportion_HATKKT1_MUS2)

quantProportions_HA_sym_MUS <- sapply(Proportion_HATKKT1_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_HA_sym_MUS <- as.data.frame(t(quantProportions_HA_sym_MUS))
quantProportions_HA_sym_MUS$categories <- rownames(quantProportions_HA_sym_MUS)
colnames(quantProportions_HA_sym_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_HA_sym_MUS$dummyNum <- 1:nrow(quantProportions_HA_sym_MUS)

quantProportions_HA_sym_MUS$categories <- factor(quantProportions_HA_sym_MUS$categories,levels = quantProportions_HA_sym_MUS$categories)

ggplot(quantProportions_HA_sym_MUS, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_HA_sym_MUS <- drm(min ~ dummyNum, data = quantProportions_HA_sym_MUS, fct = LL.3())
model_mid_HA_sym_MUS <- drm(mid ~ dummyNum, data = quantProportions_HA_sym_MUS, fct = LL.3())
model_upper_HA_sym_MUS <- drm(upper ~ dummyNum, data = quantProportions_HA_sym_MUS, fct = LL.3())

pred_min_HA_sym_MUS <- predict(model_min_HA_sym_MUS, newdata = data.frame(dummyNum = quantProportions_HA_sym_MUS$dummyNum))
pred_mid_HA_sym_MUS <- predict(model_mid_HA_sym_MUS, newdata = data.frame(dummyNum = quantProportions_HA_sym_MUS$dummyNum))
pred_upper_HA_sym_MUS <- predict(model_upper_HA_sym_MUS, newdata = data.frame(dummyNum = quantProportions_HA_sym_MUS$dummyNum))

pred_df_HA_sym_MUS <- data.frame(
  categories = rep(quantProportions_HA_sym_MUS$categories, times = 3),
  proportion = c(pred_min_HA_sym_MUS, pred_mid_HA_sym_MUS, pred_upper_HA_sym_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_HA_sym_MUS)))

pred_df_HA_sym_MUS$group <- factor(pred_df_HA_sym_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_HA_sym_MUS <- pred_df_HA_sym_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_HA_sym_MUS$categories <- factor(ribbon_df_HA_sym_MUS$categories, levels = quantProportions_HA_sym_MUS$categories)

pred_df_HA_sym_MUS$legend <- ifelse(pred_df_HA_sym_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_HA_sym_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_HA_sym_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_HA_sym_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("HeadacheproportionT1_fitted.pdf", height = 15, width = 15)
ggsave("HeadacheproportionT1_fitted.jpg", height = 15, width = 15)

## abdom_pain_sym ----
EggT1dataAP <- as.data.frame(t(EstEggT1)) %>% 
  mutate(Abdom_painSymp = MusubiSymp2004$abdom_pain)
EggT1dataAP$Abdom_painSymp[which(EggT1dataAP$Abdom_painSymp=="2")] <- 0

EggT1dataAP <- EggT1dataAP %>% 
  filter(EggT1dataAP$Abdom_painSymp=="1")
EggT1dataAP <- EggT1dataAP[,-40001]
EggT1dataAP <- round(as.matrix(t(EggT1dataAP)))

APtkkcat_Symp <- data.frame(matrix(nrow=40000, ncol=26))
colnames(APtkkcat_Symp) <- cats
APtkkcat_Symp$`0`<-rowSums(EggT1dataAP==0)

APTKK_Symp <- list()

for(i in 1:nrow(EggT1dataAP)){
  APTKK_Symp[[i]] <- list()
  for(j in 2:ncol(APtkkcat_Symp)){
    APtkkcat_Symp[i,j] <- length(which(EggT1dataAP[i,]>=Index1[[j-1]] & EggT1dataAP[i,]<=Index[[j-1]]))
    APTKK_Symp[[i]][[1]] <- which(EggT1dataAP[i,]==0)
    APTKK_Symp[[i]][[j]] <-  which(EggT1dataAP[i,]>=Index1[[j-1]] & EggT1dataAP[i,]<=Index[[j-1]])
  }
}

APtkkcat_Symp <- as.data.frame(APtkkcat_Symp)

Proportion_APTKKT1 <- as.data.frame(APtkkcat_Symp/tKKDistT1)

Proportion_APTKKT1_MUS2 <- as.matrix(Proportion_APTKKT1)

# remove the NaNs
Proportion_APTKKT1_MUS2[is.nan(Proportion_APTKKT1_MUS2)] <- 0
Proportion_APTKKT1_MUS2 <- as.data.frame(Proportion_APTKKT1_MUS2)

# check no remaining NaNs 
str(Proportion_APTKKT1_MUS2)

quantProportions_AP_sym_MUS <- sapply(Proportion_APTKKT1_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_AP_sym_MUS <- as.data.frame(t(quantProportions_AP_sym_MUS))
quantProportions_AP_sym_MUS$categories <- rownames(quantProportions_AP_sym_MUS)
colnames(quantProportions_AP_sym_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_AP_sym_MUS$dummyNum <- 1:nrow(quantProportions_AP_sym_MUS)

quantProportions_AP_sym_MUS$categories <- factor(quantProportions_AP_sym_MUS$categories,levels = quantProportions_AP_sym_MUS$categories)

ggplot(quantProportions_AP_sym_MUS, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_AP_sym_MUS <- drm(min ~ dummyNum, data = quantProportions_AP_sym_MUS, fct = LL.3())
model_mid_AP_sym_MUS <- drm(mid ~ dummyNum, data = quantProportions_AP_sym_MUS, fct = LL.3())
model_upper_AP_sym_MUS <- drm(upper ~ dummyNum, data = quantProportions_AP_sym_MUS, fct = LL.3())

pred_min_AP_sym_MUS <- predict(model_min_AP_sym_MUS, newdata = data.frame(dummyNum = quantProportions_AP_sym_MUS$dummyNum))
pred_mid_AP_sym_MUS <- predict(model_mid_AP_sym_MUS, newdata = data.frame(dummyNum = quantProportions_AP_sym_MUS$dummyNum))
pred_upper_AP_sym_MUS <- predict(model_upper_AP_sym_MUS, newdata = data.frame(dummyNum = quantProportions_AP_sym_MUS$dummyNum))

pred_df_AP_sym_MUS <- data.frame(
  categories = rep(quantProportions_AP_sym_MUS$categories, times = 3),
  proportion = c(pred_min_AP_sym_MUS, pred_mid_AP_sym_MUS, pred_upper_AP_sym_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_AP_sym_MUS)))

pred_df_AP_sym_MUS$group <- factor(pred_df_AP_sym_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_AP_sym_MUS <- pred_df_AP_sym_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_AP_sym_MUS$categories <- factor(ribbon_df_AP_sym_MUS$categories, levels = quantProportions_AP_sym_MUS$categories)

pred_df_AP_sym_MUS$legend <- ifelse(pred_df_AP_sym_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_AP_sym_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_AP_sym_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_AP_sym_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("AbdompainproportionT1_fitted.pdf", height = 15, width = 15)
ggsave("AbdompainproportionT1_fitted.jpg", height = 15, width = 15)

## urine_pain_sym ----
EggT1dataUP <- as.data.frame(t(EstEggT1)) %>% 
  mutate(Urine_painSymp = MusubiSymp2004$urine_pain)
EggT1dataUP$Urine_painSymp[which(EggT1dataUP$Urine_painSymp=="2")] <- 0

EggT1dataUP <- EggT1dataUP %>% 
  filter(EggT1dataUP$Urine_painSymp=="1")
EggT1dataUP <- EggT1dataUP[,-40001]
EggT1dataUP <- round(as.matrix(t(EggT1dataUP)))

UPtkkcat_Symp <- data.frame(matrix(nrow=40000, ncol=26))
colnames(UPtkkcat_Symp) <- cats
UPtkkcat_Symp$`0`<-rowSums(EggT1dataUP==0)

UPTKK_Symp <- list()

for(i in 1:nrow(EggT1dataUP)){
  UPTKK_Symp[[i]] <- list()
  for(j in 2:ncol(UPtkkcat_Symp)){
    UPtkkcat_Symp[i,j] <- length(which(EggT1dataUP[i,]>=Index1[[j-1]] & EggT1dataUP[i,]<=Index[[j-1]]))
    UPTKK_Symp[[i]][[1]] <- which(EggT1dataUP[i,]==0)
    UPTKK_Symp[[i]][[j]] <-  which(EggT1dataUP[i,]>=Index1[[j-1]] & EggT1dataUP[i,]<=Index[[j-1]])
  }
}

UPtkkcat_Symp <- as.data.frame(UPtkkcat_Symp)

Proportion_UPTKKT1 <- as.data.frame(UPtkkcat_Symp/tKKDistT1)

Proportion_UPTKKT1_MUS2 <- as.matrix(Proportion_UPTKKT1)

# remove the NaNs
Proportion_UPTKKT1_MUS2[is.nan(Proportion_UPTKKT1_MUS2)] <- 0
Proportion_UPTKKT1_MUS2 <- as.data.frame(Proportion_UPTKKT1_MUS2)

# check no remaining NaNs 
str(Proportion_UPTKKT1_MUS2)

quantProportions_UP_sym_MUS <- sapply(Proportion_UPTKKT1_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_UP_sym_MUS <- as.data.frame(t(quantProportions_UP_sym_MUS))
quantProportions_UP_sym_MUS$categories <- rownames(quantProportions_UP_sym_MUS)
colnames(quantProportions_UP_sym_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_UP_sym_MUS$dummyNum <- 1:nrow(quantProportions_UP_sym_MUS)

quantProportions_UP_sym_MUS$categories <- factor(quantProportions_UP_sym_MUS$categories,levels = quantProportions_UP_sym_MUS$categories)

ggplot(quantProportions_UP_sym_MUS, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_UP_sym_MUS <- drm(min ~ dummyNum, data = quantProportions_UP_sym_MUS, fct = LL.3())
model_mid_UP_sym_MUS <- drm(mid ~ dummyNum, data = quantProportions_UP_sym_MUS, fct = BC.5())
model_upper_UP_sym_MUS <- drm(upper ~ dummyNum, data = quantProportions_UP_sym_MUS, fct = BC.5())

pred_min_UP_sym_MUS <- predict(model_min_UP_sym_MUS, newdata = data.frame(dummyNum = quantProportions_UP_sym_MUS$dummyNum))
pred_mid_UP_sym_MUS <- predict(model_mid_UP_sym_MUS, newdata = data.frame(dummyNum = quantProportions_UP_sym_MUS$dummyNum))
pred_upper_UP_sym_MUS <- predict(model_upper_UP_sym_MUS, newdata = data.frame(dummyNum = quantProportions_UP_sym_MUS$dummyNum))

pred_df_UP_sym_MUS <- data.frame(
  categories = rep(quantProportions_UP_sym_MUS$categories, times = 3),
  proportion = c(pred_min_UP_sym_MUS, pred_mid_UP_sym_MUS, pred_upper_UP_sym_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_UP_sym_MUS)))

pred_df_UP_sym_MUS$group <- factor(pred_df_UP_sym_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_UP_sym_MUS <- pred_df_UP_sym_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_UP_sym_MUS$categories <- factor(ribbon_df_UP_sym_MUS$categories, levels = quantProportions_UP_sym_MUS$categories)

pred_df_UP_sym_MUS$legend <- ifelse(pred_df_UP_sym_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_UP_sym_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_UP_sym_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_UP_sym_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Urine_painproportionT1_fitted.pdf", height = 15, width = 15)
ggsave("Urine_painproportionT1_fitted.jpg", height = 15, width = 15)

## itching ----
EggT1dataIR <- as.data.frame(t(EstEggT1)) %>% 
  mutate(ItchingSymp = MusubiSymp2004$itching)
EggT1dataIR$ItchingSymp[which(EggT1dataIR$ItchingSymp=="2")] <- 0

EggT1dataIR <- EggT1dataIR %>% 
  filter(EggT1dataIR$ItchingSymp=="1")
EggT1dataIR <- EggT1dataIR[,-40001]
EggT1dataIR <- round(as.matrix(t(EggT1dataIR)))

IRtkkcat_Symp <- data.frame(matrix(nrow=40000, ncol=26))
colnames(IRtkkcat_Symp) <- cats
IRtkkcat_Symp$`0`<-rowSums(EggT1dataIR==0)

IRTKK_Symp <- list()

for(i in 1:nrow(EggT1dataIR)){
  IRTKK_Symp[[i]] <- list()
  for(j in 2:ncol(IRtkkcat_Symp)){
    IRtkkcat_Symp[i,j] <- length(which(EggT1dataIR[i,]>=Index1[[j-1]] & EggT1dataIR[i,]<=Index[[j-1]]))
    IRTKK_Symp[[i]][[1]] <- which(EggT1dataIR[i,]==0)
    IRTKK_Symp[[i]][[j]] <-  which(EggT1dataIR[i,]>=Index1[[j-1]] & EggT1dataIR[i,]<=Index[[j-1]])
  }
}

IRtkkcat_Symp <- as.data.frame(IRtkkcat_Symp)

Proportion_IRTKKT1 <- as.data.frame(IRtkkcat_Symp/tKKDistT1)
Proportion_IRTKKT1_MUS2 <- as.matrix(Proportion_IRTKKT1)

# remove the NaNs
Proportion_IRTKKT1_MUS2[is.nan(Proportion_IRTKKT1_MUS2)] <- 0
Proportion_IRTKKT1_MUS2 <- as.data.frame(Proportion_IRTKKT1_MUS2)

# check no remaining NaNs 
str(Proportion_IRTKKT1_MUS2)

quantProportions_IR_sym_MUS <- sapply(Proportion_IRTKKT1_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_IR_sym_MUS <- as.data.frame(t(quantProportions_IR_sym_MUS))
quantProportions_IR_sym_MUS$categories <- rownames(quantProportions_IR_sym_MUS)
colnames(quantProportions_IR_sym_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_IR_sym_MUS$dummyNum <- 1:nrow(quantProportions_IR_sym_MUS)

quantProportions_IR_sym_MUS$categories <- factor(quantProportions_IR_sym_MUS$categories,levels = quantProportions_IR_sym_MUS$categories)

ggplot(quantProportions_IR_sym_MUS, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_IR_sym_MUS <- drm(min ~ dummyNum, data = quantProportions_IR_sym_MUS, fct = LL.3())
model_mid_IR_sym_MUS <- drm(mid ~ dummyNum, data = quantProportions_IR_sym_MUS, fct = LL.3())
model_upper_IR_sym_MUS <- drm(upper ~ dummyNum, data = quantProportions_IR_sym_MUS, fct = LL.3())

pred_min_IR_sym_MUS <- predict(model_min_IR_sym_MUS, newdata = data.frame(dummyNum = quantProportions_IR_sym_MUS$dummyNum))
pred_mid_IR_sym_MUS <- predict(model_mid_IR_sym_MUS, newdata = data.frame(dummyNum = quantProportions_IR_sym_MUS$dummyNum))
pred_upper_IR_sym_MUS <- predict(model_upper_IR_sym_MUS, newdata = data.frame(dummyNum = quantProportions_IR_sym_MUS$dummyNum))

pred_df_IR_sym_MUS <- data.frame(
  categories = rep(quantProportions_IR_sym_MUS$categories, times = 3),
  proportion = c(pred_min_IR_sym_MUS, pred_mid_IR_sym_MUS, pred_upper_IR_sym_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_IR_sym_MUS)))

pred_df_IR_sym_MUS$group <- factor(pred_df_IR_sym_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_IR_sym_MUS <- pred_df_IR_sym_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_IR_sym_MUS$categories <- factor(ribbon_df_IR_sym_MUS$categories, levels = quantProportions_IR_sym_MUS$categories)

pred_df_IR_sym_MUS$legend <- ifelse(pred_df_IR_sym_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_IR_sym_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_IR_sym_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_IR_sym_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("ItchingproportionT1_fitted.pdf", height = 15, width = 15)
ggsave("ItchingproportionT1_fitted.jpg", height = 15, width = 15)

## diarrhoea ----
EggT1dataDH <- as.data.frame(t(EstEggT1)) %>% 
  mutate(DiarrhoeaSymp = MusubiSymp2004$diarrhoea)
EggT1dataDH$DiarrhoeaSymp[which(EggT1dataDH$DiarrhoeaSymp=="2")] <- 0

EggT1dataDH <- EggT1dataDH %>% 
  filter(EggT1dataDH$DiarrhoeaSymp=="1")
EggT1dataDH <- EggT1dataDH[,-40001]
EggT1dataDH <- round(as.matrix(t(EggT1dataDH)))

DHtkkcat_Symp <- data.frame(matrix(nrow=40000, ncol=26))
colnames(DHtkkcat_Symp) <- cats
DHtkkcat_Symp$`0`<-rowSums(EggT1dataDH==0)

DHTKK_Symp <- list()

for(i in 1:nrow(EggT1dataDH)){
  DHTKK_Symp[[i]] <- list()
  for(j in 2:ncol(DHtkkcat_Symp)){
    DHtkkcat_Symp[i,j] <- length(which(EggT1dataDH[i,]>=Index1[[j-1]] & EggT1dataDH[i,]<=Index[[j-1]]))
    DHTKK_Symp[[i]][[1]] <- which(EggT1dataDH[i,]==0)
    DHTKK_Symp[[i]][[j]] <-  which(EggT1dataDH[i,]>=Index1[[j-1]] & EggT1dataDH[i,]<=Index[[j-1]])
  }
}

DHtkkcat_Symp <- as.data.frame(DHtkkcat_Symp)

Proportion_DHTKKT1 <- as.data.frame(DHtkkcat_Symp/tKKDistT1)

Proportion_DHTKKT1_MUS2 <- as.matrix(Proportion_DHTKKT1)

# remove the NaNs
Proportion_DHTKKT1_MUS2[is.nan(Proportion_DHTKKT1_MUS2)] <- 0
Proportion_DHTKKT1_MUS2 <- as.data.frame(Proportion_DHTKKT1_MUS2)

# check no remaining NaNs 
str(Proportion_DHTKKT1_MUS2)

quantProportions_DH_sym_MUS <- sapply(Proportion_DHTKKT1_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_DH_sym_MUS <- as.data.frame(t(quantProportions_DH_sym_MUS))
quantProportions_DH_sym_MUS$categories <- rownames(quantProportions_DH_sym_MUS)
colnames(quantProportions_DH_sym_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_DH_sym_MUS$dummyNum <- 1:nrow(quantProportions_DH_sym_MUS)

quantProportions_DH_sym_MUS$categories <- factor(quantProportions_DH_sym_MUS$categories,levels = quantProportions_DH_sym_MUS$categories)

ggplot(quantProportions_DH_sym_MUS, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_DH_sym_MUS <- drm(min ~ dummyNum, data = quantProportions_DH_sym_MUS, fct = LL.3())
model_mid_DH_sym_MUS <- drm(mid ~ dummyNum, data = quantProportions_DH_sym_MUS, fct = LL.3())
model_upper_DH_sym_MUS <- drm(upper ~ dummyNum, data = quantProportions_DH_sym_MUS, fct = LL.3())

pred_min_DH_sym_MUS <- predict(model_min_DH_sym_MUS, newdata = data.frame(dummyNum = quantProportions_DH_sym_MUS$dummyNum))
pred_mid_DH_sym_MUS <- predict(model_mid_DH_sym_MUS, newdata = data.frame(dummyNum = quantProportions_DH_sym_MUS$dummyNum))
pred_upper_DH_sym_MUS <- predict(model_upper_DH_sym_MUS, newdata = data.frame(dummyNum = quantProportions_DH_sym_MUS$dummyNum))

pred_df_DH_sym_MUS <- data.frame(
  categories = rep(quantProportions_DH_sym_MUS$categories, times = 3),
  proportion = c(pred_min_DH_sym_MUS, pred_mid_DH_sym_MUS, pred_upper_DH_sym_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_DH_sym_MUS)))

pred_df_DH_sym_MUS$group <- factor(pred_df_DH_sym_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_DH_sym_MUS <- pred_df_DH_sym_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_DH_sym_MUS$categories <- factor(ribbon_df_DH_sym_MUS$categories, levels = quantProportions_DH_sym_MUS$categories)

pred_df_DH_sym_MUS$legend <- ifelse(pred_df_DH_sym_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_DH_sym_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_DH_sym_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_DH_sym_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("DiarrhoeaproportionT1_fitted.pdf", height = 15, width = 15)
ggsave("DiarrhoeaproportionT1_fitted.jpg", height = 15, width = 15)

## nausea_sym ----
EggT1dataNS <- as.data.frame(t(EstEggT1)) %>% 
  mutate(NauseaSymp = MusubiSymp2004$nausea)
EggT1dataNS$NauseaSymp[which(EggT1dataNS$NauseaSymp=="2")] <- 0

EggT1dataNS <- EggT1dataNS %>% 
  filter(EggT1dataNS$NauseaSymp=="1")
EggT1dataNS <- EggT1dataNS[,-40001]
EggT1dataNS <- round(as.matrix(t(EggT1dataNS)))

NStkkcat_Symp <- data.frame(matrix(nrow=40000, ncol=26))
colnames(NStkkcat_Symp) <- cats
NStkkcat_Symp$`0`<-rowSums(EggT1dataNS==0)

NSTKK_Symp <- list()

for(i in 1:nrow(EggT1dataNS)){
  NSTKK_Symp[[i]] <- list()
  for(j in 2:ncol(NStkkcat_Symp)){
    NStkkcat_Symp[i,j] <- length(which(EggT1dataNS[i,]>=Index1[[j-1]] & EggT1dataNS[i,]<=Index[[j-1]]))
    NSTKK_Symp[[i]][[1]] <- which(EggT1dataNS[i,]==0)
    NSTKK_Symp[[i]][[j]] <-  which(EggT1dataNS[i,]>=Index1[[j-1]] & EggT1dataNS[i,]<=Index[[j-1]])
  }
}

NStkkcat_Symp <- as.data.frame(NStkkcat_Symp)

Proportion_NSTKKT1 <- as.data.frame(NStkkcat_Symp/tKKDistT1)
Proportion_NSTKKT1_MUS2 <- as.matrix(Proportion_NSTKKT1)

# remove the NaNs
Proportion_NSTKKT1_MUS2[is.nan(Proportion_NSTKKT1_MUS2)] <- 0
Proportion_NSTKKT1_MUS2 <- as.data.frame(Proportion_NSTKKT1_MUS2)

# check no remaining NaNs 
str(Proportion_NSTKKT1_MUS2)

quantProportions_NS_sym_MUS <- sapply(Proportion_NSTKKT1_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_NS_sym_MUS <- as.data.frame(t(quantProportions_NS_sym_MUS))
quantProportions_NS_sym_MUS$categories <- rownames(quantProportions_NS_sym_MUS)
colnames(quantProportions_NS_sym_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_NS_sym_MUS$dummyNum <- 1:nrow(quantProportions_NS_sym_MUS)

quantProportions_NS_sym_MUS$categories <- factor(quantProportions_NS_sym_MUS$categories,levels = quantProportions_NS_sym_MUS$categories)

ggplot(quantProportions_NS_sym_MUS, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_NS_sym_MUS <- drm(min ~ dummyNum, data = quantProportions_NS_sym_MUS, fct = W2.3())
model_mid_NS_sym_MUS <- drm(mid ~ dummyNum, data = quantProportions_NS_sym_MUS, fct = BC.5())
model_upper_NS_sym_MUS <- drm(upper ~ dummyNum, data = quantProportions_NS_sym_MUS, fct = LL.3())

pred_min_NS_sym_MUS <- predict(model_min_NS_sym_MUS, newdata = data.frame(dummyNum = quantProportions_NS_sym_MUS$dummyNum))
pred_mid_NS_sym_MUS <- predict(model_mid_NS_sym_MUS, newdata = data.frame(dummyNum = quantProportions_NS_sym_MUS$dummyNum))
pred_upper_NS_sym_MUS <- predict(model_upper_NS_sym_MUS, newdata = data.frame(dummyNum = quantProportions_NS_sym_MUS$dummyNum))

pred_df_NS_sym_MUS <- data.frame(
  categories = rep(quantProportions_NS_sym_MUS$categories, times = 3),
  proportion = c(pred_min_NS_sym_MUS, pred_mid_NS_sym_MUS, pred_upper_NS_sym_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_NS_sym_MUS)))

pred_df_NS_sym_MUS$group <- factor(pred_df_NS_sym_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_NS_sym_MUS <- pred_df_NS_sym_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_NS_sym_MUS$categories <- factor(ribbon_df_NS_sym_MUS$categories, levels = quantProportions_NS_sym_MUS$categories)

pred_df_NS_sym_MUS$legend <- ifelse(pred_df_NS_sym_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_NS_sym_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_NS_sym_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_NS_sym_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("NauseaproportionT1_fitted.pdf", height = 15, width = 15)
ggsave("NauseaproportionT1_fitted.jpg", height = 15, width = 15)

## blood_stool_sym ----
EggT1dataBS <- as.data.frame(t(EstEggT1)) %>% 
  mutate(Blood_stoolSymp = MusubiSymp2004$blood_stool)
EggT1dataBS$Blood_stoolSymp[which(EggT1dataBS$Blood_stoolSymp=="2")] <- 0

EggT1dataBS <- EggT1dataBS %>% 
  filter(EggT1dataBS$Blood_stoolSymp=="1")
EggT1dataBS <- EggT1dataBS[,-40001]
EggT1dataBS <- round(as.matrix(t(EggT1dataBS)))

BStkkcat_Symp <- data.frame(matrix(nrow=40000, ncol=26))
colnames(BStkkcat_Symp) <- cats
BStkkcat_Symp$`0`<-rowSums(EggT1dataBS==0)
BSTKK_Symp <- list()

for(i in 1:nrow(EggT1dataBS)){
  BSTKK_Symp[[i]] <- list()
  for(j in 2:ncol(BStkkcat_Symp)){
    BStkkcat_Symp[i,j] <- length(which(EggT1dataBS[i,]>=Index1[[j-1]] & EggT1dataBS[i,]<=Index[[j-1]]))
    BSTKK_Symp[[i]][[1]] <- which(EggT1dataBS[i,]==0)
    BSTKK_Symp[[i]][[j]] <-  which(EggT1dataBS[i,]>=Index1[[j-1]] & EggT1dataBS[i,]<=Index[[j-1]])
  }
}

BStkkcat_Symp <- as.data.frame(BStkkcat_Symp)

Proportion_BSTKKT1 <- as.data.frame(BStkkcat_Symp/tKKDistT1)

Proportion_BSTKKT1_MUS2 <- as.matrix(Proportion_BSTKKT1)

# remove the NaNs
Proportion_BSTKKT1_MUS2[is.nan(Proportion_BSTKKT1_MUS2)] <- 0
Proportion_BSTKKT1_MUS2 <- as.data.frame(Proportion_BSTKKT1_MUS2)

# check no remaining NaNs 
str(Proportion_BSTKKT1_MUS2)

quantProportions_BS_sym_MUS <- sapply(Proportion_BSTKKT1_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_BS_sym_MUS <- as.data.frame(t(quantProportions_BS_sym_MUS))
quantProportions_BS_sym_MUS$categories <- rownames(quantProportions_BS_sym_MUS)
colnames(quantProportions_BS_sym_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_BS_sym_MUS$dummyNum <- 1:nrow(quantProportions_BS_sym_MUS)

quantProportions_BS_sym_MUS$categories <- factor(quantProportions_BS_sym_MUS$categories,levels = quantProportions_BS_sym_MUS$categories)

ggplot(quantProportions_BS_sym_MUS, aes(x = categories)) +
  geom_point(aes(y = min, color = "min"), size = 3, alpha = 0.4) +
  geom_point(aes(y = mid, color = "mid"), size = 3, alpha = 0.4) +
  geom_point(aes(y = upper, color = "upper"), size = 3, alpha = 0.4) +
  scale_color_manual(values = c("min" = "red", "mid" = "blue", "upper" = "green")) +
  labs(title = "Min, Mid, and Upper Quantile Proportions by Category",
       x = "Categories",
       y = "Proportion",
       color = "Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_min_BS_sym_MUS <- drm(min ~ dummyNum, data = quantProportions_BS_sym_MUS, fct = LL.3())
model_mid_BS_sym_MUS <- drm(mid ~ dummyNum, data = quantProportions_BS_sym_MUS, fct = BC.5())
model_upper_BS_sym_MUS <- drm(upper ~ dummyNum, data = quantProportions_BS_sym_MUS, fct = BC.5())

pred_min_BS_sym_MUS <- predict(model_min_BS_sym_MUS, newdata = data.frame(dummyNum = quantProportions_BS_sym_MUS$dummyNum))
pred_mid_BS_sym_MUS <- predict(model_mid_BS_sym_MUS, newdata = data.frame(dummyNum = quantProportions_BS_sym_MUS$dummyNum))
pred_upper_BS_sym_MUS <- predict(model_upper_BS_sym_MUS, newdata = data.frame(dummyNum = quantProportions_BS_sym_MUS$dummyNum))

pred_df_BS_sym_MUS <- data.frame(
  categories = rep(quantProportions_BS_sym_MUS$categories, times = 3),
  proportion = c(pred_min_BS_sym_MUS, pred_mid_BS_sym_MUS, pred_upper_BS_sym_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_BS_sym_MUS)))

pred_df_BS_sym_MUS$group <- factor(pred_df_BS_sym_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_BS_sym_MUS <- pred_df_BS_sym_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_BS_sym_MUS$categories <- factor(ribbon_df_BS_sym_MUS$categories, levels = quantProportions_BS_sym_MUS$categories)

pred_df_BS_sym_MUS$legend <- ifelse(pred_df_BS_sym_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_BS_sym_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_BS_sym_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_BS_sym_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#FF4C7F")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#FF4C7F")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Blood_stoolproportionT1_fitted.pdf", height = 15, width = 15)
ggsave("Blood_stoolproportionT1_fitted.jpg", height = 15, width = 15)

