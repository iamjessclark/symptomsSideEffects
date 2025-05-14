# Hypothesis 4: Effect of S. masoni infection intnesity on the reporting of post-treatment side effects

eggcountscats <- as.data.frame(cats)
eggcountscats$cats <- as.factor(eggcountscats$cats)

# Bugoto LV ----

## tKKDist T2 ----
EstEggT2BG[EstEggT2BG<0] <- 0
tKKDistT2_BUG <- data.frame(matrix(nrow=40000, ncol=26))

Index <- seq(120, 3000, 120) # from 0 to the highest estimated egg count in blocks of 24 because one slide is multiplied by 24 to give eggs per gram
Index1 <- seq(1, 2977, 120)
cats <-vector()

for(i in 1:25){
  cats[i]<- paste(Index1[i], Index[i],sep="-")
}

cats <- c(0,cats)

colnames(tKKDistT2_BUG) <- cats

tKKDistT2_BUG$`0`<-rowSums(EstEggT2BG==0)

#this tells me who has 0 egg counts
whoTKKT2_BUG <- list()

for(i in 1:nrow(EstEggT2BG)){
  whoTKKT2_BUG[[i]] <- list() 
  for(j in 2:ncol(tKKDistT2_BUG)){
    tKKDistT2_BUG[i,j] <- length(which(EstEggT2BG[i,]>=Index1[[j-1]] & EstEggT2BG[i,]<=Index[[j-1]]))
    whoTKKT2_BUG[[i]][[1]] <- which(EstEggT2BG[i,]==0)
    whoTKKT2_BUG[[i]][[j]] <-  which(EstEggT2BG[i,]>=Index1[[j-1]] & EstEggT2BG[i,]<=Index[[j-1]])
  }
}
tKKDistT2_BUG <- as.data.frame(tKKDistT2_BUG)

## Headache_se ----
EggT2dataHA_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(HeadacheSE = BugotoSE2004$headache)
EggT2dataHA_BUG[EggT2dataHA_BUG<0] <- 0

EggT2dataHA_BUG <- EggT2dataHA_BUG %>% 
  filter(EggT2dataHA_BUG$HeadacheSE=="1")
EggT2dataHA_BUG <- EggT2dataHA_BUG[,-40001]
EggT2dataHA_BUG <- round(as.data.frame(t(EggT2dataHA_BUG)))

EggT2dataHAdf_BUG <- as.matrix(EggT2dataHA_BUG)
HAtkkcat_SE_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(HAtkkcat_SE_BUG) <- cats
HAtkkcat_SE_BUG$`0`<-rowSums(EggT2dataHAdf_BUG==0)

HATKK_SE_BUG <- list()

for(i in 1:nrow(EggT2dataHAdf_BUG)){
  HATKK_SE_BUG[[i]] <- list()
  for(j in 2:ncol(HAtkkcat_SE_BUG)){
    HAtkkcat_SE_BUG[i,j] <- length(which(EggT2dataHAdf_BUG[i,]>=Index1[[j-1]] & EggT2dataHAdf_BUG[i,]<=Index[[j-1]]))
    HATKK_SE_BUG[[i]][[1]] <- which(EggT2dataHAdf_BUG[i,]==0)
    HATKK_SE_BUG[[i]][[j]] <-  which(EggT2dataHAdf_BUG[i,]>=Index1[[j-1]] & EggT2dataHAdf_BUG[i,]<=Index[[j-1]])
  }
}

HAtkkcat_SE_BUG <- as.data.frame(HAtkkcat_SE_BUG)

Proportion_HATKKT2_BUG <- as.data.frame(HAtkkcat_SE_BUG/tKKDistT1_BUG)

Proportion_HATKKT2_BUG2 <- as.matrix(Proportion_HATKKT2_BUG)

# remove the NaNs
Proportion_HATKKT2_BUG2[is.nan(Proportion_HATKKT2_BUG2)] <- 0
Proportion_HATKKT2_BUG2 <- as.data.frame(Proportion_HATKKT2_BUG2)

# check no remaining NaNs 
str(Proportion_HATKKT2_BUG2)

quantProportions_HA_se_BUG <- sapply(Proportion_HATKKT2_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_HA_se_BUG <- as.data.frame(t(quantProportions_HA_se_BUG))
quantProportions_HA_se_BUG$categories <- rownames(quantProportions_HA_se_BUG)
colnames(quantProportions_HA_se_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_HA_se_BUG$dummyNum <- 1:nrow(quantProportions_HA_se_BUG)

quantProportions_HA_se_BUG$categories <- factor(quantProportions_HA_se_BUG$categories,levels = quantProportions_HA_se_BUG$categories)

ggplot(quantProportions_HA_se_BUG, aes(x = categories)) +
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

model_min_HA_se_BUG <- drm(min ~ dummyNum, data = quantProportions_HA_se_BUG, fct = BC.5())
model_mid_HA_se_BUG <- drm(mid ~ dummyNum, data = quantProportions_HA_se_BUG, fct = BC.5())
model_upper_HA_se_BUG <- drm(upper ~ dummyNum, data = quantProportions_HA_se_BUG, fct = BC.5())

pred_min_HA_se_BUG <- predict(model_min_HA_se_BUG, newdata = data.frame(dummyNum = quantProportions_HA_se_BUG$dummyNum))
pred_mid_HA_se_BUG <- predict(model_mid_HA_se_BUG, newdata = data.frame(dummyNum = quantProportions_HA_se_BUG$dummyNum))
pred_upper_HA_se_BUG <- predict(model_upper_HA_se_BUG, newdata = data.frame(dummyNum = quantProportions_HA_se_BUG$dummyNum))

quantProportions_HA_se_BUG <- quantProportions_HA_se_BUG[-(16:26),]

pred_min_HA_se_BUG <- pred_min_HA_se_BUG[-(16:26)]
pred_mid_HA_se_BUG <- pred_mid_HA_se_BUG[-(16:26)]
pred_upper_HA_se_BUG <- pred_upper_HA_se_BUG[-(16:26)]

pred_df_HA_se_BUG <- data.frame(
  categories = rep(quantProportions_HA_se_BUG$categories, times = 3),
  proportion = c(pred_min_HA_se_BUG, pred_mid_HA_se_BUG, pred_upper_HA_se_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_HA_se_BUG)))

pred_df_HA_se_BUG$group <- factor(pred_df_HA_se_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_HA_se_BUG <- pred_df_HA_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_HA_se_BUG$categories <- factor(ribbon_df_HA_se_BUG$categories, levels = quantProportions_HA_se_BUG$categories)

pred_df_HA_se_BUG$legend <- ifelse(pred_df_HA_se_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_HA_se_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_HA_se_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_HA_se_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("HeadacheproportionT2_fitted_BUG.pdf", height = 15, width = 15)
ggsave("HeadacheproportionT2_fitted.jpg", height = 15, width = 15)

## abdom_pain_se ----

EggT2dataAP_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(Abdom_painSE = BugotoSE2004$abdom_pain)
EggT2dataAP_BUG[EggT2dataAP_BUG<0] <- 0

EggT2dataAP_BUG <- EggT2dataAP_BUG %>% 
  filter(EggT2dataAP_BUG$Abdom_painSE=="1")
EggT2dataAP_BUG <- EggT2dataAP_BUG[,-40001]
EggT2dataAP_BUG <- round(as.data.frame(t(EggT2dataAP_BUG)))
EggT2dataAP_BUG <- as.matrix(EggT2dataAP_BUG)
APtkkcat_SE_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(APtkkcat_SE_BUG) <- cats
APtkkcat_SE_BUG$`0`<-rowSums(EggT2dataAP_BUG==0)

APTKK_SE_BUG <- list()

for(i in 1:nrow(EggT2dataAP_BUG)){
  APTKK_SE_BUG[[i]] <- list()
  for(j in 2:ncol(APtkkcat_SE_BUG)){
    APtkkcat_SE_BUG[i,j] <- length(which(EggT2dataAP_BUG[i,]>=Index1[[j-1]] & EggT2dataAP_BUG[i,]<=Index[[j-1]]))
    APTKK_SE_BUG[[i]][[1]] <- which(EggT2dataAP_BUG[i,]==0)
    APTKK_SE_BUG[[i]][[j]] <-  which(EggT2dataAP_BUG[i,]>=Index1[[j-1]] & EggT2dataAP_BUG[i,]<=Index[[j-1]])
  }
}

APtkkcat_SE_BUG <- as.data.frame(APtkkcat_SE_BUG)

eggcountscats <- as.data.frame(cats)
eggcountscats$cats <- as.factor(eggcountscats$cats)

Proportion_APTKKT2_BUG <- as.data.frame(APtkkcat_SE_BUG/tKKDistT1_BUG)

Proportion_APTKKT2_BUG2 <- as.matrix(Proportion_APTKKT2_BUG)

# remove the NaNs
Proportion_APTKKT2_BUG2[is.nan(Proportion_APTKKT2_BUG2)] <- 0
Proportion_APTKKT2_BUG2 <- as.data.frame(Proportion_APTKKT2_BUG2)

# check no remaining NaNs 
str(Proportion_APTKKT2_BUG2)

quantProportions_AP_se_BUG <- sapply(Proportion_APTKKT2_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_AP_se_BUG <- as.data.frame(t(quantProportions_AP_se_BUG))
quantProportions_AP_se_BUG$categories <- rownames(quantProportions_AP_se_BUG)
colnames(quantProportions_AP_se_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_AP_se_BUG$dummyNum <- 1:nrow(quantProportions_AP_se_BUG)

quantProportions_AP_se_BUG$categories <- factor(quantProportions_AP_se_BUG$categories,levels = quantProportions_AP_se_BUG$categories)

ggplot(quantProportions_AP_se_BUG, aes(x = categories)) +
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

model_min_AP_se_BUG <- drm(min ~ dummyNum, data = quantProportions_AP_se_BUG, fct = BC.5())
model_mid_AP_se_BUG <- drm(mid ~ dummyNum, data = quantProportions_AP_se_BUG, fct = LN.3())
model_upper_AP_se_BUG <- drm(upper ~ dummyNum, data = quantProportions_AP_se_BUG, fct = BC.5())

pred_min_AP_se_BUG <- predict(model_min_AP_se_BUG, newdata = data.frame(dummyNum = quantProportions_AP_se_BUG$dummyNum))
pred_mid_AP_se_BUG <- predict(model_mid_AP_se_BUG, newdata = data.frame(dummyNum = quantProportions_AP_se_BUG$dummyNum))
pred_upper_AP_se_BUG <- predict(model_upper_AP_se_BUG, newdata = data.frame(dummyNum = quantProportions_AP_se_BUG$dummyNum))

quantProportions_AP_se_BUG <- quantProportions_AP_se_BUG[-(16:26),]

pred_min_AP_se_BUG <- pred_min_AP_se_BUG[-(16:26)]
pred_mid_AP_se_BUG <- pred_mid_AP_se_BUG[-(16:26)]
pred_upper_AP_se_BUG <- pred_upper_AP_se_BUG[-(16:26)]

pred_df_AP_se_BUG <- data.frame(
  categories = rep(quantProportions_AP_se_BUG$categories, times = 3),
  proportion = c(pred_min_AP_se_BUG, pred_mid_AP_se_BUG, pred_upper_AP_se_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_AP_se_BUG)))

pred_df_AP_se_BUG$group <- factor(pred_df_AP_se_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_AP_se_BUG <- pred_df_AP_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_AP_se_BUG$categories <- factor(ribbon_df_AP_se_BUG$categories, levels = quantProportions_AP_se_BUG$categories)

pred_df_AP_se_BUG$legend <- ifelse(pred_df_AP_se_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_AP_se_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_AP_se_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_AP_se_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Abdom_painproportionT2_fitted_BUG.pdf", height = 15, width = 15)
ggsave("Abdom_painproportionT2_fitted_BUG.jpg", height = 15, width = 15)

## urine_pain_se ----
EggT2dataUP_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(Urine_painSE = BugotoSE2004$urine_pain)
EggT2dataUP_BUG[EggT2dataUP_BUG<0] <- 0

EggT2dataUP_BUG <- EggT2dataUP_BUG %>% 
  filter(EggT2dataUP_BUG$Urine_painSE=="1")
EggT2dataUP_BUG <- EggT2dataUP_BUG[,-40001]
EggT2dataUP_BUG <- round(as.data.frame(t(EggT2dataUP_BUG)))
EggT2dataUP_BUG <- as.matrix(EggT2dataUP_BUG)
UPtkkcat_SE_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(UPtkkcat_SE_BUG) <- cats
UPtkkcat_SE_BUG$`0`<-rowSums(EggT2dataUP_BUG==0)

UPTKK_SE_BUG <- list()

for(i in 1:nrow(EggT2dataUP_BUG)){
  UPTKK_SE_BUG[[i]] <- list()
  for(j in 2:ncol(UPtkkcat_SE_BUG)){
    UPtkkcat_SE_BUG[i,j] <- length(which(EggT2dataUP_BUG[i,]>=Index1[[j-1]] & EggT2dataUP_BUG[i,]<=Index[[j-1]]))
    UPTKK_SE_BUG[[i]][[1]] <- which(EggT2dataUP_BUG[i,]==0)
    UPTKK_SE_BUG[[i]][[j]] <-  which(EggT2dataUP_BUG[i,]>=Index1[[j-1]] & EggT2dataUP_BUG[i,]<=Index[[j-1]])
  }
}

UPtkkcat_SE_BUG <- as.data.frame(UPtkkcat_SE_BUG)

Proportion_UPTKKT2_BUG <- as.data.frame(UPtkkcat_SE_BUG/tKKDistT1_BUG)
Proportion_UPTKKT2_BUG2 <- as.matrix(Proportion_UPTKKT2_BUG)

# remove the NaNs
Proportion_UPTKKT2_BUG2[is.nan(Proportion_UPTKKT2_BUG2)] <- 0
Proportion_UPTKKT2_BUG2 <- as.data.frame(Proportion_UPTKKT2_BUG2)

# check no remaining NaNs 
str(Proportion_UPTKKT2_BUG2)

quantProportions_UP_se_BUG <- sapply(Proportion_UPTKKT2_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_UP_se_BUG <- as.data.frame(t(quantProportions_UP_se_BUG))
quantProportions_UP_se_BUG$categories <- rownames(quantProportions_UP_se_BUG)
colnames(quantProportions_UP_se_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_UP_se_BUG$dummyNum <- 1:nrow(quantProportions_UP_se_BUG)

quantProportions_UP_se_BUG$categories <- factor(quantProportions_UP_se_BUG$categories,levels = quantProportions_UP_se_BUG$categories)

ggplot(quantProportions_UP_se_BUG, aes(x = categories)) +
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

quantProportions_UP_se_BUG <- quantProportions_UP_se_BUG[-(16:26),]

pred_df_UP_se_BUG <- data.frame(
  categories = rep(quantProportions_UP_se_BUG$categories, times = 3),
  proportion = rep(0, length.out = 3 * nrow(quantProportions_UP_se_BUG)),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_UP_se_BUG))
)

pred_df_UP_se_BUG$group <- factor(pred_df_UP_se_BUG$group, levels = c("min", "mid", "upper"))

ribbon_df_UP_se_BUG <- pred_df_UP_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_UP_se_BUG$categories <- factor(ribbon_df_UP_se_BUG$categories, levels = quantProportions_UP_se_BUG$categories)

pred_df_UP_se_BUG$legend <- ifelse(pred_df_UP_se_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_UP_se_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_UP_se_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_UP_se_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Urine_painproportionT2_fitted_BUG.pdf", height = 15, width = 15)
ggsave("Urine_painproportionT2_fitted_BUG.jpg", height = 15, width = 15)

## itching_se ----
EggT2dataIR_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(ItchingSE = BugotoSE2004$rash)
EggT2dataIR_BUG[EggT2dataIR_BUG<0] <- 0

EggT2dataIR_BUG <- EggT2dataIR_BUG %>% 
  filter(EggT2dataIR_BUG$ItchingSE=="1")
EggT2dataIR_BUG <- EggT2dataIR_BUG[,-40001]
EggT2dataIR_BUG <- round(as.data.frame(t(EggT2dataIR_BUG)))
EggT2dataIR_BUG <- as.matrix(EggT2dataIR_BUG)
IRtkkcat_SE_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(IRtkkcat_SE_BUG) <- cats
IRtkkcat_SE_BUG$`0`<-rowSums(EggT2dataIR_BUG==0)

IRTKK_SE_BUG <- list()

for(i in 1:nrow(EggT2dataIR_BUG)){
  IRTKK_SE_BUG[[i]] <- list()
  for(j in 2:ncol(IRtkkcat_SE_BUG)){
    IRtkkcat_SE_BUG[i,j] <- length(which(EggT2dataIR_BUG[i,]>=Index1[[j-1]] & EggT2dataIR_BUG[i,]<=Index[[j-1]]))
    IRTKK_SE_BUG[[i]][[1]] <- which(EggT2dataIR_BUG[i,]==0)
    IRTKK_SE_BUG[[i]][[j]] <-  which(EggT2dataIR_BUG[i,]>=Index1[[j-1]] & EggT2dataIR_BUG[i,]<=Index[[j-1]])
  }
}

IRtkkcat_SE_BUG <- as.data.frame(IRtkkcat_SE_BUG)

Proportion_IRTKKT2_BUG <- as.data.frame(IRtkkcat_SE_BUG/tKKDistT1_BUG)

Proportion_IRTKKT2_BUG2 <- as.matrix(Proportion_IRTKKT2_BUG)

# remove the NaNs
Proportion_IRTKKT2_BUG2[is.nan(Proportion_IRTKKT2_BUG2)] <- 0
Proportion_IRTKKT2_BUG2 <- as.data.frame(Proportion_IRTKKT2_BUG2)

# check no remaining NaNs 
str(Proportion_IRTKKT2_BUG2)

quantProportions_IR_se_BUG <- sapply(Proportion_IRTKKT2_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_IR_se_BUG <- as.data.frame(t(quantProportions_IR_se_BUG))
quantProportions_IR_se_BUG$categories <- rownames(quantProportions_IR_se_BUG)
colnames(quantProportions_IR_se_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_IR_se_BUG$dummyNum <- 1:nrow(quantProportions_IR_se_BUG)

quantProportions_IR_se_BUG$categories <- factor(quantProportions_IR_se_BUG$categories,levels = quantProportions_IR_se_BUG$categories)

ggplot(quantProportions_IR_se_BUG, aes(x = categories)) +
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

quantProportions_IR_se_BUG <- quantProportions_IR_se_BUG[-(16:26),]

pred_df_IR_se_BUG <- data.frame(
  categories = rep(quantProportions_IR_se_BUG$categories, times = 3),
  proportion = rep(0, length.out = 3 * nrow(quantProportions_IR_se_BUG)),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_IR_se_BUG))
)

pred_df_IR_se_BUG$group <- factor(pred_df_IR_se_BUG$group, levels = c("min", "mid", "upper"))

ribbon_df_IR_se_BUG <- pred_df_IR_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_IR_se_BUG$categories <- factor(ribbon_df_IR_se_BUG$categories, levels = quantProportions_IR_se_BUG$categories)

pred_df_IR_se_BUG$legend <- ifelse(pred_df_IR_se_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_IR_se_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_IR_se_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_IR_se_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("ItchingproportionT2_fitted_BUG.pdf", height = 15, width = 15)
ggsave("ItchingproportionT2_fitted_BUG.jpg", height = 15, width = 15)

## diarrhoea_se ----
EggT2dataDH_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(DiarrhoeaSE = BugotoSE2004$diarrhoea)
EggT2dataDH_BUG[EggT2dataDH_BUG<0] <- 0

EggT2dataDH_BUG <- EggT2dataDH_BUG %>% 
  filter(EggT2dataDH_BUG$DiarrhoeaSE=="1")
EggT2dataDH_BUG <- EggT2dataDH_BUG[,-40001]
EggT2dataDH_BUG <- round(as.data.frame(t(EggT2dataDH_BUG)))
EggT2dataDH_BUG <- as.matrix(EggT2dataDH_BUG)
DHtkkcat_SE_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(DHtkkcat_SE_BUG) <- cats
DHtkkcat_SE_BUG$`0`<-rowSums(EggT2dataDH_BUG==0)

DHTKK_SE_BUG <- list()

for(i in 1:nrow(EggT2dataDH_BUG)){
  DHTKK_SE_BUG[[i]] <- list()
  for(j in 2:ncol(DHtkkcat_SE_BUG)){
    DHtkkcat_SE_BUG[i,j] <- length(which(EggT2dataDH_BUG[i,]>=Index1[[j-1]] & EggT2dataDH_BUG[i,]<=Index[[j-1]]))
    DHTKK_SE_BUG[[i]][[1]] <- which(EggT2dataDH_BUG[i,]==0)
    DHTKK_SE_BUG[[i]][[j]] <-  which(EggT2dataDH_BUG[i,]>=Index1[[j-1]] & EggT2dataDH_BUG[i,]<=Index[[j-1]])
  }
}

DHtkkcat_SE_BUG <- as.data.frame(DHtkkcat_SE_BUG)

Proportion_DHTKKT2_BUG <- as.data.frame(DHtkkcat_SE_BUG/tKKDistT1_BUG)

Proportion_DHTKKT2_BUG2 <- as.matrix(Proportion_DHTKKT2_BUG)

# remove the NaNs
Proportion_DHTKKT2_BUG2[is.nan(Proportion_DHTKKT2_BUG2)] <- 0
Proportion_DHTKKT2_BUG2 <- as.data.frame(Proportion_DHTKKT2_BUG2)

# check no remaining NaNs 
str(Proportion_DHTKKT2_BUG2)

quantProportions_DH_se_BUG <- sapply(Proportion_DHTKKT2_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_DH_se_BUG <- as.data.frame(t(quantProportions_DH_se_BUG))
quantProportions_DH_se_BUG$categories <- rownames(quantProportions_DH_se_BUG)
colnames(quantProportions_DH_se_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_DH_se_BUG$dummyNum <- 1:nrow(quantProportions_DH_se_BUG)

quantProportions_DH_se_BUG$categories <- factor(quantProportions_DH_se_BUG$categories,levels = quantProportions_DH_se_BUG$categories)

ggplot(quantProportions_DH_se_BUG, aes(x = categories)) +
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

model_min_DH_se_BUG <- drm(min ~ dummyNum, data = quantProportions_DH_se_BUG, fct = BC.5())
model_mid_DH_se_BUG <- drm(mid ~ dummyNum, data = quantProportions_DH_se_BUG, fct = CRS.6())
model_upper_DH_se_BUG <- drm(upper ~ dummyNum, data = quantProportions_DH_se_BUG, fct = BC.5())

pred_min_DH_se_BUG <- predict(model_min_DH_se_BUG, newdata = data.frame(dummyNum = quantProportions_DH_se_BUG$dummyNum))
pred_mid_DH_se_BUG <- predict(model_mid_DH_se_BUG, newdata = data.frame(dummyNum = quantProportions_DH_se_BUG$dummyNum))
pred_upper_DH_se_BUG <- predict(model_upper_DH_se_BUG, newdata = data.frame(dummyNum = quantProportions_DH_se_BUG$dummyNum))

quantProportions_DH_se_BUG <- quantProportions_DH_se_BUG[-(16:26),]

pred_min_DH_se_BUG <- pred_min_DH_se_BUG[-(16:26)]
pred_mid_DH_se_BUG <- pred_mid_DH_se_BUG[-(16:26)]
pred_upper_DH_se_BUG <- pred_upper_DH_se_BUG[-(16:26)]

pred_df_DH_se_BUG <- data.frame(
  categories = rep(quantProportions_DH_se_BUG$categories, times = 3),
  proportion = c(pred_min_DH_se_BUG, pred_mid_DH_se_BUG, pred_upper_DH_se_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_DH_se_BUG)))

pred_df_DH_se_BUG$group <- factor(pred_df_DH_se_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_DH_se_BUG <- pred_df_DH_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_DH_se_BUG$categories <- factor(ribbon_df_DH_se_BUG$categories, levels = quantProportions_DH_se_BUG$categories)

pred_df_DH_se_BUG$legend <- ifelse(pred_df_DH_se_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_DH_se_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_DH_se_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_DH_se_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("DiarrhoeaproportionT2_fitted_BUG.pdf", height = 15, width = 15)
ggsave("DiarrhoeaproportionT2_fitted_BUG.jpg", height = 15, width = 15)

datpostDH_BUG = datpostDH_BUG[,-3]

## nausea_se ----
EggT2dataNS_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(NauseaSE = BugotoSE2004$nausea)
EggT2dataNS_BUG[EggT2dataNS_BUG<0] <- 0

EggT2dataNS_BUG <- EggT2dataNS_BUG %>% 
  filter(EggT2dataNS_BUG$NauseaSE=="1")
EggT2dataNS_BUG <- EggT2dataNS_BUG[,-40001]
EggT2dataNS_BUG <- round(as.data.frame(t(EggT2dataNS_BUG)))
EggT2dataNS_BUG <- as.matrix(EggT2dataNS_BUG)
NStkkcat_SE_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(NStkkcat_SE_BUG) <- cats
NStkkcat_SE_BUG$`0`<-rowSums(EggT2dataNS_BUG==0)

NSTKK_SE_BUG <- list()

for(i in 1:nrow(EggT2dataNS_BUG)){
  NSTKK_SE_BUG[[i]] <- list()
  for(j in 2:ncol(NStkkcat_SE_BUG)){
    NStkkcat_SE_BUG[i,j] <- length(which(EggT2dataNS_BUG[i,]>=Index1[[j-1]] & EggT2dataNS_BUG[i,]<=Index[[j-1]]))
    NSTKK_SE_BUG[[i]][[1]] <- which(EggT2dataNS_BUG[i,]==0)
    NSTKK_SE_BUG[[i]][[j]] <-  which(EggT2dataNS_BUG[i,]>=Index1[[j-1]] & EggT2dataNS_BUG[i,]<=Index[[j-1]])
  }
}

NStkkcat_SE_BUG <- as.data.frame(NStkkcat_SE_BUG)

Proportion_NSTKKT2_BUG <- as.data.frame(NStkkcat_SE_BUG/tKKDistT1_BUG)

Proportion_NSTKKT2_BUG2 <- as.matrix(Proportion_NSTKKT2_BUG)

# remove the NaNs
Proportion_NSTKKT2_BUG2[is.nan(Proportion_NSTKKT2_BUG2)] <- 0
Proportion_NSTKKT2_BUG2 <- as.data.frame(Proportion_NSTKKT2_BUG2)

# check no remaining NaNs 
str(Proportion_NSTKKT2_BUG2)

quantProportions_NS_se_BUG <- sapply(Proportion_NSTKKT2_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_NS_se_BUG <- as.data.frame(t(quantProportions_NS_se_BUG))
quantProportions_NS_se_BUG$categories <- rownames(quantProportions_NS_se_BUG)
colnames(quantProportions_NS_se_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_NS_se_BUG$dummyNum <- 1:nrow(quantProportions_NS_se_BUG)

quantProportions_NS_se_BUG$categories <- factor(quantProportions_NS_se_BUG$categories,levels = quantProportions_NS_se_BUG$categories)

ggplot(quantProportions_NS_se_BUG, aes(x = categories)) +
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

quantProportions_NS_se_BUG <- quantProportions_NS_se_BUG[-(16:26),]

pred_df_NS_se_BUG <- data.frame(
  categories = rep(quantProportions_NS_se_BUG$categories, times = 3),
  proportion = rep(0, length.out = 3 * nrow(quantProportions_NS_se_BUG)),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_NS_se_BUG))
)

pred_df_NS_se_BUG$group <- factor(pred_df_NS_se_BUG$group, levels = c("min", "mid", "upper"))

ribbon_df_NS_se_BUG <- pred_df_NS_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_NS_se_BUG$categories <- factor(ribbon_df_NS_se_BUG$categories, levels = quantProportions_NS_se_BUG$categories)

pred_df_NS_se_BUG$legend <- ifelse(pred_df_NS_se_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_NS_se_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_NS_se_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_NS_se_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("NauseaproportionT2_fitted_BUG.pdf", height = 15, width = 15)
ggsave("NauseaproportionT2_fitted_BUG.jpg", height = 15, width = 15)

## blood_stool ----
EggT2dataBS_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(Blood_stoolSE = BugotoSE2004$blood_stool)
EggT2dataBS_BUG[EggT2dataBS_BUG<0] <- 0

EggT2dataBS_BUG <- EggT2dataBS_BUG %>% 
  filter(EggT2dataBS_BUG$Blood_stoolSE=="1")
EggT2dataBS_BUG <- EggT2dataBS_BUG[,-40001]
EggT2dataBS_BUG <- round(as.data.frame(t(EggT2dataBS_BUG)))
EggT2dataBS_BUG <- as.matrix(EggT2dataBS_BUG)
BStkkcat_SE_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(BStkkcat_SE_BUG) <- cats
BStkkcat_SE_BUG$`0`<-rowSums(EggT2dataBS_BUG==0)

BSTKK_SE_BUG <- list()

for(i in 1:nrow(EggT2dataBS_BUG)){
  BSTKK_SE_BUG[[i]] <- list()
  for(j in 2:ncol(BStkkcat_SE_BUG)){
    BStkkcat_SE_BUG[i,j] <- length(which(EggT2dataBS_BUG[i,]>=Index1[[j-1]] & EggT2dataBS_BUG[i,]<=Index[[j-1]]))
    BSTKK_SE_BUG[[i]][[1]] <- which(EggT2dataBS_BUG[i,]==0)
    BSTKK_SE_BUG[[i]][[j]] <-  which(EggT2dataBS_BUG[i,]>=Index1[[j-1]] & EggT2dataBS_BUG[i,]<=Index[[j-1]])
  }
}

BStkkcat_SE_BUG <- as.data.frame(BStkkcat_SE_BUG)

Proportion_BSTKKT2_BUG <- as.data.frame(BStkkcat_SE_BUG/tKKDistT1_BUG)

Proportion_BSTKKT2_BUG2 <- as.matrix(Proportion_BSTKKT2_BUG)

# remove the NaNs
Proportion_BSTKKT2_BUG2[is.nan(Proportion_BSTKKT2_BUG2)] <- 0
Proportion_BSTKKT2_BUG2 <- as.data.frame(Proportion_BSTKKT2_BUG2)

# check no remaining NaNs 
str(Proportion_BSTKKT2_BUG2)

quantProportions_BS_se_BUG <- sapply(Proportion_BSTKKT2_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_BS_se_BUG <- as.data.frame(t(quantProportions_BS_se_BUG))
quantProportions_BS_se_BUG$categories <- rownames(quantProportions_BS_se_BUG)
colnames(quantProportions_BS_se_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_BS_se_BUG$dummyNum <- 1:nrow(quantProportions_BS_se_BUG)

quantProportions_BS_se_BUG$categories <- factor(quantProportions_BS_se_BUG$categories,levels = quantProportions_BS_se_BUG$categories)

ggplot(quantProportions_BS_se_BUG, aes(x = categories)) +
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

quantProportions_BS_se_BUG <- quantProportions_BS_se_BUG[-(16:26),]

pred_df_BS_se_BUG <- data.frame(
  categories = rep(quantProportions_BS_se_BUG$categories, times = 3),
  proportion = rep(0, length.out = 3 * nrow(quantProportions_BS_se_BUG)),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_BS_se_BUG))
)

pred_df_BS_se_BUG$group <- factor(pred_df_BS_se_BUG$group, levels = c("min", "mid", "upper"))

ribbon_df_BS_se_BUG <- pred_df_BS_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_BS_se_BUG$categories <- factor(ribbon_df_BS_se_BUG$categories, levels = quantProportions_BS_se_BUG$categories)

pred_df_BS_se_BUG$legend <- ifelse(pred_df_BS_se_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_BS_se_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_BS_se_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_BS_se_BUG, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Blood_stoolproportionT2_fitted_BUG.pdf", height = 15, width = 15)
ggsave("Blood_stoolproportionT2_fitted_BUG.jpg", height = 15, width = 15)

## vomit_se ----
EggT2dataVM_BUG <- as.data.frame(t(EstEggT1BG)) %>% 
  mutate(VomitiSE = BugotoSE2004$vomit)
EggT2dataVM_BUG[EggT2dataVM_BUG<0] <- 0

EggT2dataVM_BUG <- EggT2dataVM_BUG %>% 
  filter(EggT2dataVM_BUG$VomitiSE=="1")
EggT2dataVM_BUG <- EggT2dataVM_BUG[,-40001]
EggT2dataVM_BUG <- round(as.data.frame(t(EggT2dataVM_BUG)))
EggT2dataVM_BUG <- as.matrix(EggT2dataVM_BUG)
VMtkkcat_SE_BUG <- data.frame(matrix(nrow=40000, ncol=26))
colnames(VMtkkcat_SE_BUG) <- cats
VMtkkcat_SE_BUG$`0`<-rowSums(EggT2dataVM_BUG==0)

VMTKK_SE_BUG <- list()

for(i in 1:nrow(EggT2dataVM_BUG)){
  VMTKK_SE_BUG[[i]] <- list()
  for(j in 2:ncol(VMtkkcat_SE_BUG)){
    VMtkkcat_SE_BUG[i,j] <- length(which(EggT2dataVM_BUG[i,]>=Index1[[j-1]] & EggT2dataVM_BUG[i,]<=Index[[j-1]]))
    VMTKK_SE_BUG[[i]][[1]] <- which(EggT2dataVM_BUG[i,]==0)
    VMTKK_SE_BUG[[i]][[j]] <-  which(EggT2dataVM_BUG[i,]>=Index1[[j-1]] & EggT2dataVM_BUG[i,]<=Index[[j-1]])
  }
}

VMtkkcat_SE_BUG <- as.data.frame(VMtkkcat_SE_BUG)

Proportion_VMTKKT2_BUG <- as.data.frame(VMtkkcat_SE_BUG/tKKDistT1_BUG)

Proportion_VMTKKT2_BUG2 <- as.matrix(Proportion_VMTKKT2_BUG)

# remove the NaNs
Proportion_VMTKKT2_BUG2[is.nan(Proportion_VMTKKT2_BUG2)] <- 0
Proportion_VMTKKT2_BUG2 <- as.data.frame(Proportion_VMTKKT2_BUG2)

# check no remaining NaNs 
str(Proportion_VMTKKT2_BUG2)

quantProportions_VM_se_BUG <- sapply(Proportion_VMTKKT2_BUG2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_VM_se_BUG <- as.data.frame(t(quantProportions_VM_se_BUG))
quantProportions_VM_se_BUG$categories <- rownames(quantProportions_VM_se_BUG)
colnames(quantProportions_VM_se_BUG) <- c("min", "mid", "upper", "categories")

quantProportions_VM_se_BUG$dummyNum <- 1:nrow(quantProportions_VM_se_BUG)

quantProportions_VM_se_BUG$categories <- factor(quantProportions_VM_se_BUG$categories,levels = quantProportions_VM_se_BUG$categories)

ggplot(quantProportions_VM_se_BUG, aes(x = categories)) +
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

model_min_VM_se_BUG <- drm(min ~ dummyNum, data = quantProportions_VM_se_BUG, fct = BC.5())
model_mid_VM_se_BUG <- drm(mid ~ dummyNum, data = quantProportions_VM_se_BUG, fct = BC.5())
model_upper_VM_se_BUG <- drm(upper ~ dummyNum, data = quantProportions_VM_se_BUG, fct = BC.5())

pred_min_VM_se_BUG <- predict(model_min_VM_se_BUG, newdata = data.frame(dummyNum = quantProportions_VM_se_BUG$dummyNum))
pred_mid_VM_se_BUG <- predict(model_mid_VM_se_BUG, newdata = data.frame(dummyNum = quantProportions_VM_se_BUG$dummyNum))
pred_upper_VM_se_BUG <- predict(model_upper_VM_se_BUG, newdata = data.frame(dummyNum = quantProportions_VM_se_BUG$dummyNum))

quantProportions_VM_se_BUG <- quantProportions_VM_se_BUG[-(16:26),]

pred_min_VM_se_BUG <- pred_min_VM_se_BUG[-(16:26)]
pred_mid_VM_se_BUG <- pred_mid_VM_se_BUG[-(16:26)]
pred_upper_VM_se_BUG <- pred_upper_VM_se_BUG[-(16:26)]

pred_df_VM_se_BUG <- data.frame(
  categories = rep(quantProportions_VM_se_BUG$categories, times = 3),
  proportion = c(pred_min_VM_se_BUG, pred_mid_VM_se_BUG, pred_upper_VM_se_BUG),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_VM_se_BUG)))

pred_df_VM_se_BUG$group <- factor(pred_df_VM_se_BUG$group, levels = c("min", "mid", "upper")) 

ribbon_df_VM_se_BUG <- pred_df_VM_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_VM_se_BUG$categories <- factor(ribbon_df_VM_se_BUG$categories, levels = quantProportions_VM_se_BUG$categories)

pred_df_VM_se_BUG$legend <- ifelse(pred_df_VM_se_BUG$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ribbon_df_VM_se_BUG <- pred_df_VM_se_BUG %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper) %>%
  left_join(pred_df_VM_se_BUG %>% 
              filter(group == "mid") %>% 
              rename(mid = proportion) %>% 
              dplyr::select(categories, mid), by = "categories")

ribbon_df_VM_se_BUG <- ribbon_df_VM_se_BUG %>%
  mutate(ymin = pmin(ymin, mid, ymax),
         ymax = pmax(ymin, mid, ymax))

ggplot() +
  geom_ribbon(data = ribbon_df_VM_se_BUG, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_VM_se_BUG %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_VM_se_BUG, 
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
ggsave("VomitiproportionT2_fitted_BUG.pdf", height = 15, width = 15)
ggsave("VomitiproportionT2_fitted_BUG.jpg", height = 15, width = 15)

ribbon_df_VM_se_BUG <- ribbon_df_VM_se_BUG[, -c(2, 5)]

# Musubi ----

## tKKDist T2 ----
EstEggT2[EstEggT2<0] <- 0
tKKDistT2 <- data.frame(matrix(nrow=40000, ncol=26))

Index <- seq(120, 3000, 120) # from 0 to the highest estimated egg count in blocks of 120 because one slide is multiplied by 120 to give eggs per gram
Index1 <- seq(1, 2977, 120)
cats <-vector()

for(i in 1:25){
  cats[i]<- paste(Index1[i], Index[i],sep="-")
}

cats <- c(0,cats)

colnames(tKKDistT2) <- cats

tKKDistT2$`0`<-rowSums(EstEggT2==0)

#this tells me who has 0 egg counts
whoTKKT2 <- list()

for(i in 1:nrow(EstEggT2)){
  whoTKKT2[[i]] <- list()
  for(j in 2:ncol(tKKDistT2)){
    tKKDistT2[i,j] <- length(which(EstEggT2[i,]>=Index1[[j-1]] & EstEggT2[i,]<=Index[[j-1]]))
    whoTKKT2[[i]][[1]] <- which(EstEggT2[i,]==0)
    whoTKKT2[[i]][[j]] <-  which(EstEggT2[i,]>=Index1[[j-1]] & EstEggT2[i,]<=Index[[j-1]])
  }
}
tKKDistT2 <- as.data.frame(tKKDistT2)

## Headache_se ----
EggT2dataHA <- as.data.frame(t(EstEggT1)) %>% 
  mutate(HeadacheSE = MusubiSE2004$headache)
EggT2dataHA[EggT2dataHA<0] <- 0

EggT2dataHA <- EggT2dataHA %>% 
  filter(EggT2dataHA$HeadacheSE=="1")
EggT2dataHA <- EggT2dataHA[,-40001]
EggT2dataHA <- round(as.data.frame(t(EggT2dataHA)))

EggT2dataHAdf <- as.matrix(EggT2dataHA)
HAtkkcat_SE <- data.frame(matrix(nrow=40000, ncol=26))
colnames(HAtkkcat_SE) <- cats
HAtkkcat_SE$`0`<-rowSums(EggT2dataHAdf==0)

HATKK_SE <- list()

for(i in 1:nrow(EggT2dataHAdf)){
  HATKK_SE[[i]] <- list()
  for(j in 2:ncol(HAtkkcat_SE)){
    HAtkkcat_SE[i,j] <- length(which(EggT2dataHAdf[i,]>=Index1[[j-1]] & EggT2dataHAdf[i,]<=Index[[j-1]]))
    HATKK_SE[[i]][[1]] <- which(EggT2dataHAdf[i,]==0)
    HATKK_SE[[i]][[j]] <-  which(EggT2dataHAdf[i,]>=Index1[[j-1]] & EggT2dataHAdf[i,]<=Index[[j-1]])
  }
}

HAtkkcat_SE <- as.data.frame(HAtkkcat_SE)

Proportion_HATKKT2 <- as.data.frame(HAtkkcat_SE/tKKDistT1)
Proportion_HATKKT2_MUS2 <- as.matrix(Proportion_HATKKT2)

# remove the NaNs
Proportion_HATKKT2_MUS2[is.nan(Proportion_HATKKT2_MUS2)] <- 0
Proportion_HATKKT2_MUS2 <- as.data.frame(Proportion_HATKKT2_MUS2)

# check no remaining NaNs 
str(Proportion_HATKKT2_MUS2)

quantProportions_HA_se_MUS <- sapply(Proportion_HATKKT2_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_HA_se_MUS <- as.data.frame(t(quantProportions_HA_se_MUS))
quantProportions_HA_se_MUS$categories <- rownames(quantProportions_HA_se_MUS)
colnames(quantProportions_HA_se_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_HA_se_MUS$dummyNum <- 1:nrow(quantProportions_HA_se_MUS)

quantProportions_HA_se_MUS$categories <- factor(quantProportions_HA_se_MUS$categories,levels = quantProportions_HA_se_MUS$categories)

ggplot(quantProportions_HA_se_MUS, aes(x = categories)) +
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

model_min_HA_se_MUS <- drm(min ~ dummyNum, data = quantProportions_HA_se_MUS, fct = W1.3())
model_mid_HA_se_MUS <- drm(mid ~ dummyNum, data = quantProportions_HA_se_MUS, fct = LL.3())
model_upper_HA_se_MUS <- drm(upper ~ dummyNum, data = quantProportions_HA_se_MUS, fct = BC.5())

pred_min_HA_se_MUS <- predict(model_min_HA_se_MUS, newdata = data.frame(dummyNum = quantProportions_HA_se_MUS$dummyNum))
pred_mid_HA_se_MUS <- predict(model_mid_HA_se_MUS, newdata = data.frame(dummyNum = quantProportions_HA_se_MUS$dummyNum))
pred_upper_HA_se_MUS <- predict(model_upper_HA_se_MUS, newdata = data.frame(dummyNum = quantProportions_HA_se_MUS$dummyNum))

pred_df_HA_se_MUS <- data.frame(
  categories = rep(quantProportions_HA_se_MUS$categories, times = 3),
  proportion = c(pred_min_HA_se_MUS, pred_mid_HA_se_MUS, pred_upper_HA_se_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_HA_se_MUS)))

pred_df_HA_se_MUS$group <- factor(pred_df_HA_se_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_HA_se_MUS <- pred_df_HA_se_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_HA_se_MUS$categories <- factor(ribbon_df_HA_se_MUS$categories, levels = quantProportions_HA_se_MUS$categories)

pred_df_HA_se_MUS$legend <- ifelse(pred_df_HA_se_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_HA_se_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_HA_se_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_HA_se_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("HeadacheproportionT2_fitted.pdf", height = 15, width = 15)
ggsave("HeadacheproportionT2_fitted.jpg", height = 15, width = 15)

## abdom_pain_se ----

EggT2dataAP <- as.data.frame(t(EstEggT1)) %>% 
  mutate(Abdom_painSE = MusubiSE2004$abdom_pain)
EggT2dataAP[EggT2dataAP<0] <- 0

EggT2dataAP <- EggT2dataAP %>% 
  filter(EggT2dataAP$Abdom_painSE=="1")
EggT2dataAP <- EggT2dataAP[,-40001]
EggT2dataAP <- round(as.data.frame(t(EggT2dataAP)))
EggT2dataAP <- as.matrix(EggT2dataAP)
APtkkcat_SE <- data.frame(matrix(nrow=40000, ncol=26))
colnames(APtkkcat_SE) <- cats
APtkkcat_SE$`0`<-rowSums(EggT2dataAP==0)

APTKK_SE <- list()

for(i in 1:nrow(EggT2dataAP)){
  APTKK_SE[[i]] <- list()
  for(j in 2:ncol(APtkkcat_SE)){
    APtkkcat_SE[i,j] <- length(which(EggT2dataAP[i,]>=Index1[[j-1]] & EggT2dataAP[i,]<=Index[[j-1]]))
    APTKK_SE[[i]][[1]] <- which(EggT2dataAP[i,]==0)
    APTKK_SE[[i]][[j]] <-  which(EggT2dataAP[i,]>=Index1[[j-1]] & EggT2dataAP[i,]<=Index[[j-1]])
  }
}

APtkkcat_SE <- as.data.frame(APtkkcat_SE)

Proportion_APTKKT2 <- as.data.frame(APtkkcat_SE/tKKDistT1)
Proportion_APTKKT2_MUS2 <- as.matrix(Proportion_APTKKT2)

# remove the NaNs
Proportion_APTKKT2_MUS2[is.nan(Proportion_APTKKT2_MUS2)] <- 0
Proportion_APTKKT2_MUS2 <- as.data.frame(Proportion_APTKKT2_MUS2)

# check no remaining NaNs 
str(Proportion_APTKKT2_MUS2)

quantProportions_AP_se_MUS <- sapply(Proportion_APTKKT2_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_AP_se_MUS <- as.data.frame(t(quantProportions_AP_se_MUS))
quantProportions_AP_se_MUS$categories <- rownames(quantProportions_AP_se_MUS)
colnames(quantProportions_AP_se_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_AP_se_MUS$dummyNum <- 1:nrow(quantProportions_AP_se_MUS)

quantProportions_AP_se_MUS$categories <- factor(quantProportions_AP_se_MUS$categories,levels = quantProportions_AP_se_MUS$categories)

ggplot(quantProportions_AP_se_MUS, aes(x = categories)) +
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

model_min_AP_se_MUS <- drm(min ~ dummyNum, data = quantProportions_AP_se_MUS, fct = BC.5())
model_mid_AP_se_MUS <- drm(mid ~ dummyNum, data = quantProportions_AP_se_MUS, fct = CRS.6())
model_upper_AP_se_MUS <- drm(upper ~ dummyNum, data = quantProportions_AP_se_MUS, fct = CRS.6())

pred_min_AP_se_MUS <- predict(model_min_AP_se_MUS, newdata = data.frame(dummyNum = quantProportions_AP_se_MUS$dummyNum))
pred_mid_AP_se_MUS <- predict(model_mid_AP_se_MUS, newdata = data.frame(dummyNum = quantProportions_AP_se_MUS$dummyNum))
pred_upper_AP_se_MUS <- predict(model_upper_AP_se_MUS, newdata = data.frame(dummyNum = quantProportions_AP_se_MUS$dummyNum))

pred_df_AP_se_MUS <- data.frame(
  categories = rep(quantProportions_AP_se_MUS$categories, times = 3),
  proportion = c(pred_min_AP_se_MUS, pred_mid_AP_se_MUS, pred_upper_AP_se_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_AP_se_MUS)))

pred_df_AP_se_MUS$group <- factor(pred_df_AP_se_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_AP_se_MUS <- pred_df_AP_se_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_AP_se_MUS$categories <- factor(ribbon_df_AP_se_MUS$categories, levels = quantProportions_AP_se_MUS$categories)

pred_df_AP_se_MUS$legend <- ifelse(pred_df_AP_se_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_AP_se_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_AP_se_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_AP_se_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Abdom_painproportionT2_fitted.pdf", height = 15, width = 15)
ggsave("Abdom_painproportionT2_fitted.jpg", height = 15, width = 15)

## urine_pain_se ----
EggT2dataUP <- as.data.frame(t(EstEggT1)) %>% 
  mutate(Urine_painSE = MusubiSE2004$urine_pain)
EggT2dataUP[EggT2dataUP<0] <- 0

EggT2dataUP <- EggT2dataUP %>% 
  filter(EggT2dataUP$Urine_painSE=="1")
EggT2dataUP <- EggT2dataUP[,-40001]
EggT2dataUP <- round(as.data.frame(t(EggT2dataUP)))
EggT2dataUP <- as.matrix(EggT2dataUP)
UPtkkcat_SE <- data.frame(matrix(nrow=40000, ncol=26))
colnames(UPtkkcat_SE) <- cats
UPtkkcat_SE$`0`<-rowSums(EggT2dataUP==0)

UPTKK_SE <- list()

for(i in 1:nrow(EggT2dataUP)){
  UPTKK_SE[[i]] <- list()
  for(j in 2:ncol(UPtkkcat_SE)){
    UPtkkcat_SE[i,j] <- length(which(EggT2dataUP[i,]>=Index1[[j-1]] & EggT2dataUP[i,]<=Index[[j-1]]))
    UPTKK_SE[[i]][[1]] <- which(EggT2dataUP[i,]==0)
    UPTKK_SE[[i]][[j]] <-  which(EggT2dataUP[i,]>=Index1[[j-1]] & EggT2dataUP[i,]<=Index[[j-1]])
  }
}

UPtkkcat_SE <- as.data.frame(UPtkkcat_SE)

Proportion_UPTKKT2 <- as.data.frame(UPtkkcat_SE/tKKDistT1)
Proportion_UPTKKT2_MUS2 <- as.matrix(Proportion_UPTKKT2)

# remove the NaNs
Proportion_UPTKKT2_MUS2[is.nan(Proportion_UPTKKT2_MUS2)] <- 0
Proportion_UPTKKT2_MUS2 <- as.data.frame(Proportion_UPTKKT2_MUS2)

# check no remaining NaNs 
str(Proportion_UPTKKT2_MUS2)

quantProportions_UP_se_MUS <- sapply(Proportion_UPTKKT2_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_UP_se_MUS <- as.data.frame(t(quantProportions_UP_se_MUS))
quantProportions_UP_se_MUS$categories <- rownames(quantProportions_UP_se_MUS)
colnames(quantProportions_UP_se_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_UP_se_MUS$dummyNum <- 1:nrow(quantProportions_UP_se_MUS)

quantProportions_UP_se_MUS$categories <- factor(quantProportions_UP_se_MUS$categories,levels = quantProportions_UP_se_MUS$categories)

ggplot(quantProportions_UP_se_MUS, aes(x = categories)) +
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

pred_df_UP_se_MUS <- data.frame(
  categories = rep(quantProportions_UP_se_MUS$categories, times = 3),
  proportion = rep(0, length.out = 3 * nrow(quantProportions_UP_se_MUS)),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_UP_se_MUS))
)

pred_df_UP_se_MUS$group <- factor(pred_df_UP_se_MUS$group, levels = c("min", "mid", "upper"))

ribbon_df_UP_se_MUS <- pred_df_UP_se_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_UP_se_MUS$categories <- factor(ribbon_df_UP_se_MUS$categories, levels = quantProportions_UP_se_MUS$categories)

pred_df_UP_se_MUS$legend <- ifelse(pred_df_UP_se_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_UP_se_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_UP_se_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_UP_se_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Urine_painproportionT2_fitted.pdf", height = 15, width = 15)
ggsave("Urine_painproportionT2_fitted.jpg", height = 15, width = 15)

## itching ----
EggT2dataIR <- as.data.frame(t(EstEggT1)) %>% 
  mutate(ItchingSE = MusubiSE2004$rash)
EggT2dataIR[EggT2dataIR<0] <- 0

EggT2dataIR <- EggT2dataIR %>% 
  filter(EggT2dataIR$ItchingSE=="1")
EggT2dataIR <- EggT2dataIR[,-40001]
EggT2dataIR <- round(as.data.frame(t(EggT2dataIR)))
EggT2dataIR <- as.matrix(EggT2dataIR)
IRtkkcat_SE <- data.frame(matrix(nrow=40000, ncol=26))
colnames(IRtkkcat_SE) <- cats
IRtkkcat_SE$`0`<-rowSums(EggT2dataIR==0)

IRTKK_SE <- list()

for(i in 1:nrow(EggT2dataIR)){
  IRTKK_SE[[i]] <- list()
  for(j in 2:ncol(IRtkkcat_SE)){
    IRtkkcat_SE[i,j] <- length(which(EggT2dataIR[i,]>=Index1[[j-1]] & EggT2dataIR[i,]<=Index[[j-1]]))
    IRTKK_SE[[i]][[1]] <- which(EggT2dataIR[i,]==0)
    IRTKK_SE[[i]][[j]] <-  which(EggT2dataIR[i,]>=Index1[[j-1]] & EggT2dataIR[i,]<=Index[[j-1]])
  }
}

IRtkkcat_SE <- as.data.frame(IRtkkcat_SE)

Proportion_IRTKKT2 <- as.data.frame(IRtkkcat_SE/tKKDistT1)
Proportion_IRTKKT2_MUS2 <- as.matrix(Proportion_IRTKKT2)

# remove the NaNs
Proportion_IRTKKT2_MUS2[is.nan(Proportion_IRTKKT2_MUS2)] <- 0
Proportion_IRTKKT2_MUS2 <- as.data.frame(Proportion_IRTKKT2_MUS2)

# check no remaining NaNs 
str(Proportion_IRTKKT2_MUS2)

quantProportions_IR_se_MUS <- sapply(Proportion_IRTKKT2_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_IR_se_MUS <- as.data.frame(t(quantProportions_IR_se_MUS))
quantProportions_IR_se_MUS$categories <- rownames(quantProportions_IR_se_MUS)
colnames(quantProportions_IR_se_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_IR_se_MUS$dummyNum <- 1:nrow(quantProportions_IR_se_MUS)

quantProportions_IR_se_MUS$categories <- factor(quantProportions_IR_se_MUS$categories,levels = quantProportions_IR_se_MUS$categories)

ggplot(quantProportions_IR_se_MUS, aes(x = categories)) +
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

pred_df_IR_se_MUS <- data.frame(
  categories = rep(quantProportions_IR_se_MUS$categories, times = 3),
  proportion = rep(0, length.out = 3 * nrow(quantProportions_IR_se_MUS)),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_IR_se_MUS))
)

pred_df_IR_se_MUS$group <- factor(pred_df_IR_se_MUS$group, levels = c("min", "mid", "upper"))

ribbon_df_IR_se_MUS <- pred_df_IR_se_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_IR_se_MUS$categories <- factor(ribbon_df_IR_se_MUS$categories, levels = quantProportions_IR_se_MUS$categories)

pred_df_IR_se_MUS$legend <- ifelse(pred_df_IR_se_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_IR_se_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_IR_se_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_IR_se_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("ItchingproportionT2_fitted.pdf", height = 15, width = 15)
ggsave("ItchingproportionT2_fitted.jpg", height = 15, width = 15)

## diarrhoea ----
EggT2dataDH <- as.data.frame(t(EstEggT1)) %>% 
  mutate(DiarrhoeaSE = MusubiSE2004$diarrhoea)
EggT2dataDH[EggT2dataDH<0] <- 0

EggT2dataDH <- EggT2dataDH %>% 
  filter(EggT2dataDH$DiarrhoeaSE=="1")
EggT2dataDH <- EggT2dataDH[,-40001]
EggT2dataDH <- round(as.data.frame(t(EggT2dataDH)))
EggT2dataDH <- as.matrix(EggT2dataDH)
DHtkkcat_SE <- data.frame(matrix(nrow=40000, ncol=26))
colnames(DHtkkcat_SE) <- cats
DHtkkcat_SE$`0`<-rowSums(EggT2dataDH==0)

DHTKK_SE <- list()

for(i in 1:nrow(EggT2dataDH)){
  DHTKK_SE[[i]] <- list()
  for(j in 2:ncol(DHtkkcat_SE)){
    DHtkkcat_SE[i,j] <- length(which(EggT2dataDH[i,]>=Index1[[j-1]] & EggT2dataDH[i,]<=Index[[j-1]]))
    DHTKK_SE[[i]][[1]] <- which(EggT2dataDH[i,]==0)
    DHTKK_SE[[i]][[j]] <-  which(EggT2dataDH[i,]>=Index1[[j-1]] & EggT2dataDH[i,]<=Index[[j-1]])
  }
}

DHtkkcat_SE <- as.data.frame(DHtkkcat_SE)

Proportion_DHTKKT2 <- as.data.frame(DHtkkcat_SE/tKKDistT1)
Proportion_DHTKKT2_MUS2 <- as.matrix(Proportion_DHTKKT2)

# remove the NaNs
Proportion_DHTKKT2_MUS2[is.nan(Proportion_DHTKKT2_MUS2)] <- 0
Proportion_DHTKKT2_MUS2 <- as.data.frame(Proportion_DHTKKT2_MUS2)

# check no remaining NaNs 
str(Proportion_DHTKKT2_MUS2)

quantProportions_DH_se_MUS <- sapply(Proportion_DHTKKT2_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_DH_se_MUS <- as.data.frame(t(quantProportions_DH_se_MUS))
quantProportions_DH_se_MUS$categories <- rownames(quantProportions_DH_se_MUS)
colnames(quantProportions_DH_se_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_DH_se_MUS$dummyNum <- 1:nrow(quantProportions_DH_se_MUS)

quantProportions_DH_se_MUS$categories <- factor(quantProportions_DH_se_MUS$categories,levels = quantProportions_DH_se_MUS$categories)

ggplot(quantProportions_DH_se_MUS, aes(x = categories)) +
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

model_min_DH_se_MUS <- drm(min ~ dummyNum, data = quantProportions_DH_se_MUS, fct = BC.5())
model_mid_DH_se_MUS <- drm(mid ~ dummyNum, data = quantProportions_DH_se_MUS, fct = BC.5())
model_upper_DH_se_MUS <- drm(upper ~ dummyNum, data = quantProportions_DH_se_MUS, fct = CRS.6())

pred_min_DH_se_MUS <- predict(model_min_DH_se_MUS, newdata = data.frame(dummyNum = quantProportions_DH_se_MUS$dummyNum))
pred_mid_DH_se_MUS <- predict(model_mid_DH_se_MUS, newdata = data.frame(dummyNum = quantProportions_DH_se_MUS$dummyNum))
pred_upper_DH_se_MUS <- predict(model_upper_DH_se_MUS, newdata = data.frame(dummyNum = quantProportions_DH_se_MUS$dummyNum))

pred_df_DH_se_MUS <- data.frame(
  categories = rep(quantProportions_DH_se_MUS$categories, times = 3),
  proportion = c(pred_min_DH_se_MUS, pred_mid_DH_se_MUS, pred_upper_DH_se_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_DH_se_MUS)))

pred_df_DH_se_MUS$group <- factor(pred_df_DH_se_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_DH_se_MUS <- pred_df_DH_se_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_DH_se_MUS$categories <- factor(ribbon_df_DH_se_MUS$categories, levels = quantProportions_DH_se_MUS$categories)

pred_df_DH_se_MUS$legend <- ifelse(pred_df_DH_se_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_DH_se_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_DH_se_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_DH_se_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("DiarrhoeaproportionT2_fitted.pdf", height = 15, width = 15)
ggsave("DiarrhoeaproportionT2_fitted.jpg", height = 15, width = 15)

## nausea ----
EggT2dataNS <- as.data.frame(t(EstEggT1)) %>% 
  mutate(NauseaSE = MusubiSE2004$nausea)
EggT2dataNS[EggT2dataNS<0] <- 0

EggT2dataNS <- EggT2dataNS %>% 
  filter(EggT2dataNS$NauseaSE=="1")
EggT2dataNS <- EggT2dataNS[,-40001]
EggT2dataNS <- round(as.data.frame(t(EggT2dataNS)))
EggT2dataNS <- as.matrix(EggT2dataNS)
NStkkcat_SE <- data.frame(matrix(nrow=40000, ncol=26))
colnames(NStkkcat_SE) <- cats
NStkkcat_SE$`0`<-rowSums(EggT2dataNS==0)

NSTKK_SE <- list()

for(i in 1:nrow(EggT2dataNS)){
  NSTKK_SE[[i]] <- list()
  for(j in 2:ncol(NStkkcat_SE)){
    NStkkcat_SE[i,j] <- length(which(EggT2dataNS[i,]>=Index1[[j-1]] & EggT2dataNS[i,]<=Index[[j-1]]))
    NSTKK_SE[[i]][[1]] <- which(EggT2dataNS[i,]==0)
    NSTKK_SE[[i]][[j]] <-  which(EggT2dataNS[i,]>=Index1[[j-1]] & EggT2dataNS[i,]<=Index[[j-1]])
  }
}

NStkkcat_SE <- as.data.frame(NStkkcat_SE)

Proportion_NSTKKT2 <- as.data.frame(NStkkcat_SE/tKKDistT1)
Proportion_NSTKKT2_MUS2 <- as.matrix(Proportion_NSTKKT2)

# remove the NaNs
Proportion_NSTKKT2_MUS2[is.nan(Proportion_NSTKKT2_MUS2)] <- 0
Proportion_NSTKKT2_MUS2 <- as.data.frame(Proportion_NSTKKT2_MUS2)

# check no remaining NaNs 
str(Proportion_NSTKKT2_MUS2)

quantProportions_NS_se_MUS <- sapply(Proportion_NSTKKT2_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_NS_se_MUS <- as.data.frame(t(quantProportions_NS_se_MUS))
quantProportions_NS_se_MUS$categories <- rownames(quantProportions_NS_se_MUS)
colnames(quantProportions_NS_se_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_NS_se_MUS$dummyNum <- 1:nrow(quantProportions_NS_se_MUS)

quantProportions_NS_se_MUS$categories <- factor(quantProportions_NS_se_MUS$categories,levels = quantProportions_NS_se_MUS$categories)

ggplot(quantProportions_NS_se_MUS, aes(x = categories)) +
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

pred_df_NS_se_MUS <- data.frame(
  categories = rep(quantProportions_NS_se_MUS$categories, times = 3),
  proportion = rep(0, length.out = 3 * nrow(quantProportions_NS_se_MUS)),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_NS_se_MUS))
)

pred_df_NS_se_MUS$group <- factor(pred_df_NS_se_MUS$group, levels = c("min", "mid", "upper"))

ribbon_df_NS_se_MUS <- pred_df_NS_se_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_NS_se_MUS$categories <- factor(ribbon_df_NS_se_MUS$categories, levels = quantProportions_NS_se_MUS$categories)

pred_df_NS_se_MUS$legend <- ifelse(pred_df_NS_se_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_NS_se_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_NS_se_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_NS_se_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("NauseaproportionT2_fitted.pdf", height = 15, width = 15)
ggsave("NauseaproportionT2_fitted.jpg", height = 15, width = 15)

## blood_stool ----
EggT2dataBS <- as.data.frame(t(EstEggT1)) %>% 
  mutate(Blood_stoolSE = MusubiSE2004$blood_stool)
EggT2dataBS[EggT2dataBS<0] <- 0

EggT2dataBS <- EggT2dataBS %>% 
  filter(EggT2dataBS$Blood_stoolSE=="1")
EggT2dataBS <- EggT2dataBS[,-40001]
EggT2dataBS <- round(as.data.frame(t(EggT2dataBS)))
EggT2dataBS <- as.matrix(EggT2dataBS)
BStkkcat_SE <- data.frame(matrix(nrow=40000, ncol=26))
colnames(BStkkcat_SE) <- cats
BStkkcat_SE$`0`<-rowSums(EggT2dataBS==0)

BSTKK_SE <- list()

for(i in 1:nrow(EggT2dataBS)){
  BSTKK_SE[[i]] <- list()
  for(j in 2:ncol(BStkkcat_SE)){
    BStkkcat_SE[i,j] <- length(which(EggT2dataBS[i,]>=Index1[[j-1]] & EggT2dataBS[i,]<=Index[[j-1]]))
    BSTKK_SE[[i]][[1]] <- which(EggT2dataBS[i,]==0)
    BSTKK_SE[[i]][[j]] <-  which(EggT2dataBS[i,]>=Index1[[j-1]] & EggT2dataBS[i,]<=Index[[j-1]])
  }
}

BStkkcat_SE <- as.data.frame(BStkkcat_SE)

Proportion_BSTKKT2 <- as.data.frame(BStkkcat_SE/tKKDistT1)
Proportion_BSTKKT2_MUS2 <- as.matrix(Proportion_BSTKKT2)

# remove the NaNs
Proportion_BSTKKT2_MUS2[is.nan(Proportion_BSTKKT2_MUS2)] <- 0
Proportion_BSTKKT2_MUS2 <- as.data.frame(Proportion_BSTKKT2_MUS2)

# check no remaining NaNs 
str(Proportion_BSTKKT2_MUS2)

quantProportions_BS_se_MUS <- sapply(Proportion_BSTKKT2_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_BS_se_MUS <- as.data.frame(t(quantProportions_BS_se_MUS))
quantProportions_BS_se_MUS$categories <- rownames(quantProportions_BS_se_MUS)
colnames(quantProportions_BS_se_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_BS_se_MUS$dummyNum <- 1:nrow(quantProportions_BS_se_MUS)

quantProportions_BS_se_MUS$categories <- factor(quantProportions_BS_se_MUS$categories,levels = quantProportions_BS_se_MUS$categories)

ggplot(quantProportions_BS_se_MUS, aes(x = categories)) +
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

pred_df_BS_se_MUS <- data.frame(
  categories = rep(quantProportions_BS_se_MUS$categories, times = 3),
  proportion = rep(0, length.out = 3 * nrow(quantProportions_BS_se_MUS)),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_BS_se_MUS))
)

pred_df_BS_se_MUS$group <- factor(pred_df_BS_se_MUS$group, levels = c("min", "mid", "upper"))

ribbon_df_BS_se_MUS <- pred_df_BS_se_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_BS_se_MUS$categories <- factor(ribbon_df_BS_se_MUS$categories, levels = quantProportions_BS_se_MUS$categories)

pred_df_BS_se_MUS$legend <- ifelse(pred_df_BS_se_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_BS_se_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_BS_se_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_BS_se_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Blood_stoolproportionT2_fitted.pdf", height = 15, width = 15)
ggsave("Blood_stoolproportionT2_fitted.jpg", height = 15, width = 15)

## vomiting ----
EggT2dataVM <- as.data.frame(t(EstEggT1)) %>% 
  mutate(VomitSE = MusubiSE2004$vomit)
EggT2dataVM[EggT2dataVM<0] <- 0

EggT2dataVM <- EggT2dataVM %>% 
  filter(EggT2dataVM$VomitSE=="1")
EggT2dataVM <- EggT2dataVM[,-40001]
EggT2dataVM <- round(as.data.frame(t(EggT2dataVM)))
EggT2dataVM <- as.matrix(EggT2dataVM)
VMtkkcat_SE <- data.frame(matrix(nrow=40000, ncol=26))
colnames(VMtkkcat_SE) <- cats
VMtkkcat_SE$`0`<-rowSums(EggT2dataVM==0)

VMTKK_SE <- list()

for(i in 1:nrow(EggT2dataVM)){
  VMTKK_SE[[i]] <- list()
  for(j in 2:ncol(VMtkkcat_SE)){
    VMtkkcat_SE[i,j] <- length(which(EggT2dataVM[i,]>=Index1[[j-1]] & EggT2dataVM[i,]<=Index[[j-1]]))
    VMTKK_SE[[i]][[1]] <- which(EggT2dataVM[i,]==0)
    VMTKK_SE[[i]][[j]] <-  which(EggT2dataVM[i,]>=Index1[[j-1]] & EggT2dataVM[i,]<=Index[[j-1]])
  }
}

VMtkkcat_SE <- as.data.frame(VMtkkcat_SE)

Proportion_VMTKKT2 <- as.data.frame(VMtkkcat_SE/tKKDistT1)
Proportion_VMTKKT2_MUS2 <- as.matrix(Proportion_VMTKKT2)

# remove the NaNs
Proportion_VMTKKT2_MUS2[is.nan(Proportion_VMTKKT2_MUS2)] <- 0
Proportion_VMTKKT2_MUS2 <- as.data.frame(Proportion_VMTKKT2_MUS2)

# check no remaining NaNs 
str(Proportion_VMTKKT2_MUS2)

quantProportions_VM_se_MUS <- sapply(Proportion_VMTKKT2_MUS2, quantile, probs=c(0.025, 0.5, 0.975))

quantProportions_VM_se_MUS <- as.data.frame(t(quantProportions_VM_se_MUS))
quantProportions_VM_se_MUS$categories <- rownames(quantProportions_VM_se_MUS)
colnames(quantProportions_VM_se_MUS) <- c("min", "mid", "upper", "categories")

quantProportions_VM_se_MUS$dummyNum <- 1:nrow(quantProportions_VM_se_MUS)

quantProportions_VM_se_MUS$categories <- factor(quantProportions_VM_se_MUS$categories,levels = quantProportions_VM_se_MUS$categories)

ggplot(quantProportions_VM_se_MUS, aes(x = categories)) +
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

model_min_VM_se_MUS <- drm(min ~ dummyNum, data = quantProportions_VM_se_MUS, fct = LL.5())
model_mid_VM_se_MUS <- drm(mid ~ dummyNum, data = quantProportions_VM_se_MUS, fct = BC.5())
model_upper_VM_se_MUS <- drm(upper ~ dummyNum, data = quantProportions_VM_se_MUS, fct = BC.5())

pred_min_VM_se_MUS <- predict(model_min_VM_se_MUS, newdata = data.frame(dummyNum = quantProportions_VM_se_MUS$dummyNum))
pred_mid_VM_se_MUS <- predict(model_mid_VM_se_MUS, newdata = data.frame(dummyNum = quantProportions_VM_se_MUS$dummyNum))
pred_upper_VM_se_MUS <- predict(model_upper_VM_se_MUS, newdata = data.frame(dummyNum = quantProportions_VM_se_MUS$dummyNum))

pred_df_VM_se_MUS <- data.frame(
  categories = rep(quantProportions_VM_se_MUS$categories, times = 3),
  proportion = c(pred_min_VM_se_MUS, pred_mid_VM_se_MUS, pred_upper_VM_se_MUS),
  group = rep(c("min", "mid", "upper"), each = nrow(quantProportions_VM_se_MUS)))

pred_df_VM_se_MUS$group <- factor(pred_df_VM_se_MUS$group, levels = c("min", "mid", "upper")) 

ribbon_df_VM_se_MUS <- pred_df_VM_se_MUS %>%
  filter(group %in% c("min", "upper")) %>%
  pivot_wider(names_from = group, values_from = proportion) %>%
  rename(ymin = min, ymax = upper)

ribbon_df_VM_se_MUS$categories <- factor(ribbon_df_VM_se_MUS$categories, levels = quantProportions_VM_se_MUS$categories)

pred_df_VM_se_MUS$legend <- ifelse(pred_df_VM_se_MUS$group == "mid", "Median", "2.5% and 97.5% Quantiles") 

ggplot() +
  geom_ribbon(data = ribbon_df_VM_se_MUS, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = "2.5% - 97.5% Quantiles", group = 1), 
              alpha = 0.3) +
  geom_line(data = pred_df_VM_se_MUS %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = "50% Quantiles", linetype = "50% Quantiles", group = 1), 
            size = 1.2) +
  geom_point(data = quantProportions_VM_se_MUS, 
             aes(x = categories, y = mid, color = "50% Quantiles"), 
             size = 2, alpha = 0.6) +
  scale_fill_manual(name = "Quantile", 
                    values = c("2.5% - 97.5% Quantiles" = "#82CFFF")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("50% Quantiles" = "#82CFFF")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("50% Quantiles" = "solid")) +  
  labs(title = "Proportion by Category with 2.5%, 50%, 97.5% Quantiles",
       x = "Categories",
       y = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("vomitproportionT2_fitted.pdf", height = 15, width = 15)
ggsave("vomitproportionT2_fitted.jpg", height = 15, width = 15)

# H3 & H4 PLOT ----

#### Musubi ----
pred_df_AP_sym_MUS$Symptom <- "Abdominal pain"
ribbon_df_AP_sym_MUS$Symptom <- "Abdominal pain"
pred_df_AP_sym_MUS$Time <- "pre_treatment"
ribbon_df_AP_sym_MUS$Time <- "pre_treatment"
quantProportions_AP_sym_MUS$Symptom <- "Abdominal pain"
quantProportions_AP_sym_MUS$Time <- "pre_treatment"

pred_df_AP_se_MUS$Symptom <- "Abdominal pain"
ribbon_df_AP_se_MUS$Symptom <- "Abdominal pain"
pred_df_AP_se_MUS$Time <- "post_treatment"
ribbon_df_AP_se_MUS$Time <- "post_treatment"
quantProportions_AP_se_MUS$Symptom <- "Abdominal pain"
quantProportions_AP_se_MUS$Time <- "post_treatment"

pred_df_BS_sym_MUS$Symptom <- "Blood stool"
ribbon_df_BS_sym_MUS$Symptom <- "Blood stool"
pred_df_BS_sym_MUS$Time <- "pre_treatment"
ribbon_df_BS_sym_MUS$Time <- "pre_treatment"
quantProportions_BS_sym_MUS$Symptom <- "Blood stool"
quantProportions_BS_sym_MUS$Time <- "pre_treatment"

pred_df_BS_se_MUS$Symptom <- "Blood stool"
ribbon_df_BS_se_MUS$Symptom <- "Blood stool"
pred_df_BS_se_MUS$Time <- "post_treatment"
ribbon_df_BS_se_MUS$Time <- "post_treatment"
quantProportions_BS_se_MUS$Symptom <- "Blood stool"
quantProportions_BS_se_MUS$Time <- "post_treatment"

pred_df_DH_sym_MUS$Symptom <- "Diarrhoea"
ribbon_df_DH_sym_MUS$Symptom <- "Diarrhoea"
pred_df_DH_sym_MUS$Time <- "pre_treatment"
ribbon_df_DH_sym_MUS$Time <- "pre_treatment"
quantProportions_DH_sym_MUS$Symptom <- "Diarrhoea"
quantProportions_DH_sym_MUS$Time <- "pre_treatment"

pred_df_DH_se_MUS$Symptom <- "Diarrhoea"
ribbon_df_DH_se_MUS$Symptom <- "Diarrhoea"
pred_df_DH_se_MUS$Time <- "post_treatment"
ribbon_df_DH_se_MUS$Time <- "post_treatment"
quantProportions_DH_se_MUS$Symptom <- "Diarrhoea"
quantProportions_DH_se_MUS$Time <- "post_treatment"

pred_df_HA_sym_MUS$Symptom <- "Headache"
ribbon_df_HA_sym_MUS$Symptom <- "Headache"
pred_df_HA_sym_MUS$Time <- "pre_treatment"
ribbon_df_HA_sym_MUS$Time <- "pre_treatment"
quantProportions_HA_sym_MUS$Symptom <- "Headache"
quantProportions_HA_sym_MUS$Time <- "pre_treatment"

pred_df_HA_se_MUS$Symptom <- "Headache"
ribbon_df_HA_se_MUS$Symptom <- "Headache"
pred_df_HA_se_MUS$Time <- "post_treatment"
ribbon_df_HA_se_MUS$Time <- "post_treatment"
quantProportions_HA_se_MUS$Symptom <- "Headache"
quantProportions_HA_se_MUS$Time <- "post_treatment"

pred_df_IR_sym_MUS$Symptom <- "Itching rash"
ribbon_df_IR_sym_MUS$Symptom <- "Itching rash"
pred_df_IR_sym_MUS$Time <- "pre_treatment"
ribbon_df_IR_sym_MUS$Time <- "pre_treatment"
quantProportions_IR_sym_MUS$Symptom <- "Itching rash"
quantProportions_IR_sym_MUS$Time <- "pre_treatment"

pred_df_IR_se_MUS$Symptom <- "Itching rash"
ribbon_df_IR_se_MUS$Symptom <- "Itching rash"
pred_df_IR_se_MUS$Time <- "post_treatment"
ribbon_df_IR_se_MUS$Time <- "post_treatment"
quantProportions_IR_se_MUS$Symptom <- "Itching rash"
quantProportions_IR_se_MUS$Time <- "post_treatment"

pred_df_NS_sym_MUS$Symptom <- "Nausea"
ribbon_df_NS_sym_MUS$Symptom <- "Nausea"
pred_df_NS_sym_MUS$Time <- "pre_treatment"
ribbon_df_NS_sym_MUS$Time <- "pre_treatment"
quantProportions_NS_sym_MUS$Symptom <- "Nausea"
quantProportions_NS_sym_MUS$Time <- "pre_treatment"

pred_df_NS_se_MUS$Symptom <- "Nausea"
ribbon_df_NS_se_MUS$Symptom <- "Nausea"
pred_df_NS_se_MUS$Time <- "post_treatment"
ribbon_df_NS_se_MUS$Time <- "post_treatment"
quantProportions_NS_se_MUS$Symptom <- "Nausea"
quantProportions_NS_se_MUS$Time <- "post_treatment"

pred_df_UP_sym_MUS$Symptom <- "Urinate pain"
ribbon_df_UP_sym_MUS$Symptom <- "Urinate pain"
pred_df_UP_sym_MUS$Time <- "pre_treatment"
ribbon_df_UP_sym_MUS$Time <- "pre_treatment"
quantProportions_UP_sym_MUS$Symptom <- "Urinate pain"
quantProportions_UP_sym_MUS$Time <- "pre_treatment"

pred_df_UP_se_MUS$Symptom <- "Urinate pain"
ribbon_df_UP_se_MUS$Symptom <- "Urinate pain"
pred_df_UP_se_MUS$Time <- "post_treatment"
ribbon_df_UP_se_MUS$Time <- "post_treatment"
quantProportions_UP_se_MUS$Symptom <- "Urinate pain"
quantProportions_UP_se_MUS$Time <- "post_treatment"

pred_df_VM_se_MUS$Symptom <- "Vomiting"
ribbon_df_VM_se_MUS$Symptom <- "Vomiting"
pred_df_VM_se_MUS$Time <- "post_treatment"
ribbon_df_VM_se_MUS$Time <- "post_treatment"
quantProportions_VM_se_MUS$Symptom <- "Vomiting"
quantProportions_VM_se_MUS$Time <- "post_treatment"


sym_se_plot_Musubi_line <- bind_rows(pred_df_AP_sym_MUS, pred_df_AP_se_MUS, pred_df_BS_sym_MUS, pred_df_BS_se_MUS,
                                     pred_df_DH_sym_MUS, pred_df_DH_se_MUS, pred_df_HA_sym_MUS, pred_df_HA_se_MUS,
                                     pred_df_IR_sym_MUS, pred_df_IR_se_MUS, pred_df_NS_sym_MUS, pred_df_NS_se_MUS,
                                     pred_df_UP_sym_MUS, pred_df_UP_se_MUS, pred_df_VM_se_MUS)

sym_se_plot_Musubi_shade <- bind_rows(ribbon_df_AP_sym_MUS, ribbon_df_AP_se_MUS, ribbon_df_BS_sym_MUS, ribbon_df_BS_se_MUS,
                                     ribbon_df_DH_sym_MUS, ribbon_df_DH_se_MUS, ribbon_df_HA_sym_MUS, ribbon_df_HA_se_MUS,
                                     ribbon_df_IR_sym_MUS, ribbon_df_IR_se_MUS, ribbon_df_NS_sym_MUS, ribbon_df_NS_se_MUS,
                                     ribbon_df_UP_sym_MUS, ribbon_df_UP_se_MUS, ribbon_df_VM_se_MUS)

sym_se_plot_Musubi_point <- bind_rows(quantProportions_AP_sym_MUS, quantProportions_AP_se_MUS, quantProportions_BS_sym_MUS, quantProportions_BS_se_MUS,
                                      quantProportions_DH_sym_MUS, quantProportions_DH_se_MUS, quantProportions_HA_sym_MUS, quantProportions_HA_se_MUS,
                                      quantProportions_IR_sym_MUS, quantProportions_IR_se_MUS, quantProportions_NS_sym_MUS, quantProportions_NS_se_MUS,
                                      quantProportions_UP_sym_MUS, quantProportions_UP_se_MUS, quantProportions_VM_se_MUS)

sym_se_plot_Musubi_line$Time <- factor(sym_se_plot_Musubi_line$Time, levels = c("pre_treatment", "post_treatment"))
sym_se_plot_Musubi_line$Symptom <- factor(sym_se_plot_Musubi_line$Symptom, levels = c("Abdominal pain", "Blood stool", "Diarrhoea", "Headache", "Itching rash", "Nausea", "Urinate pain", "Vomiting"))

sym_se_plot_Musubi_shade$Time <- factor(sym_se_plot_Musubi_shade$Time, levels = c("pre_treatment", "post_treatment"))
sym_se_plot_Musubi_shade$Symptom <- factor(sym_se_plot_Musubi_shade$Symptom, levels = c("Abdominal pain", "Blood stool", "Diarrhoea", "Headache", "Itching rash", "Nausea", "Urinate pain", "Vomiting"))

sym_se_plot_Musubi_point$Time <- factor(sym_se_plot_Musubi_point$Time, levels = c("pre_treatment", "post_treatment"))
sym_se_plot_Musubi_point$Symptom <- factor(sym_se_plot_Musubi_point$Symptom, levels = c("Abdominal pain", "Blood stool", "Diarrhoea", "Headache", "Itching rash", "Nausea", "Urinate pain", "Vomiting"))

musubi_cats <- ggplot() + 
  geom_ribbon(data = sym_se_plot_Musubi_shade, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = Time, group = interaction(Symptom, Time)), 
              alpha = 0.3) +
  geom_line(data = sym_se_plot_Musubi_line %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = Time, linetype = Time, group = interaction(Symptom, Time)), 
            size = 1.1) +
  geom_point(data = sym_se_plot_Musubi_point, 
             aes(x = categories, y = mid, color = Time), 
             size = 2, alpha = 0.5) +
  scale_fill_manual(name = "Quantile", 
                    values = c("pre_treatment" = "#FF4C7F", "post_treatment" = "#82CFFF"),
                    labels = c("Pre-treatment symptom", "Post-treatment side effect")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("pre_treatment" = "#FF4C7F", "post_treatment" = "#82CFFF"),
                     labels = c("Pre-treatment symptom", "Post-treatment side effect")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("pre_treatment" = "solid", "post_treatment" = "solid"),
                        labels = c("Pre-treatment symptom", "Post-treatment side effect")) +  
  facet_grid(Symptom ~ .) +
  labs(x = "Infection intensity(model estimated eggs per gram of stool)",
       y = "Proportion of students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(size=12), legend.position = "bottom")

ggsave("sym_se_proportion_Musubi.pdf", height = 10)
ggsave("sym_se_proportion_Musubi.jpg", height = 10)

#### Bugoto LV ----
pred_df_AP_sym_BUG$Symptom <- "Abdominal pain"
ribbon_df_AP_sym_BUG$Symptom <- "Abdominal pain"
pred_df_AP_sym_BUG$Time <- "pre_treatment"
ribbon_df_AP_sym_BUG$Time <- "pre_treatment"
quantProportions_AP_sym_BUG$Symptom <- "Abdominal pain"
quantProportions_AP_sym_BUG$Time <- "pre_treatment"

pred_df_AP_se_BUG$Symptom <- "Abdominal pain"
ribbon_df_AP_se_BUG$Symptom <- "Abdominal pain"
pred_df_AP_se_BUG$Time <- "post_treatment"
ribbon_df_AP_se_BUG$Time <- "post_treatment"
quantProportions_AP_se_BUG$Symptom <- "Abdominal pain"
quantProportions_AP_se_BUG$Time <- "post_treatment"

pred_df_BS_sym_BUG$Symptom <- "Blood stool"
ribbon_df_BS_sym_BUG$Symptom <- "Blood stool"
pred_df_BS_sym_BUG$Time <- "pre_treatment"
ribbon_df_BS_sym_BUG$Time <- "pre_treatment"
quantProportions_BS_sym_BUG$Symptom <- "Blood stool"
quantProportions_BS_sym_BUG$Time <- "pre_treatment"

pred_df_BS_se_BUG$Symptom <- "Blood stool"
ribbon_df_BS_se_BUG$Symptom <- "Blood stool"
pred_df_BS_se_BUG$Time <- "post_treatment"
ribbon_df_BS_se_BUG$Time <- "post_treatment"
quantProportions_BS_se_BUG$Symptom <- "Blood stool"
quantProportions_BS_se_BUG$Time <- "post_treatment"

pred_df_DH_sym_BUG$Symptom <- "Diarrhoea"
ribbon_df_DH_sym_BUG$Symptom <- "Diarrhoea"
pred_df_DH_sym_BUG$Time <- "pre_treatment"
ribbon_df_DH_sym_BUG$Time <- "pre_treatment"
quantProportions_DH_sym_BUG$Symptom <- "Diarrhoea"
quantProportions_DH_sym_BUG$Time <- "pre_treatment"

pred_df_DH_se_BUG$Symptom <- "Diarrhoea"
ribbon_df_DH_se_BUG$Symptom <- "Diarrhoea"
pred_df_DH_se_BUG$Time <- "post_treatment"
ribbon_df_DH_se_BUG$Time <- "post_treatment"
quantProportions_DH_se_BUG$Symptom <- "Diarrhoea"
quantProportions_DH_se_BUG$Time <- "post_treatment"

pred_df_HA_sym_BUG$Symptom <- "Headache"
ribbon_df_HA_sym_BUG$Symptom <- "Headache"
pred_df_HA_sym_BUG$Time <- "pre_treatment"
ribbon_df_HA_sym_BUG$Time <- "pre_treatment"
quantProportions_HA_sym_BUG$Symptom <- "Headache"
quantProportions_HA_sym_BUG$Time <- "pre_treatment"

pred_df_HA_se_BUG$Symptom <- "Headache"
ribbon_df_HA_se_BUG$Symptom <- "Headache"
pred_df_HA_se_BUG$Time <- "post_treatment"
ribbon_df_HA_se_BUG$Time <- "post_treatment"
quantProportions_HA_se_BUG$Symptom <- "Headache"
quantProportions_HA_se_BUG$Time <- "post_treatment"

pred_df_IR_sym_BUG$Symptom <- "Itching rash"
ribbon_df_IR_sym_BUG$Symptom <- "Itching rash"
pred_df_IR_sym_BUG$Time <- "pre_treatment"
ribbon_df_IR_sym_BUG$Time <- "pre_treatment"
quantProportions_IR_sym_BUG$Symptom <- "Itching rash"
quantProportions_IR_sym_BUG$Time <- "pre_treatment"

pred_df_IR_se_BUG$Symptom <- "Itching rash"
ribbon_df_IR_se_BUG$Symptom <- "Itching rash"
pred_df_IR_se_BUG$Time <- "post_treatment"
ribbon_df_IR_se_BUG$Time <- "post_treatment"
quantProportions_IR_se_BUG$Symptom <- "Itching rash"
quantProportions_IR_se_BUG$Time <- "post_treatment"

pred_df_NS_sym_BUG$Symptom <- "Nausea"
ribbon_df_NS_sym_BUG$Symptom <- "Nausea"
pred_df_NS_sym_BUG$Time <- "pre_treatment"
ribbon_df_NS_sym_BUG$Time <- "pre_treatment"
quantProportions_NS_sym_BUG$Symptom <- "Nausea"
quantProportions_NS_sym_BUG$Time <- "pre_treatment"

pred_df_NS_se_BUG$Symptom <- "Nausea"
ribbon_df_NS_se_BUG$Symptom <- "Nausea"
pred_df_NS_se_BUG$Time <- "post_treatment"
ribbon_df_NS_se_BUG$Time <- "post_treatment"
quantProportions_NS_se_BUG$Symptom <- "Nausea"
quantProportions_NS_se_BUG$Time <- "post_treatment"

pred_df_UP_sym_BUG$Symptom <- "Urinate pain"
ribbon_df_UP_sym_BUG$Symptom <- "Urinate pain"
pred_df_UP_sym_BUG$Time <- "pre_treatment"
ribbon_df_UP_sym_BUG$Time <- "pre_treatment"
quantProportions_UP_sym_BUG$Symptom <- "Urinate pain"
quantProportions_UP_sym_BUG$Time <- "pre_treatment"

pred_df_UP_se_BUG$Symptom <- "Urinate pain"
ribbon_df_UP_se_BUG$Symptom <- "Urinate pain"
pred_df_UP_se_BUG$Time <- "post_treatment"
ribbon_df_UP_se_BUG$Time <- "post_treatment"
quantProportions_UP_se_BUG$Symptom <- "Urinate pain"
quantProportions_UP_se_BUG$Time <- "post_treatment"

pred_df_VM_se_BUG$Symptom <- "Vomiting"
ribbon_df_VM_se_BUG$Symptom <- "Vomiting"
pred_df_VM_se_BUG$Time <- "post_treatment"
ribbon_df_VM_se_BUG$Time <- "post_treatment"
quantProportions_VM_se_BUG$Symptom <- "Vomiting"
quantProportions_VM_se_BUG$Time <- "post_treatment"

ribbon_df_AP_sym_BUG <- ribbon_df_AP_sym_BUG[-(16:26),]
ribbon_df_AP_se_BUG <- ribbon_df_AP_se_BUG[-(16:26),]

ribbon_df_BS_sym_BUG <- ribbon_df_BS_sym_BUG[-(16:26),]
ribbon_df_BS_se_BUG <- ribbon_df_BS_se_BUG[-(16:26),]

ribbon_df_DH_sym_BUG <- ribbon_df_DH_sym_BUG[-(16:26),]
ribbon_df_DH_se_BUG <- ribbon_df_DH_se_BUG[-(16:26),]

ribbon_df_HA_sym_BUG <- ribbon_df_HA_sym_BUG[-(16:26),]
ribbon_df_HA_se_BUG <- ribbon_df_HA_se_BUG[-(16:26),]

ribbon_df_IR_sym_BUG <- ribbon_df_IR_sym_BUG[-(16:26),]
ribbon_df_IR_se_BUG <- ribbon_df_IR_se_BUG[-(16:26),]

ribbon_df_NS_sym_BUG <- ribbon_df_NS_sym_BUG[-(16:26),]
ribbon_df_NS_se_BUG <- ribbon_df_NS_se_BUG[-(16:26),]

ribbon_df_UP_sym_BUG <- ribbon_df_UP_sym_BUG[-(16:26),]
ribbon_df_UP_se_BUG <- ribbon_df_UP_se_BUG[-(16:26),]

ribbon_df_VM_se_BUG <- ribbon_df_VM_se_BUG[-(16:26),]

sym_se_plot_Bugoto_line <- bind_rows(pred_df_AP_sym_BUG, pred_df_AP_se_BUG, pred_df_BS_sym_BUG, pred_df_BS_se_BUG,
                                     pred_df_DH_sym_BUG, pred_df_DH_se_BUG, pred_df_HA_sym_BUG, pred_df_HA_se_BUG,
                                     pred_df_IR_sym_BUG, pred_df_IR_se_BUG, pred_df_NS_sym_BUG, pred_df_NS_se_BUG,
                                     pred_df_UP_sym_BUG, pred_df_UP_se_BUG, pred_df_VM_se_BUG)

sym_se_plot_Bugoto_shade <- bind_rows(ribbon_df_AP_sym_BUG, ribbon_df_AP_se_BUG, ribbon_df_BS_sym_BUG, ribbon_df_BS_se_BUG,
                                      ribbon_df_DH_sym_BUG, ribbon_df_DH_se_BUG, ribbon_df_HA_sym_BUG, ribbon_df_HA_se_BUG,
                                      ribbon_df_IR_sym_BUG, ribbon_df_IR_se_BUG, ribbon_df_NS_sym_BUG, ribbon_df_NS_se_BUG,
                                      ribbon_df_UP_sym_BUG, ribbon_df_UP_se_BUG, ribbon_df_VM_se_BUG)

sym_se_plot_Bugoto_point <- bind_rows(quantProportions_AP_sym_BUG, quantProportions_AP_se_BUG, quantProportions_BS_sym_BUG, quantProportions_BS_se_BUG,
                                      quantProportions_DH_sym_BUG, quantProportions_DH_se_BUG, quantProportions_HA_sym_BUG, quantProportions_HA_se_BUG,
                                      quantProportions_IR_sym_BUG, quantProportions_IR_se_BUG, quantProportions_NS_sym_BUG, quantProportions_NS_se_BUG,
                                      quantProportions_UP_sym_BUG, quantProportions_UP_se_BUG, quantProportions_VM_se_BUG)

sym_se_plot_Bugoto_line$Time <- factor(sym_se_plot_Bugoto_line$Time, levels = c("pre_treatment", "post_treatment"))
sym_se_plot_Bugoto_line$Symptom <- factor(sym_se_plot_Bugoto_line$Symptom, levels = c("Abdominal pain", "Blood stool", "Diarrhoea", "Headache", "Itching rash", "Nausea", "Urinate pain", "Vomiting"))

sym_se_plot_Bugoto_shade$Time <- factor(sym_se_plot_Bugoto_shade$Time, levels = c("pre_treatment", "post_treatment"))
sym_se_plot_Bugoto_shade$Symptom <- factor(sym_se_plot_Bugoto_shade$Symptom, levels = c("Abdominal pain", "Blood stool", "Diarrhoea", "Headache", "Itching rash", "Nausea", "Urinate pain", "Vomiting"))

sym_se_plot_Bugoto_point$Time <- factor(sym_se_plot_Bugoto_point$Time, levels = c("pre_treatment", "post_treatment"))
sym_se_plot_Bugoto_point$Symptom <- factor(sym_se_plot_Bugoto_point$Symptom, levels = c("Abdominal pain", "Blood stool", "Diarrhoea", "Headache", "Itching rash", "Nausea", "Urinate pain", "Vomiting"))

bugoto_cats <- ggplot() + 
  geom_ribbon(data = sym_se_plot_Bugoto_shade, 
              aes(x = categories, ymin = ymin, ymax = ymax, fill = Time, group = interaction(Symptom, Time)), 
              alpha = 0.3) +
  geom_line(data = sym_se_plot_Bugoto_line %>% filter(group == "mid"), 
            aes(x = categories, y = proportion, color = Time, linetype = Time, group = interaction(Symptom, Time)), 
            size = 1.1) +
  geom_point(data = sym_se_plot_Bugoto_point, 
             aes(x = categories, y = mid, color = Time), 
             size = 2, alpha = 0.5) +
  scale_fill_manual(name = "Quantile", 
                    values = c("pre_treatment" = "#FF4C7F", "post_treatment" = "#82CFFF"),
                    labels = c("Pre-treatment symptom", "Post-treatment side effect")) +  
  scale_color_manual(name = "Quantile", 
                     values = c("pre_treatment" = "#FF4C7F", "post_treatment" = "#82CFFF"),
                     labels = c("Pre-treatment symptom", "Post-treatment side effect")) +  
  scale_linetype_manual(name = "Quantile", 
                        values = c("pre_treatment" = "solid", "post_treatment" = "solid"),
                        labels = c("Pre-treatment symptom", "Post-treatment side effect")) +  
  facet_grid(Symptom ~ .) +
  labs(x = "Infection intensity(model estimated eggs per gram of stool)",
       y = "Proportion of students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(size=12), legend.position = "bottom")

ggsave("sym_se_proportion_Bugoto.pdf", height = 10)
ggsave("sym_se_proportion_Bugoto.jpg", height = 10)

bugoto_cats <- ggplot()+
  geom_point(data = sym_se_plot_Bugoto, 
             aes(x = EggCountCats, y = proportion, colour = Time), size = 2,  alpha = 0.5)+
  geom_errorbar(data = sym_se_plot_Bugoto, 
                aes(x = EggCountCats, ymin = proportion-SE, ymax = proportion+SE, colour = Time),
                width=.5)+
  geom_line(data = sym_se_plot_Bugoto, 
            aes(x = x, y = nls, colour = Time), size = 1, alpha = 0.8)+
  theme_bw()+
  facet_grid(Symptom~.)+
  scale_colour_manual(values=col_post_pre,
                      labels = c("pre-treatment symptom", "post-treatment side effect"))+
  ylab("Proportion of students")+
  xlab("Infection intensity(model estimated eggs per gram of stool)")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1), text=element_text(size=12), legend.position = "bottom")

ggsave("sym_se_proportion_Bugoto.pdf", height = 10)
ggsave("sym_se_proportion_Bugoto.jpg", height = 10)

sym_se_proportion_musbug <- bugoto_cats + musubi_cats + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("sym_se_proportion_musbug.pdf", height = 10, width = 10.84)
ggsave("sym_se_proportion_musbug.jpg", height = 10, width = 10.84)

