# HMM Model Source ----

source("Functions.R")

# Bugoto LV ----

BugotoLV2004 <- read.csv("BugotoLV.csv")
katokatzBG <- loadkkdata(BugotoLV2004) # there is a warning message here, don't worry about it 
KKIDsBG <- getKKChildIDs("BugotoLV.csv") 

ccadataBG <- getCCA(BugotoLV2004)
CCAIDsBG <- getCCAChildIDs("BugotoLV.csv") # Get CCAIDs

# check all the same kids are there
CIDBG <- intersect(KKIDsBG,CCAIDsBG) 
katokatzBG <- katokatzBG[match(CIDBG,KKIDsBG),,]
ccadataBG <- ccadataBG[match(CIDBG,CCAIDsBG),]

#### Parasite data 2004 Bugoto LV ----

para2004BG <- read_csv("Baseline2004Bugoto.csv")%>%
  dplyr::select(School, Recruityr, CID, SEX, AGE, 
                CCA0, CCA2, CCA3,
                sm0d1s1:sm0d3s2,
                sm2d1s1: sm2d3s2,
                sm3d1s1:sm3d3s2,
                Height0,Weight0, Hb0, se0:sebldst0)%>%
  rename(height=Height0, weight=Weight0, 
         haemcount=Hb0, Age=AGE,
         side_effects=se0, abdom_pain=seabdpn0, 
         headache=sehead0, vomit=sevom0, 
         blood_vom=sebldvom0, diarrhoea=sediar0, 
         dizzy=sediz0, breathing=sedifbth0, 
         swelling=sebdyswe0, rash=serash0, 
         urine_pain=sepnur0, weakness=seweak0, 
         nausea=senausea0, blood_stool=sebldst0)%>%
  mutate(year="2004")

BugotoKK <- para2004BG %>%
  dplyr::select(CID, sm0d1s1:sm3d3s2)

BugotoCCA <- para2004BG %>%
  dplyr::select(CID, CCA0:CCA3)

BugotoAll <- left_join(BugotoCCA, BugotoKK, by="CID")

Bug04KK <- loadkkdata(BugotoAll)
Bug04CCA <- getCCA(BugotoAll)

BugotoSE2004 <- para2004BG %>% 
  dplyr::select(CID, side_effects:blood_stool)
BugotoSE2004[BugotoSE2004==2]<-0

BugotoSymp2004 <- BugotoLV2004 %>% 
  dplyr::select(School, CID, pnurE0, abdpnE0, headE0, itchE0, diaE0, nauE0, bldstlE0)%>%
  rename(abdom_pain=abdpnE0, headache=headE0, diarrhoea=diaE0, 
         itching=itchE0, urine_pain=pnurE0, 
         nausea=nauE0, blood_stool=bldstlE0)
BugotoSymp2004[BugotoSymp2004==2]<-0

#### Run model: HMM ----

RunHMMBG <- ModelRun(N=nrow(Bug04CCA), Ti=3, R=6, KK=Bug04KK, CCA=Bug04CCA)

#### Extract posteriors ----

prevBG <- density(c(RunHMMBG$mcmc[[1]][,"prev"], RunHMMBG$mcmc[[2]][,"prev"]))

BLto4wkCLBG <- density(c(RunHMMBG$mcmc[[1]][,"clearance[2]"], RunHMMBG$mcmc[[2]][,"clearance[2]"]))
Fourwkto6mCLBG <- density(c(RunHMMBG$mcmc[[1]][,"clearance[3]"], RunHMMBG$mcmc[[2]][,"clearance[3]"]))

ReinfFourwkto6mBG <- density(c(RunHMMBG$mcmc[[1]][,"reinfec[3]"], RunHMMBG$mcmc[[2]][,"reinfec[3]"]))

rtnbBG <- density(c(RunHMMBG$mcmc[[1]][,"rtnb"], RunHMMBG$mcmc[[2]][,"rtnb"]))

kvBG <- density(c(RunHMMBG$mcmc[[1]][,"k"], RunHMMBG$mcmc[[2]][,"k"]))

interceptBG <- density(c(RunHMMBG$mcmc[[1]][,"intercept"], RunHMMBG$mcmc[[2]][,"intercept"]))

shapeBG <- density(c(RunHMMBG$mcmc[[1]][,"sh"], RunHMMBG$mcmc[[2]][,"sh"]))
rtBG <- density(c(RunHMMBG$mcmc[[1]][,"rt"], RunHMMBG$mcmc[[2]][,"rt"]))

#### Plots of models ----

# prevBG 
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0.7,1.01), ylim = c(0,1), xlab="Prev", ylab="Scaled Density", cex.axis=1)
lines(prevBG$x,prevBG$y/max(prevBG$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(prevBG$x), prevBG$x), c(rev(prevBG$y/max(prevBG$y)), rep(0,length(prevBG$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "prevBG.pdf", height = 6, width = 6)
dev.off()

# clearanceBG
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(-0.05,1), ylim = c(0,1), xlab="Clearance", ylab="Scaled Density", cex.axis=1)

lines(BLto4wkCLBG$x,BLto4wkCLBG$y/max(BLto4wkCLBG$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(BLto4wkCLBG$x), BLto4wkCLBG$x), c(rev(BLto4wkCLBG$y/max(BLto4wkCLBG$y)), rep(0,length(BLto4wkCLBG$y))), 
        col = adjustcolor("darkorange3", alpha=.3))

lines(Fourwkto6mCLBG$x,Fourwkto6mCLBG$y/max(Fourwkto6mCLBG$y), lwd = 2.5, col="blue")
polygon(c(rev(Fourwkto6mCLBG$x), Fourwkto6mCLBG$x), c(rev(Fourwkto6mCLBG$y/max(Fourwkto6mCLBG$y)), rep(0,length(Fourwkto6mCLBG$y))),
        col = adjustcolor("blue", alpha=.3))

legend("topright", c("clear BL-4wks", "clear 4wks-6mos"), 
       col=c("darkorange3", "blue"), 
       lty=1, bty="n", cex=0.75)

dev.copy(pdf, "clearance.pdf", height = 6, width = 6)
dev.off()

# reinfec
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0,1), ylim = c(0,1), xlab="Reinfection", ylab="Scaled Density", cex.axis=1)
lines(ReinfFourwkto6mBG$x,ReinfFourwkto6mBG$y/max(ReinfFourwkto6mBG$y), lwd = 2.5, col="blue")
polygon(c(rev(ReinfFourwkto6mBG$x), ReinfFourwkto6mBG$x), c(rev(ReinfFourwkto6mBG$y/max(ReinfFourwkto6mBG$y)), rep(0,length(ReinfFourwkto6mBG$y))),
        col = adjustcolor("blue", alpha=.3))

legend("topright", c("reinfect 4wks-6mos"), 
       col=c("blue"), 
       lty=1, bty="n", cex=0.75)

dev.copy(pdf, "reinfection.pdf", height = 6, width = 6)
dev.off()

# rtnbBG
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0,0.7), ylim = c(0,1), xlab="rtnb", ylab="Scaled Density", cex.axis=1)
lines(rtnbBG$x,rtnbBG$y/max(rtnbBG$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(rtnbBG$x), rtnbBG$x), c(rev(rtnbBG$y/max(rtnbBG$y)), rep(0,length(rtnbBG$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "rtnbBG.pdf", height = 6, width = 6)
dev.off()

#kvBG
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0,0.008), ylim = c(0,1), xlab="kv", ylab="Scaled Density", cex.axis=1)
lines(kvBG$x,kvBG$y/max(kvBG$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(kvBG$x), kvBG$x), c(rev(kvBG$y/max(kvBG$y)), rep(0,length(kvBG$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "kvBG.pdf", height = 6, width = 6)
dev.off()

# interceptBG
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(-1,10), ylim = c(0,1), xlab="intercept", ylab="Scaled Density", cex.axis=1)
lines(interceptBG$x,interceptBG$y/max(interceptBG$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(interceptBG$x), interceptBG$x), c(rev(interceptBG$y/max(interceptBG$y)), rep(0,length(interceptBG$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "interceptBG.pdf", height = 6, width = 6)
dev.off()

# shapeBG
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0,1.5), ylim = c(0,1), xlab="shape", ylab="Scaled Density", cex.axis=1)
lines(shapeBG$x,shapeBG$y/max(shapeBG$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(shapeBG$x), shapeBG$x), c(rev(shapeBG$y/max(shapeBG$y)), rep(0,length(shapeBG$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "shapeBG.pdf", height = 6, width = 6)
dev.off()

# rtBG
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
# where it says klogKKcca here, just put the object name for your posterior distribution
plot(NA,NA, xlim = c(0,0.017), ylim = c(0,1), xlab="rt", ylab="Scaled Density", cex.axis=1)
lines(rtBG$x,rtBG$y/max(rtBG$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(rtBG$x), rtBG$x), c(rev(rtBG$y/max(rtBG$y)), rep(0,length(rtBG$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "rtBG.pdf", height = 6, width = 6)
dev.off()

#### Extract clearance ----

# This is an estimation of each persons Clearance (cleared=1 not=0) at each time point in each iteration of the model (2 chains, 20,000 runs = 40,000 iterations)  
ClearanceBG <- as.data.frame(as.matrix(as.mcmc(RunHMMBG))) %>% 
  dplyr::select(`Clearance[1,1]`:`Clearance[94,2]`)

# the probability that someone cleared between a time step can then be calculated. 
ClearanceBG[nrow(ClearanceBG)+1,] <- colSums(ClearanceBG)/nrow(ClearanceBG)
ClearProb1BG <- as.data.frame(t(ClearanceBG[nrow(ClearanceBG),c(1:94)]))%>%
  rename(Clear=`40001`) %>%
  mutate(time="baseline to 4 wks") %>% 
  mutate(CID = BugotoAll$CID)

ClearProb2BG <- as.data.frame(t(ClearanceBG[nrow(ClearanceBG),c(95:ncol(ClearanceBG))])) %>%
  rename(Clear=`40001`) %>%
  mutate(time="4 weeks to 6 months") %>% 
  mutate(CID = BugotoAll$CID)

ClearProbsBG <- bind_rows(ClearProb1BG, ClearProb2BG)

#### Extract status ----

# This is an estimation of each persons status (infected=1 not=0) at each time point in each iteration of the model (2 chains, 20,000 runs = 40,000 iterations)
StatusBG <- as.data.frame(as.matrix(as.mcmc(RunHMMBG))) %>%
  dplyr::select(`Status[1,1]`:`Status[94,3]`)

StatusBG[nrow(StatusBG)+1,] <- (colSums(StatusBG)/nrow(StatusBG))
StatusT1BG <- as.data.frame(t(StatusBG[nrow(StatusBG),c(1:94)]))%>%
  rename(ProbInf=`40001`) %>%
  mutate(time="baseline")
StatusT2BG <- as.data.frame(t(StatusBG[nrow(StatusBG),c(95:188)]))%>%
  rename(ProbInf=`40001`) %>%
  mutate(time="four weeks")
StatusT3BG <- as.data.frame(t(StatusBG[nrow(StatusBG),c(189:ncol(StatusBG))]))%>%
  rename(ProbInf=`40001`) %>%
  mutate(time="six months")

StatusAllTBG <- bind_rows(StatusT1BG, StatusT2BG, StatusT3BG)

#### Extract the estimated true egg count for each individual ----

# this is just filled with 0s because as I said above, we make the assumption that if you are not infected you cannot have any eggs
tKK0BG <- as.data.frame(as.matrix(as.mcmc(RunHMMBG))) %>%
  dplyr::select(`tKK[1,1,1]`:`tKK[94,1,1]`,
                `tKK[1,1,2]`:`tKK[94,1,2]`,
                `tKK[1,1,3]`:`tKK[94,1,3]`) 

tKK1BG <- as.data.frame(as.matrix(as.mcmc(RunHMMBG))) %>%
  dplyr::select(`tKK[1,2,1]`:`tKK[94,2,1]`,
                `tKK[1,2,2]`:`tKK[94,2,2]`,
                `tKK[1,2,3]`:`tKK[94,2,3]` )

# make an empty matrix 
EstEggCtsBG <- matrix(NA, nrow = nrow(tKK0BG), ncol=ncol(tKK0BG))

# fill it depending on whether your status is infected or not. 
for(rows in 1:nrow(tKK0BG)){
  for(columns in 1:ncol(tKK0BG)){
    if(StatusBG[rows, columns]==1){
      EstEggCtsBG[rows, columns] <- tKK1BG[[rows, columns]]
    } else {
      EstEggCtsBG[rows, columns] <- 0 
    }
  }
}


options(scipen = 200) 


EstEggT1BG <- EstEggCtsBG[,c(1:94)]
EstEggT1BG <- round(EstEggT1BG)
EggT1meanBG <- as.data.frame(colMeans(EstEggT1BG)) %>% 
  rename(mean = `colMeans(EstEggT1BG)`) %>% 
  mutate(CID = BugotoAll$CID)

EstEggT1dfBG <- as.data.frame(EstEggT1BG)
EggT1varBG <- vector()
for (i in 1:ncol(EstEggT1dfBG)){
  EggT1varBG = c(EggT1varBG,var(EstEggT1dfBG[,i]))
}

EggT1vardfBG <- t(rbind(EggT1varBG))
EggT1minmaxBG <- as.data.frame(EggT1vardfBG) 
EggT1minmaxBG <- as.data.frame(EggT1minmaxBG) %>% 
  mutate(SD = sqrt(EggT1minmaxBG$EggT1varBG), mean = EggT1meanBG$mean, 
         max = mean+SD, min = mean-SD)
EggT1minmaxBG <- round(EggT1minmaxBG)
EggT1minmaxBG$min[which(EggT1minmaxBG$min<0)] <- 0


EstEggT2BG <- EstEggCtsBG[,c(95:188)]
EstEggT2BG <- round(EstEggT2BG)
EggT2meanBG <- as.data.frame(colMeans(EstEggT2BG)) %>% 
  rename(mean = `colMeans(EstEggT2BG)`) %>% 
  mutate(CID = BugotoAll$CID)

EstEggT2dfBG <- as.data.frame(EstEggT2BG)
EggT2varBG <- vector()
for (i in 1:ncol(EstEggT2dfBG)){
  EggT2varBG = c(EggT2varBG,var(EstEggT2dfBG[,i]))
}

EggT2vardfBG <- t(rbind(EggT2varBG))
EggT2minmaxBG <- as.data.frame(EggT2vardfBG) 
EggT2minmaxBG <- as.data.frame(EggT2minmaxBG) %>% 
  mutate(SD = sqrt(EggT2minmaxBG$EggT2varBG), mean = EggT2meanBG$mean, 
         max = mean+SD, min = mean-SD)
EggT2minmaxBG <- round(EggT2minmaxBG)
EggT2minmaxBG$min[which(EggT2minmaxBG$min<0)] <- 0


EstEggT3BG <- EstEggCtsBG[,c(189:282)]
EstEggT3BG <- round(EstEggT3BG)
EggT3meanBG <- as.data.frame(colMeans(EstEggT3BG)) %>% 
  rename(mean = `colMeans(EstEggT3BG)`) %>% 
  mutate(CID = BugotoAll$CID)

EstEggT3dfBG <- as.data.frame(EstEggT3BG)
EggT3varBG <- vector()
for (i in 1:ncol(EstEggT3dfBG)){
  EggT3varBG = c(EggT3varBG,var(EstEggT3dfBG[,i]))
}

EggT3vardfBG <- t(rbind(EggT3varBG))
EggT3minmaxBG <- as.data.frame(EggT3vardfBG) 
EggT3minmaxBG <- as.data.frame(EggT3minmaxBG) %>% 
  mutate(SD = sqrt(EggT3minmaxBG$EggT3varBG), mean = EggT3meanBG$mean, 
         max = mean+SD, min = mean-SD)
EggT3minmaxBG <- round(EggT3minmaxBG)
EggT3minmaxBG$min[which(EggT3minmaxBG$min<0)] <- 0

# Musubi ----

Musubi2004 <- read.csv("SE_hmm_data.csv") 
katokatz <- loadkkdata(Musubi2004) # there is a warning message here, don't worry about it 
KKIDs <- getKKChildIDs("SE_hmm_data.csv") 

ccadata <- getCCA(Musubi2004)
CCAIDs <- getCCAChildIDs("SE_hmm_data.csv") # Get CCAIDs

# check all the same kids are there
CID <- intersect(KKIDs,CCAIDs) 
katokatz <- katokatz[match(CID,KKIDs),,]
ccadata <- ccadata[match(CID,CCAIDs),]

#### Parasite data 2004 Musubi ----

# only loading 2004 because there is no CCA data for 05 or 06

para2004 <- read_csv("Baseline 2004 + six months.csv")%>%
  dplyr::select(School, Recruityr, CID, SEX, AGE, 
                CCA0, CCA2, CCA3,
                sm0d1s1:sm0d3s2,
                sm2d1s1: sm2d3s2,
                sm3d1s1:sm3d3s2,
                Height0,Weight0, Hb0, se0:sebldst0)%>%
  rename(height=Height0, weight=Weight0, 
         haemcount=Hb0, Age=AGE,
         side_effects=se0, abdom_pain=seabdpn0, 
         headache=sehead0, vomit=sevom0, 
         blood_vom=sebldvom0, diarrhoea=sediar0, 
         dizzy=sediz0, breathing=sedifbth0, 
         swelling=sebdyswe0, rash=serash0, 
         urine_pain=sepnur0, weakness=seweak0, 
         nausea=senausea0, blood_stool=sebldst0)%>%
  mutate(year="2004")%>%
  filter(!is.na(School))

MusubiKK <- para2004 %>%
  filter(School=="Musubi") %>%
  dplyr::select(CID, sm0d1s1:sm3d3s2)

MusubiCCA <- para2004 %>%
  filter(School=="Musubi") %>%
  dplyr::select(CID, CCA0:CCA3)

MusubiAll <- left_join(MusubiCCA, MusubiKK, by="CID")

Mus04KK <- loadkkdata(MusubiAll)
Mus04CCA <- getCCA(MusubiAll)

MusubiSE2004 <- para2004 %>% 
  filter(School=="Musubi") %>% 
  dplyr::select(CID, side_effects:blood_stool)
MusubiSE2004[MusubiSE2004==2]<-0

MusubiSymp2004 <- Musubi2004 %>% 
  dplyr::select(School, CID, pnurE0, abdpnE0, headE0, itchE0, diaE0, nauE0, bldstlE0)%>%
  filter(School=="Musubi") %>%
  rename(abdom_pain=abdpnE0, headache=headE0, diarrhoea=diaE0, 
         itching=itchE0, urine_pain=pnurE0, 
         nausea=nauE0, blood_stool=bldstlE0)
MusubiSymp2004[MusubiSymp2004==2]<-0

#### Run model: HMM ----

RunHMM <- ModelRun(N=nrow(Mus04CCA), Ti=3, R=6, KK=Mus04KK, CCA=Mus04CCA)

#### Extract posteriors ----

prev <- density(c(RunHMM$mcmc[[1]][,"prev"], RunHMM$mcmc[[2]][,"prev"]))

BLto4wkCL <- density(c(RunHMM$mcmc[[1]][,"clearance[2]"], RunHMM$mcmc[[2]][,"clearance[2]"]))
Fourwkto6mCL <- density(c(RunHMM$mcmc[[1]][,"clearance[3]"], RunHMM$mcmc[[2]][,"clearance[3]"]))

ReinfFourwkto6m <- density(c(RunHMM$mcmc[[1]][,"reinfec[3]"], RunHMM$mcmc[[2]][,"reinfec[3]"]))

rtnb <- density(c(RunHMM$mcmc[[1]][,"rtnb"], RunHMM$mcmc[[2]][,"rtnb"]))

kv <- density(c(RunHMM$mcmc[[1]][,"k"], RunHMM$mcmc[[2]][,"k"]))

intercept <- density(c(RunHMM$mcmc[[1]][,"intercept"], RunHMM$mcmc[[2]][,"intercept"]))

shape <- density(c(RunHMM$mcmc[[1]][,"sh"], RunHMM$mcmc[[2]][,"sh"]))
rt <- density(c(RunHMM$mcmc[[1]][,"rt"], RunHMM$mcmc[[2]][,"rt"]))

#### Plots of models ----

# prev
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0.7,1.01), ylim = c(0,1), xlab="Prev", ylab="Scaled Density", cex.axis=1)
lines(prev$x,prev$y/max(prev$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(prev$x), prev$x), c(rev(prev$y/max(prev$y)), rep(0,length(prev$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "prev.pdf", height = 6, width = 6)
dev.off()

# clearance
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(-0.05,1), ylim = c(0,1), xlab="Clearance", ylab="Scaled Density", cex.axis=1)

lines(BLto4wkCL$x,BLto4wkCL$y/max(BLto4wkCL$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(BLto4wkCL$x), BLto4wkCL$x), c(rev(BLto4wkCL$y/max(BLto4wkCL$y)), rep(0,length(BLto4wkCL$y))), 
        col = adjustcolor("darkorange3", alpha=.3))

lines(Fourwkto6mCL$x,Fourwkto6mCL$y/max(Fourwkto6mCL$y), lwd = 2.5, col="blue")
polygon(c(rev(Fourwkto6mCL$x), Fourwkto6mCL$x), c(rev(Fourwkto6mCL$y/max(Fourwkto6mCL$y)), rep(0,length(Fourwkto6mCL$y))),
        col = adjustcolor("blue", alpha=.3))

legend("topright", c("clear BL-4wks", "clear 4wks-6mos"), 
       col=c("darkorange3", "blue"), 
       lty=1, bty="n", cex=0.75)

dev.copy(pdf, "clearance.pdf", height = 6, width = 6)
dev.off()

# reinfec
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0,1.5), ylim = c(0,1), xlab="Reinfection", ylab="Scaled Density", cex.axis=1)
lines(ReinfFourwkto6m$x,ReinfFourwkto6m$y/max(ReinfFourwkto6m$y), lwd = 2.5, col="blue")
polygon(c(rev(ReinfFourwkto6m$x), ReinfFourwkto6m$x), c(rev(ReinfFourwkto6m$y/max(ReinfFourwkto6m$y)), rep(0,length(ReinfFourwkto6m$y))),
        col = adjustcolor("blue", alpha=.3))

legend("topright", c("reinfect 4wks-6mos"), 
       col=c("blue"), 
       lty=1, bty="n", cex=0.75)

dev.copy(pdf, "reinfection.pdf", height = 6, width = 6)
dev.off()

# rtnb
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0,1), ylim = c(0,1), xlab="rtnb", ylab="Scaled Density", cex.axis=1)
lines(rtnb$x,rtnb$y/max(rtnb$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(rtnb$x), rtnb$x), c(rev(rtnb$y/max(rtnb$y)), rep(0,length(rtnb$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "rtnb.pdf", height = 6, width = 6)
dev.off()

#kv
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0,0.006), ylim = c(0,1), xlab="kv", ylab="Scaled Density", cex.axis=1)
lines(kv$x,kv$y/max(kv$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(kv$x), kv$x), c(rev(kv$y/max(kv$y)), rep(0,length(kv$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "kv.pdf", height = 6, width = 6)
dev.off()

# intercept
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(-1,10), ylim = c(0,1), xlab="intercept", ylab="Scaled Density", cex.axis=1)
lines(intercept$x,intercept$y/max(intercept$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(intercept$x), intercept$x), c(rev(intercept$y/max(intercept$y)), rep(0,length(intercept$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "intercept.pdf", height = 6, width = 6)
dev.off()

# shape
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
plot(NA,NA, xlim = c(0,1.5), ylim = c(0,1), xlab="shape", ylab="Scaled Density", cex.axis=1)
lines(shape$x,shape$y/max(shape$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(shape$x), shape$x), c(rev(shape$y/max(shape$y)), rep(0,length(shape$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "shape.pdf", height = 6, width = 6)
dev.off()

# rt
dev.new(height = 6, width = 6)
par(mfrow=c(1,1))
par(mar=c(6,6,4,1),mgp=c(3,1,0))
par(fig = c(0,1,0,1))
# where it says klogKKcca here, just put the object name for your posterior distribution
plot(NA,NA, xlim = c(0,0.015), ylim = c(0,1), xlab="rt", ylab="Scaled Density", cex.axis=1)
lines(rt$x,rt$y/max(rt$y), lwd = 2.5, col="darkorange3")
polygon(c(rev(rt$x), rt$x), c(rev(rt$y/max(rt$y)), rep(0,length(rt$y))),
        col = adjustcolor("darkorange3", alpha=.3))
dev.copy(pdf, "rt.pdf", height = 6, width = 6)
dev.off()

#### Extract clearance ----
# This is an estimation of each persons Clearance (cleared=1 not=0) at each time point in each iteration of the model (2 chains, 20,000 runs = 40,000 iterations)  
Clearance <- as.data.frame(as.matrix(as.mcmc(RunHMM))) %>%
  dplyr::select(`Clearance[1,1]`:`Clearance[68,2]`)

# the probability that someone cleared between a time step can then be calculated. 
Clearance[nrow(Clearance)+1,] <- colSums(Clearance)/nrow(Clearance)
ClearProb1 <- as.data.frame(t(Clearance[nrow(Clearance),c(1:68)]))%>%
  rename(Clear=`40001`) %>%
  mutate(time="baseline to 4 wks") %>% 
  mutate(CID = MusubiAll$CID)

ClearProb2 <- as.data.frame(t(Clearance[nrow(Clearance),c(69:ncol(Clearance))])) %>%
  rename(Clear=`40001`) %>%
  mutate(time="4 weeks to 6 months") %>% 
  mutate(CID = MusubiAll$CID)

ClearProbs <- bind_rows(ClearProb1, ClearProb2)

#### Extract status ----

# This is an estimation of each persons status (infected=1 not=0) at each time point in each iteration of the model (2 chains, 20,000 runs = 40,000 iterations)
Status <- as.data.frame(as.matrix(as.mcmc(RunHMM))) %>%
  dplyr::select(`Status[1,1]`:`Status[68,3]`)

Status[nrow(Status)+1,] <- (colSums(Status)/nrow(Status))
StatusT1 <- as.data.frame(t(Status[nrow(Status),c(1:68)]))%>%
  rename(ProbInf=`40001`) %>%
  mutate(time="baseline")
StatusT2 <- as.data.frame(t(Status[nrow(Status),c(69:136)]))%>%
  rename(ProbInf=`40001`) %>%
  mutate(time="four weeks")
StatusT3 <- as.data.frame(t(Status[nrow(Status),c(137:ncol(Status))]))%>%
  rename(ProbInf=`40001`) %>%
  mutate(time="six months")

StatusAllT <- bind_rows(StatusT1, StatusT2, StatusT3)

#### Extract the estimated true egg count for each individual ----

tKK0 <- as.data.frame(as.matrix(as.mcmc(RunHMM))) %>%
  dplyr::select(`tKK[1,1,1]`:`tKK[68,1,1]`,
         `tKK[1,1,2]`:`tKK[68,1,2]`,
         `tKK[1,1,3]`:`tKK[68,1,3]`) 

tKK1 <- as.data.frame(as.matrix(as.mcmc(RunHMM))) %>%
  dplyr::select(`tKK[1,2,1]`:`tKK[68,2,1]`,
         `tKK[1,2,2]`:`tKK[68,2,2]`,
         `tKK[1,2,3]`:`tKK[68,2,3]` )

# make an empty matrix 
EstEggCts <- matrix(NA, nrow = nrow(tKK0), ncol=ncol(tKK0))

# fill it depending on whether your status is infected or not. 
for(rows in 1:nrow(tKK0)){
  for(columns in 1:ncol(tKK0)){
    if(Status[rows, columns]==1){
      EstEggCts[rows, columns] <- tKK1[[rows, columns]]
    } else {
      EstEggCts[rows, columns] <- 0 
    }
  }
}


options(scipen = 200) 


EstEggT1 <- EstEggCts[,c(1:68)]
EstEggT1 <- round(EstEggT1)
EggT1mean <- as.data.frame(colMeans(EstEggT1)) %>% 
  rename(mean = `colMeans(EstEggT1)`) %>% 
  mutate(CID = MusubiAll$CID)

EstEggT1df <- as.data.frame(EstEggT1)
EggT1var <- vector()
for (i in 1:ncol(EstEggT1df)){
  EggT1var = c(EggT1var,var(EstEggT1df[,i]))
}

EggT1vardf <- t(rbind(EggT1var))
EggT1minmax <- as.data.frame(EggT1vardf) 
EggT1minmax <- as.data.frame(EggT1minmax) %>% 
  mutate(SD = sqrt(EggT1minmax$EggT1var), mean = EggT1mean$mean, 
         max = mean+SD, min = mean-SD)
EggT1minmax <- round(EggT1minmax)
EggT1minmax$min[which(EggT1minmax$min<0)] <- 0


EstEggT2 <- EstEggCts[,c(69:136)]
EstEggT2 <- round(EstEggT2)
EggT2mean <- as.data.frame(colMeans(EstEggT2)) %>% 
  rename(mean = `colMeans(EstEggT2)`) %>% 
  mutate(CID = MusubiAll$CID)

EstEggT2df <- as.data.frame(EstEggT2)
EggT2var <- vector()
for (i in 1:ncol(EstEggT2df)){
  EggT2var = c(EggT2var,var(EstEggT2df[,i]))
}

EggT2vardf <- t(rbind(EggT2var))
EggT2minmax <- as.data.frame(EggT2vardf) 
EggT2minmax <- as.data.frame(EggT2minmax) %>% 
  mutate(SD = sqrt(EggT2minmax$EggT2var), mean = EggT2mean$mean, 
         max = mean+SD, min = mean-SD)
EggT2minmax <- round(EggT2minmax)
EggT2minmax$min[which(EggT2minmax$min<0)] <- 0


EstEggT3 <- EstEggCts[,c(137:204)]
EstEggT3 <- round(EstEggT3)
EggT3mean <- as.data.frame(colMeans(EstEggT3)) %>% 
  rename(mean = `colMeans(EstEggT3)`) %>% 
  mutate(CID = MusubiAll$CID)

EstEggT3df <- as.data.frame(EstEggT3)
EggT3var <- vector()
for (i in 1:ncol(EstEggT3df)){
  EggT3var = c(EggT3var,var(EstEggT3df[,i]))
}

EggT3vardf <- t(rbind(EggT3var))
EggT3minmax <- as.data.frame(EggT3vardf) 
EggT3minmax <- as.data.frame(EggT3minmax) %>% 
  mutate(SD = sqrt(EggT3minmax$EggT3var), mean = EggT3mean$mean, 
         max = mean+SD, min = mean-SD)
EggT3minmax <- round(EggT3minmax)
EggT3minmax$min[which(EggT3minmax$min<0)] <- 0

