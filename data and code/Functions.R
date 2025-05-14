# packages ----

require(tidyverse)
require(lme4)
require(lmerTest)
require(runjags)
require(coda)
require(data.table)
library(fitdistrplus)
library(betareg)
require(DHARMa)
require(glmmTMB)
require(TMB)
library(reshape2)
library(ggalluvial)
require(ggsci)
require(colourpicker)
require(drc)
require(patchwork)
require(scales)
library(ggplot2)
library(stargazer)


# hidden markov model function ----

ModelRun <- function (N, Ti, R, KK, CCA) {
  ## Set seed ##
  ##this is so that they start on different values ##
  .RNG.seed <- function(chain)
    return( switch(chain, "1"= 1, "2"= 2) )
  .RNG.name <- function(chain)
    return( switch(chain, "1" = "base::Super-Duper", "2" = "base::Wichmann-Hill") )
  
  ## Initialize Status ##
  IniStatus <- rep(1,N)
  Clearance <- matrix(0,nrow=N,ncol=Ti-1)
  Reinfec <- matrix(1,nrow=N,ncol=Ti-1)
  prob <- array(rep(CCA,2),dim=c(N,Ti,2))
  sigma<- c(rep(NA,2),0.01)
  
  m <- "model {
  # Prior prevalence / clearance / reinfection #
  prev ~ dbeta(1,1)

  clearance[2] ~ dbeta(1,1)
  clearance[3] ~ dbeta(1,1)

  
  reinfec[2] <- 0
  reinfec[3] ~ dbeta(1,1)

  
  # Prior KKs 
  rtnb ~ dgamma(286.3235, 223.4650)
  sh ~ dgamma(83.88592,125.70331)
  rt1 ~ dbeta(47.13542,633.08366)
  rt <- rt1/(1-rt1)
  
  # Priors for the random walk #
  # accounting for the difference in length of time steps because there will be different amounts of variance
  sigma[3] ~ dgamma(0.001, 0.001) 
  sigma[2] <- sigma[3]*(4/20)
  sigma[1] <- 0 #not used but needed to initialize sigma
  
  # Priors CCA #
  k ~ dnorm(0.063291330, 1/0.009817479^2)
  intercept ~ dunif(0.0139464, 8.5045100)

  # MODEL 
  for(n in 1:N){	# run through pop

    IniStatus[n] ~ dbern(prev)
    Status[n,1] <- IniStatus[n]
    
    # these are the true baseline counts set outside of the time loop for the auto-regressive part 
    tKK[n,1,1] <- 0
    tKK[n,2,1] ~ dgamma(sh,rt) 
 
    prob[n,1,1] ~ dnorm(0,3.093451)
    prob[n,1,2] ~ dnorm(4 / (1 + exp(-k*(tKK[n,2,1]-intercept))),3.093451)

    CCA[n,1] ~ dround(prob[n,1,Status[n,1]+1],0)
    
    for (t in 1:Ti){ # run through timepoints
      lambda[n,t] <- tKK[n,Status[n,t]+1,t] # this needs to go here because each time the r loop reiterates it will replace this value
        for( r in 1:R){  # run through repeat measures
          KK[n,t,r] ~ dnegbin(rtnb/(lambda[n,t]+rtnb),rtnb) # generating the data with noise and then sampling from the dataset with the gamma sh/rt parameters?
      } # end or r loop
    } # end timestep 1 t loop
    
    for (t in 2:Ti){ # HMM component
      tKK[n,1,t] <- 0                                                                                                                                                                                                                                                                                               
      tKK[n,2,t] ~ dnorm(tKK[n,Status[n,t-1]+1,t-1],sigma[t])
      
      prob[n,t,1] ~ dnorm(0,3.093451)T(0,4)
      prob[n,t,2] ~ dnorm(4 / (1 + exp(-k*(tKK[n,2,t]-intercept))),3.093451)
       
      CCA[n,t] ~ dround(prob[n,t,Status[n,t]+1],0)
      
      Clearance[n,t-1] ~ dbern(clearance[t])
      Reinfec[n,t-1] ~ dbern(reinfec[t])

      Status[n,t] <- Status[n,t-1] * (1-Clearance[n,t-1]) + (1-Status[n,t-1]) * Reinfec[n,t-1] 
    }
    
  }  
  
  #inits# .RNG.seed, .RNG.name, IniStatus, Clearance, Reinfec, sigma, prob   
  #data# N, Ti, R, KK, CCA
  #monitor#  prev, clearance, reinfec, rtnb, k, intercept, sh, rt, Status, Clearance, tKK
}"
  
  # Run model #
  Results <- run.jags(m, burnin=10000, sample=20000, n.chains=2, jags.refresh = 1, method = 'parallel',
                      plots = F, silent.jags = F)
  return(Results)
}


# functions for loading data ----

loadkkdata <- function(data){
  
  preT <- data %>%
    dplyr::select(CID, sm0d1s1:sm0d3s2)
  colnames(preT) <- c("CID", "sm1", "sm2", "sm3", "sm4", "sm5", "sm6")
  preT$time <- "pre-treatment"
  
  fourweeks <- data %>%
    dplyr::select(CID, sm2d1s1:sm2d3s2)
  colnames(fourweeks) <- c("CID","sm1", "sm2", "sm3", "sm4", "sm5", "sm6")
  fourweeks$time <- "4wks"
  fourweeks$sm2 <- as.integer(fourweeks$sm2)

  sixmonths <- data %>%
    dplyr::select(CID, sm3d1s1:sm3d3s2)
  colnames(sixmonths) <- c("CID","sm1", "sm2", "sm3", "sm4", "sm5", "sm6")
  sixmonths$time <- "6mths"

  kkdata <- bind_rows(preT, fourweeks, sixmonths)
  
  children <- unique(kkdata$CID)
  
  dates <- c(na.omit(unique(kkdata$time)))
  
  KK <- array(NA,dim = c(length(children),3,6)) # length(children)
  for (i in 1:length(children)){
    KK[i,,] <- getKKChild(children[i],kkdata,dates)
  }
  
  return(KK)
  
}

getKKChild <- function(ID,kkdata,dates){
  dts <- subset(kkdata,CID==ID, select = c(CID,sm1,sm2, sm3, sm4, sm5, sm6, time))
  KKChild <- matrix(NA,nrow=length(dates),ncol=6)
  for (i in 1:length(dates)){
    KKChild[i,] <- getKKChildWeek(dates[i],dts)
  }
  return(KKChild)
}

getKKChildWeek <- function(weekN,dts){
  repeatsKK <- c(dts$sm1[which(dts$time==weekN)],dts$sm2[which(dts$time==weekN)], 
                 dts$sm3[which(dts$time==weekN)],dts$sm4[which(dts$time==weekN)], 
                 dts$sm5[which(dts$time==weekN)],dts$sm6[which(dts$time==weekN)])
  length(repeatsKK)<-6 #set max number of repeats!
  return(repeatsKK)
}

getKKChildIDs <- function(nameFile){
  dt <- read.csv(nameFile)
  return(sort(unique(dt$CID)))
}

getCCA <- function(all_data){
  
  dt <- all_data %>%
    dplyr::select(CCA0, CCA2, CCA3)
  
  dt$CCA0[which(dt$CCA0=="T")] <- 1
  dt$CCA0[which(dt$CCA0=="trace")] <- 1
  dt$CCA0[which(dt$CCA0==3)]<-4
  dt$CCA0[which(dt$CCA0==2)]<-3
  dt$CCA0[which(dt$CCA0==1)]<-2
  dt$CCA0[which(dt$CCA0==0.5)]<-1
  
  dt$CCA2[which(dt$CCA2=="T")] <- 1
  dt$CCA2[which(dt$CCA2=="trace")] <- 1
  dt$CCA2[which(dt$CCA2==3)]<-4
  dt$CCA2[which(dt$CCA2==2)]<-3
  dt$CCA2[which(dt$CCA2==1)]<-2
  dt$CCA2[which(dt$CCA2==0.5)]<-1
  
  dt$CCA3[which(dt$CCA3=="T")] <- 1
  dt$CCA3[which(dt$CCA3=="trace")] <- 1
  dt$CCA3[which(dt$CCA3==3)]<-4
  dt$CCA3[which(dt$CCA3==2)]<-3
  dt$CCA3[which(dt$CCA3==1)]<-2
  dt$CCA3[which(dt$CCA3==0.5)]<-1
  
  dt <- as.matrix(dt)
  
  return(dt)
  
}

getCCAChildIDs <- function(nameFile){
  dt <- read.csv(nameFile)
  return(sort(unique(dt$CID)))
}


# Color pick ----

col_SEX <- c("#E64B35", "#4BBAD3")
col_School <- c("#138787", "#445787", "#957683")
col_status <- c("#FF7019", "#1218FA")
col_4_status <- c("#C05E71", "#BFA5A8", "#A6ABBD", "#567CC3")
col_post_pre <- c("#FF4C7F", "#82CFFF")
col_2School <- c("#138787", "#445787")
