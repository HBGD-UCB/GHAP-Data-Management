
rm(list=ls())
library(tidyverse)

#merge outcomes with covariates

setwd("U:/UCB-SuperLearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_temp_clean_covariates.rds")

#load outcomes
load("st_prev.rdata")
load("st_cuminc.rdata")


dim(prev)
dim(cuminc)


colnames(prev)
colnames(cuminc)


head(prev)
head(cuminc)

# cuminc <- cuminc %>% rename(Y=ever_stunted)
# 
# 
# #reshape to wide
# cuminc$ever_stunted <-NA
# cuminc$ever_stunted[cuminc$agecat=="3 months"] <-3
# cuminc$ever_stunted[cuminc$agecat=="6 months"] <-6
# cuminc$ever_stunted[cuminc$agecat=="12 months"] <-12
# cuminc$ever_stunted[cuminc$agecat=="18 months"] <-18
# cuminc$ever_stunted[cuminc$agecat=="24 months"] <-24
# cuminc <- cuminc %>% subset(., select = -c(agecat))
# 
# cuminc_w<-spread(cuminc, key = ever_stunted, value = Y, sep="_")
# head(cuminc_w)
# 
# #merge in covariates
# d <- left_join(cuminc_w, cov, by=c("studyid", "subjid"))
# head(d)

#merge in covariates
d <- left_join(cuminc, cov, by=c("studyid", "subjid"))
head(d)


#Vector of outcome names
Y<-c("ever_stunted")

#Vector of risk factor names
A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart")

#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")


save(d, Y, A,V, file="st_cuminc_rf.Rdata")