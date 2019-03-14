


rm(list=ls())
library(tidyverse)
load("U:/Data/Stunting/int_stunting_data.RData")

unique(d$studyid)

studies <- c("ki1000110-WASH-Bangladesh","ki1000111-WASH-Kenya",               
             "ki1000304b-SAS-FoodSuppl",
             "ki1033518-iLiNS-DYAD-G",   "ki1066203-TanzaniaChild2",
             "ki1112895-iLiNS-Zinc",        
             "ki1148112-iLiNS-DOSE",     "ki1148112-iLiNS-DYAD-M",   "ki1148112-LCNI-5",         "kiGH5241-JiVitA-3",        "kiGH5241-JiVitA-4")

d <- d[d$studyid %in% studies,]

#grab the final observation per trial
summary(d$agedays)
d <- d %>% group_by(studyid, subjid) %>% arrange(-agedays) %>% slice(1)

summary(d$agedays)

head(d)


#load and merge covariates
setwd("U:/ucb-superlearner/")
cov<-readRDS("Stunting rallies/FINAL_clean_covariates.rds")
cov <- cov %>% subset(., select = -c(tr))

dim(d)
d <- left_join(d, cov, by=c("studyid", "subjid", "country"))
dim(d)


save(d, file="sonali_thesis_dataset.Rdata")

