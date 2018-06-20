
rm(list=ls())
library(tidyverse)

#merge outcomes with covariates

# setwd("U:/UCB-SuperLearner/Stunting rallies/")
setwd("U:/ucb-superlearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_temp_clean_covariates.rds")

#load outcomes
load("st_prev.rdata")
load("st_cuminc.rdata")
load("st_rec.rdata")


dim(prev)
dim(cuminc)
dim(rev)


colnames(prev)
colnames(cuminc)
colnames(rev)


head(prev)
head(cuminc)
head(rev)

#convert subjid to character for the merge with covariate dataset
cov$subjid <- as.character(cov$subjid)
prev$subjid <- as.character(prev$subjid)
cuminc$subjid <- as.character(cuminc$subjid)
rev$subjid <- as.character(rev$subjid)


null<-c("")


#------------------------------------
# Create cumulative incidence dataset
#------------------------------------

#merge in covariates
di <- left_join(cuminc, cov, by=c("studyid", "subjid", "country"))
dp <- left_join(prev, cov, by=c("studyid", "subjid", "country"))
dr <- left_join(rev, cov, by=c("studyid", "subjid", "country"))


dim(di)
dim(dp)
dim(dr)


table(di$studyid, !is.na(di$mage))
table(dp$studyid, !is.na(dp$mage))


di <- merge(cuminc, cov, by=c("studyid", "subjid", "country"), all.x=T, all.y=F)


head(di)
head(dp)
head(dr)
head(cov)


head(cov)
head(cuminc)


load("st_cuminc_rf.Rdata")
d1<-d
load("st_prev_rf.Rdata")
d2<-d

table(d1$studyid, d1$mage)
table(d2$studyid, d2$mage)


di %>% group_by(studyid, country) %>% summarise(mn =mean(!is.na(mage),na.rm=T), Y=sum(ever_stunted)) %>% filter(mn>0 & Y>5) %>% ungroup %>% summarise(n())
dp %>% group_by(studyid, country) %>% summarise(mn =mean(!is.na(mage),na.rm=T), Y=sum(stunted)) %>% filter(mn>0 & Y>5) %>% ungroup %>% summarise(n())


di %>% group_by(studyid, country, agecat) %>% summarise(mn =mean(!is.na(mage),na.rm=T), Y=sum(ever_stunted)) %>% filter(mn>0 & Y>5) %>% ungroup %>% group_by(agecat) %>% summarise(n())
dp %>% group_by(studyid, country, agecat) %>% summarise(mn =mean(!is.na(mage),na.rm=T), Y=sum(stunted)) %>% filter(mn>0 & Y>5) %>% ungroup %>% group_by(agecat) %>% summarise(n())



dp %>% group_by(studyid, country, agecat, mage) %>% 
  summarise(mn =mean(!is.na(mage),na.rm=T), Y=sum(stunted)) %>% ungroup() %>%
  group_by(studyid, country, agecat) %>% summarize(mn=mean(mn), Y=min(Y)) %>%
  filter(mn>0 & Y>5) %>% filter(agecat=="Birth") %>%
  as.data.frame()
  # Kenaba is here, but not in report
table(dp$mage[dp$studyid=="ki1101329-Keneba" & dp$agecat=="Birth" & dp$stunted==1])



#
di %>% group_by(studyid, country, agecat, mage) %>% 
  summarise(mn =mean(!is.na(mage),na.rm=T), Y=sum(ever_stunted)) %>% ungroup() %>%
  group_by(studyid, country, agecat) %>% summarize(mn=mean(mn), Y=min(Y)) %>%
  filter(mn>0 & Y>5) %>% group_by(agecat) %>% summarise(n())


dp %>% group_by(studyid, country, agecat, mage) %>% 
  summarise(mn =mean(!is.na(mage),na.rm=T), Y=sum(stunted)) %>% ungroup() %>%
  group_by(studyid, country, agecat) %>% summarize(mn=mean(mn), Y=min(Y)) %>%
  filter(mn>0 & Y>5) %>% group_by(agecat) %>% summarise(n())

  
  
  ungroup %>% group_by(agecat) %>% summarise(n())
