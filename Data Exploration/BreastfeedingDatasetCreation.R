


rm(list=ls())
library(tidyverse)
library(data.table)

setwd("U:/data/Breastfeeding datasets")


dir<-getwd()
list.files(dir)

bfzn <- readRDS("bf_bfzn.rds")    
ee <- readRDS("bf_ee.rds")
gmsn <- readRDS("bf_gmsn.rds")
jvt3 <- readRDS("bf_jvt3.rds") 
jvt4 <- readRDS("bf_jvt4.rds")
mled <- readRDS("bf_mled.rds") 
prbt <- readRDS("bf_prbt.rds")   

head(bfzn)    	
head(ee)    	
head(gmsn)    	
head(jvt3)    	
head(jvt4)    	
head(mled)    	
head(prbt)  

#rename or combine columns



#Combine raw data BF datasets
# - don't add in mled and prbt - they already have aggregate variables
bf_df <- bind_rows(bfzn, ee)
bf_df <- bind_rows(bf_df, gmsn)
bf_df <- bind_rows(bf_df, jvt3)
bf_df$visit <- as.character(bf_df$visit)
bf_df <- bind_rows(bf_df, jvt4)
#bf_df <- bind_rows(bf_df, mled)
#bf_df <- bind_rows(bf_df, prbt)


d <- bf_df



#Code variables
d$anmlkfl[is.na(d$anmlkfl)] <- 99  
d$pwmlkfl[is.na(d$pwmlkfl)] <- 99  
d$formlkfl[is.na(d$formlkfl)] <- 99  
d$bottlefl<- 99  #bottlefl not present in any of these studies
d$othfedfl[is.na(d$othfedfl)] <- 99  
d$h20fedfl[is.na(d$h20fedfl)] <- 99  

d$exbfedfl <- NA

#Exclusive breastfeeding
d$exclfeed_fl2 <- d$exbfedfl 
d$exclfeed_fl <- -(as.numeric((d$bfedfl==0 | (d$h20fedfl==1 |d$anmlkfl==1 | d$pwmlkfl==1 | d$formlkfl==1 | d$bottlefl==1 | d$othfedfl==1)))) + 1
d$exclfeed_fl[is.na(d$bfedfl) | (d$h20fedfl==99 & d$anmlkfl==99 & d$pwmlkfl==99 & d$formlkfl==99 &  d$bottlefl==99 & d$othfedfl==99)] <- NA

d$exclfeed_fl[is.na(d$exclfeed_fl)] <- 99
d$exclfeed_fl[d$exclfeed_fl!=1 & !is.na(d$exclfeed_fl2)] <- d$exclfeed_fl2[d$exclfeed_fl!=1 & !is.na(d$exclfeed_fl2)]
d$exclfeed_fl[d$exclfeed_fl==99] <- NA

table(d$exclfeed_fl)
table(d$studyid, d$exclfeed_fl)



#predominant breastfeeding
d$predfeed_fl <- -(as.numeric(d$bfedfl==0 | (d$anmlkfl==1 | d$pwmlkfl==1 | d$formlkfl==1 | d$bottlefl==1 | d$othfedfl==1))) + 1
d$predfeed_fl[is.na(d$bfedfl) | (d$anmlkfl==99 & d$pwmlkfl==99 & d$formlkfl==99 &  d$bottlefl==99 & d$othfedfl==99)] <- NA
table(d$predfeed_fl)
table(d$studyid,d$predfeed_fl)

#Summarize under 6 month  exclusive and predominant breastfeeding
bf_6mo<-d %>% #filter(!is.na(predfeed_fl)) %>% 
  filter(agedays < 30.4167*6) %>%
  group_by(studyid,  subjid) %>%
  mutate(predfeed6=as.numeric(mean(predfeed_fl==1, na.rm=T)==1),
         exclfeed6=as.numeric(mean(exclfeed_fl==1, na.rm=T)==1)) %>% #%>% summarize(predfeed6=mean(predfeed))
  slice(1)
table(bf_6mo$predfeed6)
table(bf_6mo$exclfeed6)
mean(bf_6mo$predfeed6, na.rm=T)
mean(bf_6mo$exclfeed6, na.rm=T)

table(bf_6mo$studyid, bf_6mo$predfeed6)
table(bf_6mo$studyid, bf_6mo$exclfeed6)

bf_6mo %>% group_by(studyid) %>% summarize(mean(predfeed6, na.rm=T), mean(exclfeed6, na.rm=T)) %>% as.data.frame()
bf_6mo_raw <- bf_6mo

#complementary feeding
# d$compfeed_fl <- as.numeric(d$bfedfl==1 & (d$sldfedfl==1 | d$weanfl==1))
# d$compfeed_fl[is.na(d$bfedfl) | (is.na(d$sldfedfl) & is.na(d$weanfl))] <- NA
# table(d$compfeed_fl)
# table(d$studyid,d$compfeed_fl)











#-------------------------------------------
#read full data csv file
#-------------------------------------------

d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T)

d <-d %>% filter(!is.na(AGEDAYS) & !is.na(WHZ)) %>%
  filter(WHZ > -5 & WHZ < 5) %>%
  filter(AGEDAYS <= 24 * 30.25)
gc()

colnames(d) <- tolower(colnames(d))
d <- d %>% subset(., select=c(studyid, country, subjid, agedays, bfedfl, exbfedfl,weanfl,  anmlkfl, pwmlkfl, formlkfl,bottlefl,h20fedfl,othfedfl,sldfedfl))


#Drop studies where we are grabbing breastfeeding info from the raw data
unique(d$studyid)
d <- d %>% filter(studyid!="ki0047075b-MAL-ED" & 
                    studyid!="ki1000109-EE" & 
                    studyid!="ki1113344-GMS-Nepal" & 
                    studyid!="kiGH5241-JiVitA-3" & 
                    studyid!="kiGH5241-JiVitA-4" & 
                    studyid!="ki1112895-Burkina Faso Zn" & 
                    studyid!="ki1119695-PROBIT")

#Code variables
d$anmlkfl[is.na(d$anmlkfl)] <- 99  
d$pwmlkfl[is.na(d$pwmlkfl)] <- 99  
d$formlkfl[is.na(d$formlkfl)] <- 99  
d$bottlefl[is.na(d$bottlefl)] <- 99  
d$othfedfl[is.na(d$othfedfl)] <- 99  
d$h20fedfl[is.na(d$h20fedfl)] <- 99  

#Exclusive breastfeeding
table(d$studyid, !is.na(d$exbfedfl) | !is.na(d$bfedfl) & !is.na(d$h20fedfl) & !is.na(d$anmlkfl) & !is.na(d$pwmlkfl) & !is.na(d$bfedfl) & !is.na(d$formlkfl) & !is.na(d$bottlefl) & !is.na(d$othfedfl))
table(d$studyid, !is.na(d$exbfedfl) | (!is.na(d$bfedfl) & (!is.na(d$h20fedfl) | !is.na(d$anmlkfl) | !is.na(d$pwmlkfl) | !is.na(d$bfedfl) | !is.na(d$formlkfl) | !is.na(d$bottlefl) | !is.na(d$othfedfl))))

table(!is.na(d$exbfedfl) | !is.na(d$bfedfl) & !is.na(d$h20fedfl) & !is.na(d$anmlkfl) & !is.na(d$pwmlkfl) & !is.na(d$bfedfl) & !is.na(d$formlkfl) & !is.na(d$bottlefl) & !is.na(d$othfedfl))
table(!is.na(d$exbfedfl) | (!is.na(d$bfedfl) & (!is.na(d$h20fedfl) | !is.na(d$anmlkfl) | !is.na(d$pwmlkfl) | !is.na(d$bfedfl) | !is.na(d$formlkfl) | !is.na(d$bottlefl) | !is.na(d$othfedfl))))

d$exclfeed_fl2 <- d$exbfedfl 
d$exclfeed_fl <- -(as.numeric((d$bfedfl==0 | (d$h20fedfl==1 |d$anmlkfl==1 | d$pwmlkfl==1 | d$formlkfl==1 | d$bottlefl==1 | d$othfedfl==1)))) + 1
d$exclfeed_fl[is.na(d$bfedfl) | (d$h20fedfl==99 & d$anmlkfl==99 & d$pwmlkfl==99 & d$formlkfl==99 &  d$bottlefl==99 & d$othfedfl==99)] <- NA

d$exclfeed_fl[is.na(d$exclfeed_fl)] <- 99
d$exclfeed_fl[d$exclfeed_fl!=1 & !is.na(d$exclfeed_fl2)] <- d$exclfeed_fl2[d$exclfeed_fl!=1 & !is.na(d$exclfeed_fl2)]
d$exclfeed_fl[d$exclfeed_fl==99] <- NA

table(d$exclfeed_fl)
table(d$studyid, d$exclfeed_fl)



#predominant breastfeeding

d$predfeed_fl <- -(as.numeric(d$bfedfl==0 | (d$anmlkfl==1 | d$pwmlkfl==1 | d$formlkfl==1 | d$bottlefl==1 | d$othfedfl==1))) + 1
d$predfeed_fl[is.na(d$bfedfl) | (d$anmlkfl==99 & d$pwmlkfl==99 & d$formlkfl==99 &  d$bottlefl==99 & d$othfedfl==99)] <- NA
table(d$predfeed_fl)
table(d$studyid,d$predfeed_fl)

#Summarize under 6 month  exclusive and predominant breastfeeding
bf_6mo<-d %>% #filter(!is.na(predfeed_fl)) %>% 
  filter(agedays < 30.4167*6) %>%
  group_by(studyid,  subjid) %>%
  mutate(predfeed6=as.numeric(mean(predfeed_fl==1, na.rm=T)==1),
         exclfeed6=as.numeric(mean(exclfeed_fl==1, na.rm=T)==1)) %>% #%>% summarize(predfeed6=mean(predfeed))
  slice(1)
table(bf_6mo$predfeed6)
table(bf_6mo$exclfeed6)
mean(bf_6mo$predfeed6, na.rm=T)
mean(bf_6mo$exclfeed6, na.rm=T)

table(bf_6mo$studyid, bf_6mo$predfeed6)
table(bf_6mo$studyid, bf_6mo$exclfeed6)

bf_6mo %>% group_by(studyid) %>% summarize(mean(predfeed6, na.rm=T), mean(exclfeed6, na.rm=T)) %>% as.data.frame()

#complementary feeding
d$compfeed_fl <- as.numeric(d$bfedfl==1 & (d$sldfedfl==1 | d$weanfl==1))
d$compfeed_fl[is.na(d$bfedfl) | (is.na(d$sldfedfl) & is.na(d$weanfl))] <- NA
table(d$compfeed_fl)
table(d$studyid,d$compfeed_fl)




#merge in raw data BF measurements
bf_6mo$subjid <- as.character(bf_6mo$subjid)
bf_6mo_raw$subjid <- as.character(bf_6mo_raw$subjid)

bf_6mo <- bind_rows(bf_6mo, bf_6mo_raw)




colnames(bf_6mo)
bf_6mo <- subset(bf_6mo, select = c(studyid, country, subjid, predfeed6, exclfeed6))



#Merge in mal-ed and probit measures

#prbt
head(prbt)

table(prbt$exbf6)
table(prbt$bf6)

table(prbt$exbf1==1 & prbt$exbf2==1 & prbt$exbf3==1 & prbt$exbf6==1)

prbt$exclfeed6 <- as.numeric(prbt$exbf1==1 & prbt$exbf2==1 & prbt$exbf3==1 & prbt$exbf6==1)
prbt$exclfeed6[is.na(prbt$exbf1) & is.na(prbt$exbf2) & is.na(prbt$exbf3) & is.na(prbt$exbf6)] <- NA

prbt <- prbt %>% subset(., select=c(subjid, exclfeed6))
prbt$studyid <- "ki1119695-PROBIT"

#mled
head(mled)

#cumexc: Cumulative days of exclusive breastfeeding from birth. This is a cumulative sum of the variable "excrun", i.e., number of days, to date from birth, that the child was reported to be exclusively breastfed. This has been filled in between birth and minage with EBF if child began breastfeeding at all (cafbegbf~=.). Exclusive breastfeeding prevalence can be calculated by dividing cumexc / age (or days of follow up). 
#cumpred: Cumulative days of predominant breastfeeding (cumulative sum of predrun, i.e., number of days from birth, to date, that the child was reported to be predominantly breastfed).
#cumpart: Cumulative days of partial breastfeeding (cumulative sum of partrun, ie, number of days from birth, to date, that the child was reported to be partially breastfed).
#cumnobf: Cumulative days of no breastfeeding (cumulative sum of nobfrun, ie, number of days from birth, to date, that the child was reported to not be breastfed).

mled <- mled %>% filter(visit=="Month 6") %>%
  mutate(predfeed6=as.numeric((cumexc>0 | cumpred>0) & cumpart==0 & cumnobf==0),
         exclfeed6=as.numeric(cumexc>0 & cumpred==0 & cumpart==0 & cumnobf==0))
table(mled$predfeed6)
table(mled$exclfeed6)


mled <- mled %>% subset(., select=c(studyid, subjid, country, predfeed6, exclfeed6))



prbt$subjid <- as.character(prbt$subjid)
mled$subjid <- as.character(mled$subjid)
bf_6mo <- bind_rows(bf_6mo, prbt)
bf_6mo <- bind_rows(bf_6mo, mled)

table(bf_6mo$exclfeed6)

#Create predominant or exclusive breastfeeding category

bf_6mo$exclfeed6[is.na(bf_6mo$exclfeed6)] <- 99
bf_6mo$predfeed6[is.na(bf_6mo$predfeed6)] <- 99

bf_6mo$predexfd6 <- as.numeric(bf_6mo$exclfeed6==1 | bf_6mo$predfeed6==1)
bf_6mo$predexfd6[bf_6mo$exclfeed6==99 & bf_6mo$predfeed6==99] <- NA

table(bf_6mo$predexfd6)
table(bf_6mo$studyid, bf_6mo$predexfd6)

save(bf_6mo, file="U:/data/Raw Data Cleaning/BF_dataset.Rdata")




