


rm(list=ls())
library(tidyverse)
library(data.table)
library(xlsx)
library(haven)


source("U:/GHAP-Data-Management/HBGDki_functions.R")


setwd("U:/data")

#read csv file
d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T)


#Read rds file
#d<- readRDS("FINAL.rds")



gc()

colnames(d) <- tolower(colnames(d))
colnames(d)

#Drop un-needed columns
d <- d %>% subset(., select=c(studyid, subjid, agedays, diarfl,diarfl_r, dur_r))


#Drop studies Vishak added to data product that don't meet inclusion criteria
dim(d)
d <- d[d$studyid!="ki1000301-DIVIDS" & d$studyid!="ki1055867-WomenFirst" & d$studyid!="ki1135782-INCAP",]
dim(d)

#Replace diar flag with 1-day recall 
table(dfull$studyid, !is.na(dfull$diarfl))
d$diarfl[is.na(d$diarfl) & !is.na(d$diarfl_r) & d$dur_r==1] <- d$diarfl_r[is.na(d$diarfl) & !is.na(d$diarfl_r) & d$dur_r==1]
table(dfull$studyid, !is.na(dfull$diarfl))


#drop unneeded variables
d <- d %>% subset(., select= -c(diarfl_r, dur_r))

#Look for unrealistic diarrhea prevalences
d %>% group_by(studyid) %>% summarize(diar_prev=round(mean(diarfl, na.rm=T)*100,1)) %>% as.data.frame()


#Check how much diarrhea
table(d$studyid, d$diarfl)

#Check how much diarrhea is missing
tab<-table(d$studyid, !is.na(d$diarfl))
tab[,2]/(tab[,1] + tab[,2]) * 100


dfull <- d







#aga khan

d <- read_sas("U:/data/AgaKhanUniv/raw/childmorbidityandimmunization.sas7bdat")
head(d)
d$subjido <- gsub("C-Y-C-","",d$frmid)
d$subjido <- gsub("I-Y-C-","",d$subjido)

d$visitnum <- as.numeric(d$visit_)
d$agedays<- round(d$age*30.42)

#a1: During the last 24 hours, has <child> have more than 3 liquid stools (diarrhea)
table(d$a1)

akup<-readRDS("U:/data/akup.rds")
colnames(akup) <- tolower(colnames(akup))
akup$visitnum <- as.numeric(akup$visitnum )
head(akup)

akup <- left_join(akup, d, by=c("subjido","visitnum"))
table(akup$a1)
table(akup$diarfl, akup$a1)

table(d$visitnum)
table(akup$visitnum)

table(d$subjido)
table(akup$subjido)

#There isn't enough info to merge diarrhea with outcome dataset, but could append as its own rows with its own ages
df_akup <- dfull %>% filter(studyid=="ki1000125-AgaKhanUniv")
table(df_akup$diarfl)
table(akup$diarfl)
table(akup$a1)

akup <- akup %>% subset(., select = c(subjid, agedays.x, studyid, a1)) %>% 
                 rename(agedays = agedays.x, diarfl2 = a1)

dfull$diarfl[dfull$studyid=="ki1000125-AgaKhanUniv"] <- NA

akup$subjid <- as.numeric(akup$subjid)
dfull$subjid <- as.numeric(dfull$subjid)
dfull <- left_join(dfull , akup, by=c("studyid", "subjid", "agedays"))
table(akup$diarfl2)
table(dfull$diarfl2)
dfull$diarfl[!is.na(dfull$diarfl2)] <- dfull$diarfl2[!is.na(dfull$diarfl2)]
dfull<-dfull[,1:4]



#Ilins Dose
d <- read_sas("U:/data/iLiNS-DOSE/raw/morbid18tab.sas7bdat")
head(d)

table(d$HomNumberStool)
d$HomNumberStool[d$HomNumberStool==88] <- NA
d$HomNumberStool[d$HomNumberStool==99] <- NA
table(d$HomNumberStool>2) #diarrhea defined by 3 or more loose stools


ilnd<-readRDS("U:/data/ilnd.rds")
colnames(ilnd) <- tolower(colnames(ilnd))
#ilnd$visitnum <- as.numeric(ilnd$visitnum )
head(ilnd)

table(ilnd$diarfl)

#Analysis dataset looks good, just merge in to final dataset
ilnd <- ilnd %>% subset(., select = c(studyid, country, subjid, agedays, diarfl))
head(ilnd)

ilnd$diarfl <- as.numeric(ilnd$diarfl)
dfull <- dfull %>% filter(studyid!="ki1148112-iLiNS-DOSE")
dfull <- bind_rows(dfull, ilnd)
table(dfull$studyid, dfull$diarfl)
table(ilnd$diarfl)





#Tanzania child
d <- read_sas("U:/data/TanzaniaChild2/import/childnurse.sas7bdat")
head(d)
colnames(d) <- tolower(colnames(d))

table(d$cndt) #diarrhea today
d$cndt <- d$cndt - 1
mean(d$cndt, na.rm=T)

table(d$cchadm) #visit number

d <- d %>% rename(subjid=idno2, visitnum=cchadm, diarfl=cndt) %>% mutate(subjid=as.numeric(subjid)) %>% select(subjid, visitnum, diarfl)

tzc2<-readRDS("U:/data/tzc2.rds")
colnames(tzc2) <- tolower(colnames(tzc2))
tzc2<- tzc2 %>% select(studyid, subjid, visitnum, country, agedays)
head(tzc2)

table(tzc2$visitnum)
tzc2$visitnum <- as.numeric(tzc2$visitnum)

tzc2 <- left_join(tzc2, d, by=c("subjid","visitnum"))
table(tzc2$diarfl)

tzc2 <- tzc2 %>% subset(., select = c(studyid, country, subjid, agedays, diarfl))
head(tzc2)

tzc2$diarfl <- as.numeric(tzc2$diarfl)
dfull <- dfull %>% filter(studyid!="ki1066203-TanzaniaChild2")
dfull <- bind_rows(dfull, tzc2)
table(dfull$studyid, dfull$diarfl)
table(tzc2$diarfl)

diar_df<-dfull[,1:4]


res <-diar_df %>% 
  filter(agedays < 30.4167*6) %>% filter(!is.na(diarfl)) %>%
  group_by(studyid, subjid) %>%
  summarise(n=n(), prev=mean(diarfl, na.rm=T)) %>% 
  ungroup() %>%
  group_by(studyid) %>%
  summarise(num_obs=sum(n), mean_obs=mean(n), mean_prev=mean(prev) *100) %>%
  as.data.frame()
knitr::kable(res, digits=1)

res2 <-diar_df %>% 
  filter(agedays < 30.4167*24) %>% filter(!is.na(diarfl)) %>%
  group_by(studyid, subjid) %>%
  summarise(n=n(), prev=mean(diarfl, na.rm=T)) %>% 
  ungroup() %>%
  group_by(studyid) %>%
  summarise(num_ons=sum(n), mean_obs=mean(n), mean_prev=mean(prev) *100) %>%
  as.data.frame()
knitr::kable(res2, digits=1)


#Drop studies without correct survellance information or with too few measurements (<100)
diar_df <- diar_df %>% 
  #Inaccurate non-case data
  filter(studyid!="ki1000108-CMC-V-BCS-2002" &
                                studyid!="ki1000108-IRC" &
                                studyid!="ki1112895-Burkina Faso Zn" &
                                studyid!="ki1113344-GMS-Nepal" &
                                studyid!="ki1148112-iLiNS-DYAD-M ")# %>%
  #Too few observations
  # filter(studyid!="ki1148112-iLiNS-DOSE " &
  #          studyid!="ki1148112-LCNI-5" &
  #          studyid!="ki1000125-AgaKhanUniv" &
  #          studyid!="ki1000304-EU" &
  #          studyid!="ki1148112-iLiNS-DOSE" &
  #          studyid!="ki1148112-iLiNS-DYAD-M" &
  #          studyid!="ki1000304-VITAMIN-A" &
  #          studyid!="ki1000304-ZnMort" &
  #          studyid!="ki1000304b-SAS-CompFeed" &
  #          studyid!="ki1066203-TanzaniaChild2" &
  #          studyid!="ki1126311-ZVITAMBO" )



#Summarize under 6 month  diarrhea
diar_6mo <- diar_df %>% #filter(!is.na(predfeed_fl)) %>% 
  filter(agedays < 30.4167*6) %>%
  group_by(studyid,  subjid) %>%
  mutate(n=n(), perdiar6=as.numeric(mean(diarfl, na.rm=T))) %>%
  ungroup() %>% group_by(studyid) %>% mutate(meanN=mean(n)) %>% filter(meanN >= 100) %>% #Set as NA if less than 100 obs under 6 months
  ungroup() %>% group_by(studyid,subjid) %>% slice(1) %>% subset(., select = -c(n, meanN, agedays, diarfl))
summary(diar_6mo$perdiar6)




#Summarize 0-24 month  diarrhea
diar_24mo <- diar_df %>% #filter(!is.na(predfeed_fl)) %>% 
  filter(agedays < 30.4167*24) %>%
  group_by(studyid,  subjid) %>%
  mutate(n=n(), perdiar24=as.numeric(mean(diarfl, na.rm=T))) %>%
  ungroup() %>% group_by(studyid) %>% mutate(meanN=mean(n)) %>% filter(meanN >= 100) %>% #Set as NA if less than 100 obs under 24 months
  ungroup() %>% group_by(studyid,subjid) %>% slice(1) %>% subset(., select = -c(n, meanN, agedays, diarfl))
summary(diar_24mo$perdiar24)

# head(diar_df)
# diar_df <- left_join(diar_df, diar_6mo, by=c("studyid","subjid"))
# diar_df <- left_join(diar_df, diar_24mo, by=c("studyid","subjid"))

diar <- merge(diar_6mo, diar_24mo, by=c("studyid","subjid"), all.x = T, all.y = T)

save(diar, file="U:/data/Raw Data Cleaning/rawdiar_df.Rdata")

table(diar$studyid)
summary(diar$perdiar6)
summary(diar_6mo$perdiar6)

summary(diar$perdiar24)



