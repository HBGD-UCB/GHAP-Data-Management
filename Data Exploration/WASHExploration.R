




rm(list=ls())
library(tidyverse)
library(data.table)
library(xlsx)

source("U:/GHAP-Data-Management/HBGDki_functions.R")


setwd("U:/data")

#read csv file
d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T)

#Read rds file
#d<- readRDS("FINAL.rds")


d <-d %>% filter(!is.na(AGEDAYS) & !is.na(WHZ)) %>%
  filter(WHZ > -5 & WHZ < 5) %>% group_by(STUDYID, COUNTRY, SUBJID) %>%
  arrange(AGEDAYS) %>%
  slice(1)
gc()



#Drop un-needed columns
d <- subset(d, select= -c(AGEIMPFL, WTKG,HTCM,LENCM,WAZ,     
                          BAZ,HCAZ,MUAZ,SITEID,  REGION,
                          REGCTRY,  REGCTYP,  CITYTOWN, LATITUDE, LONGITUD, CLUSTID, 
                          HHID, BRTHYR, INCTOTU, CAUSEDTH))

colnames(d) <- tolower(colnames(d))
colnames(d)


gc()
d$subjid <- as.character(d$subjid)



dfull<-as.data.frame(d)



#--------------------------------------------------------------------------
# WASH and HH variables
#--------------------------------------------------------------------------
d <- dfull %>% subset(., select=c(studyid, country, subjid, agedays, impsan,  soap,    safeh2o, trth2o,  cleanck,
                                  impfloor,h2otime, nrooms,  nhh,     nchldlt5,chicken, cow,     cattle,  ses,     inctot ,   hfoodsec))


#--------------------------------------------------------------------------
#Improved sanitation
#--------------------------------------------------------------------------

table(d$studyid, d$impsan)
#compare to wash benefits
wsb <- readRDS("U:/data/wsb.rds")
table(wsb$SANITATN)
colnames(wsb) <- tolower(colnames(wsb))
wsb$subjid <- as.character(wsb$subjid)

d2 <- d %>% filter(studyid=="ki1000110-WASH-Bangladesh") %>% subset(., select=c(subjid, impsan))
table(d2$impsan)

wsb <- left_join(wsb, d2, by="subjid")
table(wsb$impsan)
table(wsb$sanitatn, wsb$impsan)


library(haven)
enrol<-read_sas("U:/data/WASH-Bangladesh/raw/washbenrol.sas7bdat")
head(data.frame(enrol))


#kenya
wsk <- readRDS("U:/data/wsk.rds")
table(wsk$SANITATN)
colnames(wsk) <- tolower(colnames(wsk))
wsk$subjid <- as.character(wsk$subjid)

d2 <- d %>% filter(studyid=="ki1000111-WASH-Kenya") %>% subset(., select=c(subjid, impsan))
table(d2$impsan)

enrol<-read_sas("U:/git/hbgd/ki1000111/WASH-BK/raw/enrol.sas7bdat")
head(data.frame(enrol))


uptake<-read_sas("U:/git/hbgd/ki1000111/WASH-BK/raw/uptake.sas7bdat")
head(data.frame(uptake))
table(uptake$IMPR_LAT)

#--------------------------------------------------------------------------
#Safe water
#--------------------------------------------------------------------------

table(d$studyid, d$safeh2o)

#compare to jivita 3
jvt3 <- readRDS("U:/data/jvt3.rds")
table(jvt3$H2OSRCP)
table(jvt3$H2OSRCB)

#compare to jivita 4
jvt4 <- readRDS("U:/data/jvt4.rds")
table(jvt4$H2OSRCP)
table(jvt4$H2OSRC)

#--------------------------------------------------------------------------
#treats water
#--------------------------------------------------------------------------
table(d$studyid, d$trth2o)

#--------------------------------------------------------------------------
#clean cook
#--------------------------------------------------------------------------
table(d$studyid, d$cleanck)

#--------------------------------------------------------------------------
#improved floor
#--------------------------------------------------------------------------

table(d$studyid, d$impfloor)

#compare to IRC
irc <- readRDS("U:/data/irc.rds")
table(irc$FLOOR)


#compare to CMC
cmc <- readRDS("U:/data/cmc.rds")
table(cmc$FLOOR)

