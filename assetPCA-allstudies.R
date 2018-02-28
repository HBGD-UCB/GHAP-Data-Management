

#------------------------------------------------------
# Author: Andrew Mertens
# amertens@berkeley.edu
#
# Calculate household wealth score from the first
# principal component of household asset ownership
# indicators
#-----------------------------------------------------





#------------------------------------
# Household asset PCA
#------------------------------------

rm(list=ls())
library(dplyr)
library(tidyr)
library(SuperLearner)
library(washb)
library(tmle)
library(caret)

#Open log
sink("U:/Perminant files/Logs/assetPCA-allstudies.txt")



#Identify studies measuring household assets 
setwd("U:/Perminant files/R scripts/")
source("HBGDki_function.R")

setwd("U:/data")


load("allGHAPstudies.Rdata")
colnames(d)


dim(d)
d<- d %>% subset(select=c(WHZ, shortid, STUDYID,  SUBJID, SEX, AGEDAYS, region, BICYCLE, CAR, CART, COOKFUEL, FRIG, MCYCLE, PHONE, RADIO, TV, WASHMAC, MOBILE, CHAIR, WATCH, SOFA, FAN, TABLE)) %>%
  subset(!is.na(BICYCLE)|!is.na(CAR)|!is.na(CART)|!is.na(COOKFUEL)|!is.na(FRIG)|!is.na(MCYCLE)|!is.na(PHONE)|!is.na(RADIO)|!is.na(TV)|!is.na(WASHMAC)|!is.na(MOBILE)|!is.na(CHAIR)|
           !is.na(WATCH)|!is.na(SOFA)|!is.na(FAN)|!is.na(TABLE)) %>% 
  filter(WHZ>-5 & WHZ <5)
dim(d)
table(d$STUDYID)
unique(d$shortid)


#---------
# akup
#---------
study<-"akup"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
#varlist<-colnames(d)[ -1 + c(53:57,59:60,63:66,68:69)] #Not converging
varlist<-colnames(d)[ -1 + c(53:57,59:60,63:66)]
d<-assetPCA(d, varlist, reorder=T)
table(d$HHwealth_quart)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# bngr
#---------
study<-"bngr"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(31:34,37,42,44:46,49,50)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# cmc
#---------
study<-"cmc"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(49:52,57:60)]
d<-assetPCA(d, varlist, reorder=F)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )


#---------
# cntt 
#---------
study<-"cntt"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(30,31,34,38,41)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# dvds
#---------
study<-"dvds"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(36:41,45,50:56,58)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# eegg- Note: only 1 asset
#---------
# study<-"eegg"
# d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
# colnames(d)
# varlist<-colnames(d)[c()]
# d<-assetPCA(d, varlist, reorder=F)
# saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# eczn
#---------
study<-"eczn"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(80,81,83,87:89)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

# #---------
# # gems
# #---------
# study<-"gems"
# d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
# colnames(d)
# varlist<-colnames(d)[ -1 + c(32:36,38,41,44,52,57,58,63)]
# d<-assetPCA(d, varlist, reorder=F)
# saveRDS(d, file=paste0(study, '.HHwealth.rds') )
# 
# #---------
# # gmsa
# #---------
# study<-"gmsa"
# d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
# colnames(d)
# varlist<-colnames(d)[ -1 + c(32:36,41,44,57,58,63)]
# d<-assetPCA(d, varlist, reorder=T)
# saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# gmsn
#---------
study<-"gmsn"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(67:70,72,77:79,82:88)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# ilnd
#---------
study<-"ilnd"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(318:328,331:334,350:354,358:360,363:368,370,371,373,375)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# ildm
#---------
study<-"ildm"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(420:423,427:439,442:445,460:464,470,471,473:475,478:481)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# irc
#---------
study<-"irc"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(51:54,57:58,60:64)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# jvt3
#---------
study<-"jvt3"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(99,100,103,104,106,108,113,114,119:122,124,125,134)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# jvt4
#---------
study<-"jvt4"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(112:113,116,117,119,121,123,125,128:129,131,132,133:135,137,138,139,149)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# lcn5
#---------
study<-"lcn5"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(20:22,26,31,34,36)]
d<-assetPCA(d, varlist, reorder=F)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# nbrt -no assets
#---------
# study<-"nbrt"
# vars=c("SUBJID",
#        "SITEID",
#        "SEXN",
#        "SEX",
#        "AGEDAYS",
#        "WTKG",
#        "HTCM",
#        "LENCM",
#        "BMI",
#        "MUACCM",
#        "WAZ",
#        "HAZ",
#        "WHZ",
#        "BAZ",
#        "MUAZ",
#        "HOMETOT",
#        "BFEDFL",
#        "LSSTLFL",
#        "NUMLS",
#        "SUMEP",
#        "SUMDIAR",
#        "SUMDAYS",
#        "PCTDIAR",
#        "MAGE",
#        "MHTCM",
#        "MWTKG",
#        "MBMI",
#        "MEDUCYRS",
#        "CHICKEN",
#        "COW",
#        "DOGS",
#        "GOAT",
#        "INCTOT",
#        "NPERSON")
# d<-as.data.frame(bindGHAP_Fill(study, d=NULL, varlist=vars))
# colnames(d)
# varlist<-colnames(d)[c()]
# d<-assetPCA(d, varlist, reorder=F)
# saveRDS(d, file=paste0(study, '.HHwealth.rds') )



#---------
# npre -only TV asset
#---------
# study<-"npre"
# d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
# colnames(d)
# varlist<-colnames(d)[c()]
# d<-assetPCA(d, varlist, reorder=F)
# saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# ppd only 1 asset
#---------
# study<-"ppd"
# d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
# colnames(d)
# varlist<-colnames(d)[c(28,31)]
# d<-assetPCA(d, varlist, reorder=F)
# saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# pzn
#---------
study<-"pzn"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(44,45,47,50,60,62,64,65,66)]
d<-assetPCA(d, varlist, reorder=F)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# prvd
#---------
study<-"prvd"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(416:430, 441, 442,451,452)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# tzc2
#---------
study<-"tzc2"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(34:35,40:42)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# tdc
#---------
study<-"tdc"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(41:45,47,49:55)]
d<-assetPCA(d, varlist, reorder=F)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )


#---------
# wsb
#---------
study<-"wsb"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(69,71:75,77,80:82,86:90,97,99)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )

#---------
# wsk
#---------
study<-"wsk"
d<-as.data.frame(bindGHAP_Fill(study, d=NULL))
colnames(d)
varlist<-colnames(d)[ -1 + c(22:23,27,29,30,32:34,37,39,40, 42)]
d<-assetPCA(d, varlist, reorder=T)
saveRDS(d, file=paste0(study, '.HHwealth.rds') )







#----------------------------------------------
# Bind studies
#----------------------------------------------

asset_study <- c(
  "akup",
  "bngr",
  "cmc",
  "cntt",
  "dvds",
  "eczn",
  "gmsn",
  "ilnd",
  "ildm",
  "irc",
  "jvt3",
  "jvt4",
  "lcn5",
  "pzn",
  "prvd",
  "tzc2",
  "tdc",
  "wsb",
  "wsk")

#Add in study id
d<-NULL
for(i in 1:length(asset_study)){
  temp<-readRDS(paste0(asset_study[i], '.HHwealth.rds'))
  if("HHwealth_quart" %in% colnames(temp)){
  d <- rbind(d, temp)
  }
}

#check which studies don't have wealth quartile
asset_study
unique(d$STUDYID)



pca<-d
save(pca, file="allGHAPstudies-HHwealth.Rdata")

sink()

