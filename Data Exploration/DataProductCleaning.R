



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
  filter(WHZ > -5 & WHZ < 5) %>%
  filter(AGEDAYS <= 24 * 30.25)
gc()



#Drop un-needed columns
d <- subset(d, select= -c(AGEIMPFL, WTKG,HTCM,LENCM,WAZ,     
                          BAZ,HCAZ,MUAZ,SITEID,  REGION,
                          REGCTRY,  REGCTYP,  CITYTOWN, LATITUDE, LONGITUD, CLUSTID, 
                          HHID, BRTHYR, INCTOTU, CAUSEDTH))

colnames(d) <- tolower(colnames(d))
colnames(d)

#Drop studies Vishak added to data product that don't meet inclusion criteria
dim(d)
d <- d[d$studyid!="ki1000301-DIVIDS" & d$studyid!="ki1055867-WomenFirst" & d$studyid!="ki1135782-INCAP",]
dim(d)



#mark measure frequencies
d$measurefreq <- NA

d$measurefreq[d$studyid %in% c(
  "ki0047075b-MAL-ED",   
  "ki1000108-CMC-V-BCS-2002",              
  "ki1000108-IRC",               
  "ki1000109-EE",           
  "ki1000109-ResPak",  
  "ki1017093b-PROVIDE",  
  "ki1066203-TanzaniaChild2",           
  "ki1101329-Keneba",  
  "ki1112895-Guatemala BSC",       
  "ki1113344-GMS-Nepal",             
  "ki1114097-CONTENT"
)] <- "monthly"

d$measurefreq[d$studyid %in% c(
  "ki1112895-iLiNS-Zinc",  
  "kiGH5241-JiVitA-3",          
  "kiGH5241-JiVitA-4", 
  "ki1148112-LCNI-5",          
  "ki1017093-NIH-Birth",
  "ki1017093c-NIH-Crypto",   
  "ki1119695-PROBIT",         
  "ki1000304b-SAS-CompFeed",   
  "ki1000304b-SAS-FoodSuppl",   
  "ki1126311-ZVITAMBO",   
  "ki1114097-CMIN",                 
  "ki1135781-COHORTS"
)] <- "quarterly"

d$measurefreq[d$studyid %in% c(
  "ki1000110-WASH-Bangladesh",       
  "ki1000111-WASH-Kenya",  
  "ki1148112-iLiNS-DOSE",     
  "ki1148112-iLiNS-DYAD-M", 
  "ki1033518-iLiNS-DYAD-G",
  "ki1000125-AgaKhanUniv",           
  "ki1112895-Burkina Faso Zn",    
  "ki1000304-VITAMIN-A",  
  "ki1000304-Vitamin-B12",
  "ki1000107-Serrinha-VitA",   
  "ki1000304-EU",        
  "ki1000304-ZnMort"
)] <- "yearly"

#Mark COHORTS and CMIN cohorts with different measurement frequency than quarterly
d$measurefreq[d$studyid=="ki1114097-CMIN" & d$country=="BANGLADESH"] <- "monthly"
d$measurefreq[d$studyid=="ki1114097-CMIN" & d$country=="PERU"] <- "monthly"
d<- d[!(d$studyid=="ki1135781-COHORTS" & d$country=="BRAZIL"),] #Drop because yearly but not an RCT
d<- d[!(d$studyid=="ki1135781-COHORTS" & d$country=="SOUTH AFRICA"),] #Drop because yearly but not an RCT


#Keep monthly and quarterly studies
d <- d %>% filter(measurefreq!="yearly")

#Subset to control arms for intervention studies
d <- filter(d, tr=="Control" | tr=="")


#Work to try and convert subjid
# as.character(format(as.numeric(d$subjid[1:10]), scientific = FALSE))
# 
# as.character(sprintf(d$subjid[1:10]), scientific = FALSE))
# 
# 
# as.character(sprintf("%61.384f",d$subjid[1:10]))
# 
# #convert subjid to character
gc()
d$subjid <- as.character(d$subjid)



dfull<-as.data.frame(d)



#--------------------------------------------------------------------------
# WASH and HH variables
#--------------------------------------------------------------------------
d <- dfull %>% subset(., select=c(studyid, country, subjid, agedays, impsan,  soap,    safeh2o, trth2o,  cleanck,
                                  impfloor,h2otime, nrooms,  nhh,     nchldlt5,chicken, cow,     cattle,  ses,     inctot ,   hfoodsec))

#Improved sanitation
  table(d$studyid, d$impsan)
  #compare to wash benefits
  wsb <- readRDS("U:/data/wsb.rds")
  table(wsb$SANITATN)

#Improved soap
  table(d$studyid, d$soap)
   #drop variable
  d <- d %>% subset(., select= -c(soap))

  


#Safe water
  table(d$studyid, d$safeh2o)

  #compare to jivita 3
  jvt3 <- readRDS("U:/data/jvt3.rds")
  table(jvt3$H2OSRCP)
  table(jvt3$H2OSRCB)

  #compare to jivita 4
  jvt4 <- readRDS("U:/data/jvt4.rds")
  table(jvt4$H2OSRCP)
  table(jvt4$H2OSRC)

#treats water
  table(d$studyid, d$trth2o)

#clean cook
  table(d$studyid, d$cleanck)

#improved floor
  table(d$studyid, d$impfloor)
  
  #compare to IRC
  irc <- readRDS("U:/data/irc.rds")
  table(irc$FLOOR)

  
  #compare to CMC
  cmc <- readRDS("U:/data/cmc.rds")
  table(cmc$FLOOR)
  
  
#time to water
  table(d$studyid, is.na(d$h2otime))
  # drop variable- not in any risk factor study
  d <- d %>% subset(., select= -c(h2otime))


#number of rooms
  table(d$studyid, d$nrooms)

#number in household
  table(d$studyid, d$nhh)

#n animals
  table(d$studyid, d$chicken)
  table(d$studyid, d$cow)
  table(d$studyid, d$cattle)

  #drop animals; not enough variation or studies
  d <- d %>% subset(., select=-c(chicken, cow, cattle))
  
#reported or coded SES
  table(d$studyid, d$ses)

  #merge in household assets PCA wealth
  load("U:/results/allGHAPstudies-HHwealth.Rdata")
  table(pca$STUDYID, pca$HHwealth_quart)
  #Note, only the COHORTS study has SES but no HH wealth quartile
  colnames(pca) <- tolower(colnames(pca))
  pca <- as.data.frame(pca)
  pca$subjid <-as.character(pca$subjid)
  d <- left_join(d, pca, by=c("studyid", "country", "subjid"))
  table(d$ses, d$hhwealth_quart)
  table(d$studyid, d$hhwealth_quart)
  
  #merge in ses variable for COHORTS for all countries except INDIA. The other countries have wealth based on 
  #an asset-based PCA index, but India is based on father's occupation.
  d$hhwealth_quart <- as.character(d$hhwealth_quart)
  chtses<- d$ses[is.na(d$hhwealth_quart) & d$studyid=="ki1135781-COHORTS" & d$country!="INDIA"]
  chtses[chtses==""] <- NA
  chtses[chtses=="Low"] <- "Wealth Q1"
  chtses[chtses=="Lower-mi"] <- "Wealth Q2"
  chtses[chtses=="Middle"] <- "Wealth Q3"
  chtses[chtses=="Upper-mi"] <- "Wealth Q4"
  chtses[chtses=="Upper"] <- "Wealth Q4"
  
  d$hhwealth_quart[is.na(d$hhwealth_quart) & d$studyid=="ki1135781-COHORTS" & d$country!="INDIA"] <-chtses
  d$hhwealth_quart <- factor(d$hhwealth_quart)
  
  #Check and make sure all merged
  df <- d %>% filter(!is.na(hhwealth_quart)) %>% group_by(studyid, subjid) %>% slice(1)
  table(pca$studyid, pca$hhwealth_quart)
  table(df$studyid, df$hhwealth_quart)
  
  #tanzania child and jivita-4 have less. Check merging on ID
  
  
#Reported income
  table(d$studyid, is.na(d$inctot))
  #Drop:
  d <- d %>% subset(., select=-c(inctot))

#Food security
  table(d$studyid, d$hfoodsec)

  #Recode into 4 harmonized categories
  unique(d$hfoodsec)
  
  d$temp <- NA
  
  d$temp[d$hfoodsec=="Mildly Food Insecure"] <- "Mildly Food Insecure"
  d$temp[d$hfoodsec=="Food Insecure"] <- "Moderately Food Insecure"
  d$temp[d$hfoodsec=="Food secure"] <- "Food Secure"
  
  d$temp[d$hfoodsec=="Neither Deficit Nor Surplus"] <- "Mildly Food Insecure"
  d$temp[d$hfoodsec=="Sometimes Deficit"] <- "Moderately Food Insecure"
  d$temp[d$hfoodsec=="Deficit In Whole Year"] <- "Severely Food Insecure"
  d$temp[d$hfoodsec=="Surplus"] <- "Food Secure"
  
  d$temp[d$hfoodsec=="Neither deficit nor surplus"] <- "Mildly Food Insecure"
  d$temp[d$hfoodsec=="Sometimes deficit"] <- "Moderately Food Insecure"
  d$temp[d$hfoodsec=="Deficit in whole year"] <- "Severely Food Insecure"
  
  d$temp[d$hfoodsec=="Mildly Food Insecure Access"] <- "Mildly Food Insecure"
  d$temp[d$hfoodsec=="Moderately Food Insecure Access"] <- "Moderately Food Insecure"
  d$temp[d$hfoodsec=="Severely Food Insecure Access"] <- "Severely Food Insecure"
  d$temp[d$hfoodsec=="Food Secure"] <- "Food Secure"
  
  d$hfoodsec <- d$temp
  d <- d %>% subset(., select=-c(temp))
  
  table(d$studyid, d$hfoodsec)
  

#--------------------------------------------------------------------------
# Diarrhea
#--------------------------------------------------------------------------
d <- dfull %>% subset(., select=c(studyid, subjid, agedays, sumep,   sumdiar, sumdays, pctdiar, diarfl,  lsstlfl, numls,   bldstlfl,diarfl_r,lsstfl_r,numls_r, bldstl_r,dur_r))
colnames(d)

#drop symptom variables
d <- d %>% subset(., select= -c(lsstlfl, numls,   bldstlfl,lsstfl_r,numls_r, bldstl_r))

table(d$studyid, is.na(d$sumep))
table(d$studyid, is.na(d$sumdiar))
table(d$studyid, is.na(d$sumdays))
table(d$studyid, is.na(d$pctdiar))

#drop sum and pct variables because they are not time varying
d <- d %>% subset(., select= -c(sumep, sumdiar, sumdays, pctdiar))

d %>% group_by(studyid) %>% do(as.data.frame(t(as.data.frame(t(summary(.$pctdiar)))))) %>% as.data.frame()


table(d$studyid, d$diarfl)
table(d$studyid, d$diarfl_r)
table(d$studyid, d$dur_r)

#Replace diar flag with 1-day recall 
d$diarfl[is.na(d$diarfl) & !is.na(d$diarfl_r) & d$dur_r==1] <- d$diarfl_r[is.na(d$diarfl) & !is.na(d$diarfl_r) & d$dur_r==1]

#drop unneeded variables
d <- d %>% subset(., select= -c(diarfl_r, dur_r))

#Look for unrealistic diarrhea prevalences
d %>% group_by(studyid) %>% summarize(mean(diarfl, na.rm=T)) %>% as.data.frame()

table(d$studyid, d$diarfl)


#Calculate cumulative percent days with diarrhea for each individual
d <- d %>% arrange(studyid, subjid, agedays) %>% 
  group_by(studyid, subjid) %>% 
  mutate(numdiar = cumsum(diarfl),
         numdays = cumsum(!is.na(diarfl)),
         pctdiar=numdiar/numdays * 100)

d %>% group_by(studyid) %>% summarize(mean(pctdiar, na.rm=T)) %>% as.data.frame()


#--------------------------------------------------------------------------
# Breastfeeding
#--------------------------------------------------------------------------
d <- dfull %>% subset(., select=c(studyid, subjid, agedays, feeding, durbrst,   brfeed, bfedfl, exbfedfl,weanfl,  anmlkfl, pwmlkfl, formlkfl,bottlefl,h20fedfl,othfedfl,sldfedfl,nbfyes,  earlybf, cmfdint))

table(d$studyid, d$feeding)
table(d$studyid, !is.na(d$durbrst))
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$durbrst))))) %>% as.data.frame()

table(d$studyid, d$brfeed)
table(d$studyid, d$bfedfl) #Why so little from Jivita-3?
table(d$studyid, d$exbfedfl)
table(d$studyid, d$weanfl) #Mal-ED seems wrong
table(d$studyid, d$anmlkfl)
table(d$studyid, d$pwmlkfl)
table(d$studyid, d$formlkfl)
table(d$studyid, d$bottlefl)
table(d$studyid, d$h20fedfl)
table(d$studyid, d$othfedfl)
table(d$studyid, d$sldfedfl)
table(d$studyid, d$nbfyes)
table(d$studyid, d$earlybf)

table(d$studyid, !is.na(d$brfeed))
table(d$studyid, !is.na(d$bfedfl)) #only 18 obs in ki1112895-Guatemala BSC and 85 in ki1113344-GMS-Nepal and 54 in kiGH5241-JiVitA-3
table(d$studyid, !is.na(d$exbfedfl)) # Mostly missing from ki1119695-PROBIT, ki1000108-CMC-V-BCS-2002, and ki1000108-IRC
table(d$studyid, !is.na(d$weanfl)) # Mostly missing from ki1119695-PROBIT, ki1000108-CMC-V-BCS-2002, and ki1000108-IRC
table(d$studyid, !is.na(d$anmlkfl)) # Mostly missing from kiGH5241-JiVitA-3, ki1000304b-SAS-FoodSuppl, ki1000304b-SAS-CompFeed, and ki1000109-EE
table(d$studyid, !is.na(d$pwmlkfl)) # Mostly missing from kiGH5241-JiVitA-3, ki1000304b-SAS-FoodSuppl, ki0047075b-MAL-ED, and ki1113344-GMS-Nepal
table(d$studyid, !is.na(d$formlkfl)) # Mostly missing from kiGH5241-JiVitA-3, ki1119695-PROBIT, ki0047075b-MAL-ED, ki1000304b-SAS-CompFeed, and ki1113344-GMS-Nepal
table(d$studyid, !is.na(d$bottlefl)) #Only measures in ki1000304b-SAS-CompFeed, and mostly missing
table(d$studyid, !is.na(d$h20fedfl))# Mostly missing from ki1000109-EE, ki1000304b-SAS-CompFeed, and ki1113344-GMS-Nepal
table(d$studyid, !is.na(d$othfedfl)) # Mostly missing from ki1000109-EE
table(d$studyid, !is.na(d$sldfedfl)) # Mostly missing from ki1000109-EE
table(d$studyid, !is.na(d$nbfyes)) # Mostly missing from  ki0047075b-MAL-ED, ki1113344-GMS-Nepal, kiGH5241-JiVitA-3
table(d$studyid, !is.na(d$earlybf))


table(d$studyid, !is.na(d$cmfdint))
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$cmfdint))))) %>% as.data.frame()

#check presence of all needed indicators across studies

#Exclusive breastfeeding
table(d$studyid, !is.na(d$exbfedfl) | !is.na(d$bfedfl) & !is.na(d$h20fedfl) & !is.na(d$anmlkfl) & !is.na(d$pwmlkfl) & !is.na(d$bfedfl) & !is.na(d$formlkfl) & !is.na(d$bottlefl) & !is.na(d$othfedfl))
table(d$studyid, !is.na(d$exbfedfl) | (!is.na(d$bfedfl) & (!is.na(d$h20fedfl) | !is.na(d$anmlkfl) | !is.na(d$pwmlkfl) | !is.na(d$bfedfl) | !is.na(d$formlkfl) | !is.na(d$bottlefl) | !is.na(d$othfedfl))))


#predominant breastfeeding
table(d$studyid, !is.na(d$bfedfl) & !is.na(d$h20fedfl) & !is.na(d$anmlkfl) & !is.na(d$pwmlkfl) & !is.na(d$bfedfl) & !is.na(d$formlkfl) & !is.na(d$bottlefl) & !is.na(d$othfedfl))
table(d$studyid, !is.na(d$bfedfl) & (!is.na(d$h20fedfl) | !is.na(d$anmlkfl) | !is.na(d$pwmlkfl) | !is.na(d$bfedfl) | !is.na(d$formlkfl) | !is.na(d$bottlefl) | !is.na(d$othfedfl)))

#complementary feeding
table(d$studyid, !is.na(d$bfedfl) & !is.na(d$sldfedfl) | !is.na(d$bfedfl) & !is.na(d$weanfl))

#breastfeeding
table(d$studyid, !is.na(d$bfedfl))

#bottle-feeding*
table(d$studyid, !is.na(d$bottlefl))

#drop derived variables
d <- d %>% subset(., select=-c(feeding, brfeed))

#drop variables insufficiently measured
d <- d %>% subset(., select=-c(bottlefl, cmfdint))


#--------------------------------------------------------------------------
# birth characteristics
#--------------------------------------------------------------------------
d <- dfull %>% subset(., select=c(studyid, subjid, agedays,gagebrth, parity,  brthordr,birthwt, birthlen, enstunt, vagbrth, hdlvry))

#Continious vars
table(d$studyid, !is.na(d$gagebrth))
table(d$studyid, !is.na(d$birthwt))
table(d$studyid, !is.na(d$birthlen))

#gestational age at birth
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$gagebrth))))) %>% as.data.frame()


#Birthweight and length - look good
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$birthwt))))) %>% as.data.frame()
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$birthlen))))) %>% as.data.frame()



#parity
  #Combine parity and birthorder
  table(d$studyid, d$parity)
  table(d$studyid, d$brthordr)
  
  d$parity[is.na(d$parity)] <- d$brthordr[is.na(d$parity)]
  
  
  #shift observations of the studies that start at 0 rightward by one.
  d$parity[!is.na(d$parity) & d$studyid %in% c("ki1066203-TanzaniaChild2", "ki1066203-TanzaniaChild2", "ki1101329-Keneba",
                                               "ki1113344-GMS-Nepal", "kiGH5241-JiVitA-3")] <-d$parity[!is.na(d$parity) & d$studyid %in% c("ki1066203-TanzaniaChild2", "ki1066203-TanzaniaChild2", "ki1101329-Keneba",
                                                                                                                                          "ki1113344-GMS-Nepal", "kiGH5241-JiVitA-3")] + 1
  
  #Fix 14 obs of 0 in ki1000304b-SAS-FoodSuppl
  d$parity[d$studyid=="ki1000304b-SAS-FoodSuppl" & d$parity==0] <- NA
  table(d$studyid, d$parity)
  
#vaginal birth and home delivery
table(d$studyid, d$vagbrth)
table(d$studyid, d$hdlvry)

#Enrolled stunted
table(d$studyid, d$enstunt)

#Missing many obs... code it myself and compare here
d_enrol <- dfull %>% group_by(studyid, subjid) %>% 
  arrange(agedays) %>% 
  slice(1) %>% 
  mutate(enstunt2= as.numeric(haz < -2)) %>%
  subset(., select=c(studyid, subjid, enstunt2))

d <- left_join(d, d_enrol, by=c("studyid", "subjid"))
table(d$studyid, d$enstunt2)

table(d$enstunt, d$enstunt2)

#Use newly coded enrol stunting
d$enstunt <- d$enstunt2
d <- d %>% subset(., select=-c(enstunt2))

#--------------------------------------------------------------------------
# parent characteristics
#--------------------------------------------------------------------------
d <- dfull %>% subset(., select=c(studyid, subjid, agedays,mage,    mhtcm,   mwtkg,   mbmi,    meducyrs,single, fage,    fhtcm,   fwtkg,  fbmi,    feducyrs))

#single mom
table(d$studyid, d$single)
#Note Jivita-4 single mother seems way too high
#compare to jivita 4 analysis dataset
jvt4 <- readRDS("U:/data/jvt4.rds")


#Calculate bmi from height and weight, and vice versa, for when 2 of 3 are measured
#bmi
flag <- is.na(d$mbmi) & !is.na(d$mhtcm) & !is.na(d$mwtkg)
d$mbmi[flag] <- d$mwtkg[flag] / (d$mhtcm[flag] / 100)^2

flag <- is.na(d$fbmi) & !is.na(d$fhtcm) & !is.na(d$fwtkg)
d$fbmi[flag] <- d$fwtkg[flag] / (d$fhtcm[flag] / 100)^2

#weight
flag <- is.na(d$mwtkg) & !is.na(d$mhtcm) & !is.na(d$mbmi)
d$mwtkg[flag] <- d$mbmi[flag] * (d$mhtcm[flag] / 100)^2

flag <- is.na(d$fwtkg) & !is.na(d$fhtcm) & !is.na(d$fbmi)
d$fwtkg[flag] <- d$fbmi[flag] * (d$fhtcm[flag] / 100)^2

#height
flag <- is.na(d$mhtcm) & !is.na(d$mwtkg) & !is.na(d$mbmi)
d$mhtcm[flag] <- sqrt(d$mwtkg[flag] / d$mbmi[flag]) * 100

flag <- is.na(d$fhtcm) & !is.na(d$fwtkg) & !is.na(d$fbmi)
d$fhtcm[flag] <- sqrt(d$fwtkg[flag] / d$fbmi[flag]) * 100


#height weight and bmi
table(d$studyid, !is.na(d$mhtcm))
table(d$studyid, !is.na(d$mwtkg))
table(d$studyid, !is.na(d$mbmi))

table(d$studyid, !is.na(d$fhtcm))
table(d$studyid, !is.na(d$fwtkg))
table(d$studyid, !is.na(d$fbmi))

#Drop father weight and bmi- they are only measured in PROBIT
d <- d %>% subset(., select=-c(fwtkg, fbmi))

d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$mhtcm))))) %>% as.data.frame()
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$mwtkg))))) %>% as.data.frame()
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$mbmi))))) %>% as.data.frame()

d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$fhtcm))))) %>% as.data.frame()





# age and education
table(d$studyid, !is.na(d$mage))
table(d$studyid, !is.na(d$meducyrs))
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$mage))))) %>% as.data.frame()
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$meducyrs))))) %>% as.data.frame()


table(d$studyid, !is.na(d$fage))
table(d$studyid, !is.na(d$feducyrs))
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$fage))))) %>% as.data.frame()
d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$feducyrs))))) %>% as.data.frame()

#--------------------------------------------------------------------------
# death
#--------------------------------------------------------------------------
d <- dfull %>% subset(., select=c(studyid, subjid, agedays,  dead,   agedth))



table(d$studyid, !is.na(d$dead))
table(d$studyid, (d$dead))

table(d$studyid, !is.na(d$agedth))

d %>% group_by(studyid) %>% do(as.data.frame(t(as.numeric(summary(.$agedth))))) %>% as.data.frame()


#--------------------------------------------------------------------------
# Other
#--------------------------------------------------------------------------
d <- dfull %>% subset(., select=c(studyid, subjid, agedays, month,      arm,     tr,  brthweek,brthmon))

table(d$studyid, !is.na(d$month))
table(d$studyid, !is.na(d$arm))
table(d$studyid, !is.na(d$tr))
table(d$studyid, !is.na(d$brthweek))
table(d$studyid, !is.na(d$brthmon))

#Calculate birthmonth from brthweek where brthmonth is missing
d$brthmon[is.na(d$brthmon)] <- ceiling(d$brthweek[is.na(d$brthmon)]/53 *12)


table(d$studyid, (d$month))
table(d$studyid, (d$arm))
table(d$studyid, (d$tr))
table(d$studyid, (d$brthweek))
table(d$studyid, (d$brthmon))
