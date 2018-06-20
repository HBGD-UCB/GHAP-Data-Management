
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


# #convert subjid to character
gc()
d$subjid <- as.character(d$subjid)



dfull<-as.data.frame(d)


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
table(d$bfedfl) 
table(d$anmlkfl) 
table(d$pwmlkfl) 
table(d$formlkfl) 
table(d$bottlefl) 
table(d$othfedfl) 
table(d$studyid, !is.na(d$bfedfl) & !is.na(d$anmlkfl) & !is.na(d$pwmlkfl) & !is.na(d$formlkfl) & !is.na(d$bottlefl) & !is.na(d$othfedfl))
table(d$studyid, !is.na(d$bfedfl) & (!is.na(d$anmlkfl) | !is.na(d$pwmlkfl) | !is.na(d$formlkfl) | !is.na(d$bottlefl) | !is.na(d$othfedfl)))

temp <- d[d$studyid=="ki1119695-PROBIT" & !is.na(d$bfedfl) & (!is.na(d$anmlkfl) | !is.na(d$pwmlkfl) | !is.na(d$formlkfl) | !is.na(d$bottlefl) | !is.na(d$othfedfl)),]


#complementary feeding
table(d$studyid, !is.na(d$bfedfl) & !is.na(d$sldfedfl) | !is.na(d$bfedfl) & !is.na(d$weanfl))

#breastfeeding
table(d$studyid, !is.na(d$bfedfl))

#bottle-feeding
table(d$studyid, !is.na(d$bottlefl))



table(d$anmlkfl==1 | d$pwmlkfl==1 | d$formlkfl==1 | d$bottlefl==1 | d$othfedfl==1)
table(d$anmlkfl==1 | d$pwmlkfl==1 | d$formlkfl==1 | d$othfedfl==1)


#Code variables

#Exclusive breastfeeding
table(d$studyid, !is.na(d$exbfedfl) | !is.na(d$bfedfl) & !is.na(d$h20fedfl) & !is.na(d$anmlkfl) & !is.na(d$pwmlkfl) & !is.na(d$bfedfl) & !is.na(d$formlkfl) & !is.na(d$bottlefl) & !is.na(d$othfedfl))
table(d$studyid, !is.na(d$exbfedfl) | (!is.na(d$bfedfl) & (!is.na(d$h20fedfl) | !is.na(d$anmlkfl) | !is.na(d$pwmlkfl) | !is.na(d$bfedfl) | !is.na(d$formlkfl) | !is.na(d$bottlefl) | !is.na(d$othfedfl))))


#predominant breastfeeding
d$predfeed_fl <- as.numeric(d$anmlkfl==1 | d$pwmlkfl==1 | d$formlkfl==1 | d$bottlefl==1 | d$othfedfl==1) * (-1) + 1
d$predfeed_fl[is.na(d$bfedfl) | (is.na(d$anmlkfl) & is.na(d$pwmlkfl) & is.na(d$formlkfl)& is.na(d$bottlefl)& is.na(d$othfedfl))] <- NA
table(d$predfeed_fl)
table(d$studyid,d$predfeed_fl)

d$anmlkfl[is.na(d$anmlkfl)] <- 99  
d$pwmlkfl[is.na(d$pwmlkfl)] <- 99  
d$formlkfl[is.na(d$formlkfl)] <- 99  
d$bottlefl[is.na(d$bottlefl)] <- 99  
d$othfedfl[is.na(d$othfedfl)] <- 99  

d$predfeed_fl <- -(as.numeric(d$anmlkfl==1 | d$pwmlkfl==1 | d$formlkfl==1 | d$bottlefl==1 | d$othfedfl==1)) + 1
d$predfeed_fl[is.na(d$bfedfl) | (d$anmlkfl==99 & d$pwmlkfl==99 & d$formlkfl==99 &  d$bottlefl==99 & d$othfedfl==99)] <- NA
table(d$predfeed_fl)
table(d$studyid,d$predfeed_fl)

#Summarize 6 month complimentary feeding
test<-d %>% filter(!is.na(predfeed_fl)) %>% filter(agedays < 30.4*6) %>%
  group_by(studyid,  subjid) %>%
  mutate(predfeed=as.numeric(mean(predfeed_fl==1, na.rm=T))) %>% 
  summarize(predfeed6=mean(predfeed))
table(test$predfeed6)
table(test$studyid, test$predfeed6)

#complementary feeding
d$compfeed_fl <- as.numeric(d$bfedfl==1 & (d$sldfedfl==1 | d$weanfl==1))
d$compfeed_fl[is.na(d$bfedfl) | (is.na(d$sldfedfl) & is.na(d$weanfl))] <- NA
table(d$compfeed_fl)
table(d$studyid,d$compfeed_fl)





#-----------------------
# kiGH5241-JiVitA-3
#---------------------
library(haven)


#-6 different time points at 6 different files
#Child 24 Month Postpartum  Infant 1 Month Postpartum Infant 12 Month Postpartum  Infant 3 Month Postpartum  Infant 6 Month Postpartum 
# Infant Birth Assessment


#analysis dataset
jvta3 <- readRDS("U:/data/jvt3.rds")


# Infant Birth Assessment
ibaf<-read_sas("U:/data/JiVitA-3/raw/hbgd_ibaf.sas7bdat")
# Infant 1 Month Postpartum
i1mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_i1mop.sas7bdat")
# Infant 3 Month Postpartum
i3mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_i3mop.sas7bdat")
# Infant 6 Month Postpartum 
i6mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_i6mop.sas7bdat")
# Infant 12 Month Postpartum
i12mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_i12mop.sas7bdat")
# Child 24 Month Postpartum 
c24mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_c24mop.sas7bdat")





















#analysis dataset
jvta3 <- readRDS("U:/data/jvt3.rds")
head(data.frame(jvta3))
table(jvta3$ANTPT)

# 6 months post partum
jvt3<-read_sas("U:/data/JiVitA-3/raw/hbgd_i6mop.sas7bdat")
head(data.frame(jvt3))

table(jvt3$i6sbfed)



# 24 months post partum
jvt3<-read_sas("U:/data/JiVitA-3/raw/hbgd_c24mop.sas7bdat")
head(data.frame(jvt3))

table(jvt3$c24sbfed)
table(d$bfedfl[d$studyid=="kiGH5241-JiVitA-3"])

#   How many months after birth did you stop breastfeeding this child?
table(jvt3$c24sbfedm)
table(d$durbrst[d$studyid=="kiGH5241-JiVitA-3"])

#   Why did you stop breastfeeding?
table(jvt3$c24sbfeds)

# During all of yesterday and last night how many times did you breastfeed this child?
headtable(d$nbfyes[d$studyid=="kiGH5241-JiVitA-3"])

table(jvt3$c24sbfed)

#   In the past 12 months, has the child been fed cow or goat or sheep or buffalo milk?
table(jvt3$c24amilk)
#   How many days in the past 7 days has the child been fed cow or goat or sheep or buffalo milk?
table(jvt3$c24amilkw)
table(d$anmlkfl[d$studyid=="kiGH5241-JiVitA-3"])


#   How many days in the past 7 days has the child been fed powdered milk?
table(jvt3$c24pmilk)
table(jvt3$c24pmilkw)
table(d$pwmlkfl[d$studyid=="kiGH5241-JiVitA-3"])

#   How many days in the past 7 days has the child been fed baby formula?
table(jvt3$c24formu)
table(jvt3$c24formuw)
table(d$formlkfl[d$studyid=="kiGH5241-JiVitA-3"])


C24PMILK
C24PMILKW
C24PMILKT
C24FORMU
C24FORMUW
C24FORMUT

#   In the past 12 months, has the child been fed cow or goat or sheep or buffalo milk?
#   During these days how many times a day did you feed cow or goat or sheep or buffalo milk?
#   In the past 12 months, has the child been fed powdered milk?
#   How many days in the past 7 days has the child been fed powdered milk?
#   During these days how many times a day did you feed powdered milk?
#   In the past 12 months, has the child been fed baby formula?
#   How many days in the past 7 days has the child been fed baby formula?
#   During these days how many times a day did you feed baby formula?
  



#-----------------------
# ki1112895-Guatemala BSC
#---------------------
gbsc<-read_sas("U:/data/JiVitA-3/raw/hbgd_c24mop.sas7bdat")
head(data.frame(gbsc))


#-----------------------
# ki1113344-GMS-Nepal
#---------------------




