

# Instructions for downloading FINAL dataset

# Go to https://git.ghap.io/stash/projects/HBGD/repos/adhoc/browse
# click clone button
# Copy link (mine is https://andrew.mertens@git.ghap.io/stash/scm/hbgd/adhoc.git)
# Open Sourcetree (Click window icon in bottom left, then search magnifying glass icon
# in the top right, and search Sourcetree to find)
# Click clone button in source tree 
# Paste link in source path box
# Add U:/data/FINAL/ to the destination path (make sure FINAL folder is empty)
# Click clone



rm(list=ls())
library(tidyverse)
library(data.table)
# options(repos = c(CRAN = "http://cran.rstudio.com/",
#  deltarho = "http://packages.deltarho.org"))
# install.packages("growthstandards")
library(growthstandards)



setwd("U:/data")
gc()



#Read rds file and drop unneeded columns
d<-fread("U:/data/Stunting/Full-compiled-data/FINAL.csv", header = T,
         drop = c( "AGEIMPFL",  "WTKG",    "HTCM",    "LENCM",
                   "BAZ",     "HCAZ",    "MUAZ",    
                   "REGCTRY", "REGCTYP", "CITYTOWN","LATITUDE","LONGITUD", "HHID",    
                   "DEAD",    "AGEDTH",  "CAUSEDTH","FEEDING",
                   "DURBRST", "BRTHYR", "ENSTUNT", "FWTKG", "FBMI",
                   "BRFEED", "SUMEP",   "SUMDIAR", "SUMDAYS",
                   "PCTDIAR", "IMPSAN",  "SOAP",    "SAFEH2O", "H2OTIME",
                   "CHICKEN", "COW",     "CATTLE",  "INCTOT", 
                   "INCTOTU", "BFEDFL",  "EXBFEDFL","WEANFL",  "ANMLKFL", "PWMLKFL",
                   "FORMLKFL","BOTTLEFL","H20FEDFL","OTHFEDFL","SLDFEDFL","NBFYES",   "CMFDINT", "DIARFL",  "LSSTLFL",
                   "NUMLS",   "BLDSTLFL","DIARFL_R","LSSTFL_R","NUMLS_R", "BLDSTL_R",
                   "DUR_R"))


colnames(d) <- tolower(colnames(d))
gc()

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


#Temp
#d <- d[d$measurefreq!="yearly",]

#--------------------------------------------------------
# Calculate longitudinal prevalence of wasting and stunting
# in the first 6 months.
#--------------------------------------------------------

lprev <- d %>% group_by(studyid, country, subjid) %>%
  #only keep it in monthly studies
  filter(measurefreq=="monthly") %>%
  filter(agedays<6*30.4167) %>%
  mutate(wast=as.numeric(whz < -2), stunt=as.numeric(haz < -2)) %>%
  summarize(wastprev06=mean(wast, na.rm=T), stuntprev06=mean(stunt, na.rm=T), anywast06=as.numeric(wastprev06>0), anystunt06=as.numeric(stuntprev06>0), 
            pers_wast=as.numeric(wastprev06>=0.5), pers_stunt=as.numeric(stuntprev06>=0.5))
head(lprev)

table(lprev$anywast06)
table(lprev$anystunt06)
table(lprev$pers_wast)
table(lprev$pers_stunt)

lprev <- lprev %>% subset(., select = c(studyid, country,subjid, anywast06,  pers_wast))

d <- left_join(d, lprev, by=c("studyid","country","subjid"))

#--------------------------------------------------------
#Code to keep monthly and quarterly studies
#--------------------------------------------------------
#d <- d %>% filter(measurefreq!="yearly")
#NOTE: not run here because subsetting to correct studies is done
#in the outcome datasets
d <- d %>% subset(., select=-c(measurefreq))





#--------------------------------------------------------
#Calculate stunting and wastingat enrollment and keep one observation per child
#Also check if children without a recorded birthweight or birthlength have WAZ or HAZ in the first day of life
#--------------------------------------------------------
d <- d %>% group_by(studyid, subjid) %>% 
  arrange(studyid, subjid, agedays) %>% 
  filter(!is.na(haz)) %>%
  mutate(enstunt= as.numeric(haz < -2),
         enwast= as.numeric(whz < -2),
         birthLAZ= haz,
         birthWAZ= waz) %>%
  slice(1) 

table(is.na(d$birthwt), d$agedays>1)

#keep where anthro is measured on first 3 days (from Parul), but birth anthro is not recorded
d$birthLAZ[d$agedays>3] <- NA 
d$birthWAZ[d$agedays>3] <- NA
d$birthmeas_age <- 1
d$birthmeas_age[d$agedays <= 3] <- d$agedays[d$agedays <= 3]
d <- d %>% subset(., select=-c(agedays, haz, waz, whz))

table(d$studyid, d$enwast)
table(d$studyid, d$enstunt)


#--------------------------------------------------------
#merge in household assets PCA wealth
#--------------------------------------------------------

#convert subjid to character for the merge with covariate dataset
d$subjid <- as.character(d$subjid)

#load in pca results
load("U:/results/allGHAPstudies-HHwealth.Rdata")
table(pca$STUDYID, pca$HHwealth_quart)
#Note, only the COHORTS study has SES but no HH wealth quartile
colnames(pca) <- tolower(colnames(pca))
pca <- as.data.frame(pca)
pca$subjid <-as.character(pca$subjid)
d <- left_join(d, pca, by=c("studyid", "country", "subjid"))

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
#df <- d %>% filter(!is.na(hhwealth_quart)) %>% group_by(studyid, subjid) %>% slice(1)
#table(pca$studyid, pca$hhwealth_quart)
#table(df$studyid, df$hhwealth_quart)




#--------------------------------------------------------------------------
# Code Food security
#--------------------------------------------------------------------------

#Recode into 3 harmonized categories (so that all 3 levels are present across all studies)
unique(d$hfoodsec)

d$temp <- NA
d$temp[d$hfoodsec=="Mildly Food Insecure"] <- "Mildly Food Insecure"
d$temp[d$hfoodsec=="Food Insecure"] <- "Food Insecure"
d$temp[d$hfoodsec=="Food secure"] <- "Food Secure"

d$temp[d$hfoodsec=="Neither Deficit Nor Surplus"] <- "Mildly Food Insecure"
d$temp[d$hfoodsec=="Sometimes Deficit"] <- "Food Insecure"
d$temp[d$hfoodsec=="Deficit In Whole Year"] <- "Food Insecure"
d$temp[d$hfoodsec=="Surplus"] <- "Food Secure"

d$temp[d$hfoodsec=="Neither deficit nor surplus"] <- "Mildly Food Insecure"
d$temp[d$hfoodsec=="Sometimes deficit"] <- "Food Insecure"
d$temp[d$hfoodsec=="Deficit in whole year"] <- "Food Insecure"

d$temp[d$hfoodsec=="Mildly Food Insecure Access"] <- "Mildly Food Insecure"
d$temp[d$hfoodsec=="Moderately Food Insecure Access"] <- "Food Insecure"
d$temp[d$hfoodsec=="Severely Food Insecure Access"] <- "Food Insecure"
d$temp[d$hfoodsec=="Food Secure"] <- "Food Secure"



d$hfoodsec <- d$temp
d <- d %>% subset(., select=-c(temp))

table(d$studyid, d$hfoodsec)

d$hfoodsec <- factor(d$hfoodsec, levels=c("Food Secure", "Mildly Food Insecure", "Food Insecure"))



#--------------------------------------------------------------------------
# birth characteristics
#--------------------------------------------------------------------------


# drop gestational age in studies with no variations (measured it at the month level)
d$gagebrth[d$studyid=="ki1113344-GMS-Nepal"] <- NA

#parity
#Combine parity and birthorder
table(d$studyid, d$parity)
table(d$studyid, d$brthordr)

d$parity[is.na(d$parity)] <- d$brthordr[is.na(d$parity)]


#Fix 21 obs of 0 in ki1000304b-SAS-FoodSuppl
d$parity[d$studyid=="ki1000304b-SAS-FoodSuppl" & d$parity==0] <- NA

#Fix right shift of Tanzania child
d$parity[d$studyid=="ki1066203-TanzaniaChild2" & d$parity==1] <- NA
d$parity[d$studyid=="ki1066203-TanzaniaChild2"] <- d$parity[d$studyid=="ki1066203-TanzaniaChild2"] -1 
table(d$studyid, d$parity)

#Convert birth Zscore to absolute units
table(d$studyid, is.na(d$birthlen))
table(d$studyid, is.na(d$birthwt))

#sex must be "Male" or "Female"
table(d$sex)
table(is.na(d$sex))

d$birthlen2 <- who_zscore2htcm(d$birthmeas_age, d$birthLAZ, sex = d$sex)
d$birthwt2 <- who_zscore2wtkg(d$birthmeas_age, d$birthWAZ, sex = d$sex) * 1000
d$birthlen2[!is.finite(d$birthlen2)] <- NA
d$birthwt2[!is.finite(d$birthwt2)] <- NA



#Check if children without a recorded birthweight or birthlength have WAZ or HAZ in the first year of life
#and add into birthweight variable

summary(d$birthlen)
summary(d$birthlen2)
summary(d$birthwt)
summary(d$birthwt2)

table(is.na(d$birthlen), is.na(d$birthlen2))
table(is.na(d$birthwt), is.na(d$birthwt2))

d$birthlen[is.na(d$birthlen)] <- d$birthlen2[is.na(d$birthlen)]
d$birthwt[is.na(d$birthwt)] <- d$birthwt2[is.na(d$birthwt)]


table(d$studyid, is.na(d$birthlen))
table(d$studyid, is.na(d$birthwt))

#--------------------------------------------------------------------------
# parent characteristics
#--------------------------------------------------------------------------

#single mom
table(d$studyid, d$single)

#Note Jivita-4 single mother seems way too high
#drop single mother in kiGH5241-JiVitA-4 until the raw data can be checked more thoroughly
d$single[d$studyid=="kiGH5241-JiVitA-4"] <-NA


#Calculate bmi from height and weight, and vice versa, for when 2 of 3 are measured
#bmi
flag <- is.na(d$mbmi) & !is.na(d$mhtcm) & !is.na(d$mwtkg)
d$mbmi[flag] <- d$mwtkg[flag] / (d$mhtcm[flag] / 100)^2

#weight
flag <- is.na(d$mwtkg) & !is.na(d$mhtcm) & !is.na(d$mbmi)
d$mwtkg[flag] <- d$mbmi[flag] * (d$mhtcm[flag] / 100)^2

#height
flag <- is.na(d$mhtcm) & !is.na(d$mwtkg) & !is.na(d$mbmi)
d$mhtcm[flag] <- sqrt(d$mwtkg[flag] / d$mbmi[flag]) * 100


#drop maternal weight and bmi in kiGH5241-JiVitA-3 as it is measured during pregnancy
d$mbmi[d$studyid=="kiGH5241-JiVitA-3"] <-NA
d$mwtkg[d$studyid=="kiGH5241-JiVitA-3"] <-NA

table(d$studyid, !is.na(d$mbmi))

#--------------------------------------------------------------------------
# house characteristics
#--------------------------------------------------------------------------

#set to missing any observations of 0 rooms
d$nrooms[d$nrooms==0] <- NA

#check the number of children < 5 and set to missing if 0
table(d$nchldlt5)
table(d$studyid, d$nchldlt5)

#Need to shift full distribution by 1 in studies with 0 marked- 
#  inconsistent marking of subject in the count across studies
d$nchldlt5[d$studyid=="ki1148112-LCNI-5" & d$nchldlt5==0] <- NA #LCNI has 4 children marked as 0
d$nchldlt5[d$studyid=="ki1000108-IRC"] <- d$nchldlt5[d$studyid=="ki1000108-IRC"] + 1
d$nchldlt5[d$studyid=="ki1017093b-PROVIDE"] <- d$nchldlt5[d$studyid=="ki1017093b-PROVIDE"] + 1
d$nchldlt5[d$studyid=="ki1017093c-NIH-Crypto"] <- d$nchldlt5[d$studyid=="ki1017093c-NIH-Crypto"] + 1
d$nchldlt5[d$studyid=="ki1066203-TanzaniaChild2"] <- d$nchldlt5[d$studyid=="ki1066203-TanzaniaChild2"] + 1
d$nchldlt5[d$studyid=="ki1148112-iLiNS-DYAD-M"] <- d$nchldlt5[d$studyid=="ki1148112-iLiNS-DYAD-M"] + 1
d$nchldlt5[d$studyid=="kiGH5241-JiVitA-3"] <- d$nchldlt5[d$studyid=="kiGH5241-JiVitA-3"] + 1


#--------------------------------------------------------------------------
# birthmonth
#--------------------------------------------------------------------------

#Calculate birthmonth from brthweek where brthmonth is missing
d$brthmon[is.na(d$brthmon)] <- ceiling(d$brthweek[is.na(d$brthmon)]/53 *12)


#--------------------------------------------------------
# create id variable for unit of independent observation
#--------------------------------------------------------

d$id <- NA
d$id[d$studyid %in% c("ki1112895-iLiNS-Zinc",
                      "kiGH5241-JiVitA-3",    
                      "kiGH5241-JiVitA-4",
                      "ki1119695-PROBIT",
                      "ki1000304b-SAS-CompFeed")] <-d$clustid[d$studyid %in% c("ki1112895-iLiNS-Zinc",
                                                                               "kiGH5241-JiVitA-3",    
                                                                               "kiGH5241-JiVitA-4",
                                                                               "ki1119695-PROBIT",
                                                                               "ki1000304b-SAS-CompFeed")]
d$id[!(d$studyid %in% c("ki1112895-iLiNS-Zinc",
                        "kiGH5241-JiVitA-3",    
                        "kiGH5241-JiVitA-4",
                        "ki1119695-PROBIT",
                        "ki1000304b-SAS-CompFeed"))] <-d$subjid[!(d$studyid %in% c("ki1112895-iLiNS-Zinc",
                                                                                   "kiGH5241-JiVitA-3",    
                                                                                   "kiGH5241-JiVitA-4",
                                                                                   "ki1119695-PROBIT",
                                                                                   "ki1000304b-SAS-CompFeed"))]

#No site id in the cohorts guatemala raw data
#d$id[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"] <-d$clustid[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"]

#use siteid from PROBIT
d$id[d$studyid %in% c("ki1119695-PROBIT")] <-d$siteid[d$studyid %in% c("ki1119695-PROBIT")]

table(is.na(d$id))


#--------------------------------------------------------
# Classify intervention arms
#--------------------------------------------------------

arms <- d %>% filter(arm!="") %>% group_by(studyid) %>% do(levels = unique(.$arm))
arms


d$tr <- NA


d$tr[d$studyid=="ki1000107-Serrinha-VitA" & d$arm=="Placebo"] <- "Control"
d$tr[d$studyid=="ki1000107-Serrinha-VitA" & d$arm=="Vitamin A"] <- "VitA"


d$tr[(d$studyid=="ki1000110-WASH-Bangladesh" | d$studyid=="ki1000111-WASH-Kenya")] <- "Other"
d$tr[(d$studyid=="ki1000110-WASH-Bangladesh" | d$studyid=="ki1000111-WASH-Kenya") & (d$arm=="Control" | d$arm=="Passive Control")] <- "Control"
d$tr[(d$studyid=="ki1000110-WASH-Bangladesh" | d$studyid=="ki1000111-WASH-Kenya") & (d$arm=="Nutrition" | d$arm=="Nutrition + WSH")] <- "LNS"

d$tr[d$studyid=="ki1000125-AgaKhanUniv" & d$arm=="Control"] <- "Control"
d$tr[d$studyid=="ki1000125-AgaKhanUniv" & d$arm=="Intervention"] <- "Maternal"

d$tr[d$studyid=="ki1000304-EU" & d$arm=="Placebo"] <- "Control"
d$tr[d$studyid=="ki1000304-EU" & d$arm=="Zinc"] <- "Zinc"

d$tr[d$studyid=="ki1000304-VITAMIN-A" & d$arm=="Control"] <- "Control"
d$tr[d$studyid=="ki1000304-VITAMIN-A" & d$arm=="Vitamin A"] <- "VitA"

d$tr[d$studyid=="ki1000304-Vitamin-B12" ] <- "Other"
d$tr[d$studyid=="ki1000304-Vitamin-B12" & d$arm=="Placebo"] <- "Control"

d$tr[d$studyid=="ki1000304-ZnMort" & d$arm=="IFA"] <- "Control"
d$tr[d$studyid=="ki1000304-ZnMort" & d$arm=="Zinc+IFA"] <- "Zinc"

d$tr[d$studyid=="ki1000304b-SAS-CompFeed" & d$arm=="Control"] <- "Control"
d$tr[d$studyid=="ki1000304b-SAS-CompFeed" & d$arm=="Intervention"] <- "Other"

d$tr[d$studyid=="ki1000304b-SAS-FoodSuppl" ] <- "Other"
d$tr[d$studyid=="ki1000304b-SAS-FoodSuppl" & d$arm=="No intervention"] <- "Control"

d$tr[d$studyid=="ki1017093b-PROVIDE" ] <- "Other"
d$tr[d$studyid=="ki1017093b-PROVIDE" & d$arm=="No Rotarix + No IPV (175)"] <- "Control"

d$tr[d$studyid=="ki1066203-TanzaniaChild2" & d$arm=="Control"] <- "Control"
d$tr[d$studyid=="ki1066203-TanzaniaChild2" & d$arm=="Multivitamin Alone"] <- "Other"
d$tr[d$studyid=="ki1066203-TanzaniaChild2" & (d$arm=="Zinc Alone" | d$arm=="Zinc + Multivitamin")] <- "Zinc"

d$tr[d$studyid=="ki1112895-Burkina Faso Zn" ] <- "Zinc"
d$tr[d$studyid=="ki1112895-Burkina Faso Zn" & d$arm=="Control (no Zinc)"] <- "Control"

d$tr[d$studyid=="ki1112895-Guatemala BSC" ] <- "Other"
d$tr[d$studyid=="ki1112895-Guatemala BSC" & (d$arm=="WPC"|d$arm=="MNT + WPC")] <- "Control"

d$tr[d$studyid=="ki1112895-iLiNS-Zinc" ] <- "LNS"
d$tr[d$studyid=="ki1112895-iLiNS-Zinc" & d$arm=="e.Control"] <- "Control"

#Create secondary dataset for Zinc+LNS vs LNS contrast
iLiNS_Zinc_df <- d[d$studyid=="ki1112895-iLiNS-Zinc" & d$arm!="e.Control", ]
iLiNS_Zinc_df$tr <- "Zinc"
iLiNS_Zinc_df$tr[iLiNS_Zinc_df$arm=="a.LNS-Zn0"] <- "Control"
iLiNS_Zinc_df$studyid <- "iLiNS-Zinc_ZvLNS"

d$tr[d$studyid=="ki1119695-PROBIT" ] <- "Maternal"
d$tr[d$studyid=="ki1119695-PROBIT" & d$arm=="Control group"] <- "Control"

d$tr[d$studyid=="ki1126311-ZVITAMBO" ] <- "VitA"
d$tr[d$studyid=="ki1126311-ZVITAMBO" & d$arm=="Placebo nippled + Placebo Oval"] <- "Control"


d$tr[d$studyid=="ki1135781-COHORTS" & d$arm=="Control"] <- "Control"
d$tr[d$studyid=="ki1135781-COHORTS" & d$arm=="Intervention"] <- "Other"

d$tr[d$studyid=="ki1148112-iLiNS-DOSE" & d$arm=="Control"] <- "Control"
d$tr[d$studyid=="ki1148112-iLiNS-DOSE" & d$arm!="Control"] <- "LNS"

d$tr[d$studyid=="ki1148112-iLiNS-DYAD-M" & d$arm=="Iron and folic acid supplementation"] <- "Control"
d$tr[d$studyid=="ki1148112-iLiNS-DYAD-M" & d$arm!="Iron and folic acid supplementation"] <- "Maternal"


iLiNS_DYADM_df <- d[d$studyid=="ki1148112-iLiNS-DYAD-M" & d$arm!="Multiple micronutrient supplementation", ]
iLiNS_DYADM_df$tr <- "Control"
iLiNS_DYADM_df$tr[iLiNS_DYADM_df$arm!="Iron and folic acid supplementation"] <- "LNS"
iLiNS_DYADM_df$studyid <- "iLiNS_DYADM_LNS"

d$tr[d$studyid=="ki1148112-LCNI-5"& d$arm=="Standard(Control)"] <- "Control"
d$tr[d$studyid=="ki1148112-LCNI-5"& (d$arm=="Milk FS"|d$arm=="Soy FS")] <- "LNS"
d$tr[d$studyid=="ki1148112-LCNI-5"& d$arm=="Likuni Phala"] <- "Other"

d$tr[d$studyid=="kiGH5241-JiVitA-3" & d$arm=="Iron Folic Acid"] <- "Control"
d$tr[d$studyid=="kiGH5241-JiVitA-3" & d$arm=="Multiple Micronutrients"] <- "Maternal"

d$tr[d$studyid=="kiGH5241-JiVitA-4"] <- "Other"
d$tr[d$studyid=="kiGH5241-JiVitA-4" & d$arm=="CFC"] <- "Control"
d$tr[d$studyid=="kiGH5241-JiVitA-4" & d$arm=="Plumpy Doz"] <- "LNS"

table(d$studyid, d$tr)
table(d$tr)


#--------------------------------------------------------
# Drop risk factors without enough studies or unneeded variables 
#--------------------------------------------------------

colnames(d)
d <- subset(d, select = -c(siteid, region,  clustid, brthweek,   brthordr, ses, birthlen2, birthwt2, birthmeas_age, birthLAZ, birthWAZ))






#--------------------------------------------------------
#Convert continious variables to quartiled categorical 
#--------------------------------------------------------



#quantiling functions
quantile_rf <- function(data, A, labs=NULL, Acuts=NULL, units=NULL){
  A<-as.numeric(A)
  if(sum(is.na(A))!=length(A)){
    if(is.null(Acuts)){
      Acuts=c(0, as.numeric(quantile(A, probs = c(.25,.5,.75), na.rm=T)), max(A, na.rm=T))
    }
    
    if(length(Acuts)==3){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0(">=",round(Acuts[2],2))) 
    }    
    if(length(Acuts)==4){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
                paste0(">=",round(Acuts[3],2))) 
    }
    if(length(Acuts)==5){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
                paste0("[",round(Acuts[3],2),"-",round(Acuts[4],2),")"), 
                paste0(">=",round(Acuts[4],2))) 
    }
    if(length(Acuts)==6){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
                paste0("[",round(Acuts[3],2),"-",round(Acuts[4],2),")"),
                paste0("[",round(Acuts[4],2),"-",round(Acuts[5],2),")"), 
                paste0(">=",round(Acuts[5],2))) 
    }    
    
    
    if(!is.null(labs)){
      Alevels=labs
    }
    if(!is.null(units)){
      Alevels=paste0(Alevels, " ", units)
    }
    
    if(length(unique(Acuts))==length((Acuts))){
      A <- cut(A, include.lowest = T, right = FALSE, breaks=Acuts,labels=Alevels)
    }else{
      A <- cut(A, include.lowest = T, right = FALSE, breaks=4,labels=c("Q1","Q2","Q3","Q4","Q5")[1:(length(Acuts)-1)])
    }
    A <- factor(A)
    
    printdf <- data.frame(id=paste0(data$studyid," ", data$country), A)
    printdf <- printdf %>% filter(!is.na(A))
    printdf <- droplevels(printdf) 
    print(table(printdf$id, printdf$A))
    
    print(table( printdf$A))
    return(A)
  }
}





#gestational age at birth
#<37 weeks = preterm
#37-38 weeks = early term
#39-40 weeks = full term (baseline)
#>=41 weeks = late/post term
#maternal BMI (is this measured when pregnant or not? if pregnant, then we may need to change these categories)
#<18.5 = underweight
#>=18.5 and <25 = normal weight (baseline)
#>=25 and <30 = overweight
#>=30 = obese
#maternal height (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3095774/)
#less than 145 cm
#145-149.9 cm
#150-154.9 cm
#155-159.9 cm
#160.0 cm or greater. (baseline)
#maternal weight?
#mother's/father's education
#highest education level = baseline
#father age
#oldest = baseline
#father height?

class(d$gagebrth)

#Save continious variables to use as risk factors
d$W_gagebrth <- d$gagebrth
d$W_birthwt <- d$birthwt
d$W_birthlen <- d$birthlen
d$W_mage <- d$mage
d$W_mhtcm <- d$mhtcm
d$W_mwtkg <- d$mwtkg
d$W_mbmi <- d$mbmi
d$W_fage <- d$fage
d$W_fhtcm <- d$fhtcm
d$W_meducyrs <- d$meducyrs
d$W_feducyrs <- d$feducyrs

d$W_nrooms <- d$nrooms
d$W_nhh <- d$nhh
d$W_nchldlt5 <- d$nchldlt5
d$W_parity <- d$parity




#Overall a-priori quantiles
d$gagebrth <- quantile_rf(d, d$W_gagebrth, Acuts=c(0,260,274,max(d$W_gagebrth, na.rm=T)), labs=c("Preterm", "Early term", "Full or late term"))
d$birthwt <- quantile_rf(d, d$W_birthwt, Acuts=c(0,2500,max(d$W_birthwt, na.rm=T)), labs=c("Low birth weight", "Normal or high birthweight"))
d$birthlen <- quantile_rf(d, d$W_birthlen, Acuts=c(0,48, 50, max(d$W_birthlen, na.rm=T)), units="cm")

d$W_mage[d$studyid=="ki1112895-Burkina Faso Zn"] <- d$W_mage[d$studyid=="ki1112895-Burkina Faso Zn"] -1 # Fix Ages in Burkino Faso Zinc, which are the upper limit of age ranges
d$mage <- quantile_rf(d, d$W_mage, Acuts=c(0,20,30,max(d$W_mage, na.rm=T)))

d$mhtcm <- quantile_rf(d, d$W_mhtcm, Acuts=c(0,151,155,max(d$W_mhtcm, na.rm=T)), units="cm")
d$mwtkg <- quantile_rf(d, d$W_mwtkg, Acuts=c(0,52,58,max(d$W_mwtkg, na.rm=T)), units="kg")
d$mbmi <- quantile_rf(d, d$W_mbmi, Acuts=c(0,18.5,25,max(d$W_mbmi, na.rm=T)), labs=c("Underweight", "Normal weight", "Overweight or Obese"))
d$fage <- quantile_rf(d, d$W_fage, Acuts=c(0,32,38,max(d$W_fage, na.rm=T)))
d$fhtcm <- quantile_rf(d, d$W_fhtcm, Acuts=c(0,162,167,max(d$W_fhtcm, na.rm=T)), units="cm")



#Make education categorizing function that handles the irregular distribution across studies.
#d<-dfull[dfull$studyid=="ki1000111-WASH-Kenya",]
# d2 <- d
# d <- d[d$studyid=="ki1066203-TanzaniaChild2",]
#  d <- d[d$studyid=="ki0047075b-MAL-ED" & d$country=="BRAZIL",]
# table(d$W_meducyrs[d$studyid=="ki1000109-EE" & d$country=="PAKISTAN"])

quantile_rf_edu <- function(d, Avar="meducyrs"){
  dfull <-d
  
  A0 <- NULL
  d <- data.frame(id=1:nrow(dfull), A=as.data.frame(dfull[,Avar])[,1])
  
  if(sum(is.na(d$A))!=length(d$A)){
    
    Acuts=c(0, as.numeric(quantile(d$A, probs = c(1/3, 2/3), na.rm=T)), max(d$A, na.rm=T))
    if(length(Acuts)==length(unique(Acuts))){
      
      Alevels=c("Low","Medium","High")
      A1 <- cut(d$A, include.lowest = T, right = T, breaks=Acuts,labels=Alevels)
      A2 <- cut(d$A, include.lowest = T, right = F, breaks=Acuts,labels=Alevels)
      rght=F
      if(min(table(A1)) >= min(table(A2))) rght=T
      A <- cut(d$A, include.lowest = T, right = rght, breaks=Acuts,labels=Alevels)
      
    }else{
      if(Acuts[2]==Acuts[3] & Acuts[2]!=0){
        A<-rep(NA, nrow(d))
        A[d$A < Acuts[2] & !is.na(d$A)] <- "Low"
        A[d$A == Acuts[2] & !is.na(d$A)] <- "Medium"
        A[d$A > Acuts[2] & !is.na(d$A)] <- "High"
        A <- factor(A, levels = c("Low","Medium","High"))
      }else{
        
        if(sum(d$A==0, na.rm=T)>0){
          A0 <- d[d$A==0,]
          A0 <- A0[!is.na(A0$A),]
          A0$A <- "Low"
        }
        
        A <- d[d$A!=0 | is.na(d$A),]
        Acuts=c(0, as.numeric(quantile(A$A, probs = 0.5, na.rm=T)), max(A$A, na.rm=T))
        Alevels=c("Medium","High")    
        A$A <- cut(A$A, include.lowest = T, right = T, breaks=Acuts,labels=Alevels)
        if(!is.null(A0)){
          df<-rbind(A,A0)
        }else{
          df<-A
        }
        
        df <- df %>% arrange(id)
        A <- factor(df$A, levels = c("Low","Medium","High"))
      }
    }
    dfull[,Avar] <- A
  }
  return(dfull)
}

d <- d %>% group_by(studyid, country) %>%
  do(quantile_rf_edu(., Avar="meducyrs"))
d <- d %>% group_by(studyid, country) %>%
  do(quantile_rf_edu(., Avar="feducyrs"))
table(d$meducyrs)
table(d$feducyrs)

table(paste0(d$studyid," ", d$country), d$meducyrs)
table(paste0(d$studyid," ", d$country), d$feducyrs)




#Categorize nrooms, nhh, nchild5
table(d$nrooms)
table(paste0(d$studyid," ", d$country), d$nrooms)
nroom<-NA
nroom[d$nrooms<2] <- "1"
nroom[d$nrooms==2] <- "2"
nroom[d$nrooms==3] <- "3"
nroom[d$nrooms>3] <- "4+"
d$nrooms <- as.factor(nroom)
table(d$nrooms)
table(paste0(d$studyid," ", d$country), d$nrooms)


table(d$nhh)  
table(paste0(d$studyid," ", d$country), d$nhh)

nhh<-NA
nhh[d$nhh<4] <- "3 or less"
nhh[d$nhh>3 & d$nhh<6] <- "4-5"
nhh[d$nhh>5 & d$nhh<8] <- "6-7"
nhh[d$nhh>7] <- "8+"
d$nhh <- as.factor(nhh)
table(d$nhh)
table(paste0(d$studyid," ", d$country), d$nhh)


table(d$nchldlt5)
table(paste0(d$studyid," ", d$country), d$nchldlt5)

nchild5<-NA
nchild5[d$nchldlt5==1] <- "1"
nchild5[d$nchldlt5>2] <- "2+"
d$nchldlt5 <- as.factor(nchild5)
table(d$nchldlt5)
table(paste0(d$studyid," ", d$country), d$nchldlt5)

d$nchldlt5 <- relevel(d$nchldlt5, ref="1")

table(d$parity)
table(paste0(d$studyid," ", d$country), d$parity)

parity<-NA
parity[d$parity==0] <- NA 
parity[d$parity==1] <- "1"
parity[d$parity==2] <- "2"
parity[d$parity>2] <- "3+"
d$parity <- as.factor(parity)
table(d$parity)
table(paste0(d$studyid," ", d$country), d$parity)



d$parity <- relevel(d$parity, ref="1")




#---------------------------------------
# Set reference levels
#---------------------------------------

#birthweight
# Low birth weight: < 2500
# healthy birth weight 2500-4200

d$birthwt <- relevel(d$birthwt, ref="Normal or high birthweight")


#birth length: 
#No WHO categories:
#Based on quantiles

d$birthlen <- relevel(d$birthlen, ref=">=50 cm")

#wealth index: 
#wealthiest quartile - Q4 is baseline
table(paste0(d$studyid," ", d$country), d$hhwealth_quart)
d$hhwealth_quart <- relevel(d$hhwealth_quart, ref="Wealth Q4")

# children < 5 in HH
#not sure how this could be zero - can you double check this? 
#baseline should be smallest number

d$nchldlt5 <- relevel(d$nchldlt5, ref="1")

#gestational age at birth
#<37 weeks = preterm
#37-38 weeks = early term
#39-40 weeks = full term (baseline)
#>=41 weeks = late/post term

d$gagebrth <- relevel(d$gagebrth, ref="Full or late term")

#maternal BMI (is this measured when pregnant or not? if pregnant, then we may need to change these categories)
#<18.5 = underweight
#>=18.5 and <25 = normal weight (baseline)
#>=25 and <30 = overweight
#>=30 = obese

d$mbmi <- relevel(d$mbmi, ref="Normal weight")

#maternal height (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3095774/)
#less than 145 cm
#145-149.9 cm
#150-154.9 cm
#155-159.9 cm
#160.0 cm or greater. (baseline)

d$mhtcm <- relevel(d$mhtcm, ref=">=155 cm")

#maternal weight?
d$mwtkg <- relevel(d$mwtkg, ref=">=58 kg")

#mother's/father's education
#lowest education level = baseline
d$meducyrs <- relevel(factor(d$meducyrs), ref="Low")
d$feducyrs <- relevel(factor(d$feducyrs), ref="Low")

#mother's age
#middle = baseline
d$mage <- relevel(d$mage, ref="[20-30)")

#father age
#oldest = baseline
d$fage <- relevel(d$fage, ref=">=38")

#father height
d$fhtcm <- relevel(d$fhtcm, ref=">=167 cm")

#parental education
d$meducyrs <- relevel(d$meducyrs, ref="High")
d$feducyrs <- relevel(d$feducyrs, ref="High")

#number of rooms
d$nrooms <- relevel(d$nrooms, ref="4+")



#Set remaining risk factors to factors
d$brthmon <- factor(d$brthmon)
d$month <- factor(d$month)
d$single <- factor(d$single)
d$vagbrth <- factor(d$vagbrth)
d$hdlvry <- factor(d$hdlvry)
d$hfoodsec <- factor(d$hfoodsec)
d$enstunt <- factor(d$enstunt)
d$sex <- factor(d$sex)
d$meducyrs <- factor(d$meducyrs)

#Check that all risk factor variables are set as factors
d<-as.data.frame(d)
for(i in 1:ncol(d)){
  cat(colnames(d)[i], ": ", class(d[,i]), "\n")
}



#Tabulate missingness
for(i in 1:ncol(d)){
  print(colnames(d)[i])
  print(table(is.na(d[,i])))
  print(levels(d[,i]))
}



#--------------------------------------------
# Check for sparsity across RF levels
#--------------------------------------------

tabRF <- function(d, Avar){
  tab <- table(paste0(d$studyid, " ",d$country), d[,Avar])
  tab <- tab[rowSums(tab)!=0, ]
  print(tab)
}





tabRF(d, "gagebrth")
tabRF(d, "birthwt") #Check the added length/weight
tabRF(d, "birthlen")
tabRF(d, "parity") #Check Tanzania child
tabRF(d, "mage")
tabRF(d, "mhtcm") #Collapse categories
tabRF(d, "mwtkg") #Collapse?
tabRF(d, "mbmi") #Add labels
tabRF(d, "fage") #Collapse?
tabRF(d, "fhtcm")
tabRF(d, "feducyrs")
tabRF(d, "nrooms")
tabRF(d, "nhh")
tabRF(d, "nchldlt5")



#--------------------------------------------
# Save dataset
#--------------------------------------------


saveRDS(d, file="FINAL_temp_clean_covariates.rds")

saveRDS(d, file="U:/UCB-SuperLearner/Stunting rallies/FINAL_temp_clean_covariates.rds")

save(iLiNS_Zinc_df, iLiNS_DYADM_df,
     file="int_studies_secondary_contrasts.Rdata")
