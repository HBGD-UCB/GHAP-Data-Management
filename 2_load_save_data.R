

#------------------------------------------------------
# Author: Andrew Mertens
# amertens@berkeley.edu
#
# Script to download/load select datasets into R using
# the ghap() package, save as  rds file, and fill in
# time-static covariates across all observations of
# each child.
# (already done for some studies, not others)
#
#-----------------------------------------------------




rm(list=ls())
library(dplyr)
library(tidyr)
library(caret)
library(ghap)

setwd("U:/data")
set_git_base_path("U:/git")
get_git_base_path()

#Create function to fill in static covariates
impute_static_vars<-function(d){
  
  
  varlist<-c("STUDYID","SUBJID", "shortid",
             "SITEID","SEXN",
             "SEX","AGEDAYS",
             "CTRYCD","COUNTRY",
             "WTKG","WAZ",
             "HAZ","BAZ",
             "BMI","WHZ",
             "LENCM","LATITUDE",
             "LONGITUD","HTCM",
             "SUBJIDO","CITYTOWN",
             "ELEVATN","BIRTHWT",
             "MUACCM","MAGE",
             "MUAZ","MEDUCYRS",
             "HCIRCM","GAGEBRTH",
             "ARMCD","ARM",
             "SANITATN","NPERSON",
             "MHTCM",
             "BIRTHLEN","FEEDINGN",
             "FEEDING","TV",
             "BRTHYR","REGCTRY",
             "RADIO",
             "SUMEP","SUMDIAR",
             "SUMDAYS","PCTDIAR",
             "DLVLOC",
             "BRTHWEEK","FLOOR",
             "DELIVERY",
             "AGEDTH","FEDUCYRS",
             "PARITY","NROOMS",
             "CAR","COOKFUEL",
             "WALL",
             "VISITNUM","VISIT",
             "BICYCLE","MWORK",
             "H2OSRCP","ELEC",
             "FRIG","GAGEDAYS",
             "DEAD","NCHLDLT5",
             "ROOF",
             "MMARITN","MMARIT",
             "H2OTRTP","MWTKG",
             "MCYCLE","INCTOT",
             "APGAR5","FWORK",
             "H2OSRC","GOAT",
             "BIRTHHC","DURBRST",
             "MBMI","SEWING",
             "NCHLD","FAGE",
             "NLCHILD","PHONE",
             "COW",
             "APGAR1",
             "SMOKED","AGLAND",
             "COUGHFL","CHCIRCM",
             "LLPHONE","BED",
             "CLTHCAB","COOKPLAC",
             "NADULT","FHTCM",
             "CHICKEN","VISITDY",
             "SESN","SES",
             "SHEEP","MULTBRTH",
             "WATCH","CHAIR",
             "TABLE","STRATUMN",
             "STRATUM","EDUCCRGV",
             "RACE",
             "APGAR10","M_WTKG",
             "BFEDFL","DOGS",
             "CATTLE","H2OSRCC",
             "MRACE","METHNIC",
             "OWNHOME",
             "FAN",
             "ANIMALS","CART",
             "WASHMAC","SMOKAMT",
             "NFCHILD","NMCHILD",
             "H2OSRCB",
             "ETHNIC","M_HTCM",
             "FWTKG","FHOUSEH",
             "CATS",
             "AGLANDSZ",
             "FOODDFCT",
             "NUMHCVIS",
             "PREGOUT","RICKSHAW",
             "MATTRESS","H2OAVAIL",
             "ANTPTNUM",
             "ANTPT","MAGE1ST",
             "NSLEEP","KITCHDSC",
             "M_BMI",
             "PREECLMP",
             "SOAP","EDUCHH",
             "MSMKSTAT",
             "RODENTS",
             "VITD",
             "H2OSRCS",
             "FOWL","STOVE",
             "MHOUSEH","H2OFREQ",
             "H2OUNTRT","WASHCCHD",
             "WASHCOOK","WASHNURS",
             "R_MUAZ",
             "BEDNET","H2OSRCK",
             "FRACE",
             "SINK","SOFA",
             "NBEDROOM",
             "FSMKSTAT",
             "COHORTN","COHORT",
             "EDUCFAM",
             "INCMOM","PIGS",
             "CASTE",
             "MBRTHWT",
             "MTRIBE","FTRIBE",
             "WTKGM","HTCMM",
             "BMIM","WAZM",
             "HAZM","WHZM",
             "BAZM",
             "SMOKSTAT","SMOKYRS",
             "DIARFL",
             #WBK variables
             "STUDYID", "SUBJID",  "CLUSTID", "SITEID",  "HHID",    "SEXN",    "SEX",     "ARMCD",   "ARM",     "STRATUMN","STRATUM",
             "COHORTN", "COHORT",  "BRTHYR",  "BRTHWEEK","MHTCM",   "MEDUCYRS","CTRYCD",  "COUNTRY", "BICYCLE", "CAR",     "CHICKEN",
             "COW",     "DOGS",    "ELEC",    "GOAT",    "MOBILE",  "MCYCLE",  "HUNGER",  "RADIO",   "STOVE",   "TV",      "H2OTIME",
             "NCOMP",   "WATCH",   "FLOOR",   "COOKFUEL","ROOF",    "SANITATN","WALL",    "CHLDLT18","AGEDAYS", "VISITNUM","VISIT",  
             "WTKG",    "LENCM",   "BMI",     "HCIRCM",  "WAZ",     "HAZ",     "WHZ",     "BAZ",     "HCAZ",    "FREECHL", "IMPRLAT",
             "WATSOAP", "LNSN",    "LNSP",    "FCSAFDIS",
             #additional diarrhea vars
             "DIARFL","SUMDIAR",  
             "DIARDAYS", "CSUMDIAR",
             "DIAREPS" , "DIARBFEN" ,
             "DIARRHOEA","DIARRHOEA_NEONATAL")
  
  
  dynamicvars<-c(
    "AGEDAYS",
    "WTKG","WAZ",
    "HAZ","BAZ",
    "BMI","WHZ",
    "LENCM","HTCM",
    "HCIRCM",
    "SUMEP","SUMDIAR",
    "SUMDAYS","PCTDIAR",
    "VISITNUM","VISIT",
    "DEAD","DURBRST",
    "COUGHFL","CHCIRCM",
    "VISITDY",
    "ANTPTNUM",
    "FREECHL", "IMPRLAT",
    "WATSOAP", "LNSN",    "LNSP",    "FCSAFDIS",
    #additional diarrhea vars
    "DIARFL","SUMDIAR",  
    "DIARDAYS", "CSUMDIAR",
    "DIAREPS" , "DIARBFEN" ,
    "DIARRHOEA","DIARRHOEA_NEONATAL")
  
  
  
  
  
  
  
  staticvars<-varlist[-which(varlist %in% dynamicvars)]
  
  study.d <- d %>% filter(!is.na(AGEDAYS) & !is.na(HAZ))
  
  if(!is.null(varlist)){
    study.d<-study.d[,which(colnames(study.d) %in% varlist)]
  }
  study.d <- apply(study.d, 2, as.character)
  study.d<-as.data.frame(study.d)
  
  
  #Set "" and other missing codes to missing
  for(i in 1:ncol(study.d)){
    study.d[,i]<-ifelse(study.d[,i]=="",NA,as.character(study.d[,i]))
  } 
  
  #seperate out dynamic variables
  study.d.varying<-study.d[,which(colnames(study.d) %in% c("SUBJID", "AGEDAYS", dynamicvars))]
  
  #fill in missing static variables
  study.d.static<-study.d[,which(colnames(study.d) %in% c("SUBJID", "AGEDAYS", staticvars))]
  
  study.d.static<-study.d.static %>%  
    group_by(SUBJID) %>%
    do(fill(.,everything())) %>% 
    do(fill(.,everything(), .direction = 'up')) 
  
  study.d <- merge(study.d.static, study.d.varying, by=c("SUBJID","AGEDAYS"))
  
  study.d$AGEDAYS<-as.numeric(as.character(study.d$AGEDAYS))
  study.d$HAZ<-as.numeric(as.character(study.d$HAZ))
  study.d$WHZ<-as.numeric(as.character(study.d$WHZ))
  study.d$SUBJID<-as.numeric(as.character(study.d$SUBJID))
  
  return(study.d)
}



setwd("U:/data/")


d<-read.csv("U:/data/Serrinha-VitA/Serrinha-VitA/adam/full_ki1000107_Serrinha_VitA.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="svta_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="svta.rds")
rm(d) 



d<-use_study("Vitamin-B12")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="vb12_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="vb12.rds")
rm(d) 

d<-use_study("agakhanuniv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="akup_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="akup.rds")
rm(d) 


#d<-use_study("burkina_faso_zn")
d<-read.csv("U:/data/BurkinaFasoZn/adam/ANTHA.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bfzn_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="bfzn.rds")
rm(d) 

d<-use_study("cmc_v_bcs_2002") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmc_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="cmc.rds")
rm(d)  

d<-use_study("cmin")       
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmin_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="cmin.rds")
rm(d) 

d<-use_study("cohorts")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cort_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="cort.rds")
rm(d) 

d<-use_study("content")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cntt_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="cntt.rds")
rm(d) 

d<-use_study("ee")             
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ee_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="ee.rds")
rm(d)   

d<-use_study("eu") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="eu_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="eu.rds")
rm(d)   

d<-use_study("gms_nepal")      
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gmsn_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="gmsn.rds")
rm(d)

# d<-use_study("gusto")          
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="gsto_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="gsto.rds")
# rm(d) 

d<-use_study("guatemala_bsc")  
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gbsc_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="gbsc.rds")
rm(d) 

d<-use_study("irc")            
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="irc_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="irc.rds")
rm(d)  

d<-use_study("jivita_3")       
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="jvt3_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="jvt3.rds")
rm(d) 

d<-use_study("jivita_4")       
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="jvt4_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="jvt4.rds")

d<-use_study("keneba")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="knba_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="knba.rds")
rm(d) 

d<-use_study("lcni_5")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="lcn5_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="lcn5.rds")
rm(d) 

#d<-use_study("mal_ed") #Mal ED not loading
#Temp fix
d<-read.csv("U:/data/MALED-201501/adam/trellisdat.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="mled_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="mled.rds")
rm(d) 

d<-use_study("nih_birth")      
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="nbrt_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="nbrt.rds")
rm(d) 

d<-use_study("probit")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="prbt_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="prbt.rds")
rm(d) 

#d<-use_study("peru_huascar") 
d<-read.csv("U:/data/PMID2646919/adam/ANTHA.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="phua_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="phua.rds")
rm(d) 

d<-use_study("respak")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="rspk_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="rspk.rds")
rm(d) 

d<-use_study("sas_compfeed")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmpf_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="cmpf.rds")
rm(d) 

d<-use_study("sas_foodsuppl")  
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="fspp_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="fspp.rds")
rm(d) 

d<-use_study("tdc")            
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="tdc_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="tdc.rds")
rm(d)  

d<-use_study("tanzaniachild2") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="tzc2_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="tzc2.rds")
rm(d) 

d<-use_study("zvitambo")      
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zvit_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="zvit.rds")
rm(d) 

d<-use_study("znmort")        
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zmrt_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="zmrt.rds")
rm(d) 



#d<-use_study("ilins_zinc") 
d<-read.csv("U:/data/iLiNS-Zinc/adam/ads_full_KI1112895_ILINS.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="lnsz_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="lnsz.rds")
rm(d)



d<-use_study("wash_bangladesh") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="wsb_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="wsb.rds")
rm(d)


d<-use_study("wash_kenya") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="wsk_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="wsk.rds")
rm(d)


# d<-use_study("bigcs_ultrasound") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="bigu_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="bigu.rds")
# rm(d)

d<-use_study("ilins_dose") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ilnd_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="ilnd.rds")
rm(d)

d<-use_study("ilins_dyad_m") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ildm_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="ildm.rds")
rm(d)

d<-use_study("imnci") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="imnc_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="imnc.rds")
rm(d)

# d<-use_study("amanhi") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="amni_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="amni.rds")
# rm(d)

# d<-use_study("peru_zn") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="pzn_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="pzn.rds")
# rm(d)

# d<-use_study("Ecuador Egg") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="eegg_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="eegg.rds")
# rm(d)

# d<-use_study("Bangladesh Diarrhea") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="bngd_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="bngd.rds")
# rm(d)





#Add studies neeeded to be added to metadata
d<-use_study("ncry") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ncry_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="ncry.rds")
rm(d)


d<-use_study("zinf") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zinf_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="zinf.rds")
rm(d)


# d<-use_study("gual") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="gual_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="gual.rds")
# rm(d)


# d<-use_study("ppd") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="ppd_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="ppd.rds")
# rm(d)


# d<-use_study("mahn") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="mahn_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="mahn.rds")
# rm(d)


# d<-use_study("incp") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="incp_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="incp.rds")
# rm(d)


# d<-use_study("gsto") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="gsto_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="gsto.rds")
# rm(d)
# 
# 
# d<-use_study("grip") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="grip_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="grip.rds")
# rm(d)


# d<-use_study("gtwn") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="gtwn_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="gtwn.rds")
# rm(d)


# d<-use_study("eczn") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="akup_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="eczn.rds")
# rm(d)

d<-use_study("prvd")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="prvd_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="prvd.rds")
rm(d)


# d<-use_study("mmam") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="mmam_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="mmam.rds")
# rm(d)


d<-use_study("dvds") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="dvds_raw.rds")
d <- impute_static_vars(d)
saveRDS(d, file="dvds.rds")
rm(d)


# d<-use_study("bigu") 
# colnames(d)<- toupper(colnames(d))
# saveRDS(d, file="bigu_raw.rds")
# d <- impute_static_vars(d)
# saveRDS(d, file="bigu.rds")
# rm(d)



