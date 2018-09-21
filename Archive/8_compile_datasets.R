

rm(list=ls())
library(caret)
library(tidyverse)

source("U:/GHAP-Data-Management/Wast_incidence_functions.R")
source("U:/GHAP-Data-Management/dataset_compiling_functions.R")

setwd("U:/data/WastIncDatasets")


#------------------------------------------------
# 24 month CI dataframe: uncleaned convariates
# as test dataset to clean covariates
#------------------------------------------------

compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets/",
                    filename="PooledUncleaned24mo_tempdata.Rdata", 
                    rds=F,
                    suffix="_inc",
                    dont_clean=T)



#------------------------------------------------
# Longitudinal data
#------------------------------------------------

# longitudinal data
compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=F, recoveryoutcome=F,
                    long.data=T,
                    data_location = "U:/data/",
                    file_location="U:/data/Compiled Datasets",
                    filename="CompiledLongData.Rdata", 
                    rds=T,
                    suffix="_inc")


#full longitudinal data 
compile_hbgdki_data(minage=0, maxage=999999999,
                    cum_inc=F, recoveryoutcome=F,
                    long.data=T,
                    data_location = "U:/data/",
                    file_location="U:/data/Compiled Datasets",
                    filename="CompiledLongData_AllAges.Rdata", 
                    rds=T,
                    suffix="_inc")


#full longitudinal data 
compile_hbgdki_data(minage=0, maxage=999999999,
                    cum_inc=F, recoveryoutcome=F,
                    long.data=T,
                    data_location = "U:/data/",
                    file_location="U:/data/Compiled Datasets",
                    filename="CompiledLongData_AllAges_noyearly.Rdata", 
                    rds=T,
                    suffix="_inc",
                    dont_clean=T,
                    impute_missing=F,
                    include_yearly=F)

load("U:/data/Compiled Datasets/CompiledLongData_AllAges_noyearly.Rdata")
table(d$STUDYID)
d <- d %>% subset(., select=c(STUDYID, COUNTRY, SUBJID, AGEDAYS, HAZ))
save(d, file="U:/data/Stunting/Full-compiled-data/compiled_HAZ_dataset.RData")

#------------------------------------------------
# Cumulative wasting incidence 
#------------------------------------------------

# 0-24 months
compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc0-24.Rdata",  include_yearly=F,
                    rds=F,
                    suffix="_inc")

 # load("U:/data/Compiled Datasets/WastInc0-24.Rdata")
 # table(d$STUDYID, d$tr)
# load("U:/data/WastIncDatasets/gmsn_inc.Rdata")

# 0-6 months
compile_hbgdki_data(minage=0, maxage=6*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc0-6.Rdata",  include_yearly=F,
                    rds=F,
                    suffix="_inc")

# 6-24 months
compile_hbgdki_data(minage=6*30.25, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc6-24.Rdata",  include_yearly=F,
                    rds=F,
                    suffix="_inc")

# 6-12 months
compile_hbgdki_data(minage=6*30.25, maxage=12*30.25,
                   cum_inc=T, recoveryoutcome=F,
                   data_location = "U:/data/WastIncDatasets",
                   file_location="U:/data/Compiled Datasets",
                   filename="WastInc6-12.Rdata", include_yearly=F,
                   rds=F,
                   suffix="_inc")

# 12-18 months
#compile_hbgdki_data(minage=12*30.25, maxage=18*30.25,
#                    cum_inc=T, recoveryoutcome=F,
#                    data_location = "U:/data/WastIncDatasets",
#                    file_location="U:/data/Compiled Datasets",
#                    filename="WastInc0-24.Rdata", 
#                    rds=F,
#                    suffix="_inc")

# 18-24 months
#compile_hbgdki_data(minage=18*30.25, maxage=24*30.25,
#                    cum_inc=T, recoveryoutcome=F,
#                    data_location = "U:/data/WastIncDatasets",
#                    file_location="U:/data/Compiled Datasets",
#                    filename="WastInc0-24.Rdata", 
#                    rds=F,
#                    suffix="_inc")


#------------------------------------------------
# Recovery from wasting
#------------------------------------------------

# 0-24 months
gc()
compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=F, recoveryoutcome=T,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastRec0-24.Rdata",  include_yearly=F,
                    rds=F,
                    suffix="_inc")

# 0-6 months
gc()
compile_hbgdki_data(minage=0, maxage=6*30.25,
                    cum_inc=F, recoveryoutcome=T,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastRec0-6.Rdata",  include_yearly=F,
                    rds=F,
                    suffix="_inc")

# 6-24 months
gc()
compile_hbgdki_data(minage=6*30.25, maxage=24*30.25,
                    cum_inc=F, recoveryoutcome=T,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastRec6-24.Rdata",  include_yearly=F,
                    rds=F,
                    suffix="_inc")


#------------------------------------------------
# Cumulative wasting incidence - no birthweight
#------------------------------------------------

# 0-24 months
compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc0-24_noBW.Rdata", 
                    rds=F, noBW=T,
                    suffix="_inc", include_yearly=F,
                    filesuffix="_inc_NoBirthInc")

compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc0-24.Rdata", 
                    rds=F,
                    suffix="_inc")


# 0-6 months
compile_hbgdki_data(minage=0, maxage=6*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc0-6_noBW.Rdata", 
                    rds=F, noBW=T,
                    suffix="_inc",
                    filesuffix="_inc_NoBirthInc")

# 6-24 months
compile_hbgdki_data(minage=6*30.25, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc6-24_noBW.Rdata", 
                    rds=F, noBW=T,
                    suffix="_inc",
                    filesuffix="_inc_NoBirthInc")

#------------------------------------------------
# Recovery from wasting - no birthweight
#------------------------------------------------

# 0-24 months
gc()
compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=F, recoveryoutcome=T,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastRec0-24_noBW.Rdata", 
                    rds=F,  noBW=T,
                    suffix="_inc")

#------------------------------------------------
# Prevalent wasting and stunting at 2 years
#------------------------------------------------

compile_hbgdki_data(age=24*30.25, agerange=c(12*30.25, 36*30.25),
                    cum_inc=F, recoveryoutcome=F,
                    data_location = "U:/data/",
                    file_location="U:/data/Compiled Datasets",
                    filename="PrevStunt24.Rdata", 
                    rds=T,
                    suffix=NULL)



#------------------------------------------------
# Cumulative wasting incidence - no imputation
#------------------------------------------------

# 0-24 months
compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc0-24_noImpute.Rdata", 
                    rds=F,
                    suffix="_inc",
                    impute_missing = F)


#Create tlapp demo data for Jeremy
library(tidyverse)
load("U:/data/Compiled Datasets/WastInc0-24_noImpute.Rdata")

colnames(d)
unique(d$STUDYID)


table(d$MEDUCYRS)
table(cut_number(d$MEDUCYRS,4))



d <- d %>% filter(is.na(tr) | tr=="C") %>%
  subset(., select= c(
    STUDYID, COUNTRY, SUBJID,AGEDAYS, WHZ, wast_inc, SEX, enrolstunt, BIRTHWT,
    MAGE, MHTCM, MWTKG, MBMI, MEDUCYRS, BIRTHLEN, GAGEBRTH, FAGE, FEDUCYRS,
    birthmonth,         homedelivery,       vagbirth, single,
    HHwealth_quart,     nroom, nchild5, chicken, cow, improved.floor,
    improved.sanitation,safe.water,         treat.water,        cleancook
  )) %>%
  filter(
    STUDYID!="ki1000107-Serrinha-VitA" & 
      STUDYID!="ki1000111-WASH-Kenya" & 
      STUDYID!="ki1000110-WASH-Bangladesh" & 
      STUDYID!="ki1112895-Burkina Faso Zn" &    
      STUDYID!="ki1000304-EU" & 
      STUDYID!="ki1000304-ZnMort" & 
      STUDYID!="ki1148112-iLiNS-DOSE" & 
      STUDYID!="ki1148112-iLiNS-DYAD-M" & 
      STUDYID!="ki1000304-VITAMIN-A" & 
      STUDYID!="ki1000304-Vitamin-B12" 
  ) %>%
  filter(
    !(STUDYID=="ki1135781-COHORTS" & COUNTRY=="SOUTH AFRICA") &
      !(STUDYID=="ki1135781-COHORTS" & COUNTRY=="BRAZIL") 
  ) %>%
  mutate(BIRTHWT= ntile(BIRTHWT,4),
         MAGE= ntile(MAGE,4),
         MHTCM= ntile(MHTCM,4),
         MWTKG= ntile(MWTKG,4),
         MBMI= ntile(MBMI,4),
         BIRTHLEN= ntile(BIRTHLEN,4),
         GAGEBRTH= ntile(GAGEBRTH,4),
         FAGE= ntile(FAGE,4),
         MEDUCYRS= ntile(MEDUCYRS,4)
  ) 

#remove grant identifier
d$STUDYID<- gsub("^k.*?-" , "", d$STUDYID)
d$STUDYID <- paste0(d$STUDYID, " ", d$COUNTRY)
d <- subset(d, select = -c(COUNTRY))

saveRDS(d, file="U:/UCB-SuperLearner/tlapp-demo/tlapp_demo.rds")









