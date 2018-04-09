

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


#------------------------------------------------
# Cumulative wasting incidence 
#------------------------------------------------

# 0-24 months
compile_hbgdki_data(minage=0, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc0-24.Rdata", 
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
                    filename="WastInc0-6.Rdata", 
                    rds=F,
                    suffix="_inc")

# 6-24 months
compile_hbgdki_data(minage=6*30.25, maxage=24*30.25,
                    cum_inc=T, recoveryoutcome=F,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastInc6-24.Rdata", 
                    rds=F,
                    suffix="_inc")

# 6-12 months
#compile_hbgdki_data(minage=6*30.25, maxage=12*30.25,
#                    cum_inc=T, recoveryoutcome=F,
#                    data_location = "U:/data/WastIncDatasets",
#                    file_location="U:/data/Compiled Datasets",
#                    filename="WastInc0-24.Rdata", 
#                    rds=F,
#                    suffix="_inc")

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
                    filename="WastRec0-24.Rdata", 
                    rds=F,
                    suffix="_inc")

# 0-6 months
gc()
compile_hbgdki_data(minage=0, maxage=6*30.25,
                    cum_inc=F, recoveryoutcome=T,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastRec0-6.Rdata", 
                    rds=F,
                    suffix="_inc")

# 6-24 months
gc()
compile_hbgdki_data(minage=6*30.25, maxage=24*30.25,
                    cum_inc=F, recoveryoutcome=T,
                    data_location = "U:/data/WastIncDatasets",
                    file_location="U:/data/Compiled Datasets",
                    filename="WastRec6-24.Rdata", 
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
                    suffix="_inc",
                    filesuffix="_inc_NoBirthInc")

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
















