


rm(list=ls())
library(tidyverse)

#merge outcomes with covariates

# setwd("U:/UCB-SuperLearner/Stunting rallies/")
setwd("U:/ucb-superlearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_temp_clean_covariates.rds")

#create country-cohort variable
cov$cohort <- paste0(cov$studyid,"-",cov$country)


#load outcomes
load("st_prev.rdata")
load("st_cuminc.rdata")
load("st_rec.rdata")



#convert subjid to character for the merge with covariate dataset
cov$subjid <- as.character(cov$subjid)
prev$subjid <- as.character(prev$subjid)
cuminc$subjid <- as.character(cuminc$subjid)
rev$subjid <- as.character(rev$subjid)

#create country-cohort variable
cuminc$cohort <- paste0(cuminc$studyid,"-",cuminc$country)
prev$cohort <- paste0(prev$studyid,"-",prev$country)
rev$cohort <- paste0(rev$studyid,"-",rev$country)


#------------------------------------
# Create cumulative incidence dataset
#------------------------------------

#merge in covariates
cuminc <- left_join(cuminc, cov, by=c("studyid", "subjid", "country","cohort"))
prev <- left_join(prev, cov, by=c("studyid", "subjid", "country","cohort"))
rev <- left_join(rev, cov, by=c("studyid", "subjid", "country","cohort"))


#Vector of outcome names
Y<-c("ever_stunted")
Y<-c("stunted","sstunted")
Y<-c("s03rec24")


#Vector of risk factor names
A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec")



#colnames(cov)
cov<-subset(cov, select=-c(month, country, brthmon, id))
#colnames(cov)[3]
#table(cov$cohort, cov[,3])

A<-data.frame(cuminc[,which(colnames(cuminc) %in% colnames(cov)[3])])
#table(cuminc$cohort, A[,1])



two.by.two<-function(df, Y, A, cv=cov){
  colnames(df)[which(colnames(df) %in% Y)] <- "Y"
  colnames(df)[which(colnames(df) %in% A)] <- "A"
  
  tab<-table(df$Y, df$A)
  Anames <- colnames(tab)
  Ynames <- rownames(tab)
  
  tabnames <- paste0("A=",rep(Anames,each=2),", Y=",rep(Ynames, length(Anames)))
  
  tabrow<-NULL
  
  for(i in 1:ncol(tab)){
    tabrow <- c(tabrow, tab[,i])
  }
  tabrow <- data.frame(t(tabrow))
  colnames(tabrow) <- tabnames
  
  return(tabrow)
}


create_tables <- function(df, Yvar, cv=cov, Avar=Avars){
  
  
  for(i in 1:length(Avar)){
    
    tab<-table(cv$cohort, is.na(cv[,Avar[i]]))
    if(dim(tab)[2]==1){
      tab<-data.frame(cohort=rownames(tab), Missing.N= tab[,1])
    }else{
      tab<-data.frame(cohort=rownames(tab), Missing.N= tab[,1], Measured.N= tab[,2])
      
    }
    cat("\n\n\n## Outcome: ", Yvar,"\n")
    cat("### Risk factor: ",Avar[i],"\n")
    
    res <- df %>% group_by(agecat,cohort) %>% 
      do(two.by.two(.,Y=Yvar, A=Avar[i]))
    #res <- res[rowSums(res[,-c(1:2)])>0,]
    res <- left_join(res, tab, by="cohort")
    print(knitr::kable(res, digits = 1))
  }
}



Avars <- c( "sex", "gagebrth",      "birthwt",      
            "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
            "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
            "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
            "feducyrs", "hfoodsec")


create_tables(df = cuminc , Yvar = "ever_stunted")

