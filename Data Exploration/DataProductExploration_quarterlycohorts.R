



rm(list=ls())
library(tidyverse)
library(data.table)
library(scales)

source("U:/GHAP-Data-Management/HBGDki_functions.R")


setwd("U:/data")



#Read rds file and drop unneeded columns
d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T,
         drop = c("AGEDAYS", "AGEIMPFL","MONTH",   "WTKG",    "HTCM",    "LENCM",   "WAZ",     "HAZ",  
                  "WHZ",     "BAZ",     "HCAZ",    "MUAZ",    "SITEID", 
                  "REGCTRY", "REGCTYP", "CITYTOWN","LATITUDE","LONGITUD", "HHID",    "ARM", 
                  "DEAD",    "AGEDTH",  "CAUSEDTH","FEEDING",
                  "DURBRST", "BRTHYR",  "BRTHWEEK","BRTHMON",
                  "BRFEED", 
                  "SUMEP",   "SUMDIAR", "SUMDAYS",
                  "PCTDIAR",  "INCTOT", 
                  "INCTOTU", "BFEDFL",  "EXBFEDFL","WEANFL",  "ANMLKFL", "PWMLKFL",
                  "FORMLKFL","BOTTLEFL","H20FEDFL","OTHFEDFL","SLDFEDFL","NBFYES",  "EARLYBF", "CMFDINT", "DIARFL",  "LSSTLFL",
                  "NUMLS",   "BLDSTLFL","DIARFL_R","LSSTFL_R","NUMLS_R", "BLDSTL_R",
                  "DUR_R"))
colnames(d) <- tolower(colnames(d))
gc()

#Drop studies Vishak added to data product that don't meet inclusion criteria
#d <- d %>% filter(studyid!="ki1000301-DIVIDS" & studyid!="ki1055867-WomenFirst" & studyid!="ki1135782-INCAP")
d <- d[studyid!="ki1000301-DIVIDS" & studyid!="ki1055867-WomenFirst" & studyid!="ki1135782-INCAP"]

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


#Keep monthly and quarterly studies
d <- d %>% filter(measurefreq!="yearly")

#Subset to control arms for intervention studies
d <- filter(d, tr=="Control" | tr=="")


#Keep one observation per child
d <- d %>% group_by(studyid, country, subjid) %>% slice(1) 


#Drop un-needed columns
colnames(d)
country<-d$country
d <- subset(d, select= -c(country))


d<-as.data.frame(d)
for(i in 1:ncol(d)){
  cat(colnames(d)[i],": ",class(d[,i]), "\n")
}

gc()
for(i in 3:ncol(d)){
  d[,i] <-ifelse(d[,i]=="",NA,d[,i])
  if(class(d[,i])=="character"){
    d[,i] <- as.factor(d[,i])
  }
}





pdf("U:/results/diagnostic figures/Data_prod_density_plots_quarterly_cohorts.pdf", width=10,height=8.5, paper="USr")

for(i in 3:ncol(d)){
  varname<-colnames(d)[i]
  
  if(length(unique(d[,i]))>20){
    
    var <- data.frame(STUDYID=paste0(d$studyid," ", country), X=as.numeric(d[,i]))
    
    p <- ggplot(var, aes(x=X)) + geom_density() + facet_wrap(~STUDYID) + ggtitle(varname)
    print(p)
  }else{
    var <- data.frame(STUDYID=paste0(d$studyid," ", country), X=as.character(d[,i]))
    
    p <- ggplot(var, aes(x=X)) +
      #geom_bar(aes(y=..count../sum(..count..), group = 1, fill = X)) + 
      geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = X)) + 
      facet_wrap(~STUDYID) + ggtitle(varname) + scale_y_continuous(labels=percent_format()) +
      theme(axis.text.x = element_text(angle=45,size=10))
    print(p)
  }
}

dev.off()



