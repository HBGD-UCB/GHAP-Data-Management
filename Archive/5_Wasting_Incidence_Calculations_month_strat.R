

rm(list=ls())
library(SuperLearner)
library(caret)
library(tidyverse)
theme_set(theme_bw())

setwd("U:/R scripts")
source("Wast_incidence_functions.R")

setwd("U:/data")

means<-NULL
inc.df<-NULL

WastIncCalc_monthStrat <- function(study,  country=NULL){
  
  meandf <- incdf <- NULL
  
  d<-readRDS(paste0(study,".rds"))
  
  if(!is.null(country)){
    d <- d %>% filter(COUNTRY==country)
  }
  
  d<-as.data.frame(d)
  if("BRTHWEEK" %in% colnames(d)){
    d$BRTHWEEK <-as.numeric(as.character(d$BRTHWEEK))
    d$studyday <- d$BRTHWEEK*7 + d$AGEDAYS
    d$month <- floor(d$BRTHWEEK/4 + d$AGEDAYS/30.25)
    d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
    d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
    d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
    d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
    d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
    d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
    
    d <- subset(d, select = c(STUDYID, SUBJID, COUNTRY, AGEDAYS, WHZ, studyday, month))
    
    for(i in 1:12){
      inc <- NULL
      df <- d[d$month==i,]
      inc <- WastIncCalc(df)
      temp <- WastIncTable(inc)$means 
      temp$month <- i
      temp$country_cohort <- paste0(df$STUDYID[1]," ",df$COUNTRY[1])
        inc$country_cohort <- paste0(df$STUDYID[1]," ",df$COUNTRY[1])
      incdf <- rbind(incdf, inc)
      meandf <- rbind(meandf, temp)
    }
    return(list(meandf, incdf))
  }else{
    return(list(meandf=NULL, incdf=NULL))
  }
}




res<-WastIncCalc_monthStrat("gmsn")
means <- rbind(means,res[[1]])
inc.df <-  rbind(inc.df,res[[2]])
rm(res)
# res<-WastIncCalc_monthStrat("zmrt")
# means <- rbind(means,res[[1]])
#rm(res)
# ##inc.df <- rbind(inc.df,res[[2]])

res<-WastIncCalc_monthStrat("cntt")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("gbsc")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("cmin", country="PERU")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

# res<-WastIncCalc_monthStrat("cmin", country="BRAZIL")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])

# res<-WastIncCalc_monthStrat("cmin", country="GUINEA BISSAU")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])

res<-WastIncCalc_monthStrat("cmin", country="BANGLADESH")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("phua")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("tzc2")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("cmc") 
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("ee")  
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("irc") 
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("tdc")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("rspk")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)


res<-WastIncCalc_monthStrat("mled", country="BANGLADESH")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("mled", country="BRAZIL")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("mled", country="INDIA")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("mled", country="NEPAL")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("mled", country="PERU")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("mled", country="SOUTH AFRICA")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

res<-WastIncCalc_monthStrat("mled", country="TANZANIA, UNITED REPUBLIC OF")
means <- rbind(means,res[[1]])
inc.df <- rbind(inc.df,res[[2]])
rm(res)

save(means, inc.df, file="WastIncDatasets/WastInc_res_monthStrat.Rdata")


# res<-WastIncCalc_monthStrat("knba")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# #Provide: GHAP metadata mentions obsolete: need to figure out
# res<-WastIncCalc_monthStrat("prvd")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("phua")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("rspk")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("cmpf")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("fspp")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# 
# res<-WastIncCalc_monthStrat("zvit")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("lnsz")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 





# res<-WastIncCalc_monthStrat("akup")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("bfzn")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# gc()
# 
# res<-WastIncCalc_monthStrat("cmc")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("cntt")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("ee")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("eu")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("gsto")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# gc()
# 
# 
# res<-WastIncCalc_monthStrat("gbsc")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# 
# res<-WastIncCalc_monthStrat("jvt3")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# gc()
# 
# 
# 
# res<-WastIncCalc_monthStrat("jvt4")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# gc()
# 
# 
# 
# res<-WastIncCalc_monthStrat("lcn5")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("nbrt")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# 
# res<-WastIncCalc_monthStrat("prbt")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# gc()
# 
# 
# #need to run:
# res<-WastIncCalc_monthStrat("cort")
# 
# cort_inc_brazil<-WastIncCalc_monthStrat(d[d$COUNTRY==unique(d$COUNTRY)[1],])
# cort_inc_guatemala<-WastIncCalc_monthStrat(d[d$COUNTRY==unique(d$COUNTRY)[2],])
# cort_inc_india<-WastIncCalc_monthStrat(d)
# cort_inc_philippines<-WastIncCalc_monthStrat(d[d$COUNTRY==unique(d$COUNTRY)[4],])
# cort_inc_southafrica <- WastIncCalc_monthStrat(d[d$COUNTRY==unique(d$COUNTRY)[5],])
# save(cort_inc_monthStrat_brazil, 
#      cort_inc_monthStrat_guatemala,
#      cort_inc_monthStrat_india,
#      cort_inc_monthStrat_philippines,
#      cort_inc_monthStrat_southafrica,
#      file="WastIncDatasets_monthStrat/cort_inc_monthStrat.Rdata")
# gc()




# 
# #Newly added 10/17
# 
# res<-WastIncCalc_monthStrat("ncry")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# 
# res<-WastIncCalc_monthStrat("incp")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("eczn")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("prvd")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# 
# res<-WastIncCalc_monthStrat("dvds")
# means <- rbind(means,res[[1]])
rm(res)
# #inc.df <- rbind(inc.df,res[[2]])


# #Add in yearly trials
# res<-WastIncCalc_monthStrat("wsb")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# save( wsb_inc_monthStrat, file="WastIncDatasets_monthStrat/wsb_inc_monthStrat.Rdata")
# 
# res<-WastIncCalc_monthStrat("wsk")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# save( wsk_inc_monthStrat, file="WastIncDatasets_monthStrat/wsk_inc_monthStrat.Rdata")
# 
# res<-WastIncCalc_monthStrat("ilnd")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])
# 
# res<-WastIncCalc_monthStrat("ildm")
# means <- rbind(means,res[[1]])
#rm(res)
# #inc.df <- rbind(inc.df,res[[2]])



