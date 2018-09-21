




rm(list=ls())
library(ghap)
library(dplyr)
library(stringr)

set_git_base_path("U:/git")
get_git_base_path()
setwd("U:/data/")

#astudies <- as.data.frame(get_study_list_anthro())
astudies <- as.data.frame(get_study_list())



# drop 1 duplicate record for PROVIDE
astudies <- astudies[!duplicated(astudies$fstudy_id),]

#Count number of countries in each study
astudies$num_countries <- str_count(astudies$country, ",") + 1
astudies$num_countries[is.na(astudies$num_countries)] <- 1
num_cohorts <- sum(astudies$num_countries)
table(astudies$num_countries)



#Duplicate rows of data table to have row per country in a cohort
astudies$index<-1:nrow(astudies)
astudies <- rbind(astudies %>% mutate(cohortnum=1),
                  astudies %>% filter(num_countries>1) %>% mutate(cohortnum=2), 
                  astudies %>% filter(num_countries>2) %>% mutate(cohortnum=3), 
                  astudies %>% filter(num_countries>3) %>% mutate(cohortnum=4), 
                  astudies %>% filter(num_countries>4) %>% mutate(cohortnum=5), 
                  astudies %>% filter(num_countries>5) %>% mutate(cohortnum=6), 
                  astudies %>% filter(num_countries>6) %>% mutate(cohortnum=7), 
                  astudies %>% filter(num_countries>7) %>% mutate(cohortnum=8)) %>%
  arrange(index)




#Calculate prevalence of wasting,
# number of countries
# number of children
# mean number of observations per child,
# presence of date variable
# presence of mortality variable
# min/ max 
# monthly wasting prevalence

v<-rep(NA, num_cohorts)
df<- cbind(astudies, data.frame(numcountry=v, countrycohort=v, wastprev=v, numsubj=v, numobs=v, median_length_between_measures=v, sd_obs=v, minages=v, maxages=v, mortality=v, birthweek=v, RCT=v, diar=v,
                                HAZsd=v, WAZsd=v, WHZsd=v, HAZsd_no_outliers=v, WAZsd_no_outliers=v, WHZsd_no_outliers=v, perc_length_decrease=v,
                                wastprev_m1=v, wastprev_m2=v, wastprev_m3=v, wastprev_m4=v, wastprev_m5=v, wastprev_m6=v, wastprev_m7=v, wastprev_m8=v,
                                wastprev_m9=v, wastprev_m10=v, wastprev_m11=v, wastprev_m12=v, wastprev_m13=v, wastprev_m14=v, wastprev_m15=v, wastprev_m16=v,
                                wastprev_m17=v, wastprev_m18=v, wastprev_m19=v, wastprev_m20=v, wastprev_m21=v, wastprev_m22=v, wastprev_m23=v, wastprev_m24=v, 
                                n1=v, n2=v, n3=v, n4=v, n5=v, n6=v, n7=v, n8=v,
                                n9=v, n10=v, n11=v, n12=v, n13=v, n14=v, n15=v, n16=v,
                                n17=v, n18=v, n19=v, n20=v, n21=v, n22=v, n23=v, n24=v,
                                variables=v))


for(i in 1:nrow(df)){
  res<-NULL
  numcountry<- countrycohort <- wastprev<- numsubj<- numobs<- minages<- maxages<- mortality<-sd_obs<-median_length_between_measures<- birthweek<- RCT<-diar<-variables<-NA
  HAZsd <- WAZsd <- WHZsd <- HAZsd_no_outliers <- WAZsd_no_outliers <- WHZsd_no_outliers <- perc_length_decrease <- NA
  wastprev_m1<- wastprev_m2<- wastprev_m3<- wastprev_m4<- wastprev_m5<- wastprev_m6<- wastprev_m7<- wastprev_m8<-NA
  wastprev_m9<- wastprev_m10<- wastprev_m11<- wastprev_m12<- wastprev_m13<- wastprev_m14<- wastprev_m15<- wastprev_m16<-NA
  wastprev_m17<- wastprev_m18<- wastprev_m19<- wastprev_m20<- wastprev_m21<- wastprev_m22<- wastprev_m23<- wastprev_m24<-NA
  n1<- n2<- n3<- n4<- n5<- n6<- n7<- n8<-NA
  n9<- n10<- n11<- n12<- n13<- n14<- n15<- n16<-NA
  n17<- n18<- n19<- n20<- n21<- n22<- n23<- n24<-NA
  
  
  d<-NULL
  try(d <- readRDS(paste0(df$short_id[i],".rds")))
  
  if(!is.null(d)){
    d<-d[!is.na(d$WHZ),]
    
    #Subset to ages 0-24months
    d<-d[d$AGEDAYS>=0 & d$AGEDAYS < 24*30,]
    
    numcountry<-length(unique(d$COUNTRY))
    
    
    #Subset to country-cohort
    d<-d[as.character(d$COUNTRY)==unique(as.character(d$COUNTRY))[df$cohortnum[i]],]
    
    countrycohort <- unique(as.character(d$COUNTRY))
    
    wastprev<- mean(as.numeric(d$WHZ < (-2)), na.rm=T)*100
    
    numsubj<-length(unique(d$SUBJID))
    
    numobs<-mean(table(d$SUBJID))
    
    sd_obs<-sd(table(d$SUBJID))
    
    measure_time <- d %>% group_by(SUBJID) %>% 
                          mutate(agelag=lag(AGEDAYS),
                                 measure_length= AGEDAYS-agelag) %>%
                          summarise(median_length=median(measure_length, na.rm=T))
    
    median_length_between_measures <- median(measure_time$median_length, na.rm=T)
    
    age <- d %>% group_by(SUBJID) %>% summarize(minage=min(AGEDAYS), maxage=max(AGEDAYS)) %>% as.data.frame()
    minages<-mean(age[,2] , na.rm = T)
    maxages<-mean(age[,3] , na.rm = T)
    
    mortality<-as.numeric("DEAD" %in% colnames(d))
    birthweek<-as.numeric("BRTHWEEK" %in% colnames(d))   
    RCT<-as.numeric("ARM" %in% colnames(d))
    diar<-as.numeric("PCTDIAR" %in% colnames(d))
    
    
    #Calculate standard deviation of monthly measurements
    HAZsd <- sd(d$HAZ)
    WAZsd <- sd(d$WAZ) 
    WHZsd <- sd(d$WHZ) 
    
    #Calculate standard deviation of monthly measurements
    HAZsd_no_outliers <- sd(d$HAZ[d$HAZ > -5 & d$HAZ < 5])
    WAZsd_no_outliers <- sd(d$WAZ[d$WAZ > -5 & d$WAZ < 5]) 
    WHZsd_no_outliers <- sd(d$WHZ[d$WHZ > -5 & d$WHZ < 5]) 
    
    #Calculate prevalence of height decrease between measurements
    #Count a height decrease if height decreases more than WHO standard (2.8 x expert TEM of 0.29 =  0.812)
    if(!is.null(d$LENCM)){
      d$LENCM<-as.numeric(d$LENCM)
    d <- d %>% arrange(SUBJID,AGEDAYS) %>% group_by(SUBJID) %>% 
      mutate(LENCMlag=dplyr::lag(LENCM, n = 1, default = NA, order_by=AGEDAYS),
             length_change=LENCM-LENCMlag,
             length_decrease=ifelse(length_change< -0.812, 1, 0)) %>% ungroup()
    perc_length_decrease <- mean(d$length_decrease, na.rm=T) *100
    }else{
      perc_length_decrease <- NA  
    }
    
    
    
    
    #Calculate monthly wasting
    for(j in 0:23){
      assign(paste('wastprev_m', j+1, sep=''),  mean(as.numeric(d$WHZ[d$AGEDAYS>=j*30 & d$AGEDAYS<(j+1)*30] < (-2)), na.rm=T)*100)
      assign(paste('n', j+1, sep=''),  length(d$WHZ[d$AGEDAYS>=j*30 & d$AGEDAYS<(j+1)*30]))
    }
    
    
    
    
    variables<-paste(colnames(d), collapse=', ' )
    
  }
  res<-t(c(numcountry, countrycohort, wastprev, numsubj, numobs, median_length_between_measures, sd_obs, minages, maxages, mortality, birthweek, RCT, diar, 
           HAZsd, WAZsd, WHZsd, HAZsd_no_outliers, WAZsd_no_outliers, WHZsd_no_outliers, perc_length_decrease, 
           wastprev_m1, wastprev_m2, wastprev_m3, wastprev_m4, wastprev_m5, wastprev_m6, wastprev_m7, wastprev_m8,
           wastprev_m9, wastprev_m10, wastprev_m11, wastprev_m12, wastprev_m13, wastprev_m14, wastprev_m15, wastprev_m16,
           wastprev_m17, wastprev_m18, wastprev_m19, wastprev_m20, wastprev_m21, wastprev_m22, wastprev_m23, wastprev_m24,
           n1, n2, n3, n4, n5, n6, n7, n8,
           n9, n10, n11, n12, n13, n14, n15, n16,
           n17, n18, n19, n20, n21, n22, n23, n24,
           variables
  ))
  
  try(df[i,46:ncol(df)] <- res)
}


df[242,-ncol(df)]

saveRDS(df, "U:/results/Metadata/GHAP_metadata.RDS")
getwd()














