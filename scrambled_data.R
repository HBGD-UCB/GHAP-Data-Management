
rm(list=ls())
library(tidyverse)
library(caret)
library(missForest)


dfull <- readRDS("U:/ucb-superlearner/Stunting rallies/FINAL_clean_covariates.rds")

table(dfull$arm)

d <- dfull %>% filter(arm!="") %>% subset(., select= -c(id))

table(d$studyid)



head(d)

#Add random missingness
table(is.na(d))

set.seed(12345)
d[, c(8:ncol(d))] <- prodNA(d[, c(8:ncol(d))], 0.2)

for(i in 8:ncol(d)){
  if(!is.numeric(d[,i])){d[,i] <- as.numeric(factor(d[,i]))}
}

# for(i in 8:ncol(d)){
#   d[sample(nrow(d), nrow(d)/5),i] <- NA
# }
# head(d)
table(is.na(d))


#KNN impute missingness by study
study_knn <- function(df){
  
  df <- as.data.frame(df) 
  too_miss <- rep(NA, ncol(df))
  n <- nrow(df)
  
  for(i in 1:ncol(df)){
    too_miss[i] <- sum(is.na(df[,i])) > n/4
  }
  
  df <- df[,!too_miss]
  
  preProc <- preProcess(df, method = c("nzv"))
  df <- predict(preProc, df)

  cat("\n",df$studyid[1],"\n")
  possk<-(apply(df, 1, function(r) all(!(is.na(r)))))
  print(table(possk))
  nk <- sum(possk==T)
  if(nk>10){nk <- 10}
  if(ncol(df)<17){nk <- ncol(df)-7}
  
  print(median(df[,8], na.rm=T))
  df[is.na(df[,8]),8] <- median(df[,8], na.rm=T) 
  
  for(i in 8:ncol(df)){
    df[1,i] <- median(df[,i], na.rm=T) 
  }
  
  if(nk==0){nk <- 1}
  
  id <- df[,1:2]
  preProcValues <- preProcess(df, method = c("knnImpute"), k = nk)
  df_imp <- predict(preProcValues, df)
  df_imp[,1:2] <- id
  return(df_imp)
}

d2 <- d %>% group_by(studyid) %>% do(res=study_knn(.))  

d3 <- do.call("bind_rows", d2[[2]])


# table(is.na(d2))
# 
# test <- df <- d[d$studyid=="ki1017093b-PROVIDE",]
# test2 <- study_knn(test)
# 
# 
# table(apply(test, 1, function(r) all(!(is.na(r)))))


#Merge in outcome
#load("U:/data/Stunting/Full-compiled-data/compiled_HAZ_dataset.RData")
load("U:/Data/Stunting/int_stunting_data.RData")
head(d)
d <- d %>% filter(!is.na(haz))

d$subjid <- (as.numeric(d$subjid))
#d3$subjid <- as.numeric(d3$subjid)

dim(d)
dim(d3)
d4 <- left_join(as.data.frame(d), d3, by=c("studyid", "country", "subjid"))
dim(d4)
d4 <- d4 %>% filter(tr.x!="")
dim(d4)
d4 <- distinct(d4, studyid, country, subjid, agedays, .keep_all=T )
dim(d4)


d <- d4
save(d, file = "U:/data/scrambled_tr_data.Rdata")



#Shuffle the subjid:

summary(as.numeric(d$subjid))
length(unique(d$subjid))

set.seed(12345)
d <- d %>% mutate(subjid = group_indices(., subjid))

summary(d$subjid)
length(unique(d$subjid))
