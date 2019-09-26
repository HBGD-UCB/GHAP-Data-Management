#------------------------------------
# Household asset PCA function
#------------------------------------

rm(list=ls())
library(tidyverse)
library(washb)
library(caret)


#PCA of asset based wealth by enrollment
#Method based on: https://programming-r-pro-bro.blogspot.com/2011/10/principal-component-analysis-use.html
#dfull = data.frame 
#varlist = list of variable names to calculate first PCA from
#reorder = If printed data.frame of the quartiled first component shows the first level as the wealthiest, use this option to reverse

assetPCA<-function(dfull, varlist, reorder=F ){
  
  varlist<-c("STUDYID","SUBJID","COUNTRY",varlist)
  
  #Subset to only needed variables for subgroup analysis
  ret <- dfull %>%
    subset(select=c(varlist)) 
  

  #Select assets
  ret<-as.data.frame(ret) 
  id<-subset(ret, select=c("STUDYID","SUBJID","COUNTRY")) #drop subjectid
  ret<-ret[,which(!(colnames(ret) %in% c("STUDYID","SUBJID","COUNTRY")))]
  
  #Replace character blank with NA
  for(i in 1:ncol(ret)){
    ret[,i]<-ifelse(ret[,i]=="",NA,ret[,i])
  } 
  
  #drop rows with no asset data
  id<-id[rowSums(is.na(ret[,4:ncol(ret)])) != ncol(ret)-3,]  
  ret<-ret[rowSums(is.na(ret[,4:ncol(ret)])) != ncol(ret)-3,]  
  
  
  #Drop assets with great missingness
  for(i in 1:ncol(ret)){
    cat(colnames(ret)[i],"\n")
    print(table(is.na(ret[,i])))
    print(class((ret[,i])))
  }
  
  #create level for for missing factor levels
  table(is.na(ret))
  for(i in 1:ncol(ret)){
    ret[,i]<-as.character(ret[,i])
    ret[is.na(ret[,i]),i]<-"miss"
    ret[,i]<-as.factor(ret[,i])
    
  }
  table(is.na(ret))
  
  #Convert factors into indicators
  ret<-droplevels(ret)
  ret<-design_matrix(ret)
  

  #Remove columns with almost no variance
  if(length(nearZeroVar(ret))>0){
    ret<-ret[,-nearZeroVar(ret)]
  }
  
  ## Convert the data into matrix ##
  ret<-as.matrix(ret)
  
  
  ##Computing the principal component using eigenvalue decomposition ##
  princ.return <- princomp(ret) 
  
  
  ## To get the first principal component in a variable ##
  load <- loadings(princ.return)[,1]   
  
  pr.cp <- ret %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
  
  HHwealth <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.
  
  #Create 4-level household weath index
  quartiles<-quantile(HHwealth, probs=seq(0, 1, 0.25))
  print(quartiles)
  ret<-as.data.frame(ret)
  ret$HHwealth_quart<-rep(1, nrow(ret))
  ret$HHwealth_quart[HHwealth>=quartiles[2]]<-2
  ret$HHwealth_quart[HHwealth>=quartiles[3]]<-3
  ret$HHwealth_quart[HHwealth>=quartiles[4]]<-4
  table(ret$HHwealth_quart)
  ret$HHwealth_quart<-factor(ret$HHwealth_quart)
  
  if(reorder==T){
    levels(ret$HHwealth_quart)<-c("Wealth Q4","Wealth Q3","Wealth Q2","Wealth Q1")
    ret$HHwealth_quart<-factor(ret$HHwealth_quart, levels=c("Wealth Q1", "Wealth Q2","Wealth Q3","Wealth Q4"))
  }else{
    levels(ret$HHwealth_quart)<-c("Wealth Q1", "Wealth Q2","Wealth Q3","Wealth Q4")
  }
  
  #Table assets by pca quartile to identify wealth/poverty levels
  d<-data.frame(id, ret)
  wealth.tab <- d %>% subset(., select=-c(STUDYID, SUBJID, COUNTRY)) %>%
    group_by(HHwealth_quart) %>%
    summarise_all(funs(mean)) %>% as.data.frame()
  print(wealth.tab)
  
  #Save just the wealth data
  pca.wealth<-d %>% subset(select=c(STUDYID, SUBJID, COUNTRY, HHwealth_quart))
  
  pca.wealth$SUBJID<-as.numeric(as.character(pca.wealth$SUBJID))
  
  d <-dfull %>% subset(., select=c("STUDYID","SUBJID","COUNTRY"))
  d$SUBJID<-as.numeric(as.character(d$SUBJID))
  d<-left_join(d, pca.wealth, by=c("STUDYID","SUBJID","COUNTRY"))
  return(d)
}



