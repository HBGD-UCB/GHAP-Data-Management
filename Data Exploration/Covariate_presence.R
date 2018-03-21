
library(tidyverse)
load("U:/data/Compiled Datasets/WastInc0-24.Rdata")


df <- d %>% group_by(STUDYID) %>% summarize_all(funs(mean), na.rm=T) %>% as.data.frame()

head(df)

df <- df[,-c(2:36)]

colnames(df)

for(i in 2:39){
  df[,i] <-ifelse(is.na(df[,i]),"",1)
}
for(i in 40:ncol(df)){
  df[,i] <-ifelse(df[,i]==1,"",1)
  colnames(df)[i]<-paste0("not",colnames(df)[i])
}

write.csv(df, file="U:/results/diagnostic figures/covariate_presence.csv")

