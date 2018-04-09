
library(tidyverse)
load("U:/data/Compiled Datasets/CompiledLongData_AllAges.Rdata")


df<-d %>% group_by(STUDYID, COUNTRY) %>%
  do(as.data.frame(t(as.vector(summary(.$AGEDAYS/30.25)))))


colnames(df)[3:8] <- c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")

df <- df %>% arrange(Max.)

df

write.csv(df, file="U:/GHAP_agesummary.csv")




load("U:/data/Compiled Datasets/PooledUncleaned24mo_tempdata.Rdata")
table(d$STUDYID,is.na(d$PARITY))