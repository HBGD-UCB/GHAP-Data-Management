

rm(list=ls())
#install.packages('DataExplorer') 
library(DataExplorer)
library(tidyverse)
library(data.table)
library(xlsx)
library(scales)

source("U:/GHAP-Data-Management/HBGDki_functions.R")


setwd("U:/data")
#load in CI of wasting dataset at 24 months
load("U:/data/Compiled Datasets/WastInc0-24.Rdata")
d.old <- d




# #d<-read.csv("FINAL.csv")
# setwd("U:/data")
# d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T)
# 
# dim(d)
# head(d)
# colnames(d)
# 
# saveRDS(d, "FINAL.rds")



#Read rds file
d<- readRDS("FINAL.rds")




#Check for duplicate agedays
dup_age <- d %>% group_by(STUDYID, SUBJID, AGEDAYS) %>% summarize(N=n())
mean(dup_age$N)

#save haz data
df<-d %>% subset(., select=c(STUDYID, SUBJID, COUNTRY, TR, AGEDAYS, HAZ))
colnames(df) <- tolower(colnames(df))
save(df, file="U:/data/Stunting/Full-compiled-data/compiled_HAZ_dataset.RData")


d <-d %>% filter(!is.na(AGEDAYS) & !is.na(WHZ)) %>%
  filter(WHZ > -5 & WHZ < 5) %>%
  filter(AGEDAYS <= 24 * 30.25)
gc()

length(unique(paste0(d$STUDYID, d$COUNTRY)))

length(unique(paste0(d$STUDYID, d$COUNTRY, d$SUBJID)))

dim(d)

#Drop un-needed columns
d <- subset(d, select= -c(AGEIMPFL, WTKG,HTCM,LENCM,WAZ,HAZ,     
                          BAZ,HCAZ,MUAZ,SITEID,  REGION,
                          REGCTRY,  REGCTYP,  CITYTOWN, LATITUDE, LONGITUD, CLUSTID, 
                          HHID, BRTHYR, INCTOTU, CAUSEDTH))
d<-as.data.frame(d)


# 
# anthrodf<- d %>% subset(., select=c(STUDYID, COUNTRY, SUBJID, AGEDAYS, HAZ, ARM)) %>% filter(!is.na(AGEDAYS) & !is.na(HAZ))
# dim(anthrodf)
# table(anthrodf$STUDYID, is.na(anthrodf$ARM))



# d<- d %>% subset(., select=c(STUDYID, COUNTRY, SUBJID, AGEDAYS, HAZ, ARM)) %>% filter(!is.na(AGEDAYS) & !is.na(HAZ)) %>%
#      filter(AGEDAYS < 30.25*24)
# 
# d %>% group_by(STUDYID, COUNTRY, SUBJID) %>% arrange(AGEDAYS) %>%
#   mutate(measure.length=AGEDAYS-lag(AGEDAYS)) %>% ungroup() %>% group_by(STUDYID, COUNTRY) %>% 
#   summarise(measure.length=median(measure.length, na.rm=T)) %>%
#   arrange(measure.length) %>% as.data.frame()
# 
# 
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# d %>% group_by(STUDYID, COUNTRY, SUBJID) %>% arrange(AGEDAYS) %>%
#   summarise(N=n()) %>% ungroup() %>% group_by(STUDYID, COUNTRY) %>% 
#   summarise(modeN=Mode(N)) %>%
#   arrange(modeN) %>% as.data.frame()

#Create simple exploratory data report 
create_report(d)


d <-d %>% filter(!is.na(AGEDAYS) & !is.na(WHZ)) %>%
  filter(WHZ > -5 & WHZ < 5) %>%
  filter(AGEDAYS <= 24 * 30.25) %>% 
  group_by(STUDYID, SUBJID) %>% 
  arrange(AGEDAYS) %>%
  slice(1) %>% 
  ungroup



#Drop un-needed columns
colnames(d)
country<-d$COUNTRY
d <- subset(d, select= -c(COUNTRY))


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


df<-d
for(i in 2:ncol(df)){
  df[,i] <- as.numeric(df[,i])
}
df <- df %>% group_by(STUDYID) %>% summarize_all(funs(mean), na.rm=T) %>% as.data.frame()

head(df)
dmeans<-df




for(i in 2:ncol(df)){
  df[,i] <-ifelse(is.na(df[,i]),"",1)
}

write.xlsx(df, file="U:/results/diagnostic figures/dataprod_covariate_presence.xlsx", sheetName="sheet1", row.names=FALSE)
write.xlsx(dmeans, file="U:/results/diagnostic figures/dataprod_covariate_presence.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)

#write.csv(df, file="U:/results/diagnostic figures/dataprod_covariate_presence.csv")









pdf("U:/results/diagnostic figures/Data_prod_density_plots.pdf", width=10,height=8.5, paper="USr")

for(i in 3:ncol(d)){
  varname<-colnames(d)[i]
  
  if(length(unique(d[,i]))>20){
    
    var <- data.frame(STUDYID=paste0(d$STUDYID," ", country), X=as.numeric(d[,i]))
    
    p <- ggplot(var, aes(x=X)) + geom_density() + facet_wrap(~STUDYID) + ggtitle(varname)
    print(p)
  }else{
    var <- data.frame(STUDYID=paste0(d$STUDYID," ", country), X=as.character(d[,i]))
    
    p <- ggplot(var, aes(x=X)) +
      #geom_bar(aes(y=..count../sum(..count..), group = 1, fill = X)) + 
      geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = X)) + 
      facet_wrap(~STUDYID) + ggtitle(varname) + scale_y_continuous(labels=percent_format()) +
      theme(axis.text.x = element_text(angle=45,size=10))
    print(p)
  }
}

dev.off()





#Make combined set of plots comparing data product to manually compiled datasets
dim(d.old)
dim(d)
unique(d.old$STUDYID)
unique(d$STUDYID)
colnames(d.old)
colnames(d)


#subset to matching studies and variables
#d.old
d.old <- d.old %>% filter(
  STUDYID!="ki1000107-Serrinha-VitA" &
    STUDYID!="ki1000109-EE" &
    STUDYID!="ki1000304-EU" &
    STUDYID!="ki1000304-Vitamin-B12" &
    STUDYID!="ki1000304b-SAS-FoodSuppl" &
    STUDYID!="ki1112895-Peru Huascar" &
    STUDYID!="ki1113344-GMS-Nepal" &
    STUDYID!="ki1114097-CONTENT" &
    STUDYID!="ki1135782-INCAP" &
    STUDYID!="ki1000108-TDC" &
    STUDYID!="ki1000301-DIVIDS" &
    STUDYID!="ki1148112-iLiNS-DOSE" &
    STUDYID!="ki1148112-LCNI-5" 
) %>%
  subset(., select=c(
    STUDYID, tr, enrolstunt, BIRTHWT, MAGE, MHTCM, MWTKG, MBMI, MEDUCYRS, BIRTHLEN, GAGEBRTH, FEDUCYRS, FAGE, FHTCM, single, DURBRST, SOAP, DIARFL,
    chicken, cow, improved.floor, improved.sanitation, safe.water, cleancook, treat.water, birthmonth, month, birthorder, 
    homedelivery, vagbirth, breastfeeding, nroom,  nchild5 
  ))




d <- d %>% filter(
  STUDYID!="ki1017093b-PROVIDE" &
    STUDYID!="ki1000301-DIVIDS" ) %>%
  subset(., select=c(
    STUDYID, TR, ENSTUNT, BIRTHWT, MAGE, MHTCM, MWTKG, MBMI, MEDUCYRS, BIRTHLEN, GAGEBRTH, FEDUCYRS, FAGE, FHTCM, SINGLE, DURBRST, SOAP, DIARFL,
    CHICKEN, COW, IMPFLOOR, IMPSAN, SAFEH2O, CLEANCK, TRTH2O, BRTHMON, MONTH, PARITY,
    HDLVRY, VAGBRTH, FEEDING,  NROOMS, NCHLDLT5
  ))


dim(d.old)
dim(d)
unique(d.old$STUDYID)
unique(d$STUDYID)
colnames(d.old)
colnames(d)


d.old$STUDYID <- as.character(d.old$STUDYID)
d$STUDYID <- as.character(d$STUDYID)

#remove grant identifier
d.old$STUDYID<- gsub("^k.*?-" , "", d.old$STUDYID)
d$STUDYID<- gsub("^k.*?-" , "", d$STUDYID)

d.old$STUDYID <- factor(d.old$STUDYID, levels=unique(d.old$STUDYID))
d$STUDYID <- factor(d$STUDYID, levels=unique(d.old$STUDYID))

levels(d.old$STUDYID)
levels(d$STUDYID)


d<-d[!is.na(d$STUDYID),]

pdf("U:/results/diagnostic figures/Data_prod_comparison_plots.pdf", width=10,height=8.5, paper="USr")

for(i in 3:ncol(d)){
  varname<-paste0("new dataset: ", colnames(d)[i])
  varname2<-paste0("old dataset: ", colnames(d.old)[i])
  
  if(length(unique(d[,i]))>20){
    
    var <- data.frame(STUDYID=d$STUDYID, X=as.numeric(d[,i]))
    var2 <- data.frame(STUDYID=d.old$STUDYID, X=as.numeric(d.old[,i]))
    
    p <- ggplot(var, aes(x=X)) + geom_density() + facet_wrap(~STUDYID) + ggtitle(varname)
    print(p)
    
    p2 <- ggplot(var2, aes(x=X)) + geom_density() + facet_wrap(~STUDYID) + ggtitle(varname2) +
      coord_cartesian(xlim = ggplot_build(p)$layout$panel_ranges[[1]]$x.range,
                      ylim = ggplot_build(p)$layout$panel_ranges[[1]]$y.range) + theme_bw()
    print(p2)
  }else{
    var <- data.frame(STUDYID=d$STUDYID, X=as.character(d[,i]))
    var2 <- data.frame(STUDYID=d.old$STUDYID, X=as.character(d.old[,i]))
    
    p <- ggplot(var, aes(x=X)) +
      geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = X)) + 
      facet_wrap(~STUDYID) + ggtitle(varname) + scale_y_continuous(labels=percent_format())
    print(p)
    
    p2 <- ggplot(var2, aes(x=X)) +
      geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = X)) + 
      facet_wrap(~STUDYID) + ggtitle(varname2) + scale_y_continuous(labels=percent_format()) +
      coord_cartesian(xlim = ggplot_build(p)$layout$panel_ranges[[1]]$x.range,
                      ylim = ggplot_build(p)$layout$panel_ranges[[1]]$y.range) + theme_bw()
    
    print(p2)
  }
}

dev.off()


