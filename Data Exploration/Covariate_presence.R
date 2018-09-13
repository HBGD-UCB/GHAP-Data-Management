
rm(list=ls())
library(tidyverse)
setwd("U:/ucb-superlearner/Stunting rallies/")

#load covariates
d<-readRDS("FINAL_clean_covariates.rds")


#Drop intervention analysis only studies

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


d <- d %>% filter(measurefreq!="yearly")




#Vector of risk factor names
A<-c( "studyid", "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec",  
      "enwast", "anywast06", "pers_wast", 
      "trth2o", "cleanck", "impfloor",  "impsan", "safeh20",
      "perdiar6", "perdiar24", "predexfd6", "earlybf")  

d <- d %>% select(A)


#Strip grant identifier and add country
d$studyid <- gsub("^k.*?-" , "", d$studyid)


#Count observations of covariates
nObs <- gather(d, variable, value, -studyid) %>% group_by(studyid) %>%
  count(variable, value) %>% filter(!is.na(value)) %>%
  group_by(studyid, variable) %>% summarize(N=sum(n))

#Summarize covariate presence
for(i in 2:ncol(d)){
  d[,i] <- as.numeric(factor(d[,i]))
}
df <- d %>% group_by(studyid) %>% summarize_all(funs(mean), na.rm=T) %>% as.data.frame()

head(df)
colnames(df)



for(i in 2:ncol(df)){
  df[,i] <-ifelse(is.na(df[,i]),0,1)
}
# for(i in 39:ncol(df)){
#   df[,i] <-ifelse(df[,i]==1,"",1)
#   colnames(df)[i]<-paste0("not",colnames(df)[i])
# }

write.csv(df, file="U:/results/diagnostic figures/covariate_presence.csv")




#------------------------------------------
# Plot out covariate presence
#------------------------------------------

library(reshape2)
plotdf <- melt(df, id.vars = c("studyid"))
head(plotdf)

#Merge in the number of observations
plotdf <- left_join(plotdf, nObs, by=c("studyid", "variable"))

#Categorize N's
summary(plotdf$N)
plotdf$Ncat <- cut(plotdf$N,breaks=c(0,250,500,1000,5000,10000,100000),
                labels=c("<250","250-500","500-1000","1000-5000","5000-10000",">10000"))
plotdf$Ncat <- factor(plotdf$Ncat)


plotdf$variable <- toupper(plotdf$variable)

varfreq <- plotdf %>% group_by(variable) %>% 
  summarize(numstudies= sum(value)) %>%
  arrange(-numstudies)
varfreq

studyfreq <- plotdf %>% group_by(studyid) %>% 
  summarize(numstudies= sum(value)) %>%
  arrange(numstudies)
studyfreq


#order variables and studies by frequency for the plot
plotdf$variable <- factor(plotdf$variable , levels=varfreq$variable)
levels(plotdf$variable)

plotdf$studyid <- factor(plotdf$studyid , levels=studyfreq$studyid)

#rename variables
plotdf<-mutate(plotdf,variable=fct_recode(variable,
                                          "Gender"="SEX",          "Enrolled stunted"="ENSTUNT",      "Enrolled wasted"="ENWAST",       "Birth month"="BRTHMON",      "Month"="MONTH",        "Mom age"="MAGE",         
                                          "Mom educ."="MEDUCYRS",     "Birthweight"="BIRTHWT",      "Mom height"="MHTCM",        "Birth length"="BIRTHLEN",     "Dad educ."="FEDUCYRS",     "Improved sanitation"="IMPSAN",       
                                          "HH wealth"="HHWEALTH_QUART", "Home delivery"="HDLVRY",       "Mom BMI"="MBMI",         "Mom weight"="MWTKG",        "# rooms"="NROOMS",       "Pred./ex. breastfeeding"="PREDEXFD6",    
                                          "Impr. H20"="SAFEH20",      "Impr. floor"="IMPFLOOR",     "Birth order"="PARITY",       "% diarrhea <24mo."="PERDIAR24",    "Any wasting <6mo."="ANYWAST06",    "Gestational age"="GAGEBRTH",     
                                          "# people in HH"="NHH",          "Persistent wasting"="PERS_WAST",    "Clean cook"="CLEANCK",      "Number child <5 in HH"="NCHLDLT5",     "Vaginal birth"="VAGBRTH",      "Early initiation of BF"="EARLYBF",      
                                          "Dad age"="FAGE",         "Food security"="HFOODSEC",     "Single parent"="SINGLE",       "% diar <6mo."="PERDIAR6",     "Treats H20"="TRTH2O",       "Dad height"="FHTCM"))



#define a color for fonts
textcol <- "grey20"

# heat map plot scheme
hm <- ggplot(plotdf,aes(x=variable,y=studyid)) +
  #add border white colour of line thickness 0.25
  geom_tile(aes(fill=Ncat), colour="white",size=0.25) +
  #remove extra space
  # scale_y_discrete(expand=c(0,0))+
  # scale_x_continuous(expand=c(0,0),
  #                    breaks=1:24,labels=1:24)+
  #one unit on x-axis is equal to one unit on y-axis.
  #equal aspect ratio x and y axis
  coord_equal()+
  #set base size for all font elements
  theme_grey(base_size=10) +
  #Set white and black scale
  #scale_fill_manual(values=c("white", "#1F77B4")) +
  #put x-axis on top
  scale_x_discrete(position = "top") +
  xlab("Risk factor variable") + ylab("Study") +
  #Add variable counts to the x-axis
  #geom_text(label=varfreq$numstudies, x=1:length(varfreq$numstudies), y=0) +
  annotate(geom = "text", x =1:length(varfreq$numstudies), y = -0.3, label = varfreq$numstudies, size = 3.5) +
  #theme options
  theme(
    # legend options
    legend.title=element_text(color=textcol,size=8),
    #reduce/remove legend margin
    legend.margin = margin(grid::unit(0.1,"cm")),
    #change legend text properties
    legend.text=element_text(colour=textcol,size=7,face="bold"),
    #change legend key height
    legend.key.height=grid::unit(0.2,"cm"),
    #set a slim legend
    legend.key.width=grid::unit(1,"cm"),
    #set x axis text size and colour and rotate
    axis.text.x=element_text(angle = 45, hjust = 0, size=12,colour=textcol,vjust=0),
    #set y axis text colour and adjust vertical justification
    axis.text.y=element_text(size=8,vjust = 0.2,colour=textcol),
    #change axis ticks thickness
    axis.ticks=element_line(size=0.4),
    # axis.ticks.x=element_blank(),
    #change title font, size, colour and justification
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    #format facet labels
    strip.text.x = element_text(size=10),
    strip.text.y = element_text(angle=270,size=10),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank(),
    #remove legend
    legend.position="bottom",
    #remove plot margins
    plot.margin = margin(0, 2, 2, 0, "cm")
  ) +
  scale_fill_brewer(palette = "YlOrRd",na.value="grey90",
                    guide=guide_legend(title="Number of children with covariate measured",title.vjust = 1,
                                       label.position="bottom",label.hjust=0.5,nrow=1)) +
  ggtitle("Covariate presence and measurement frequency across\nstudies used in the HBGDki risk factor analysis.")

hm


# Code to override clipping
library(grid)
gt <- ggplot_gtable(ggplot_build(hm))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

# # heat map
# varhm <- hm +
#   labs(x="Age in months",y="",title="Wasting prevalence by month of age") +
#   scale_fill_brewer(palette = "YlOrRd",na.value="grey90",
#                     guide=guide_legend(title="Wasting (%)",title.vjust = 1,
#                                        label.position="bottom",label.hjust=0.5,nrow=1))
# varhm

pdf("U:/Figures/Risk_Factor_Presence_Heatmap.pdf",width=12,height=12)
grid.draw(gt)
dev.off()
