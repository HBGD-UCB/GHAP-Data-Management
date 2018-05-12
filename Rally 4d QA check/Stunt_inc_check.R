



rm(list=ls())
library(dplyr)
library(tidyverse)
library(caret)
library(MASS)
library(reshape2)
library(zoo)
library(epitools)
library(binom)
theme_set(theme_bw())



setwd("C:/Users/andre/Downloads")
source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/Wast_incidence_functions.R")


load("inc_prep.Rdata")
colnames(inc.prep) <-toupper(colnames(inc.prep))
colnames(inc.prep)

inc.prep<-inc.prep %>% subset(., TR=="C" | is.na(TR))
d<-inc.prep %>% subset(., select=c(STUDYID, SUBJID, COUNTRY, TR, AGEDAYS, HAZ))

inc.prep <- subset(inc.prep, STUDYID=="ki1000108-CMC-V-BCS-2002")
d <- subset(d, STUDYID=="ki1000108-CMC-V-BCS-2002")

d <- d %>% rename(WHZ=HAZ)
d.inc <-WastIncCalc(d, washout = 750)
d.tab <- WastIncTable(d.inc)


sum(inc.prep$PDAYS)
sum(d.inc$pt_wastrisk * d.inc$wast_risk)


table(inc.prep$INCCASE)
table(d.inc$wast_inc)

d.tab$tab2$`Number of wasting episodes`
table(inc.prep$AGECAT, inc.prep$INCCASE)




agecats=c(6*30.25, 12*30.25, 18*30.25, 24*30.25)
  
agecats=c(182.5, 182.5*2, 182.5*3, 182.5*4)
d.inc$agecat <- as.factor(findInterval(d.inc$AGEDAYS, agecats, rightmost.closed=F, left.open=T))
d.inc$agecat2 <- as.factor(findInterval(d.inc$AGEDAYS, agecats, rightmost.closed=T, left.open=T))
  
table(inc.prep$AGECAT, inc.prep$INCCASE)
table(d.inc$agecat, d.inc$wast_inc)
table(d.inc$agecat2, d.inc$wast_inc)


# d.inc$wast_risk2 <- d.inc$wast_risk
# d.inc$wast_risk2[d.inc$wast_inc==1] <- 0

d.inc$period_length2 <- d.inc$period_length
d.inc$period_length2[d.inc$wast_inc==1] <- d.inc$period_length2[d.inc$wast_inc==1] /2

inc.prep %>% group_by(AGECAT) %>%  summarize(sum( PDAYS))
d.inc %>% group_by(agecat) %>%  summarize(sum(period_length * wast_risk))


#Jade's maxage for 24 months:730 days
#Mine: 726
#So in CMC, I have 1 case beyond 24 months that you count in the strata.
# You are also not dropping HAZ < -5 and above > 5 
#I also think I am cutting age categories as [), and you are using (] (left open). Don't know which matters


d1 <- inc.prep %>% subset(., select=c(SUBJID, AGEDAYS, HAZ, INCCASE, PDAYS)) %>% group_by(SUBJID) %>% arrange(SUBJID, AGEDAYS) %>%
     mutate(pt=cumsum(PDAYS)*INCCASE) 
      
d2 <- d.inc %>% subset(., select=c(SUBJID,AGEDAYS,WHZ, wast_inc,wast_risk,period_length)) %>% rename(HAZ2=WHZ) %>%
     group_by(SUBJID) %>% arrange(SUBJID, AGEDAYS) %>% 
     mutate(pday=(period_length * wast_risk), pt2=cumsum(pday)*wast_inc) 
d2$pday2<-d2$pday
d2$pday2[d2$wast_inc==1]<-d2$pday2[d2$wast_inc==1]/2
d2 <- d2 %>% group_by(SUBJID) %>% mutate( pt3=cumsum(pday2)*wast_inc)
df<-merge(d1,d2, by=c("SUBJID","AGEDAYS"))
df <- df %>% arrange(SUBJID, AGEDAYS)


df %>% summarise(sum(pt), sum(pt2), sum(pt3))
