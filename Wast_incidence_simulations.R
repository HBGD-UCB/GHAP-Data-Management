

rm(list=ls())
library(tidyverse)
library(zoo)

source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/Wast_incidence_functions.R")

#Mock data
set.seed(12345)
AGEDAYS<-c(1,31,70, 100, 200, 210,220,270,300,
           5,20,50,100,130,
           7,14,21,28,45)
AGEDAYS<-rep(sample.int(720, 60),50)
SUBJID<-c(1,1,1,1,1, 1,1,1,1,
          2,2,2,2,2,
          3,3,3,3,3)
SUBJID<-rep(1:50, each = 60)
WHZ<-c(0,-3.5,0,-3.5,0,0,0,-3.5,0,
       -2.5,-2.5,-2.5,-2.5,-2.5,
       -2.5,0,-2.5,0,0)
WHZ<-rnorm(3000,-2,1)
d<-data.frame(SUBJID,AGEDAYS,WHZ)
d<-d %>% arrange(SUBJID,AGEDAYS)


agecats=c(6*30, 12*30, 18*30, 24*30)
  d$agecat <- as.factor(findInterval(d$AGEDAYS, agecats, rightmost.closed=F))
table(d$agecat)
strat=T
agecats=c(6*30, 12*30, 18*30, 24*30)
agecat_rownames=NULL
washout=60

test<- WastIncCalc(d,washout=60)
test2<-WastIncTable(test,
                    strat=T)


#Set study size
n<-1000
meas<-ceiling(30.41 * 24)

n<-100
meas<-ceiling(30.41 * 24)

# Simulate data
#NOTE: I need to find a way to simulate a constrained washout period between incidents
d <- data.frame(SUBJID=rep(1:n, each = meas), #1000 children
                AGEDAYS=rep(1:meas,n), # Fill in ages 1-1000 for each child 
                WHZ=rep(NA, n*meas), 
                wast_inc=rbinom(n*meas, 1, 0.001), # Simulate data by generating 1 wasting incident per 1000 days per child
                ep_dur =round(runif(n*meas, min = 30, max = 150)), #simulate a mean duration of 90 days and fill in recovery time
                wast_rec=rep(0, n*meas),
                wast_ep=rep(NA, n*meas))

#Fill in recovery time

mark_rec<-function(d){
  indices<-which(d$wast_inc==1)+d$ep_dur[which(d$wast_inc==1)]
  indices<-indices[indices<=nrow(d)]
  d$wast_rec[indices] <- 1

  #Then create a variable for if the child is in an episode
  d$wast_ep[d$wast_inc==1] <- 1
  d$wast_ep[d$wast_rec==1] <- 0
  
      d <- d %>% 
      mutate(wast_ep = ifelse(AGEDAYS==min(AGEDAYS) & wast_inc==0, 0, wast_ep),
             wast_ep = ifelse(AGEDAYS==min(AGEDAYS) & wast_inc==1, 1, wast_ep),
             wast_ep = na.locf(wast_ep, fromLast=F)) %>% #Last observation carried forward 
      ungroup()
    return(d)
}

d <- d %>% group_by(SUBJID) %>%
  do(mark_rec(.))



#Then simulate WHZ based on if a child is in an episode or not
d$WHZ[d$wast_ep==0] <- 0
d$WHZ[d$wast_ep==1] <- -2.5

# #Then randomly subsample observations ~ every 30 days or so from each child to make the final dataset.
indices <- rep(1:floor(nrow(d)/30) * 30) + round(rnorm(floor(nrow(d)/30), sd=5)) #measures every 30 days
indices <- indices[indices > 0 & indices < nrow(d)]

#Full data means
full_sum <- d %>% ungroup() %>% summarise(wast_inc=mean(wast_inc),
                            duration=mean(ep_dur),
                            wast_prev=mean(wast_ep))

#Subset
dfull<-d
d<-d[indices,]

#Subset of data means
sub_sum <- d %>% ungroup() %>% summarise(wast_inc=mean(wast_inc),
                            duration=mean(ep_dur),
                            wast_prev=mean(wast_ep))


head(d)


d <- d %>% subset(., select = c(SUBJID, AGEDAYS,WHZ))

test<- WastIncCalc(d,washout=60)
test2<-WastIncTable(test,
                    strat=T)

test2
full_sum
sub_sum
# 
#  strat=T
#  agecats=c(6*30.25, 12*30.25, 18*30.25, 24*30.25)
#  agecat_rownames=c("0-6 months","6-12 months", "12-18 months", "18-24 months")
#  
#    if(strat==T){
#     test$agecat <- as.factor(findInterval(test$AGEDAYS, agecats, rightmost.closed=F))
#   }  
#   
#   summary<-WastIncSummary(test, strat=T)
#   
#   
#   
#   #Fix agestrat in wast_inc_table
#   
# summary[[1]]
# summary[[2]]
# full_sum
# sub_sum




