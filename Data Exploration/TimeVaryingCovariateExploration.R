
library(tidyverse)
load("U:/data/Compiled Datasets/CompiledLongData.Rdata")

colnames(d)



#df2<-df %>% filter(!is.na(AGEDTH)& !is.na(WHZ)) %>% group_by(SUBJID) %>% mutate(errorflag= as.numeric(max(AGEDAYS)>AGEDTH))
#ggplot(df2, aes(x=AGEDAYS, y=AGEDTH, group=SUBJID, color=errorflag)) +geom_line() + geom_point()


d<-d[,-c(1:2,6:15,18:23,29,33,34,37:42)]
for(i in 4:ncol(d)){
  d[,i] <- as.numeric(d[,i])
}

#Studies with percent days with Diarrhea
table(d$STUDYID[!is.na(d$PCTDIAR)])

#Stratified by diarrhea flag variable
table(d$STUDYID[!is.na(d$PCTDIAR)], d$DIARFL[!is.na(d$PCTDIAR)])


#Drop diarrhea and breastfeeding variables variables that are actually subject level, rather than time-varying
d <- subset(d, select= -c(DIARDAYS, DIAREPS, DURBRST, PCTDIAR, SUMEP,SUMDIAR, SUMDAYS))

res <- d %>% subset(., select=-c(AGEDTH,DEAD)) %>% group_by(STUDYID, SUBJID) %>% summarise_all(funs(sd), na.rm=T) %>% ungroup() %>%
  subset(., select=-c(SUBJID, AGEDAYS)) %>%group_by(STUDYID) %>% summarise_all(funs(mean), na.rm=T)
res

res_long <- reshape(as.data.frame(res), idvar="STUDYID", varying=list(2:4),  direction="long")
res_long$variable <- factor(res_long$time, labels=colnames(res)[-1])
res_long <- res_long %>% rename(mean_sd=COUGHFL) %>% filter(!is.na(mean_sd))
res_long$mean_sd[res_long$mean_sd==0] <- -0.1
res_long <- droplevels(res_long)


#Sort flag variables first

#Bar plot of SD's of each time varying variable
p1 <- ggplot(res_long, aes(x=variable, y=mean_sd)) +geom_bar(stat="identity") + 
      geom_hline(yintercept = 0) +
      facet_wrap(~STUDYID) + theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1





#Mortality plots

#Compare DEAD flag across studies
table(d$STUDYID, d$DEAD)

#Children marked dead who don't have an age of death
table(is.na(d$AGEDTH), d$DEAD)

df <- d %>% filter(!is.na(AGEDTH)) %>% subset(., select=c(SUBJID, AGEDAYS, STUDYID, AGEDTH, DEAD)) %>%
            group_by(STUDYID, SUBJID) %>% arrange(AGEDAYS) %>%
            mutate(errorflag= factor(as.numeric(max(AGEDAYS) > AGEDTH))) %>%
            ungroup() %>% group_by(STUDYID) %>%
            mutate(rank=rank(AGEDTH))

#For each study with mortality
#Drop children who didn't die
#rank order by age of death
#Y axis is rank
#X axis is point and line of observations (AGEDAYS) and X's (point) at agedth. Color X red if it occurs before final obs
p2 <- ggplot(df) + 
  geom_line(aes(x=AGEDAYS, y=rank, group=SUBJID), alpha=0.2) +
  geom_point(aes(x=AGEDAYS, y=rank, group=SUBJID), alpha=0.2) +
  geom_point(aes(x=AGEDTH, y=rank, group=SUBJID, color=errorflag), shape=4, size=4) + 
  facet_wrap(~STUDYID, scales="free") +
  theme_bw() + scale_color_manual(values=c("black","red"))
p2



