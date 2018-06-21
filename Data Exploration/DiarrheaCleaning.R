


#aga khan
library(haven)



d <- read_sas("U:/data/AgaKhanUniv/raw/childmorbidityandimmunization.sas7bdat")
head(d)
d$subjido <- gsub("C-Y-C-","",d$frmid)
d$subjido <- gsub("I-Y-C-","",d$subjido)

d$visitnum <- as.numeric(d$visit_)
d$agedays<- round(d$age*30.42)
table(d$a1)

akup<-readRDS("U:/data/akup.rds")
colnames(akup) <- tolower(colnames(akup))
akup$visitnum <- as.numeric(akup$visitnum )
head(akup)

akup <- left_join(akup, d, by=c("subjido","visitnum"))
table(akup$a1)
table(akup$diarfl, akup$a1)

table(d$visitnum)
table(akup$visitnum)

table(d$subjido)
table(akup$subjido)

#There isn't enough info to merge diarrhea with outcome dataset, but could append as its own rows with its own ages




#Ilins Dose
d <- read_sas("U:/data/iLiNS-DOSE/raw/morbid18tab.sas7bdat")
head(d)

table(d$HomNumberStool)
d$HomNumberStool[d$HomNumberStool==88] <- NA
d$HomNumberStool[d$HomNumberStool==99] <- NA
table(d$HomNumberStool>2) #diarrhea defined by 3 or more loose stools


ilnd<-readRDS("U:/data/ilnd.rds")
colnames(ilnd) <- tolower(colnames(ilnd))
#ilnd$visitnum <- as.numeric(ilnd$visitnum )
head(ilnd)

table(ilnd$diarfl)

#Analysis dataset looks good, just merge in to final dataset




#Tanzania child
d <- read_sas("U:/data/TanzaniaChild2/import/childnurse.sas7bdat")
head(d)
colnames(d) <- tolower(colnames(d))

table(d$cndt) #diarrhea today
d$cndt <- d$cndt - 1
mean(d$cndt, na.rm=T)

table(d$cchadm) #visit number

d <- d %>% rename(subjid=idno2, visitnum=cchadm, diarfl=cndt) %>% mutate(subjid=as.numeric(subjid)) %>% select(subjid, visitnum, diarfl)

tzc2<-readRDS("U:/data/tzc2.rds")
colnames(tzc2) <- tolower(colnames(tzc2))
tzc2<- tzc2 %>% select(studyid, subjid, visitnum)
head(tzc2)

table(tzc2$visitnum)
tzc2$visitnum <- as.numeric(tzc2$visitnum)

tzc2 <- left_join(tzc2, d, by=c("subjid","visitnum"))
table(tzc2$diarfl)





