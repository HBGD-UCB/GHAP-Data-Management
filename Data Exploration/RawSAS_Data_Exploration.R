



library(haven)


setwd("U:/git/hbgd/ki1017093c/NIH-Crypto-201707/raw")
d<-read_sas("baseline.sas7bdat")
head(d)


library(ghap)
df<-use_study("ncry")


df<-use_study("irc")
colnames(df)


setwd("U:/git/hbgd/ki1000108/PMC3894229/raw")
d<-read_sas("baseline.sas7bdat")
head(d)

d<-read_sas("diar2.sas7bdat")
head(d,30)


setwd("U:/data/MALED-201511/raw")
d<-read_sas("zscores_24m.sas7bdat")
head(d)



setwd("U:/data/GMS-Nepal-201606/raw")
d<-read_sas("gms5.sas7bdat")
head(d)
colnames(d)

d<-read_sas("gms6.sas7bdat")
colnames(d)

d<-read_sas("gms7.sas7bdat")
colnames(d)


d<-read_sas("visits.sas7bdat")
colnames(d)


setwd("U:/data/")
d<-readRDS("gmsn.rds")
colnames(d)



library(foreign)
setwd("U:/data/GMS-Nepal-201606/import")
d<-read.dta("GMS_Final_with_milestone_SE_zscore_wide_recall_long.dta")



setwd("U:/data/iLiNS-Zinc/raw")
d<-read_sas("ilins_zinc_anthro.sas7bdat")
colnames(d)

d<-read_sas("ilins_zinc_devlobsform06.sas7bdat")
colnames(d)

setwd("U:/data/IRC/raw/")
d<-read_sas("irc_growth_raw.sas7bdat")
colnames(d)


file:///U:/data/IRC/raw/conts_baseline.xlsx
file:///U:/data/IRC/raw/conts_diarrhea.xlsx
file:///U:/data/IRC/raw/conts_growth.xlsx
file:///U:/data/IRC/raw/irc_baseline_raw.sas7bdat
file:///U:/data/IRC/raw/irc_diarrhea_raw.sas7bdat
file:///U:/data/IRC/raw/irc_growth_raw.sas7bdat



#domain specific comparisons to analysis dataset
d <- read.csv("U:/git/hbgd/ki0047075b/MALED-201707/adam/full_ki0047075b_MALED_201707.csv")
colnames(d)


setwd("U:/git/hbgd/ki0047075b/MALED-201707/sdtm")

#socioeconomic variables
ss<-read_sas("ss.sas7bdat")
colnames(ss)

diarsum<-read_sas("diarsum.sas7bdat")
colnames(diarsum)

length(unique(d$SUBJID))
length(unique(diarsum$SUBJID))


ce<-read_sas("ce.sas7bdat")
colnames(ce)

daily<-read_sas("daily.sas7bdat")
colnames(daily)

qs<-read_sas("qs.sas7bdat")
colnames(qs)

#general findings
gf<-read_sas("gf.sas7bdat")
colnames(gf)

#nutrition
nt<-read_sas("nt.sas7bdat")
colnames(nt)
head(nt)

ho<-read_sas("ho.sas7bdat")
colnames(ho)

#death details
dd<-read_sas("dd.sas7bdat")
colnames(dd)
head(dd)





#XXXXXXXXXXXXXXXX
# Wash benefits

d <- read.csv("U:/git/hbgd/ki1000111/WASH-BK/adam/full_ki1000111_WASH_BK.csv")
colnames(d)

setwd("U:/git/hbgd/ki1000111/WASH-BK/sdtm")
ce<-read_sas("ce.sas7bdat")
colnames(ce)
head(ce)

gf<-read_sas("gf.sas7bdat")
colnames(gf)
head(gf)

#socioeconomic variables
ss<-read_sas("ss.sas7bdat")
colnames(ss)
head(ss)


subj<-read_sas("subj.sas7bdat")
colnames(subj)
head(subj)




d <- read.csv("U:/git/hbgd/ki1000108/PMC3894229/adam/KI1000108_PMC3894229.csv")
colnames(d)

setwd("U:/git/hbgd/ki1000108/PMC3894229/sdtm")
ce<-read_sas("ce.sas7bdat")
colnames(ce)
head(ce)



qs<-read_sas("qs.sas7bdat")
colnames(qs)
head(qs)


daily<-read_sas("daily.sas7bdat")
colnames(daily)
head(daily)