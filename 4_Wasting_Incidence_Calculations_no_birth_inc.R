
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


setwd("U:/GHAP-Data-Management")
source("Wast_incidence_functions.R")

setwd("U:/data")

d<-readRDS("svta.rds")
svta_inc<-WastIncCalc(d, dropBornWasted=T)
svta_inc_table <- WastIncTable(svta_inc)
save(svta_inc, svta_inc_table, file="WastIncDatasets/svta_NoBirthInc.Rdata")

d<-readRDS("vita.rds")
vita_inc<-WastIncCalc(d, dropBornWasted=T)
vita_inc_table <- WastIncTable(vita_inc)
save(vita_inc, vita_inc_table, file="WastIncDatasets/vita_NoBirthInc.Rdata")


d<-readRDS("vb12.rds")
vb12_inc<-WastIncCalc(d, dropBornWasted=T)
vb12_inc_table <- WastIncTable(vb12_inc)
save(vb12_inc, vb12_inc_table, file="WastIncDatasets/vb12_NoBirthInc.Rdata")


d<-readRDS("gmsn.rds")
gmsn_inc<-WastIncCalc(d, dropBornWasted=T)
gmsn_inc_table <- WastIncTable(gmsn_inc)
save(gmsn_inc, gmsn_inc_table, file="WastIncDatasets/gmsn_NoBirthInc.Rdata")

d<-readRDS("zmrt.rds")
zmrt_inc<-WastIncCalc(d, dropBornWasted=T)
zmrt_inc_table <- WastIncTable(zmrt_inc)
save(zmrt_inc, zmrt_inc_table, file="WastIncDatasets/zmrt_NoBirthInc.Rdata")

d<-readRDS("cntt.rds")
cntt_inc<-WastIncCalc(d, dropBornWasted=T)
cntt_inc_table <- WastIncTable(cntt_inc)
save(cntt_inc, cntt_inc_table, file="WastIncDatasets/cntt_NoBirthInc.Rdata")

d<-readRDS("gbsc.rds")
gbsc_inc<-WastIncCalc(d, dropBornWasted=T)
gbsc_inc_table <- WastIncTable(gbsc_inc)
save(gbsc_inc, gbsc_inc_table, file="WastIncDatasets/gbsc_NoBirthInc.Rdata")


d<-readRDS("cmin.rds")
unique(d$COUNTRY)
cmin_inc_peru<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[1],])
cmin_inc_brazil<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[2],])
cmin_inc_guinea_bissau<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[3],])
cmin_inc_bangladesh<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[4],])
cmin_inc_table_peru <- WastIncTable(cmin_inc_peru)
cmin_inc_table_brazil <- WastIncTable(cmin_inc_brazil)
cmin_inc_table_guinea_bissau <- WastIncTable(cmin_inc_guinea_bissau)
cmin_inc_table_bangladesh <- WastIncTable(cmin_inc_bangladesh)
save(cmin_inc_peru, 
     cmin_inc_brazil,
     cmin_inc_guinea_bissau,
     cmin_inc_bangladesh,
     cmin_inc_table_peru,
     cmin_inc_table_brazil,
     cmin_inc_table_guinea_bissau,
     cmin_inc_table_bangladesh,
     file="WastIncDatasets/cmin_NoBirthInc.Rdata")


d<-readRDS("phua.rds")
phua_inc<-WastIncCalc(d, dropBornWasted=T)
phua_inc_table <- WastIncTable(phua_inc)
save(phua_inc, phua_inc_table, file="WastIncDatasets/phua_NoBirthInc.Rdata")


d<-readRDS("tzc2.rds")
tzc2_inc<-WastIncCalc(d, dropBornWasted=T)
tzc2_inc_table <- WastIncTable(tzc2_inc)
save(tzc2_inc, tzc2_inc_table, file="WastIncDatasets/tzc2_NoBirthInc.Rdata")


d<-readRDS("cmc.rds") 
cmc_inc<-WastIncCalc(d, dropBornWasted=T)
cmc_inc_table <- WastIncTable(cmc_inc)
save(cmc_inc, cmc_inc_table, file="WastIncDatasets/cmc_NoBirthInc.Rdata")


d<-readRDS("ee.rds")  
ee_inc<-WastIncCalc(d, dropBornWasted=T)
ee_inc_table <- WastIncTable(ee_inc)
save(ee_inc, ee_inc_table, file="WastIncDatasets/ee_NoBirthInc.Rdata")


d<-readRDS("irc.rds") 
irc_inc<-WastIncCalc(d, dropBornWasted=T)
irc_inc_table <- WastIncTable(irc_inc)
save(irc_inc, irc_inc_table, file="WastIncDatasets/irc_NoBirthInc.Rdata")


d<-readRDS("tdc.rds")
tdc_inc<-WastIncCalc(d, dropBornWasted=T)
tdc_inc_table <- WastIncTable(tdc_inc)
save(tdc_inc, tdc_inc_table, file="WastIncDatasets/tdc_NoBirthInc.Rdata")


d<-readRDS("rspk.rds")
rspk_inc<-WastIncCalc(d, dropBornWasted=T)
rspk_inc_table <- WastIncTable(rspk_inc)
save(rspk_inc, rspk_inc_table, file="WastIncDatasets/rspk_NoBirthInc.Rdata")



d<-readRDS("mled.rds")
length(unique(d$COUNTRY))
mled_inc_bangladesh<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[1],])
mled_inc_brazil<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[2],])
mled_inc_india<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[3],])
mled_inc_nepal<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[4],])
mled_inc_peru<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[5],])
mled_inc_pakistan<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[6],])
mled_inc_southafrica<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[7],])
mled_inc_tanzania<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[8],])
mled_inc_table_bangladesh <- WastIncTable(mled_inc_bangladesh)
mled_inc_table_brazil <- WastIncTable(mled_inc_brazil)
mled_inc_table_india <- WastIncTable(mled_inc_india)
mled_inc_table_nepal <- WastIncTable(mled_inc_nepal)
mled_inc_table_peru <- WastIncTable(mled_inc_peru)
mled_inc_table_pakistan <- WastIncTable(mled_inc_pakistan)
mled_inc_table_southafrica <- WastIncTable(mled_inc_southafrica)
mled_inc_table_tanzania <- WastIncTable(mled_inc_tanzania)

save(mled_inc_bangladesh,
     mled_inc_brazil,
     mled_inc_india,
     mled_inc_nepal,
     mled_inc_peru,
     mled_inc_pakistan,
     mled_inc_southafrica,
     mled_inc_tanzania,
     mled_inc_table_bangladesh,
     mled_inc_table_brazil,
     mled_inc_table_india,
     mled_inc_table_nepal,
     mled_inc_table_peru,
     mled_inc_table_pakistan,
     mled_inc_table_southafrica,
     mled_inc_table_tanzania,
     file="WastIncDatasets/mled_NoBirthInc.Rdata")
gc()



#Non-monthly cohorts
quart_cohorts <- c("amni",  "ansa",  "bts",  "knba", "nvta", "npre", "prvd", "phua", "rspk", "cmpf",
                   "fspp", "solr", "tdc",  "tzc2", "gual", "zvit", "lnsz")



d<-readRDS("knba.rds")
table(d$COUNTRY)
knba_inc<-WastIncCalc(d, dropBornWasted=T)
knba_inc_table <- WastIncTable(knba_inc)
save(knba_inc, knba_inc_table, file="WastIncDatasets/knba_NoBirthInc.Rdata")

#Provide: GHAP metadata mentions obsolete: need to figure out
d<-readRDS("prvd.rds")
table(d$COUNTRY)
prvd_inc<-WastIncCalc(d, dropBornWasted=T)
prvd_inc_table <- WastIncTable(prvd_inc)
save(prvd_inc, prvd_inc_table, file="WastIncDatasets/prvd_NoBirthInc.Rdata")

d<-readRDS("phua.rds")
table(d$COUNTRY)
phua_inc<-WastIncCalc(d, dropBornWasted=T)
phua_inc_table <- WastIncTable(phua_inc)
save(phua_inc, phua_inc_table, file="WastIncDatasets/phua_NoBirthInc.Rdata")

d<-readRDS("rspk.rds")
table(d$COUNTRY)
rspk_inc<-WastIncCalc(d, dropBornWasted=T)
rspk_inc_table <- WastIncTable(rspk_inc)
save(rspk_inc, rspk_inc_table, file="WastIncDatasets/rspk_NoBirthInc.Rdata")

d<-readRDS("cmpf.rds")
table(d$COUNTRY)
cmpf_inc<-WastIncCalc(d, dropBornWasted=T)
cmpf_inc_table <- WastIncTable(cmpf_inc)
save(cmpf_inc, cmpf_inc_table, file="WastIncDatasets/cmpf_NoBirthInc.Rdata")

d<-readRDS("fspp.rds")
table(d$COUNTRY)
fspp_inc<-WastIncCalc(d, dropBornWasted=T)
fspp_inc_table <- WastIncTable(fspp_inc)
save(fspp_inc, fspp_inc_table, file="WastIncDatasets/fspp_NoBirthInc.Rdata")


d<-readRDS("zvit.rds")
table(d$COUNTRY)
#Have to develop workaround for memory issues:
length(unique(d$SUBJID))
summary(d$SUBJID)
zvit_inc<-WastIncCalc(d, dropBornWasted=T)
zvit_inc_table <- WastIncTable(zvit_inc)
save(zvit_inc, zvit_inc_table, file="WastIncDatasets/zvit_NoBirthInc.Rdata")

d<-readRDS("lnsz.rds")
table(d$COUNTRY)
lnsz_inc<-WastIncCalc(d, dropBornWasted=T)
lnsz_inc_table <- WastIncTable(lnsz_inc)
save(lnsz_inc, lnsz_inc_table, file="WastIncDatasets/lnsz_NoBirthInc.Rdata")






d<-readRDS("akup.rds")
table(d$COUNTRY)
akup_inc<-WastIncCalc(d, dropBornWasted=T)
akup_inc_table <- WastIncTable(akup_inc)
save(akup_inc, akup_inc_table, file="WastIncDatasets/akup_NoBirthInc.Rdata")

d<-readRDS("bfzn.rds")
table(d$COUNTRY)
bfzn_inc<-WastIncCalc(d, dropBornWasted=T)
bfzn_inc_table <- WastIncTable(bfzn_inc)
save(bfzn_inc, bfzn_inc_table, file="WastIncDatasets/bfzn_NoBirthInc.Rdata")
gc()

d<-readRDS("cmc.rds")
table(d$COUNTRY)
cmc_inc<-WastIncCalc(d, dropBornWasted=T)
cmc_inc_table <- WastIncTable(cmc_inc)
save(cmc_inc, cmc_inc_table, file="WastIncDatasets/cmc_NoBirthInc.Rdata")

d<-readRDS("cntt.rds")
table(d$COUNTRY)
cntt_inc<-WastIncCalc(d, dropBornWasted=T)
cntt_inc_table <- WastIncTable(cntt_inc)
save(cntt_inc, cntt_inc_table, file="WastIncDatasets/cntt_NoBirthInc.Rdata")

d<-readRDS("ee.rds")
table(d$COUNTRY)
ee_inc<-WastIncCalc(d, dropBornWasted=T)
ee_inc_table <- WastIncTable(ee_inc)
save(ee_inc, ee_inc_table, file="WastIncDatasets/ee_NoBirthInc.Rdata")

d<-readRDS("eu.rds")
table(d$COUNTRY)
eu_inc<-WastIncCalc(d, dropBornWasted=T)
eu_inc_table <- WastIncTable(eu_inc)
save(eu_inc, eu_inc_table, file="WastIncDatasets/eu_NoBirthInc.Rdata")

d<-readRDS("gsto.rds")
table(d$COUNTRY)
gsto_inc<-WastIncCalc(d, dropBornWasted=T)
gsto_inc_table <- WastIncTable(gsto_inc)
save(gsto_inc, gsto_inc_table, file="WastIncDatasets/gsto_NoBirthInc.Rdata")
gc()


d<-readRDS("gbsc.rds")
table(d$COUNTRY)
gbsc_inc<-WastIncCalc(d, dropBornWasted=T)
gbsc_inc_table <- WastIncTable(gbsc_inc)
save(gbsc_inc, gbsc_inc_table, file="WastIncDatasets/gbsc_NoBirthInc.Rdata")


d<-readRDS("jvt3.rds")
table(d$COUNTRY)
jvt3_inc<-WastIncCalc(d, dropBornWasted=T)
jvt3_inc_table <- WastIncTable(jvt3_inc)
save(jvt3_inc, jvt3_inc_table, file="WastIncDatasets/jvt3_NoBirthInc.Rdata")
gc()



d<-readRDS("jvt4.rds")
jvt4_inc <- WastIncCalc(d, dropBornWasted=T)
jvt4_inc_table <- WastIncTable(jvt4_inc)
save(jvt4_inc, jvt4_inc_table, file="WastIncDatasets/jvt4_NoBirthInc.Rdata")
gc()



d<-readRDS("lcn5.rds")
table(d$COUNTRY)
lcn5_inc<-WastIncCalc(d, dropBornWasted=T)
lcn5_inc_table <- WastIncTable(lcn5_inc)
save(lcn5_inc, lcn5_inc_table, file="WastIncDatasets/lcn5_NoBirthInc.Rdata")

d<-readRDS("nbrt.rds")
table(d$COUNTRY)
nbrt_inc<-WastIncCalc(d, dropBornWasted=T)
nbrt_inc_table <- WastIncTable(nbrt_inc)
save(nbrt_inc, nbrt_inc_table, file="WastIncDatasets/nbrt_NoBirthInc.Rdata")


d<-readRDS("prbt.rds")
prbt_inc<-WastIncCalc(d, dropBornWasted=T)
prbt_inc_table <- WastIncTable(prbt_inc)
save(prbt_inc, prbt_inc_table, file="WastIncDatasets/prbt_NoBirthInc.Rdata")
gc()


#need to run:
d<-readRDS("cort.rds")
unique(d$COUNTRY)
cort_inc_brazil<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[1],])
cort_inc_guatemala<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[2],])
cort_inc_india<-WastIncCalc(d, dropBornWasted=T)
cort_inc_philippines<-WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[4],])
cort_inc_southafrica <- WastIncCalc(d[d$COUNTRY==unique(d$COUNTRY)[5],])
cort_inc_table_brazil <- WastIncTable(cort_inc_brazil)
cort_inc_table_guatemala <- WastIncTable(cort_inc_guatemala)
cort_inc_table_india <- WastIncTable(cort_inc_india)
cort_inc_table_philippines <- WastIncTable(cort_inc_philippines)
cort_inc_table_southafrica <- WastIncTable(cort_inc_southafrica)
save(cort_inc_brazil, 
     cort_inc_guatemala,
     cort_inc_india,
     cort_inc_philippines,
     cort_inc_southafrica,
     cort_inc_table_brazil,
     cort_inc_table_guatemala,
     cort_inc_table_india,
     cort_inc_table_philippines,
     cort_inc_table_southafrica,
     file="WastIncDatasets/cort_NoBirthInc.Rdata")
gc()





#Newly added 10/17

d<-readRDS("ncry.rds")
table(d$COUNTRY)
ncry_inc<-WastIncCalc(d, dropBornWasted=T)
ncry_inc_table <- WastIncTable(ncry_inc)
save(ncry_inc, ncry_inc_table, file="WastIncDatasets/ncry_NoBirthInc.Rdata")


d<-readRDS("incp.rds")
table(d$COUNTRY)
incp_inc<-WastIncCalc(d, dropBornWasted=T)
incp_inc_table <- WastIncTable(incp_inc)
save(incp_inc, incp_inc_table, file="WastIncDatasets/incp_NoBirthInc.Rdata")

d<-readRDS("eczn.rds")
table(d$COUNTRY)
eczn_inc<-WastIncCalc(d, dropBornWasted=T)
eczn_inc_table <- WastIncTable(eczn_inc)
save(eczn_inc, eczn_inc_table, file="WastIncDatasets/eczn_NoBirthInc.Rdata")

d<-readRDS("prvd.rds")
table(d$COUNTRY)
prvd_inc<-WastIncCalc(d, dropBornWasted=T)
prvd_inc_table <- WastIncTable(prvd_inc)
save(prvd_inc, prvd_inc_table, file="WastIncDatasets/prvd_NoBirthInc.Rdata")


d<-readRDS("dvds.rds")
table(d$COUNTRY)
dvds_inc<-WastIncCalc(d, dropBornWasted=T)
dvds_inc_table <- WastIncTable(dvds_inc)
save(dvds_inc, dvds_inc_table, file="WastIncDatasets/dvds_NoBirthInc.Rdata")




#Add in yearly trials
d<-readRDS("wsb.rds")
table(d$COUNTRY)
wsb_inc<-WastIncCalc(d, dropBornWasted=T)
wsb_inc_table <- WastIncTable(wsb_inc)
save(wsb_inc, wsb_inc_table, file="WastIncDatasets/wsb_NoBirthInc.Rdata")

d<-readRDS("wsk.rds")
table(d$COUNTRY)
wsk_inc<-WastIncCalc(d, dropBornWasted=T)
wsk_inc_table <- WastIncTable(wsk_inc)
save(wsk_inc, wsk_inc_table, file="WastIncDatasets/wsk_NoBirthInc.Rdata")

d<-readRDS("ilnd.rds")
table(d$COUNTRY)
ilnd_inc<-WastIncCalc(d, dropBornWasted=T)
ilnd_inc_table <- WastIncTable(ilnd_inc)
save(ilnd_inc, ilnd_inc_table, file="WastIncDatasets/ilnd_NoBirthInc.Rdata")

d<-readRDS("ildm.rds")
table(d$COUNTRY)
ildm_inc<-WastIncCalc(d, dropBornWasted=T)
ildm_inc_table <- WastIncTable(ildm_inc)
save(ildm_inc, ildm_inc_table, file="WastIncDatasets/ildm_NoBirthInc.Rdata")



