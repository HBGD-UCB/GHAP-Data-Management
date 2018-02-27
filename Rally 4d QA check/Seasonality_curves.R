



rm(list=ls())
library(tidyverse)

setwd("U:/data/Rally4b_data")



subset_vec <- c("SUBJID", "STUDYID","COUNTRY","WHZ","AGEDAYS","BRTHWEEK","BRTHYR","LATITUDE", "LONGITUD",
                "HAZ","WAZ","ARM","BRTHYR","DURBRST", "BRSTFEED", "BFEDFL", "FEEDING","SUMEP",
                "SUMDIAR",
                "SUMDAYS",
                "PCTDIAR",
                "diarfl",
                "DIARFL")


dfull <- NULL
#"agakhanuniv.
d<-readRDS("akup.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"burkina_faso_zn

d<-readRDS("bfzn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"cmc_v_bcs_2002 

d<-readRDS("cmc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)  

#"cmin.      

d<-readRDS("cmin.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"cohorts

d<-readRDS("cort.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"content  

d<-readRDS("cntt.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"ee           

d<-readRDS("ee.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)   

#"eu

d<-readRDS("eu.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)   

#"gms_nepal     

d<-readRDS("gmsn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"gusto       

d<-readRDS("gsto.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"guatemala_bsc 

d<-readRDS("gbsc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"irc          

d<-readRDS("irc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)  

#"jivita_3       
# 
# d<-readRDS("jvt3.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] 
# dfull <- bind_rows(dfull, d) 

#"jivita_4    

d<-readRDS("jvt4.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"keneba        

d<-readRDS("knba.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"lcni_5      

d<-readRDS("lcn5.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"mal_ed        

d<-readRDS("mled.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"nih_birth    

d<-readRDS("nbrt.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"probit         

# d<-readRDS("prbt.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] 
# dfull <- bind_rows(dfull, d) 

#"peru_huascar.

d<-readRDS("phua.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"respak        

d<-readRDS("rspk.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"sas_compfeed.rds") 
d<-readRDS("cmpf.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"sas_foodsuppl.rds") 
d<-readRDS("fspp.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"tdc.rds") 
d<-readRDS("tdc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)  

#"tanzaniachild2.rds") 
d<-readRDS("tzc2.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"zvitambo.rds") 
d<-readRDS("zvit.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"znmort.rds") 
d<-readRDS("zmrt.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d) 

#"ilins_zinc.rds") 
d<-readRDS("lnsz.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)


# 
# #"wash_bangladesh.rds") 
# d<-readRDS("wbb.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] 
# dfull <- bind_rows(dfull, d)
# 
# 
# #"wash_kenya.rds") 
# d<-readRDS("wbk.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] 
# dfull <- bind_rows(dfull, d)


#"bigcs_ultrasound.rds") 
d<-readRDS("bigu.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"ilins_dose.rds") 
d<-readRDS("ilnd.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"ilins_dyad_m.rds") 
d<-readRDS("ildm.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"imnci.rds") 
d<-readRDS("imnc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"amanhi.rds") 
d<-readRDS("amni.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"peru_zn.rds") 
d<-readRDS("pzn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"Ecuador Egg.rds") 
d<-readRDS("eegg.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

#"Bangladesh Diarrhea.rds") 
d<-readRDS("bngd.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)



d<-readRDS("gual.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("zinf.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("prvd.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("ppd.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("ncry.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("mmam.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("mahn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("incp.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("grip.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("gtwn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)

d<-readRDS("eczn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)


d<-readRDS("dvds.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] 
dfull <- bind_rows(dfull, d)





#Calculate week of the year
df <- dfull %>% group_by(STUDYID, COUNTRY) %>% filter(!is.na(BRTHWEEK) & !is.na(WHZ) & !is.na(AGEDAYS)) %>%
  filter(WHZ > -5 & WHZ < 5) %>%
  mutate(julian_day = AGEDAYS + BRTHWEEK*7 + (BRTHYR -min(BRTHYR)),
         studyday=AGEDAYS + BRTHWEEK*7 + (BRTHYR -min(BRTHYR)),
         year=studyday/365)
df$julian_day[df$julian_day > 365] <-  df$julian_day[df$julian_day > 365] - 365
df$julian_day[df$julian_day > 365] <-  df$julian_day[df$julian_day > 365] - 365
df$julian_day[df$julian_day > 365] <-  df$julian_day[df$julian_day > 365] - 365
df$julian_day[df$julian_day > 365] <-  df$julian_day[df$julian_day > 365] - 365
df$julian_day[df$julian_day > 365] <-  df$julian_day[df$julian_day > 365] - 365
df$julian_day[df$julian_day > 365] <-  df$julian_day[df$julian_day > 365] - 365
df$julian_day[df$julian_day < 0] <- 0
df$COUNTRY[df$COUNTRY=="TANZANIA, UNITED REPUBLIC OF"] <- "TANZANIA"
df$cohort <- paste0(df$STUDYID, " ", df$COUNTRY)


df$region <- "South Asia"
df$region[df$COUNTRY=="BURKINA FASO" | 
            df$COUNTRY=="GAMBIA" | 
            df$COUNTRY=="MALAWI" | 
            df$COUNTRY=="ZIMBABWE" | 
            df$COUNTRY=="TANZANIA" | 
            df$COUNTRY=="GUINEA-BISSAU"] <- "Sub-Saharan Africa"
df$region[df$COUNTRY=="PERU" | 
            df$COUNTRY=="GUATEMALA" | 
            df$COUNTRY=="BRAZIL"] <- "South America"
df$region <- factor(df$region, levels=c("South Asia", "Sub-Saharan Africa", "South America"))


save(df, file="season_df.Rdata")



#Plot 
theme_set(theme_bw())

# grab a color blind friendly palette
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols <- cbPalette[c(1,3,7)]

p <- gmsn_age<-ggplot(df, aes(x = julian_day)) +
  geom_smooth(aes(y=WHZ), color="#D55E00") +
  #coord_cartesian(ylim = c(-2, 1)) +
  #geom_jitter(aes(y=WHZ,x=julian_day), height = 0.2, width=0.2,  alpha = 0.1, size=0.5)+
  facet_wrap(region~cohort, scales="free") +
  labs(y = "WHZ",
       x = "Day of the year",
       title = "Seasonality of wasting across study cohorts") +
  theme(strip.background = element_blank())
p



asia <- gmsn_age<-ggplot(df[df$region=="South Asia",], aes(x = julian_day)) +
  geom_smooth(aes(y=WHZ), color="#D55E00") +
  #coord_cartesian(ylim = c(-2, 1)) +
  #geom_jitter(aes(y=WHZ,x=julian_day), height = 0.2, width=0.2,  alpha = 0.1, size=0.5)+
  facet_wrap(~cohort, scales="free") +
  labs(y = "WHZ",
       x = "Day of the year",
       title = "Seasonality of wasting across study cohorts") +
  theme(strip.background = element_blank())
africa <- gmsn_age<-ggplot(df[df$region=="Sub-Saharan Africa",], aes(x = julian_day)) +
  geom_smooth(aes(y=WHZ), color="#D55E00") +
  #coord_cartesian(ylim = c(-2, 1)) +
  #geom_jitter(aes(y=WHZ,x=julian_day), height = 0.2, width=0.2,  alpha = 0.1, size=0.5)+
  facet_wrap(~cohort, scales="free") +
  labs(y = "WHZ",
       x = "Day of the year",
       title = "Seasonality of wasting across study cohorts") +
  theme(strip.background = element_blank())
sAmerica <- gmsn_age<-ggplot(df[df$region=="South America",], aes(x = julian_day)) +
  geom_smooth(aes(y=WHZ), color="#D55E00") +
  #coord_cartesian(ylim = c(-2, 1)) +
  #geom_jitter(aes(y=WHZ,x=julian_day), height = 0.2, width=0.2,  alpha = 0.1, size=0.5)+
  facet_wrap(~cohort, scales="free") +
  labs(y = "WHZ",
       x = "Day of the year",
       title = "Seasonality of wasting across study cohorts") +
  theme(strip.background = element_blank())



asia
africa
sAmerica


study_period <- gmsn_age<-ggplot(df, aes(x = year)) +
  geom_smooth(aes(y=WHZ), color="#D55E00") +
  #coord_cartesian(ylim = c(-2, 1)) +
  #geom_jitter(aes(y=WHZ,x=julian_day), height = 0.2, width=0.2,  alpha = 0.1, size=0.5)+
  facet_wrap(region~cohort, scales="free") +
  labs(y = "WHZ",
       x = "Year of study",
       title = "Seasonality of wasting across study cohorts") +
  theme(strip.background = element_blank())
study_period

