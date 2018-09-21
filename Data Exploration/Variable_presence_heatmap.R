

rm(list=ls())
library(tidyverse)
library(forcats)

setwd("U:/data/Rally4b_data")

#Fix to load all (relevenant) rds files and check for variable and check variation
varlist <- c("FEEDING", "GAGEBRTH", "BRTHWEEK", "BIRTHWT","MWTKG", "MHTCM","MMUAC",  "MAGE",  "MEDUCYRS",       
             "MMARITN", "FAGE", "FHTCM", "FEDUCYRS",
             "PCTDIAR", "DIARFL",  "BFEDFL",                  
             "PARITY",  "NPERSON",  "NCHLDLT5",             
             "FLOOR", "H2OSRC",  "NROOMS",         
             "H2OTRTP",  "SOAP",  "CHICKEN", "COW", "GOAT",     
             "SES", "TV", "DLVLOCN" ,"DELIVERY", "ARM", "AGEDTH", "DEAD") 

varnames <- c("Breastfeeding", "Gestational age", "Birthweek", "Birthweight","Maternal Weight","Maternal Height", "Maternal MUAC",  "Maternal Age",  "Maternal education",  
              "Maternal marital status", "Paternal MUAC",  "Paternal Age",  "Paternal education", 
              "Percent days with diarrhea", "Diarrhea at visit",  "Breastfed day of visit",                  
              
              "Birth order",  "Number of persons in household",  "Number of children under 5",             
              "Floor material", "Water source", "Number of rooms",         
              "Water treatment",  "Soap in home", "Chicken ownership", "Cow ownership", "Goat ownership",     
              "SES category", "TV ownership", "Delivery location",  "Delivery method", "RCT Intervention", "Age at death", "Dead" ) 



subset_vec <- c("SUBJID", "STUDYID","WHZ","AGEDAYS",varlist)


d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-NULL

#"agakhanuniv.
d<-readRDS("akup.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character)) 
dfull <-bind_rows(dfull, d) 

#"burkina_faso_zn

d<-readRDS("bfzn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character)) 
dfull <-bind_rows(dfull, d) 

#"cmc_v_bcs_2002 

d<-readRDS("cmc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))
dfull <-bind_rows(dfull, d)  

#"cmin.      

d<-readRDS("cmin.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))
dfull <-bind_rows(dfull, d) 

#"cohorts

d<-readRDS("cort.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"content  

d<-readRDS("cntt.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"ee           

d<-readRDS("ee.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)   

#"eu

d<-readRDS("eu.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)   

#"gms_nepal     

d<-readRDS("gmsn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"gusto       

d<-readRDS("gsto.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"guatemala_bsc 

d<-readRDS("gbsc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"irc          

d<-readRDS("irc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)  

#"jivita_3       

d<-readRDS("jvt3.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"jivita_4    

d<-readRDS("jvt4.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"keneba        

d<-readRDS("knba.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"lcni_5      

d<-readRDS("lcn5.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"mal_ed        

d<-readRDS("mled.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"nih_birth    

d<-readRDS("nbrt.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"probit         

d<-readRDS("prbt.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"peru_huascar.

d<-readRDS("phua.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"respak        

d<-readRDS("rspk.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"sas_compfeed.rds") 
d<-readRDS("cmpf.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"sas_foodsuppl.rds") 
d<-readRDS("fspp.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"tdc.rds") 
d<-readRDS("tdc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)  

#"tanzaniachild2.rds") 
d<-readRDS("tzc2.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"zvitambo.rds") 
d<-readRDS("zvit.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"znmort.rds") 
d<-readRDS("zmrt.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d) 

#"ilins_zinc.rds") 
d<-readRDS("lnsz.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)



#"wash_bangladesh.rds") 
d<-readRDS("wbb.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)


#"wash_kenya.rds") 
d<-readRDS("wbk.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)


#"bigcs_ultrasound.rds") 
d<-readRDS("bigu.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"ilins_dose.rds") 
d<-readRDS("ilnd.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"ilins_dyad_m.rds") 
d<-readRDS("ildm.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"imnci.rds") 
d<-readRDS("imnc.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"amanhi.rds") 
d<-readRDS("amni.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"peru_zn.rds") 
d<-readRDS("pzn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"Ecuador Egg.rds") 
d<-readRDS("eegg.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

#"Bangladesh Diarrhea.rds") 
d<-readRDS("bngd.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)



d<-readRDS("gual.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

d<-readRDS("zinf.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

d<-readRDS("prvd.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

d<-readRDS("ppd.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)
# 
# d<-readRDS("ncry.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))
# 
# dfull <-bind_rows(dfull, d)
# 
# d<-readRDS("mmam.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))
# 
# dfull <-bind_rows(dfull, d)

# d<-readRDS("mahn.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))
# 
# dfull <-bind_rows(dfull, d)
# 
# d<-readRDS("incp.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))
# 
# dfull <-bind_rows(dfull, d)

# d<-readRDS("grip.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))
# 
# dfull <-bind_rows(dfull, d)
# 
# d<-readRDS("gtwn.rds") 
# d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))
# 
# dfull <-bind_rows(dfull, d)

d<-readRDS("eczn.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)


d<-readRDS("dvds.rds") 
d <- d[, which(colnames(d) %in% subset_vec)] %>% group_by(SUBJID) %>% slice(1) %>% mutate_each(funs(as.character))

dfull <-bind_rows(dfull, d)

df <- dfull

for(i in 3:ncol(df)){
  df[,i] <- ifelse(is.na(df[,i]),0,1)
}


plotdf <- df %>% subset(., select= -c(SUBJID)) %>% group_by(STUDYID) %>% summarise_each(funs(mean))
for(i in 2:ncol(plotdf)){
  plotdf[,i] <- ifelse(plotdf[,i]==0,0,1)
}
head(plotdf)


plotdf <- plotdf %>% subset(. , select = -c(ARM, AGEDAYS, WHZ))

library(reshape2)
plotdf <- melt(plotdf, id.vars = c("STUDYID"))
head(plotdf)

varfreq <- plotdf %>% group_by(variable) %>% 
  summarize(numstudies= sum(value)) %>%
  arrange(-numstudies)
varfreq

studyfreq <- plotdf %>% group_by(STUDYID) %>% 
  summarize(numstudies= sum(value)) %>%
  arrange(numstudies)
studyfreq


#order variables and studies by frequency for the plot
plotdf$variable <- factor(plotdf$variable , levels=varfreq$variable)
plotdf$STUDYID <- factor(plotdf$STUDYID , levels=studyfreq$STUDYID)

#rename variables
plotdf <- mutate(plotdf, variable = fct_recode(variable, 
                                               "Birth week" = "BRTHWEEK",
                                               "Birth weight" = "BIRTHWT",
                                               "Mom age" = "MAGE",
                                               "Mom height" = "MHTCM",
                                               "Mom weight" = "MWTKG",
                                               "Mom education" = "MEDUCYRS",
                                               "SES" = "SES",
                                               "Delivery location" = "DLVLOCN",
                                               "Current diarrhea" = "DIARFL",
                                               "Number of rooms" = "NROOMS",
                                               "Water source" = "H2OSRC",
                                               "TV" = "TV",
                                               "Cow" = "COW",
                                               "Goat" = "GOAT",
                                               "Diarrhea days" = "PCTDIAR",
                                               "Birth order" = "PARITY",
                                               "Number of children" = "NCHLDLT5",
                                               "Number in household" = "NPERSON",
                                               "Breastfeeding" = "FEEDING",
                                               "Delivery method" = "DELIVERY",
                                               "Floor" = "FLOOR",
                                               "Water treatment" = "H2OTRTP",
                                               "Gestational age" = "GAGEBRTH",
                                               "Marital status" = "MMARITN",
                                               "Dad age" = "FAGE",
                                               "Dad height" = "FHTCM",
                                               "Dad education" = "FEDUCYRS",
                                               "Current breastfeeding" = "BFEDFL",
                                               "Mortality" = "DEAD",
                                               "Time of death" = "AGEDTH",
                                               "Chicken" = "CHICKEN",
                                               "Soap" = "SOAP",
                                               "Mom MUAC" = "MMUAC"
))


#define a color for fonts
textcol <- "grey20"

# heat map plot scheme
hm <- ggplot(plotdf,aes(x=variable,y=STUDYID)) +
  #add border white colour of line thickness 0.25
  geom_tile(aes(fill=factor(value)), colour="white",size=0.25)+
  #remove extra space
  # scale_y_discrete(expand=c(0,0))+
  # scale_x_continuous(expand=c(0,0),
  #                    breaks=1:24,labels=1:24)+
  #one unit on x-axis is equal to one unit on y-axis.
  #equal aspect ratio x and y axis
  #coord_equal()+
  #set base size for all font elements
  theme_grey(base_size=10)+
  #Set white and black scale
  scale_fill_manual(values=c("white", "#1F77B4")) +
  #put x-axis on top
  scale_x_discrete(position = "top") +
  xlab("") + ylab("") +
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
    legend.position="none",
    #remove plot margins
    plot.margin = margin(0, 0, 2, 0, "cm")
  )

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
