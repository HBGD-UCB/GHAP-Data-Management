









library(tidyverse)
library(reshape2)
load("U:/data/Compiled Datasets/WastInc0-24.Rdata")

colnames(d)
d <- d[,-c(4:31,72:108)]



varvec <- c("SEX","BIRTHWT","MAGE","MHTCM","MWTKG","MBMI","MEDUCYRS",           
            "DIARFL","BIRTHLEN","GAGEBRTH","FAGE","FHTCM",              
            "FEDUCYRS","DURBRST","SOAP","enrolstunt","birthorder",         
            "birthmonth","homedelivery","vagbirth","single","breastfeeding",      
            "HHwealth_quart","nroom","nchild5","chicken","cow",                
            "improved.floor","improved.sanitation", "safe.water","treat.water","cleancook","month" )               


#corr <- d %>% group_by(STUDYID) %>% summarise_all(cor(., .)))


d<- d %>% group_by(STUDYID)

cor(d$WHZ,d$BIRTHWT)


d = group_by(d, STUDYID)


df <- summarize(d, cor(WHZ, as.numeric(SEX)))
res <- summarize(d, cor(WHZ, as.numeric(BIRTHWT)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(MAGE)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(MHTCM)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(MWTKG)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(MBMI)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(MEDUCYRS)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(DIARFL)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(BIRTHLEN)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(GAGEBRTH)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(FAGE)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(FHTCM)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(FEDUCYRS)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(DURBRST)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(SOAP)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(enrolstunt)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(birthorder)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(birthmonth)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(homedelivery)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(vagbirth)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(single)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(breastfeeding)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(HHwealth_quart)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(nroom)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(nchild5)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(chicken)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(cow)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(improved.floor)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(improved.sanitation)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(safe.water)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(treat.water)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(cleancook)))
df <- cbind(df, res[,2])
res <- summarize(d, cor(WHZ, as.numeric(month)))
df <- cbind(df, res[,2])


colnames(df)[-1] <-varvec


plotdf<-melt(df)


#define a color for fonts
textcol <- "grey20"

# heat map plot scheme
hm <- ggplot(plotdf,aes(x=variable,y=STUDYID)) +
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  # #remove extra space
  # scale_y_discrete(expand=c(0,0))+
  # scale_x_continuous(expand=c(0,0),
  #                    breaks=1:24,labels=1:24)+
  #one unit on x-axis is equal to one unit on y-axis.
  #equal aspect ratio x and y axis
  coord_equal()+
  #set base size for all font elements
  theme_grey(base_size=10)+
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
    #move legend to the bottom
    legend.position = "bottom",
    #set x axis text size and colour
    axis.text.x=element_text(size=8,colour=textcol,angle=45,vjust=0.65),
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
    panel.border=element_blank()
    #remove plot margins
    # plot.margin=margin(grid::unit(1,"cm"))
  )


library(RColorBrewer)
# heat map
p <- hm +
  aes(fill=value) +
  labs(x="Covariate correlation with WHZ outcome",y="Study name",title="Correlation heatmap of covariates and WHZ\nacross HBGDki studies") +
  scale_fill_gradientn(colours=brewer.pal(7,"YlGn"),na.value="grey95")
                    # ,
                    # guide=colourbar(title="Wasting (%)",title.vjust = 1,
                    #                    label.position="bottom",label.hjust=0.5,nrow=1))
p


ggsave("U:/results/cormat.png",p)