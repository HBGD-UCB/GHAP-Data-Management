
library(dagitty)
library(lavaan)




g1 <- dagitty( paste0("dag {",


#---------------------------------
#Variables affecting the outcome
#---------------------------------

"
mage -> Y_Stunted    
fage -> Y_Stunted           
mhtcm -> Y_Stunted    
fhtcm -> Y_Stunted
mwtkg -> Y_Stunted    
mbmi -> Y_Stunted    
single -> Y_Stunted
meducyrs -> Y_Stunted 
feducyrs -> Y_Stunted

birthwt -> Y_Stunted  
birthlen -> Y_Stunted 
gagebrth -> Y_Stunted 
vagbrth -> Y_Stunted  
hdlvry -> Y_Stunted

sex -> Y_Stunted 
brthordr -> Y_Stunted  
brthmon -> Y_Stunted 

impsan -> Y_Stunted 
safeh2o -> Y_Stunted  
trth2o -> Y_Stunted   
cleanck -> Y_Stunted  
impfloor -> Y_Stunted 

nchldlt5 -> Y_Stunted
nhh -> Y_Stunted
nrooms -> Y_Stunted

diarrhea -> Y_Stunted

BF_practice -> Y_Stunted

hfoodsec -> Y_Stunted
hhwealth -> Y_Stunted
enstunt -> Y_Stunted
earlybf -> Y_Stunted

arm -> Y_Stunted
month -> Y_Stunted     
",

#---------------------------------
#Variables affecting other variables
#---------------------------------


#Parental_Vars: parental characteristics
  #Variables affecting: mage 
  "
      fage -> mage              
      meducyrs -> mage 
      feducyrs -> mage
  ",   
  #Variables affecting: fage
  "
      meducyrs -> fage  
      feducyrs -> fage 
  ",           
  #Variables affecting: mhtcm
  "
      meducyrs -> mhtcm  

      hfoodsec -> mhtcm 
      hhwealth -> mhtcm 
  ",    
  #Variables affecting: fhtcm
  "
      feducyrs -> fhtcm 
      hfoodsec -> fhtcm 
      hhwealth -> fhtcm 
  ",
  #Variables affecting: mwtkg
  "
      mage -> mwtkg     

      single -> mwtkg 
      meducyrs -> mwtkg  

      impsan -> mwtkg  
      safeh2o -> mwtkg   
      trth2o -> mwtkg    
      cleanck -> mwtkg   
      impfloor -> mwtkg  

      hfoodsec -> mwtkg 
      hhwealth -> mwtkg 
    
  ",    
  #Variables affecting: mbmi
  "
      mage -> mbmi     
      mhtcm -> mbmi     
      mwtkg -> mbmi     
      single -> mbmi 
      meducyrs -> mbmi  

      impsan -> mbmi  
      safeh2o -> mbmi   
      trth2o -> mbmi    
      cleanck -> mbmi   
      impfloor -> mbmi  

      hfoodsec -> mbmi 
      hhwealth -> mbmi 
  ",    
  #Variables affecting: single
  "
      mage -> single     
      meducyrs -> single  
  ",
  # #Variables affecting: meducyrs 
  # "
  #     hhwealth -> meducyrs 
  #   
  # ",
  # #Variables affecting: feducyrs
  # "
  #     hhwealth -> feducyrs 
  # ",

#Birth_Antho: birth anthropometry/modifiable birth characteristics
  #Variables affecting: birthwt 
  "   
      mhtcm -> birthwt     
      fhtcm -> birthwt 
      mwtkg -> birthwt    
      mbmi -> birthwt     
      single -> birthwt 
      meducyrs -> birthwt  
      feducyrs -> birthwt 

      gagebrth -> birthwt  

      sex -> birthwt  
      brthmon -> birthwt  

      hfoodsec -> birthwt 
      hhwealth -> birthwt 

      arm -> birthwt 
  ", 
  #Variables affecting: birthlen 
  "
      mhtcm -> birthlen     
      fhtcm -> birthlen 
      mwtkg -> birthlen     
      mbmi -> birthlen     
      single -> birthlen 
      meducyrs -> birthlen  
      feducyrs -> birthlen 

      gagebrth -> birthlen  

      sex -> birthlen  
      brthmon -> birthlen  

      hfoodsec -> birthlen 
      hhwealth -> birthlen 

      arm -> birthlen 
  ",
  #Variables affecting: gagebrth 
  "
      mhtcm -> gagebrth     
      fhtcm -> gagebrth 
      mwtkg -> gagebrth     
      mbmi -> gagebrth     
      single -> gagebrth 
      meducyrs -> gagebrth  
      feducyrs -> gagebrth 

      brthmon -> gagebrth  

      hfoodsec -> gagebrth 
      hhwealth -> gagebrth 

      arm -> gagebrth 
  ",
  #Variables affecting: vagbrth  
  "
      mage -> vagbrth     
      mhtcm -> vagbrth     
      mwtkg -> vagbrth     
      mbmi -> vagbrth     
      meducyrs -> vagbrth  
 
      hdlvry -> vagbrth 

      brthordr -> vagbrth   
      brthmon -> vagbrth  

      hhwealth -> vagbrth 
  ",
  #Variables affecting: hdlvry
  "
      mage -> hdlvry   
      mhtcm -> hdlvry       
      mwtkg -> hdlvry       
      mbmi -> hdlvry       
      meducyrs -> hdlvry    
 
      brthordr -> hdlvry     
      brthmon -> hdlvry    

      hhwealth -> hdlvry   
  ",

#Birth_Chars: other, unmodifiable birth characteristics
  #Variables affecting: sex 
      #-none

  #Variables affecting: brthordr 
  "
      mage -> brthordr     
      meducyrs -> brthordr
      hhwealth -> brthordr

  ", 
  #Variables affecting: brthmon 
    #-none
#HH_WASH: HH WASH/ construction
  #Variables affecting: impsan 
  "
      meducyrs -> impsan 
      feducyrs -> impsan
      hhwealth -> impsan
  ",
  #Variables affecting: safeh2o 
  "
      meducyrs -> safeh2o 
      feducyrs -> safeh2o
      hhwealth -> safeh2o
  ", 
  #Variables affecting: trth2o 
  "
      meducyrs -> trth2o 
      feducyrs -> trth2o
      hhwealth -> trth2o
  ",  
  #Variables affecting: cleanck
  "
      meducyrs -> cleanck 
      feducyrs -> cleanck
      hhwealth -> cleanck
  ",  
  #Variables affecting: impfloor 
"
      meducyrs -> impfloor 
      feducyrs -> impfloor
      hhwealth -> impfloor
  ",

#HH_size
  #Variables affecting: nchldlt5
  "
      mage -> nchldlt5    
      fage -> nchldlt5          
      single -> nchldlt5
      meducyrs -> nchldlt5 
      feducyrs -> nchldlt5
      hhwealth -> nchldlt5
  ",
  #Variables affecting: nhh
  "
      mage -> nhh     
      fage -> nhh            
      single -> nhh 
      meducyrs -> nhh  
      feducyrs -> nhh 
      hhwealth -> nhh 
  ",
  #Variables affecting: nrooms
"
      mage -> nrooms     
      fage -> nrooms            
      single -> nrooms 
      meducyrs -> nrooms  
      feducyrs -> nrooms 
      hhwealth -> nrooms 
      nhh -> nrooms 
      nrooms -> nrooms 
  ",

#Variables affecting: diarrhea
"
      meducyrs -> diarrhea  
      feducyrs -> diarrhea 

      birthwt -> diarrhea   
      birthlen -> diarrhea  
      gagebrth -> diarrhea  
      vagbrth -> diarrhea   
      hdlvry -> diarrhea 

      sex -> diarrhea  
      brthordr -> diarrhea   
      brthmon -> diarrhea  

      impsan -> diarrhea  
      safeh2o -> diarrhea   
      trth2o -> diarrhea    
      cleanck -> diarrhea   
      impfloor -> diarrhea  

      nchldlt5 -> diarrhea 
      nhh -> diarrhea 
      nrooms -> diarrhea 

      BF_practice -> diarrhea 

      hfoodsec -> diarrhea 
      hhwealth -> diarrhea 
      enstunt -> diarrhea 
      earlybf -> diarrhea 

      arm -> diarrhea 
      month -> diarrhea  
  ",

#Variables affecting: BF_practice
"
      mage -> BF_practice     
      fage -> BF_practice            
      mhtcm -> BF_practice     
      fhtcm -> BF_practice 
      mwtkg -> BF_practice     
      mbmi -> BF_practice     
      single -> BF_practice 
      meducyrs -> BF_practice  
      feducyrs -> BF_practice 

      birthwt -> BF_practice   
      birthlen -> BF_practice  
      gagebrth -> BF_practice  
      vagbrth -> BF_practice   
      hdlvry -> BF_practice 

      sex -> BF_practice  
      brthordr -> BF_practice   
      brthmon -> BF_practice  

      impsan -> BF_practice  
      safeh2o -> BF_practice   
      trth2o -> BF_practice    
      cleanck -> BF_practice   
      impfloor -> BF_practice  

      nchldlt5 -> BF_practice 
      nhh -> BF_practice 
      nrooms -> BF_practice 

      hfoodsec -> BF_practice 
      hhwealth -> BF_practice 
      enstunt -> BF_practice 
      earlybf -> BF_practice 

      arm -> BF_practice 
      month -> BF_practice  
  ",
#Variables affecting: hfoodsec
"
      single -> hfoodsec 
      meducyrs -> hfoodsec  
      feducyrs -> hfoodsec 

      brthordr -> hfoodsec   
      brthmon -> hfoodsec  

      nchldlt5 -> hfoodsec 
      nhh -> hfoodsec 
      nrooms -> hfoodsec 

      hhwealth -> hfoodsec 
 
      month -> hfoodsec  
  ",
#Variables affecting: hhwealth
  "
      mage -> hhwealth     
      fage -> hhwealth            
      single -> hhwealth 
      meducyrs -> hhwealth  
      feducyrs -> hhwealth 
  ",
#Variables affecting: enstunt
  "
      mage -> enstunt     
      fage -> enstunt            
      mhtcm -> enstunt     
      fhtcm -> enstunt 
      mwtkg -> enstunt     
      mbmi -> enstunt     
      single -> enstunt 
      meducyrs -> enstunt  
      feducyrs -> enstunt 

      birthwt -> enstunt   
      birthlen -> enstunt  
      gagebrth -> enstunt  
      vagbrth -> enstunt   
      hdlvry -> enstunt 

      sex -> enstunt  
      brthordr -> enstunt   
      brthmon -> enstunt  

      impsan -> enstunt  
      safeh2o -> enstunt   
      trth2o -> enstunt    
      cleanck -> enstunt   
      impfloor -> enstunt  

      nchldlt5 -> enstunt 
      nhh -> enstunt 
      nrooms -> enstunt 

      hfoodsec -> enstunt 
      hhwealth -> enstunt 
      enstunt -> enstunt 
      earlybf -> enstunt 

      arm -> enstunt 
      month -> enstunt 
  ",
#Variables affecting: earlybf
  "
      mage -> earlybf     
      mbmi -> earlybf     
      single -> earlybf 
      meducyrs -> earlybf  
      feducyrs -> earlybf 

      birthwt -> earlybf   
      birthlen -> earlybf  
      gagebrth -> earlybf  
      vagbrth -> earlybf   
      hdlvry -> earlybf 

      sex -> earlybf  
      brthordr -> earlybf   
      brthmon -> earlybf  

      hhwealth -> earlybf 
      arm -> earlybf 
  ",
#Variables affecting: arm
    #-none
#Variables affecting: month 
    #-none

"}"))



plot(graphLayout(g1))





coordinates( g1 ) <-
 list( 
	x=c(
		mage = -7,   
		fage = -7 ,           
		mhtcm = -7 ,    
		fhtcm = -7 , 
		mwtkg = -7 ,    
		mbmi =  -7,    
		single =  -7, 
		meducyrs =  -7, 
		feducyrs =  -7, 
		birthwt = -3,  
		birthlen = -3, 
		gagebrth = -3, 
		vagbrth = -3,  
		hdlvry = -3, 
		sex = -3, 
		brthordr = -3,  
		brthmon = -3, 
		impsan = 0, 
		safeh2o = 0,  
		trth2o = 0,   
		cleanck = 0,  
		impfloor = 0, 
		nchldlt5 = 3, 
		nhh = 3, 
		nrooms = 6, 
		diarrhea = 6, 
		BF_practice = 6, 
		hfoodsec = 8, 
		hhwealth = -10, 
		enstunt = -1, 
		earlybf = -1, 
		arm = 3, 
		month = 8,    
		Y_Stunted = 10
	),
	y=c(
		mage = -10,   
		fage = -8,           
		mhtcm = -6,    
		fhtcm = -4, 
		mwtkg = -2,    
		mbmi = 0,    
		single = 2, 
		meducyrs = 4, 
		feducyrs = 6, 
		birthwt = -10,  
		birthlen = -8, 
		gagebrth = -6, 
		vagbrth = -4,  
		hdlvry = -2, 
		sex = 0, 
		brthordr = 2,  
		brthmon = 4, 
		impsan = -8, 
		safeh2o = -6,  
		trth2o = -4,   
		cleanck = -2,  
		impfloor = 0, 
		nchldlt5 = 2, 
		nhh = 4, 
		nrooms = 6, 
		diarrhea = 0, 
		BF_practice = 4, 
		hfoodsec = -4, 
		hhwealth = 0, 
		enstunt = -6, 
		earlybf = 8, 
		arm = -4, 
		month = 4,    
		Y_Stunted = 0
))
plot(g1)


# 
# #List adjustment sets for specific path coefficients
 print( adjustmentSets( g1, "nchldlt5", "Y_Stunted", effect="total" ) )




#List total effects that are identifiable by regression
for( n in names(g1) ){
        a <- adjustmentSets( g1, n, "Y_Stunted" )
        if( length(a) == 0 ){a <- " NOT IDENTIFIABLE"}
            cat("The total effect of ",n," on ","Y_Stunted",
                " is identifiable controlling for:\n",sep="")
            print( a, prefix=" * " )
}


# plot(graphLayout(ancestorGraph(g1, v = "single")))



















