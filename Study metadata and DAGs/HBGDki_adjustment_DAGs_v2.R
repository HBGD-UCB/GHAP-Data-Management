

library(dagitty)
library(lavaan)
library(ggdag)
library(stringr)
library(knitr)




all_exposures = c("sex", "month", "mage",   "fage",  "meducyrs",  "feducyrs", "single", 
  "hhwealth","hfoodsec",
  "mhtcm", "fhtcm","mwtkg",  "mbmi", "arm", "hdlvry", 
  "vagbrth", "birthwt",  "birthlen",  "gagebrth", 
  "brthordr",  "brthmon", "earlybf",
"nchldlt5", "nhh", "nrooms",
"impsan",  "safeh2o",   "trth2o",  "cleanck",  "impfloor", "enstunt","BF_practice", "diarrhea")


var_labels = c("Child gender", "Month of measurement", "Mother's age",   "Father's age",  "Mother's education",  "Father's education", "Widowed/divorced/seperated/single parent", 
  "Asset-based ousehold wealth quartile","Household food security category",
  "Mother's height", "Father's height","Mother's weight",  "Mother's MBI", "Intervention arm", "Child delivered at home/hospital", 
  "Vaginal birth", "Birth weight",  "Birth length",  "Gestational age at birth",
  "First/second/thirdborn+ child",  "Birth month", "Breastfed within an hour of birth",
"Number of children <5 in house", "Number of people in house", "Number of rooms in house",
"Improved sanitation",  "Safe water source",   "Treats drinking water",  "Clean cooking fuel",  
"Improved floor", "Enrolled stunted","Exclusive or predominant breastfeeding under 6 months", "Percent days with diarrhea")



knitr::kable(data.frame(variable_name=all_exposures, description=var_labels))



d <-paste0("dag {",


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
mhtcm -> mage    
fhtcm -> mage
mwtkg -> mage    
mbmi -> mage    
single -> mage
meducyrs -> mage 
feducyrs -> mage

brthordr -> mage  

impsan -> mage 
safeh2o -> mage  
trth2o -> mage   
cleanck -> mage  
impfloor -> mage 

nchldlt5 -> mage
nhh -> mage
nrooms -> mage

hfoodsec -> mage
hhwealth -> mage 

arm -> mage
month -> mage
  ",   
  #Variables affecting: fage
  "
mage -> fage           
mhtcm -> fage    
fhtcm -> fage
mwtkg -> fage    
mbmi -> fage    
single -> fage
meducyrs -> fage 
feducyrs -> fage

brthordr -> fage  

impsan -> fage 
safeh2o -> fage  
trth2o -> fage   
cleanck -> fage  
impfloor -> fage 

nchldlt5 -> fage
nhh -> fage
nrooms -> fage

hfoodsec -> fage
hhwealth -> fage 

arm -> fage
month -> fage

  ",           
  #Variables affecting: mhtcm
  "
mage -> mhtcm    
fage -> mhtcm           
fhtcm -> mhtcm
mwtkg -> mhtcm    

single -> mhtcm
meducyrs -> mhtcm 
feducyrs -> mhtcm

impsan -> mhtcm 
safeh2o -> mhtcm  
trth2o -> mhtcm   
cleanck -> mhtcm  
impfloor -> mhtcm 

nchldlt5 -> mhtcm
nhh -> mhtcm
nrooms -> mhtcm

hfoodsec -> mhtcm
hhwealth -> mhtcm

arm -> mhtcm
month -> mhtcm

  ",    
  #Variables affecting: fhtcm
  "
mage -> fhtcm    
fage -> fhtcm           
mhtcm -> fhtcm
mwtkg -> fhtcm    

single -> fhtcm
meducyrs -> fhtcm 
feducyrs -> fhtcm

impsan -> fhtcm 
safeh2o -> fhtcm  
trth2o -> fhtcm   
cleanck -> fhtcm  
impfloor -> fhtcm 

nchldlt5 -> fhtcm
nhh -> fhtcm
nrooms -> fhtcm

hfoodsec -> fhtcm
hhwealth -> fhtcm

arm -> fhtcm
month -> fhtcm

  ",
  #Variables affecting: mwtkg
  "
mage -> mwtkg    
fage -> mwtkg           
mhtcm -> mwtkg
fhtcm -> mwtkg    

single -> mwtkg
meducyrs -> mwtkg 
feducyrs -> mwtkg

impsan -> mwtkg 
safeh2o -> mwtkg  
trth2o -> mwtkg   
cleanck -> mwtkg  
impfloor -> mwtkg 

nchldlt5 -> mwtkg
nhh -> mwtkg
nrooms -> mwtkg

hfoodsec -> mwtkg
hhwealth -> mwtkg

arm -> mwtkg
month -> mwtkg

  ",    
  #Variables affecting: mbmi
  "
mage -> mbmi    
fage -> mbmi           
single -> mbmi
meducyrs -> mbmi 
feducyrs -> mbmi
fhtcm -> mbmi    

impsan -> mbmi 
safeh2o -> mbmi  
trth2o -> mbmi   
cleanck -> mbmi  
impfloor -> mbmi 

nchldlt5 -> mbmi
nhh -> mbmi
nrooms -> mbmi

hfoodsec -> mbmi
hhwealth -> mbmi

arm -> mbmi
month -> mbmi

  ",    
  #Variables affecting: single
  "
mage -> single    
fage -> single           
mhtcm -> single    
fhtcm -> single
mwtkg -> single    
mbmi -> single    
meducyrs -> single 
feducyrs -> single

impsan -> single 
safeh2o -> single  
trth2o -> single   
cleanck -> single  
impfloor -> single 

nchldlt5 -> single
nhh -> single
nrooms -> single

hfoodsec -> single
hhwealth -> single

arm -> single
month -> single


  ",
  #Variables affecting: meducyrs
  "
mage -> meducyrs    
fage -> meducyrs           
mhtcm -> meducyrs    
fhtcm -> meducyrs
mwtkg -> meducyrs    
mbmi -> meducyrs    
single -> meducyrs
feducyrs -> meducyrs

impsan -> meducyrs 
safeh2o -> meducyrs  
cleanck -> meducyrs  
impfloor -> meducyrs 

nchldlt5 -> meducyrs
nhh -> meducyrs
nrooms -> meducyrs

hfoodsec -> meducyrs
hhwealth -> meducyrs

arm -> meducyrs
month -> meducyrs

  ",
  #Variables affecting: feducyrs
  "
mage -> feducyrs    
fage -> feducyrs           
mhtcm -> feducyrs    
fhtcm -> feducyrs
mwtkg -> feducyrs    
mbmi -> feducyrs    
single -> feducyrs
meducyrs -> feducyrs

impsan -> feducyrs 
safeh2o -> feducyrs  
cleanck -> feducyrs  
impfloor -> feducyrs 

nchldlt5 -> feducyrs
nhh -> feducyrs
nrooms -> feducyrs

hfoodsec -> feducyrs
hhwealth -> feducyrs  

arm -> feducyrs
month -> feducyrs

",

#Birth_Antho: birth anthropometry/modifiable birth characteristics
  #Variables affecting: birthwt 
  "   
mage -> birthwt    
fage -> birthwt           
mhtcm -> birthwt    
fhtcm -> birthwt
mwtkg -> birthwt    
mbmi -> birthwt    
single -> birthwt
meducyrs -> birthwt 
feducyrs -> birthwt

birthlen -> birthwt 

vagbrth -> birthwt  
hdlvry -> birthwt

sex -> birthwt 
brthordr -> birthwt  
brthmon -> birthwt 

impsan -> birthwt 
safeh2o -> birthwt  
trth2o -> birthwt   
cleanck -> birthwt  
impfloor -> birthwt 

nchldlt5 -> birthwt
nhh -> birthwt
nrooms -> birthwt

hfoodsec -> birthwt
hhwealth -> birthwt

arm -> birthwt
month -> birthwt

  ", 
  #Variables affecting: birthlen 
  "
mage -> birthlen    
fage -> birthlen           
mhtcm -> birthlen    
fhtcm -> birthlen
mwtkg -> birthlen    
mbmi -> birthlen    
single -> birthlen
meducyrs -> birthlen 
feducyrs -> birthlen

birthwt -> birthlen 

vagbrth -> birthlen  
hdlvry -> birthlen

sex -> birthlen 
brthordr -> birthlen  
brthmon -> birthlen 

impsan -> birthlen 
safeh2o -> birthlen  
trth2o -> birthlen   
cleanck -> birthlen  
impfloor -> birthlen 

nchldlt5 -> birthlen
nhh -> birthlen
nrooms -> birthlen

hfoodsec -> birthlen
hhwealth -> birthlen

arm -> birthlen
month -> birthlen


  ",
  #Variables affecting: gagebrth 
  "
mage -> gagebrth    
fage -> gagebrth           
mhtcm -> gagebrth    
fhtcm -> gagebrth
mwtkg -> gagebrth    
mbmi -> gagebrth    
single -> gagebrth
meducyrs -> gagebrth 
feducyrs -> gagebrth

vagbrth -> gagebrth  
hdlvry -> gagebrth

sex -> gagebrth 
brthordr -> gagebrth  
brthmon -> gagebrth 

impsan -> gagebrth 
safeh2o -> gagebrth  
trth2o -> gagebrth   
cleanck -> gagebrth  
impfloor -> gagebrth 

nchldlt5 -> gagebrth
nhh -> gagebrth
nrooms -> gagebrth

hfoodsec -> gagebrth
hhwealth -> gagebrth

arm -> gagebrth
month -> gagebrth


  ",
  #Variables affecting: vagbrth  
  "
mage -> vagbrth    
fage -> vagbrth           
mhtcm -> vagbrth    
fhtcm -> vagbrth
mwtkg -> vagbrth    
mbmi -> vagbrth    
single -> vagbrth
meducyrs -> vagbrth 
feducyrs -> vagbrth

birthwt -> vagbrth  
birthlen -> vagbrth 
gagebrth -> vagbrth 
hdlvry -> vagbrth

sex -> vagbrth 
brthordr -> vagbrth  
brthmon -> vagbrth 

impsan -> vagbrth 
safeh2o -> vagbrth  
trth2o -> vagbrth   
cleanck -> vagbrth  
impfloor -> vagbrth 

nchldlt5 -> vagbrth
nhh -> vagbrth
nrooms -> vagbrth

hfoodsec -> vagbrth
hhwealth -> vagbrth

arm -> vagbrth
month -> vagbrth

  ",
  #Variables affecting: hdlvry
  "
mage -> hdlvry    
fage -> hdlvry           
mhtcm -> hdlvry    
fhtcm -> hdlvry
mwtkg -> hdlvry    
mbmi -> hdlvry    
single -> hdlvry
meducyrs -> hdlvry 
feducyrs -> hdlvry

birthwt -> hdlvry  
birthlen -> hdlvry 
gagebrth -> hdlvry 
vagbrth -> hdlvry

sex -> hdlvry 
brthordr -> hdlvry  
brthmon -> hdlvry 

impsan -> hdlvry 
safeh2o -> hdlvry  
trth2o -> hdlvry   
cleanck -> hdlvry  
impfloor -> hdlvry 

nchldlt5 -> hdlvry
nhh -> hdlvry
nrooms -> hdlvry

hfoodsec -> hdlvry
hhwealth -> hdlvry

arm -> hdlvry
month -> hdlvry

  ",

#Birth_Chars: other, unmodifiable birth characteristics
  #Variables affecting: sex 
  "
mage -> sex    
fage -> sex              
single -> sex
meducyrs -> sex 
feducyrs -> sex

brthordr -> sex  

nchldlt5 -> sex
nhh -> sex
nrooms -> sex

hfoodsec -> sex
hhwealth -> sex

arm -> sex
month -> sex

",
  #Variables affecting: brthordr 
  "
mage -> brthordr    
fage -> brthordr           
mhtcm -> brthordr    
fhtcm -> brthordr
mwtkg -> brthordr    
mbmi -> brthordr    
single -> brthordr
meducyrs -> brthordr 
feducyrs -> brthordr

sex -> brthordr 

impsan -> brthordr 
safeh2o -> brthordr  
trth2o -> brthordr   
cleanck -> brthordr  
impfloor -> brthordr 

nchldlt5 -> brthordr
nhh -> brthordr
nrooms -> brthordr

hfoodsec -> brthordr
hhwealth -> brthordr

arm -> brthordr
month -> brthordr

  ", 
  #Variables affecting: brthmon 
    #-none
#HH_WASH: HH WASH/ construction
  #Variables affecting: impsan 
  "
mage -> impsan    
fage -> impsan           
mhtcm -> impsan    
fhtcm -> impsan
mwtkg -> impsan    
mbmi -> impsan    
single -> impsan
meducyrs -> impsan 
feducyrs -> impsan

birthwt -> impsan  
birthlen -> impsan 
gagebrth -> impsan 
vagbrth -> impsan  
hdlvry -> impsan

sex -> impsan 
brthordr -> impsan  
brthmon -> impsan 

safeh2o -> impsan  
trth2o -> impsan   
cleanck -> impsan  
impfloor -> impsan 

nchldlt5 -> impsan
nhh -> impsan
nrooms -> impsan

hfoodsec -> impsan
hhwealth -> impsan
earlybf -> impsan

arm -> impsan
month -> impsan

  ",
  #Variables affecting: safeh2o 
  "
mage -> safeh2o    
fage -> safeh2o           
mhtcm -> safeh2o    
fhtcm -> safeh2o
mwtkg -> safeh2o    
mbmi -> safeh2o    
single -> safeh2o
meducyrs -> safeh2o 
feducyrs -> safeh2o

birthwt -> safeh2o  
birthlen -> safeh2o 
gagebrth -> safeh2o 
vagbrth -> safeh2o  
hdlvry -> safeh2o

sex -> safeh2o 
brthordr -> safeh2o  
brthmon -> safeh2o 

impsan -> safeh2o  
trth2o -> safeh2o   
cleanck -> safeh2o  
impfloor -> safeh2o 

nchldlt5 -> safeh2o
nhh -> safeh2o
nrooms -> safeh2o

hfoodsec -> safeh2o
hhwealth -> safeh2o
earlybf -> safeh2o

arm -> safeh2o
month -> safeh2o

  ", 
  #Variables affecting: trth2o 
  "
mage -> trth2o    
fage -> trth2o           
mhtcm -> trth2o    
fhtcm -> trth2o
mwtkg -> trth2o    
mbmi -> trth2o    
single -> trth2o
meducyrs -> trth2o 
feducyrs -> trth2o

birthwt -> trth2o  
birthlen -> trth2o 
gagebrth -> trth2o 
vagbrth -> trth2o  
hdlvry -> trth2o

sex -> trth2o 
brthordr -> trth2o  
brthmon -> trth2o 

safeh2o -> trth2o  
impsan -> trth2o   
cleanck -> trth2o  
impfloor -> trth2o 

nchldlt5 -> trth2o
nhh -> trth2o
nrooms -> trth2o

hfoodsec -> trth2o
hhwealth -> trth2o
earlybf -> trth2o

arm -> trth2o
month -> trth2o

  ",  
  #Variables affecting: cleanck
  "
mage -> cleanck    
fage -> cleanck           
mhtcm -> cleanck    
fhtcm -> cleanck
mwtkg -> cleanck    
mbmi -> cleanck    
single -> cleanck
meducyrs -> cleanck 
feducyrs -> cleanck

birthwt -> cleanck  
birthlen -> cleanck 
gagebrth -> cleanck 
vagbrth -> cleanck  
hdlvry -> cleanck

sex -> cleanck 
brthordr -> cleanck  
brthmon -> cleanck 

safeh2o -> cleanck  
trth2o -> cleanck   
impsan -> cleanck  
impfloor -> cleanck 

nchldlt5 -> cleanck
nhh -> cleanck
nrooms -> cleanck

hfoodsec -> cleanck
hhwealth -> cleanck
earlybf -> cleanck

arm -> cleanck
month -> cleanck

  ",  
  #Variables affecting: impfloor 
"
mage -> impfloor    
fage -> impfloor           
mhtcm -> impfloor    
fhtcm -> impfloor
mwtkg -> impfloor    
mbmi -> impfloor    
single -> impfloor
meducyrs -> impfloor 
feducyrs -> impfloor

birthwt -> impfloor  
birthlen -> impfloor 
gagebrth -> impfloor 
vagbrth -> impfloor  
hdlvry -> impfloor

sex -> impfloor 
brthordr -> impfloor  
brthmon -> impfloor 

safeh2o -> impfloor  
trth2o -> impfloor   
cleanck -> impfloor  
impsan -> impfloor 

nchldlt5 -> impfloor
nhh -> impfloor
nrooms -> impfloor

hfoodsec -> impfloor
hhwealth -> impfloor
earlybf -> impfloor

arm -> impfloor
month -> impfloor

  ",

#HH_size
  #Variables affecting: nchldlt5
  "
mage -> nchldlt5    
fage -> nchldlt5           
mhtcm -> nchldlt5    
fhtcm -> nchldlt5
mwtkg -> nchldlt5    
mbmi -> nchldlt5    
single -> nchldlt5
meducyrs -> nchldlt5 
feducyrs -> nchldlt5

birthwt -> nchldlt5  
birthlen -> nchldlt5 
gagebrth -> nchldlt5 
vagbrth -> nchldlt5  
hdlvry -> nchldlt5

sex -> nchldlt5 

impsan -> nchldlt5 
safeh2o -> nchldlt5  
trth2o -> nchldlt5   
cleanck -> nchldlt5  
impfloor -> nchldlt5 

nrooms -> nchldlt5

hfoodsec -> nchldlt5
hhwealth -> nchldlt5
earlybf -> nchldlt5

arm -> nchldlt5
month -> nchldlt5

  ",
  #Variables affecting: nhh
  "
mage -> nhh    
fage -> nhh           
mhtcm -> nhh    
fhtcm -> nhh
mwtkg -> nhh    
mbmi -> nhh    
single -> nhh
meducyrs -> nhh 
feducyrs -> nhh

birthwt -> nhh  
birthlen -> nhh 
gagebrth -> nhh 
vagbrth -> nhh  
hdlvry -> nhh

sex -> nhh 

impsan -> nhh 
safeh2o -> nhh  
trth2o -> nhh   
cleanck -> nhh  
impfloor -> nhh 

nrooms -> nhh

hfoodsec -> nhh
hhwealth -> nhh
earlybf -> nhh

arm -> nhh
month -> nhh

  ",
  #Variables affecting: nrooms
"
mage -> nrooms    
fage -> nrooms           
mhtcm -> nrooms    
fhtcm -> nrooms
mwtkg -> nrooms    
mbmi -> nrooms    
single -> nrooms
meducyrs -> nrooms 
feducyrs -> nrooms

birthwt -> nrooms  
birthlen -> nrooms 
gagebrth -> nrooms 
vagbrth -> nrooms  
hdlvry -> nrooms

sex -> nrooms 
brthordr -> nrooms  
brthmon -> nrooms 

impsan -> nrooms 
safeh2o -> nrooms  
trth2o -> nrooms   
cleanck -> nrooms  
impfloor -> nrooms 

nchldlt5 -> nrooms
nhh -> nrooms

hfoodsec -> nrooms
hhwealth -> nrooms
earlybf -> nrooms

arm -> nrooms
month -> nrooms

  ",

#Variables affecting: diarrhea
"
mage -> diarrhea    
fage -> diarrhea           
mhtcm -> diarrhea    
fhtcm -> diarrhea
mwtkg -> diarrhea    
mbmi -> diarrhea    
single -> diarrhea
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

hfoodsec -> diarrhea
hhwealth -> diarrhea
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
earlybf -> BF_practice

arm -> BF_practice  
month -> BF_practice

  ",
#Variables affecting: hfoodsec
"
mage -> hfoodsec    
fage -> hfoodsec           
mhtcm -> hfoodsec    
fhtcm -> hfoodsec
mwtkg -> hfoodsec    
mbmi -> hfoodsec    
single -> hfoodsec
meducyrs -> hfoodsec 
feducyrs -> hfoodsec

birthwt -> hfoodsec  
birthlen -> hfoodsec 
gagebrth -> hfoodsec 
vagbrth -> hfoodsec  
hdlvry -> hfoodsec

sex -> hfoodsec 
brthordr -> hfoodsec  
brthmon -> hfoodsec 

impsan -> hfoodsec 
safeh2o -> hfoodsec  
trth2o -> hfoodsec   
cleanck -> hfoodsec  
impfloor -> hfoodsec 

nchldlt5 -> hfoodsec
nhh -> hfoodsec
nrooms -> hfoodsec

earlybf -> hfoodsec

hhwealth -> hfoodsec
arm -> hfoodsec
month -> hfoodsec

  ",
#Variables affecting: hhwealth
  "
mage -> hhwealth    
fage -> hhwealth           
mhtcm -> hhwealth    
fhtcm -> hhwealth
mwtkg -> hhwealth    
mbmi -> hhwealth    
single -> hhwealth
meducyrs -> hhwealth 
feducyrs -> hhwealth

birthwt -> hhwealth  
birthlen -> hhwealth 
gagebrth -> hhwealth 
vagbrth -> hhwealth  
hdlvry -> hhwealth

sex -> hhwealth 
brthordr -> hhwealth  
brthmon -> hhwealth 

impsan -> hhwealth 
safeh2o -> hhwealth  
trth2o -> hhwealth   
cleanck -> hhwealth  
impfloor -> hhwealth 

nchldlt5 -> hhwealth
nhh -> hhwealth
nrooms -> hhwealth

hfoodsec -> hhwealth
earlybf -> hhwealth

arm -> hhwealth
month -> hhwealth

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
earlybf -> enstunt

arm -> enstunt
month -> enstunt

  ",
#Variables affecting: earlybf
  "
mage -> earlybf    
fage -> earlybf           
mhtcm -> earlybf    
fhtcm -> earlybf
mwtkg -> earlybf    
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

impsan -> earlybf 
safeh2o -> earlybf  
trth2o -> earlybf   
cleanck -> earlybf  
impfloor -> earlybf 

nchldlt5 -> earlybf
nhh -> earlybf
nrooms -> earlybf

hfoodsec -> earlybf
hhwealth -> earlybf
enstunt -> earlybf

arm -> earlybf
month -> earlybf

  ",
#Variables affecting: arm
    #-none
#Variables affecting: month 
    #-none

"}")



RF_DAG <- function(d, Avar, all_exposures = c("sex", "month", "mage",   "fage",  "meducyrs",  "feducyrs", "single", 
                                              "hhwealth","hfoodsec",
                                              "mhtcm", "fhtcm","mwtkg",  "mbmi", "arm", "hdlvry", 
                                              "vagbrth", "birthwt",  "birthlen",  "gagebrth", 
                                              "brthordr",  "brthmon", "earlybf",
                                            "nchldlt5", "nhh", "nrooms",
                                            "impsan",  "safeh2o",   "trth2o",  "cleanck",  "impfloor", 
                                            "enstunt","BF_practice", "diarrhea")){
  
      parents <- str_extract_all(d, paste0("\\n(.*?) -> ",Avar))
        d2 <-d
      
      if(length(parents[[1]])>0){
        parents <- str_split(parents[[1]], " ->", simplify = T)[,1]
        parents <- str_split(parents, "\n", simplify = T)[,2]
        
        
        for(i in 1:length(parents)){
          d2 <- gsub(paste0(Avar," -> ",parents[i]),"", d2)
        }
        
        nchar(d)
        nchar(d2)
        
        
        for(i in 1:length(all_exposures)){
          for(j in 1:length(all_exposures)){
            
            test1 <- grepl(paste0(all_exposures[i]," -> ",all_exposures[j]) , d2)
            test2 <- grepl(paste0(all_exposures[j]," -> ",all_exposures[i]) , d2)
            
            if(test1 & test2){
                d2 <- gsub(paste0(all_exposures[j]," -> ",all_exposures[i]),"", d2)
            }
            #if(test1 & test2){cat(all_exposures[i],", ",all_exposures[j],"\n") }
          }  
        }
        nchar(d2)
      }
      
      g2 <- dagitty(d2)
      
      
      
      outcomes(g2) <- "Y_Stunted"
      
      exposures(g2) <- Avar
      
      #g3 <- dagitty(canonicalize(g2)$g)
      
      adj_set<-adjustmentSets( g2, Avar, "Y_Stunted", effect="total" )
            if(length(adj_set)==0){
                adj_set <- parents
            }else{
                adj_set <- gsub("\\{","",adj_set)
                adj_set <- gsub("}","",adj_set)
                adj_set <- gsub(" ","",adj_set)
                adj_set <- gsub("\"","",adj_set)
                adj_set <- gsub("c\\(","",adj_set)
                adj_set <- gsub(")","",adj_set)
                adj_set <- str_split(adj_set, ",")[[1]]

            }
      print( adj_set )
      
      print(ggdag_adjustment_set(g2))
      
      
      return(adj_set)
}



adj_set_list <- list()
for(i in 1:length(all_exposures)){
  adj_set_list[[i]] <- RF_DAG(d, Avar = all_exposures[i])
}

names(adj_set_list) <- all_exposures

save(adj_set_list, file="C:/Users/andre/Dropbox/HBGDki documentation/HBGDki_adj_set_list.Rdata")




