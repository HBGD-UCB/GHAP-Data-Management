


#----------------------------------------------------------
# ki0047075b-MAL-ED             60  38270
#----------------------------------------------------------

#MOA for monthly assessment form

# Are you breastfeeding <Child>? If NO, then skip to Q.6	MOABRST
# Last night, how many times did you breastfeed (Child) from sunset to sunrise?	MOABRSTN
# Yesterday, during the day, how many times did you breastfeed (child)?	MOABRSTD
# Do you give (Child) infant formula? If NO, then skip to Q.9	MOAFORM
# Last night, how many times did you feed (Child) formula from sunset to sunrise?	MOAFORMN
# Yesterday, during the day, how many times did you feed (child) formula?	MOAFORMD
# Do you give (Child) other milks, such as tinned, powdered or fresh animal milk? If NO, then skip to Q.12	MOAMILK
# Last night, how many times did you feed (child) animal milk from sunset to sunrise?	MOAMILKN
# Yesterday, during the day, how many times did you feed (child) animal milk?	MOAMILKD
# Plain water	MOAWAT
# Tea, coffee (Local examples)?	MOATEACOF
# Fruit or vegetable juices?	MOAJUICE
# Any other liquids, such as sugar water, thin soup or broth, carbonated drinks (local examples)	MOAOTH
# Is (child) eating any semi-solid, mashed or solid foods? If NO, go to Q.32	MOAFOOD
# Rice, porridge, bread, noodles or other foods made from grains?	MOAGRAIN
# White potatoes, white yams, manioc, or other foods made from roots?	MOAROOT
# Carrots, squash, or sweet potatoes that are yellow or orange inside?	MOAYVEG
# Any dark green leafy vegetables such as spinach?	MOASPI
# Foods made with beans, lentils, peas, corn, ground nuts?	MOABEAN
# Ripe mangoes, papayas, or other sweet yellow/orange or red fruit?	MOAYFRU
# Any other fruits or vegetables such as banana, apple, oranges, tomatoes, avocado?	MOAFRVEG
# Liver, kidney, heart or other organ meats?	MOAORG
# Any meat, such as chicken, beef, lamb, goat, duck (others)?	MOAMEAT
# Eggs?	MOAEGG
# Fresh or dried fish or shellfish?	MOAFISH
# Cheese, yogurt or other dairy products?	MOADAI
# Any sugary foods such as pastries, cakes or biscuits?	MOASUG
# Any commercially available foods for infants or young children?	MOACOMM
# 
# Target/scheduled month	target_month
# Assessment conducted in target window, extended window, out of window	window
# Age (days)	agedays
# Month of age by study schedule	month_ss
# Age (months)	agemonths



#----------------------------------------------------------
# ki1000109-EE                5998    365
#----------------------------------------------------------

#C1B - birth breastfeeding

#c1bfrmno	1	SUBJ	SUBJID
#c1bq1dd	16	ANTHRO	AGEDAYS	Add one so that there is no AGEDAYS=0	Age of the newborn(Days)

# c1bq31	What was the introduced to child as first diet?	1" Colostrums" 2" Mothers milk" 3" Herbal ghutti" 4" Honey" 5" Plain water" 6" Sugar water" 7" Grip water" 8" Green tea" 9" Animal milk" 1" Formula milk" 11" Butter" 12"Other Specify" 666"Missing"
# c1bq31_o	What was the introduced to child as first diet?(Other specify)	
# c1bq33	when was the child put on mothers breast for feeding at first time?	1" Immediately after birth (first hour)" 2" 1-6 hours aftr birth" 3" 7-12 hours after birth" 4" 13-24 hours after birth" 5" More than 24 hours" 888"Not applicable"
# c1bq34	Is the child currently taking breast feed?	1" Yes" 2" No"
# c1bq35a	Since this time yesterday,did the child recive any of the following (Vitamin supplement)	1" Yes" 2" No"
# c1bq35b	Since this time yesterday,did the child recive any of the following (Plain water)	1" Yes" 2" No"
# c1bq35c	Since this time yesterday,did the child recive any of the following (sweetened water/juise)	1" Yes" 2" No"
# c1bq35d	Since this time yesterday,did the child recive any of the following (ORS)	1" Yes" 2" No"
# c1bq35e	Since this time yesterday,did the child recive any of the following (Infant formula)	1" Yes" 2" No"
# c1bq35f	Since this time yesterday,did the child recive any of the following (animal milk)	1" Yes" 2" No"
# c1bq35g	Since this time yesterday,did the child recive any of the following (Any other liquids)	1" Yes" 2" No"
# c1bq36mm	If yes to using non human milk how old was the child ,when this milk was the introduced?(Months)	
# c1bq36dd	If yes to using non human milk how old was the child ,when this milk was the introduced?(Days)	
# c1bq37a	At birth(BCG)	1" Yes" 2" No"
# c1bq37b	At birth(OPV )	1" Yes" 2" No"


#C2B - monthly followup

#c2bfrmno	1		SUBJID	
#c2bageyy	8		AGEDAYS	AGEDAYS = round(365.25 * c3bageyy + 365.25*c2bagemm/12 + c2bagedd) + 1

#c2bmre Breastfeeding	1" Yes" 2" No" 666"No Answer / Missing" 777"Not Applicable" 888"Unreachable" 999"Don't Know"
#c2bmrf Infant formula milk	1" Yes" 2" No" 666"No Answer / Missing" 777"Not Applicable" 888"Unreachable" 999"Don't Know"
#c2bmrg Weaning	1" Yes" 2" No" 666"No Answer / Missing" 777"Not Applicable" 888"Unreachable" 999"Don't Know"



#----------------------------------------------------------
# ki1000304-EU                1523   1976
#----------------------------------------------------------

#Only measured at one time point - at enrollment

#U:\git\hbgd\ki1000304\EU\raw/enrol.sas7bdat

# CHILDID	Child Id	Unique identification number assigned to each child enrolled	Numeric
# AGE_M	Age (months)	06 to 30 	Numeric
# PRE-ENROLLMENT FEEDING(week preceding day of enrollment)			
# BFEED_ST	Breast feeding status	2=No, 3=exclusive, 4=partial, 5=occasional, 8=missing ,9= not applicable   	Numeric
# NBFEED_G	Non breast milk given	1= yes, 2= no,8=missing ,9= not applicable   	Numeric
# BOTTLE	Bottle	1= yes, 2= no,8=missing ,9= not applicable   	Numeric


#----------------------------------------------------------
# ki1000304-ZnMort             594   1666
#----------------------------------------------------------

#Only has indicator for "Currently breast fed" at study enrollment
#-may be able to just use cleaned data

#----------------------------------------------------------
# ki1000304b-SAS-CompFeed     8868    475
#----------------------------------------------------------

#Breastfeeding information at 3 and 9 months  (not at 6 or 12months) with some later recall variables about when BF stopped
#May not be enough to use this study

#----------------------------------------------------------
# ki1000304b-SAS-FoodSuppl    1807    414
#----------------------------------------------------------

#----------------------------------------------------------
# ki1017093-NIH-Birth          593   5568
#----------------------------------------------------------

#----------------------------------------------------------
# ki1017093b-PROVIDE             0   9039
#----------------------------------------------------------

#----------------------------------------------------------
# ki1017093c-NIH-Crypto          3   6837
#----------------------------------------------------------

#----------------------------------------------------------
# ki1033518-iLiNS-DYAD-G      5114    139
#----------------------------------------------------------

#----------------------------------------------------------
# ki1101329-Keneba           14914  24829
#----------------------------------------------------------

#----------------------------------------------------------
# ki1112895-Burkina Faso Zn   5227   9778
#----------------------------------------------------------

#----------------------------------------------------------
# ki1112895-Guatemala BSC     2474     71
#----------------------------------------------------------

#----------------------------------------------------------
# ki1113344-GMS-Nepal        12592     85
#----------------------------------------------------------

#----------------------------------------------------------
# ki1119695-PROBIT          108307  15190
#----------------------------------------------------------

#----------------------------------------------------------
# ki1126311-ZVITAMBO         12886  49843
#----------------------------------------------------------

#----------------------------------------------------------
# ki1148112-iLiNS-DOSE        1790   3250
#----------------------------------------------------------

#----------------------------------------------------------
# ki1148112-iLiNS-DYAD-M      3350    691
#----------------------------------------------------------

#----------------------------------------------------------
# ki1148112-LCNI-5              92   4098
#----------------------------------------------------------

#----------------------------------------------------------
# kiGH5241-JiVitA-3         102216     91
#----------------------------------------------------------


# Infant 1 Month Postpartum
i1mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_i1mop.sas7bdat") %>% select(hbgdkid,
                                                                       i1sbfed,
                                                                       i1bftime,
                                                                       i1amilk,
                                                                       i1pmilk,
                                                                       i1formu,
                                                                       i1suji,
                                                                       i1dal,
                                                                       i1gruel,
                                                                       i1shago,
                                                                       i1rice,
                                                                       i1khich,
                                                                       i1barl,
                                                                       i1banan,
                                                                       i1biscu,
                                                                       i1honey,
                                                                       i1othf,
                                                                       i1water,
                                                                       i1amilkw,
                                                                       i1pmilkw,
                                                                       i1formuw,
                                                                       i1sujiw,
                                                                       i1dalw,
                                                                       i1gruelw,
                                                                       i1shagow,
                                                                       i1ricew,
                                                                       i1khichw,
                                                                       i1barlw,
                                                                       i1bananw,
                                                                       i1biscuw,
                                                                       i1honeyw,
                                                                       i1othfw,
                                                                       i1waterw)  %>% mutate_all(funs(as.numeric)) %>%
  mutate(
    i1amilk= as.numeric(!(i1amilk==0 | i1amilkw==0)),
    i1pmilk= as.numeric(!(i1pmilk==0 | i1pmilkw==0)),
    i1formu= as.numeric(!(i1formu==0 | i1formuw==0)),
    i1suji= as.numeric(!(i1suji==0 | i1sujiw==0)),
    i1dal= as.numeric(!(i1dal==0 | i1dalw==0)),
    i1gruel= as.numeric(!(i1gruel==0 | i1gruelw==0)),
    i1shago= as.numeric(!(i1shago==0 | i1shagow==0)),
    i1rice= as.numeric(!(i1rice==0 | i1ricew==0)),
    i1khich= as.numeric(!(i1khich==0 | i1khichw==0)),
    i1barl= as.numeric(!(i1barl==0 | i1barlw==0)),
    i1banan= as.numeric(!(i1banan==0 | i1bananw==0)),
    i1biscu= as.numeric(!(i1biscu==0 | i1biscuw==0)),
    i1honey= as.numeric(!(i1honey==0 | i1honeyw==0)),
    i1othf= as.numeric(!(i1othf==0 | i1othfw==0)),
    i1water= as.numeric(!(i1water==0 | i1waterw==0)),
    othfedfl= as.numeric(!(i1suji+i1dal+i1gruel+i1shago+i1rice+i1khich+i1barl+i1banan+i1biscu+ i1honey+i1othf==0))) %>%
  rename(subjido=hbgdkid,
         bfedfl=i1sbfed,
         nbfyes=i1bftime,
         anmlkfl=i1amilk,
         pwmlkfl=i1pmilk,
         formlkfl=i1formu,
         h20fedfl= i1water
  ) %>% select(subjido, bfedfl, nbfyes, anmlkfl, pwmlkfl, formlkfl, h20fedfl, othfedfl) %>%
  mutate(bfedfl = as.numeric(bfedfl), 
         anmlkfl= as.numeric(anmlkfl), 
         pwmlkfl= as.numeric(pwmlkfl), 
         formlkfl= as.numeric(formlkfl), 
         h20fedfl= as.numeric(h20fedfl), 
         othfedfl= as.numeric(othfedfl),
         antpt = "Infant 1 Month Postpartum")


table(i1mop$anmlkfl)
table(is.na(i1mop$anmlkfl))

table(i1mop$othfedfl)
table(is.na(i1mop$othfedfl))

table(i1mop$h20fedfl)
table(is.na(i1mop$h20fedfl))

# Infant 3 Month Postpartum
i3mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_i3mop.sas7bdat") %>% select(hbgdkid,
                                                                       i3sbfed,
                                                                       i3bftime,
                                                                       i3amilk,
                                                                       i3pmilk,
                                                                       i3formu,
                                                                       i3suji,
                                                                       i3dal,
                                                                       i3gruel,
                                                                       i3shago,
                                                                       i3rice,
                                                                       i3khich,
                                                                       i3barl,
                                                                       i3banan,
                                                                       i3biscu,
                                                                       i3honey,
                                                                       i3othf,
                                                                       i3water,
                                                                       i3amilkw,
                                                                       i3pmilkw,
                                                                       i3formuw,
                                                                       i3sujiw,
                                                                       i3dalw,
                                                                       i3gruelw,
                                                                       i3shagow,
                                                                       i3ricew,
                                                                       i3khichw,
                                                                       i3barlw,
                                                                       i3bananw,
                                                                       i3biscuw,
                                                                       i3honeyw,
                                                                       i3othfw,
                                                                       i3waterw)  %>% mutate_all(funs(as.numeric)) %>%
  mutate(
    i3amilk= as.numeric(!(i3amilk==0 | i3amilkw==0)),
    i3pmilk= as.numeric(!(i3pmilk==0 | i3pmilkw==0)),
    i3formu= as.numeric(!(i3formu==0 | i3formuw==0)),
    i3suji= as.numeric(!(i3suji==0 | i3sujiw==0)),
    i3dal= as.numeric(!(i3dal==0 | i3dalw==0)),
    i3gruel= as.numeric(!(i3gruel==0 | i3gruelw==0)),
    i3shago= as.numeric(!(i3shago==0 | i3shagow==0)),
    i3rice= as.numeric(!(i3rice==0 | i3ricew==0)),
    i3khich= as.numeric(!(i3khich==0 | i3khichw==0)),
    i3barl= as.numeric(!(i3barl==0 | i3barlw==0)),
    i3banan= as.numeric(!(i3banan==0 | i3bananw==0)),
    i3biscu= as.numeric(!(i3biscu==0 | i3biscuw==0)),
    i3honey= as.numeric(!(i3honey==0 | i3honeyw==0)),
    i3othf= as.numeric(!(i3othf==0 | i3othfw==0)),
    i3water= as.numeric(!(i3water==0 | i3waterw==0)),
    othfedfl= as.numeric(!(i3suji+i3dal+i3gruel+i3shago+i3rice+i3khich+i3barl+i3banan+i3biscu+ i3honey+i3othf==0))) %>%
  rename(subjido=hbgdkid,
         bfedfl=i3sbfed,
         nbfyes=i3bftime,
         anmlkfl=i3amilk,
         pwmlkfl=i3pmilk,
         formlkfl=i3formu,
         h20fedfl= i3water
  ) %>% select(subjido, bfedfl, nbfyes, anmlkfl, pwmlkfl, formlkfl, h20fedfl, othfedfl) %>%
  mutate(bfedfl = as.numeric(bfedfl), 
         anmlkfl= as.numeric(anmlkfl), 
         pwmlkfl= as.numeric(pwmlkfl), 
         formlkfl= as.numeric(formlkfl), 
         h20fedfl= as.numeric(h20fedfl), 
         othfedfl= as.numeric(othfedfl),
         antpt = "Infant 3 Month Postpartum")


table(i3mop$anmlkfl)
table(is.na(i3mop$anmlkfl))

table(i3mop$othfedfl)
table(is.na(i3mop$othfedfl))

table(i3mop$h20fedfl)
table(is.na(i3mop$h20fedfl))
# Infant 6 Month Postpartum 
i6mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_i6mop.sas7bdat") %>% select(hbgdkid,
                                                                       i6sbfed,
                                                                       i6sbfedm,
                                                                       i6bftime,
                                                                       i6amilk,
                                                                       i6pmilk,
                                                                       i6formu,
                                                                       i6suji,
                                                                       i6dal,
                                                                       i6gruel,
                                                                       i6shago,
                                                                       i6rice,
                                                                       i6khich,
                                                                       i6barl,
                                                                       i6banan,
                                                                       i6biscu,
                                                                       i6oil,
                                                                       i6sug,
                                                                       i6dair,
                                                                       i6othf,
                                                                       i6water,
                                                                       i6amilkw,
                                                                       i6pmilkw,
                                                                       i6formuw,
                                                                       i6sujiw,
                                                                       i6dalw,
                                                                       i6gruelw,
                                                                       i6shagow,
                                                                       i6ricew,
                                                                       i6khichw,
                                                                       i6barlw,
                                                                       i6bananw,
                                                                       i6biscuw,
                                                                       i6oilw,
                                                                       i6sugw,
                                                                       i6dairw,
                                                                       i6othfw,
                                                                       i6waterw)  %>% mutate_all(funs(as.numeric)) %>%
  mutate(
    i6amilk= as.numeric(!(i6amilk==0 | i6amilkw==0)),
    i6pmilk= as.numeric(!(i6pmilk==0 | i6pmilkw==0)),
    i6formu= as.numeric(!(i6formu==0 | i6formuw==0)),
    i6suji= as.numeric(!(i6suji==0 | i6sujiw==0)),
    i6dal= as.numeric(!(i6dal==0 | i6dalw==0)),
    i6gruel= as.numeric(!(i6gruel==0 | i6gruelw==0)),
    i6shago= as.numeric(!(i6shago==0 | i6shagow==0)),
    i6rice= as.numeric(!(i6rice==0 | i6ricew==0)),
    i6khich= as.numeric(!(i6khich==0 | i6khichw==0)),
    i6barl= as.numeric(!(i6barl==0 | i6barlw==0)),
    i6banan= as.numeric(!(i6banan==0 | i6bananw==0)),
    i6biscu= as.numeric(!(i6biscu==0 | i6biscuw==0)),
    i6oil= as.numeric(!(i6oil==0 | i6oilw==0)),
    i6sug= as.numeric(!(i6sug==0 | i6sugw==0)),
    i6dair= as.numeric(!(i6dair==0 | i6dairw==0)),
    i6othf= as.numeric(!(i6othf==0 | i6othfw==0)),
    i6water= as.numeric(!(i6water==0 | i6waterw==0)),
    othfedfl= as.numeric(!(i6suji+i6dal+i6gruel+i6shago+i6rice+i6khich+i6barl+i6banan+i6biscu+ i6oil+ i6sug+ i6dair+i6othf==0))) %>%
  rename(subjido=hbgdkid,
         bfedfl=i6sbfed,
         nbfyes=i6bftime,
         anmlkfl=i6amilk,
         pwmlkfl=i6pmilk,
         formlkfl=i6formu,
         h20fedfl= i6water
  ) %>% select(subjido, bfedfl, nbfyes, anmlkfl, pwmlkfl, formlkfl, h20fedfl, othfedfl) %>%
  mutate(bfedfl = as.numeric(bfedfl), 
         anmlkfl= as.numeric(anmlkfl), 
         pwmlkfl= as.numeric(pwmlkfl), 
         formlkfl= as.numeric(formlkfl), 
         h20fedfl= as.numeric(h20fedfl), 
         othfedfl= as.numeric(othfedfl),
         antpt = "Infant 6 Month Postpartum")


table(i6mop$anmlkfl)
table(is.na(i6mop$anmlkfl))

table(i6mop$othfedfl)
table(is.na(i6mop$othfedfl))

table(i6mop$h20fedfl)
table(is.na(i6mop$h20fedfl))



# Infant 12 Month Postpartum
i12mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_i12mop.sas7bdat") %>% select(hbgdkid, i12sbfed,
                                                                         i12sbfed,
                                                                         i12bftime,
                                                                         i12amilk,
                                                                         i12pmilk,
                                                                         i12formu,
                                                                         i12rice,
                                                                         i12suji,
                                                                         i12dal,
                                                                         i12khich,
                                                                         i12potato,
                                                                         i12lvege,
                                                                         i12ovege,
                                                                         i12egg,
                                                                         i12meat,
                                                                         i12banan,
                                                                         i12ofruit,
                                                                         i12ruti,
                                                                         i12biscu,
                                                                         i12oil,
                                                                         i12sug,
                                                                         i12dair,
                                                                         i12atta,
                                                                         i12sprink,
                                                                         i12nnp,
                                                                         i12othf,
                                                                         i12amilkw,
                                                                         i12pmilkw,
                                                                         i12formuw,
                                                                         i12ricew,
                                                                         i12sujiw,
                                                                         i12dalw,
                                                                         i12khichw,
                                                                         i12potatow,
                                                                         i12lvegew,
                                                                         i12ovegew,
                                                                         i12eggw,
                                                                         i12meatw,
                                                                         i12bananw,
                                                                         i12ofruitw,
                                                                         i12rutiw,
                                                                         i12biscuw,
                                                                         i12oilw,
                                                                         i12sugw,
                                                                         i12dairw,
                                                                         i12attaw,
                                                                         i12sprinkw,
                                                                         i12nnpw,
                                                                         i12othfw) %>% mutate_all(funs(as.numeric)) %>%
  mutate(
    i12amilk= as.numeric(!(i12amilk==0 | i12amilkw==0)),
    i12pmilk= as.numeric(!(i12pmilk==0 | i12pmilkw==0)),
    i12formu= as.numeric(!(i12formu==0 | i12formuw==0)),
    i12suji= as.numeric(!(i12suji==0 | i12sujiw==0)),
    i12dal= as.numeric(!(i12dal==0 | i12dalw==0)),
    i12rice= as.numeric(!(i12rice==0 | i12ricew==0)),
    i12khich= as.numeric(!(i12khich==0 | i12khichw==0)),
    i12banan= as.numeric(!(i12banan==0 | i12bananw==0)),
    i12biscu= as.numeric(!(i12biscu==0 | i12biscuw==0)),
    i12oil= as.numeric(!(i12oil==0 | i12oilw==0)),
    i12sug= as.numeric(!(i12sug==0 | i12sugw==0)),
    i12dair= as.numeric(!(i12dair==0 | i12dairw==0)),
    i12potato= as.numeric(!(i12potato==0 | i12potatow==0)),
    i12lvege= as.numeric(!(i12lvege==0 | i12lvegew==0)),
    i12ovege= as.numeric(!(i12ovege==0 | i12ovegew==0)),
    i12meat= as.numeric(!(i12meat==0 | i12meatw==0)),
    i12ofruit= as.numeric(!(i12ofruit==0 | i12ofruitw==0)),
    i12ruti= as.numeric(!(i12ruti==0 | i12rutiw==0)),
    i12atta= as.numeric(!(i12atta==0 | i12attaw==0)),
    i12sprink= as.numeric(!(i12sprink==0 | i12sprinkw==0)),
    i12nnp= as.numeric(!(i12nnp==0 | i12nnpw==0)),
    othfedfl= as.numeric(!(i12suji+i12dal+i12rice+i12khich+i12banan+i12biscu+ i12oil+ i12sug+ i12dair+i12othf+
                             i12potato+i12lvege+i12ovege+i12meat+i12ofruit+i12ruti+i12atta+i12sprink+i12nnp==0)),
    h20fedfl=NA) %>% 
  rename(subjido=hbgdkid,
         bfedfl=i12sbfed,
         nbfyes=i12bftime,
         anmlkfl=i12amilk,
         pwmlkfl=i12pmilk,
         formlkfl=i12formu) %>%
  select(subjido, bfedfl, nbfyes, anmlkfl, pwmlkfl, formlkfl, h20fedfl, othfedfl) %>%
  mutate(bfedfl = as.numeric(bfedfl), 
         anmlkfl= as.numeric(anmlkfl), 
         pwmlkfl= as.numeric(pwmlkfl), 
         formlkfl= as.numeric(formlkfl), 
         h20fedfl= as.numeric(h20fedfl), 
         othfedfl= as.numeric(othfedfl),
         antpt = "Infant 12 Month Postpartum")


table(i12mop$anmlkfl)                                                                         
table(is.na(i12mop$anmlkfl))

table(i12mop$othfedfl)
table(is.na(i12mop$othfedfl))


# Child 24 Month Postpartum 
c24mop<-read_sas("U:/data/JiVitA-3/raw/hbgd_c24mop.sas7bdat")  %>% select(hbgdkid, c24sbfed,
                                                                          c24sbfed,
                                                                          c24bftime,
                                                                          c24amilk,
                                                                          c24pmilk,
                                                                          c24formu,
                                                                          c24rice,
                                                                          c24suji,
                                                                          c24dal,
                                                                          c24khich,
                                                                          c24potato,
                                                                          c24lvege,
                                                                          c24ovege,
                                                                          c24egg,
                                                                          c24meat,
                                                                          c24banan,
                                                                          c24ofruit,
                                                                          c24ruti,
                                                                          c24biscu,
                                                                          c24oil,
                                                                          c24sug,
                                                                          c24dair,
                                                                          c24atta,
                                                                          c24sprink,
                                                                          c24nnp,
                                                                          c24othf,
                                                                          c24amilkw,
                                                                          c24pmilkw,
                                                                          c24formuw,
                                                                          c24ricew,
                                                                          c24sujiw,
                                                                          c24dalw,
                                                                          c24khichw,
                                                                          c24potatow,
                                                                          c24lvegew,
                                                                          c24ovegew,
                                                                          c24eggw,
                                                                          c24meatw,
                                                                          c24bananw,
                                                                          c24ofruitw,
                                                                          c24rutiw,
                                                                          c24biscuw,
                                                                          c24oilw,
                                                                          c24sugw,
                                                                          c24dairw,
                                                                          c24attaw,
                                                                          c24sprinkw,
                                                                          c24nnpw,
                                                                          c24othfw) %>% mutate_all(funs(as.numeric)) %>%
  mutate(
    c24amilk= as.numeric(!(c24amilk==0 | c24amilkw==0)),
    c24pmilk= as.numeric(!(c24pmilk==0 | c24pmilkw==0)),
    c24formu= as.numeric(!(c24formu==0 | c24formuw==0)),
    c24suji= as.numeric(!(c24suji==0 | c24sujiw==0)),
    c24dal= as.numeric(!(c24dal==0 | c24dalw==0)),
    c24rice= as.numeric(!(c24rice==0 | c24ricew==0)),
    c24khich= as.numeric(!(c24khich==0 | c24khichw==0)),
    c24banan= as.numeric(!(c24banan==0 | c24bananw==0)),
    c24biscu= as.numeric(!(c24biscu==0 | c24biscuw==0)),
    c24oil= as.numeric(!(c24oil==0 | c24oilw==0)),
    c24sug= as.numeric(!(c24sug==0 | c24sugw==0)),
    c24dair= as.numeric(!(c24dair==0 | c24dairw==0)),
    c24potato= as.numeric(!(c24potato==0 | c24potatow==0)),
    c24lvege= as.numeric(!(c24lvege==0 | c24lvegew==0)),
    c24ovege= as.numeric(!(c24ovege==0 | c24ovegew==0)),
    c24meat= as.numeric(!(c24meat==0 | c24meatw==0)),
    c24ofruit= as.numeric(!(c24ofruit==0 | c24ofruitw==0)),
    c24ruti= as.numeric(!(c24ruti==0 | c24rutiw==0)),
    c24atta= as.numeric(!(c24atta==0 | c24attaw==0)),
    c24sprink= as.numeric(!(c24sprink==0 | c24sprinkw==0)),
    c24nnp= as.numeric(!(c24nnp==0 | c24nnpw==0)),
    othfedfl= as.numeric(!(c24suji+c24dal+c24rice+c24khich+c24banan+c24biscu+ c24oil+ c24sug+ c24dair+c24othf+
                             c24potato+c24lvege+c24ovege+c24meat+c24ofruit+c24ruti+c24atta+c24sprink+c24nnp==0)),
    h20fedfl=NA) %>% 
  rename(subjido=hbgdkid,
         bfedfl=c24sbfed,
         nbfyes=c24bftime,
         anmlkfl=c24amilk,
         pwmlkfl=c24pmilk,
         formlkfl=c24formu) %>%
  select(subjido, bfedfl, nbfyes, anmlkfl, pwmlkfl, formlkfl, h20fedfl, othfedfl) %>%
  mutate(bfedfl = as.numeric(bfedfl), 
         anmlkfl= as.numeric(anmlkfl), 
         pwmlkfl= as.numeric(pwmlkfl), 
         formlkfl= as.numeric(formlkfl), 
         h20fedfl= as.numeric(h20fedfl), 
         othfedfl= as.numeric(othfedfl),
         antpt = "Child 24 Month Postpartum")


table(c24mop$anmlkfl)                                                                         
table(is.na(c24mop$anmlkfl))

table(c24mop$othfedfl)
table(is.na(c24mop$othfedfl))


#compile datasets

d <- rbind(i1mop,i3mop,i6mop,i12mop, c24mop)
table(d$antpt)
table(d$bfedfl)
d$bfedfl[d$bfedfl==8] <- NA

#merge in with ki ID's
jvta3 <- readRDS("U:/data/jvt3.rds")
colnames(jvta3) <- tolower(colnames(jvta3))
jvta3 <- jvta3 %>% select(studyid, country, subjid, subjido, agedays, antpt) %>% mutate(subjido=as.numeric(subjido))

jvta3 <- left_join(jvta3, d, by=c("subjido", "antpt"))

#Merge with final fata
FINAL <- left_join(FINAL, jvta3, by=c("studyid", "country", "subjid"))




