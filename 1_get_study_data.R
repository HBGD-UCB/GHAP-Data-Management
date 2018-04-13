



#------------------------------------------------------
# Author: Andrew Mertens
# amertens@berkeley.edu
#
# Script to download/load all datasets from GHAP onto
# the perminent directory of a GHAP usser
#
#-----------------------------------------------------



#devtools::install_github("HBGDki/ghap")

library(ghap)
library(xlsx)

set_git_base_path("U:/git")
get_git_base_path()

studies <- get_study_list()
studies<-as.data.frame(studies)
#write.xlsx(studies, "U:/results/GHAP_metadata_full.xlsx")


astudies <- get_study_list_anthro()
astudies<-as.data.frame(astudies)
#write.xlsx(astudies, "U:/results/GHAP_metadata.xlsx")



head(astudies)





#Get all studies' data
#(Note: takes a long time: run overnight)
astudies <- get_study_list_anthro()
for (id in astudies$short_id){
  tmp <- use_study(id)
}
