
#devtools::install_github("HBGDki/ghap")

library(ghap)


set_git_base_path("U:/git")
get_git_base_path()

#studies <- get_study_list()

astudies <- get_study_list_anthro()
astudies<-as.data.frame(astudies)

head(astudies)

d <- use_study("mlex")
head(d)




#Get all studies' data
#(Note: takes a long time: run overnight)
astudies <- get_study_list_anthro()
for (id in astudies$short_id){
  tmp <- use_study(id)
}
