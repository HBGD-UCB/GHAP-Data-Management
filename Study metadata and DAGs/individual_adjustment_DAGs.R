
library(dagitty)
library(lavaan)
library(ggdag)


A_ancestors= c("mage", "fage", "mhtcm","fhtcm", "mwtkg",  "mbmi")
Avar = "gagebrth"

createDAG <- function(Avar, A_ancestors, all_exposures = c("mage",    
                                                      "fage",           
                                                      "mhtcm",    
                                                      "fhtcm",
                                                      "mwtkg",    
                                                      "mbmi",    
                                                      "single",
                                                      "meducyrs", 
                                                      "feducyrs",
                                                      
                                                      "birthwt",  
                                                      "birthlen", 
                                                      "gagebrth", 
                                                      "vagbrth",  
                                                      "hdlvry",
                                                      
                                                      "sex", 
                                                      "brthordr",  
                                                      "brthmon", 
                                                      
                                                      "impsan", 
                                                      "safeh2o",  
                                                      "trth2o",   
                                                      "cleanck",  
                                                      "impfloor", 
                                                      
                                                      "nchldlt5",
                                                      "nhh",
                                                      "nrooms",
                                                      
                                                      "diarrhea",
                                                      
                                                      "BF_practice",
                                                      
                                                      "hfoodsec",
                                                      "hhwealth",
                                                      "enstunt",
                                                      "earlybf",
                                                      
                                                      "arm",
                                                      "month")){
  
  
  not_A_ancestors <- all_exposures[!(A_ancestors %in% all_exposures)]
  vars<-c(A_ancestors, Avar, "Y_Stunted")
  
  
  DAG <- paste0("dag {",
  paste0(c(A_ancestors,Avar), " -> Y_Stunted ", collapse=""),
  paste0(A_ancestors, " -> ",Avar," ", collapse=""),
  "}")
  
  

outcomes(DAG) <- "Y_Stunted"
exposures(DAG) <- Avar


tidy_dag <- tidy_dagitty(DAG)

  
# DAG_coordinates <- list(x=c(1:length(A_ancestors),2,length(A_ancestors)+1), y=c(rep(c(-1,1),10)[1:(length(A_ancestors))],0,0))
# names(DAG_coordinates$x) <- names(DAG_coordinates$y) <- vars
# 
# coordinates( DAG ) <- DAG_coordinates
# coordinates( DAG ) <- DAG_coordinates
# 
# plot(DAG)
# 
# title(paste0("DAG for ",Avar))
  
adj_set <- adjustmentSets( DAG, Avar, "Y_Stunted", effect="total" )


print(adj_set )

print(ggdag_adjustment_set(DAG))

  return(list(DAG=DAG, adj_set=adj_set))
}


DAG_list <- list()
DAG_list$gagebrth <- createDAG(Avar = "gagebrth", A_ancestors=c("mage",    
                                                      "fage",           
                                                      "mhtcm",    
                                                      "fhtcm",
                                                      "mwtkg",    
                                                      "mbmi",    
                                                      "single",
                                                      "meducyrs", 
                                                      "feducyrs",
                                                      "birthwt",  
                                                      "birthlen", 
                                                      "gagebrth", 
                                                      "vagbrth",  
                                                      "hdlvry",
                                                      "sex", 
                                                      "brthordr",  
                                                      "brthmon", 
                                                      "impsan", 
                                                      "safeh2o",  
                                                      "trth2o",   
                                                      "cleanck",  
                                                      "impfloor", 
                                                      "nchldlt5",
                                                      "nhh",
                                                      "nrooms",
                                                      "diarrhea",
                                                      "BF_practice",
                                                      "hfoodsec",
                                                      "hhwealth",
                                                      "enstunt",
                                                      "earlybf",
                                                      "arm",
                                                      "month"))
plot(test$DAG)
