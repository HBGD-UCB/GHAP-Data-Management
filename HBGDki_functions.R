


#------------------------------------------------------
# Author: Andrew Mertens
# amertens@berkeley.edu
#
# Calculate household wealth score from the first
# principal component of household asset ownership
# indicators
#-------------------------------------------------------



select_groups <- function(data, groups, ...) {
  data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]}




loadGHAP<-function(study, varlist){
  
  d<-readRDS(paste0(study,".rds"))
  
  d<-d[,which(colnames(d) %in% varlist)]
  #d <- lapply(d,as.factor)
  d <- apply(d, 2, as.character)
  d<-as.data.frame(d)
  return(d)
}



design_matrix<-function (W) 
{
  if (class(W) != "matrix" & class(W) != "data.frame") {
    W <- data.frame(W)
    if (is.null(ncol(W)) | ncol(W) == 0) {
      stop("Something is wrong with W.\nTo be safe, please try specifying it as class=data.frame.")
    }
  }
  ncolW <- ncol(W)
  flist <- numeric()
  for (i in 1:ncolW) {
    if (class(W[, i]) != "factor") {
      next
    }
    else {
      flist <- c(flist, i)
      W[, i] <- factor(W[, i])
      mm <- model.matrix(~-1 + W[, i])
      mW <- mm[, -c(1)]
      levs <- gsub(" ", "", levels(W[, i]))[-c(1)]
      if (length(levs) < 2) 
        mW <- matrix(mW, ncol = 1)
      colnames(mW) <- paste(names(W)[i], levs, sep = "")
      W <- data.frame(W, mW)
    }
  }
  if (length(flist) > 0) {
    W <- W[, -c(flist)]
  }
  return(W)
}




missing.data.SLimpute<-function(Y, d,family="gaussian", lib){
  
  #Missingness indicator
  Xmiss<-ifelse(is.na(d[,Y]),1,0)
  
  #Drop predictors with missingness
  dY<-subset(d, select=Y)
  dX<-d[,!(names(d) %in% Y)] 
  
  miss.col<-rep(T,ncol(dX))
  for(i in 1:ncol(dX)){
    if(sum(is.na(dX[,i]))!=0){
      miss.col[i]<-F
    }
  }
  
  d.temp<-subset(dX, select= colnames(dX)[which(miss.col==F)])
  #d.temp<-as.data.frame(dX[,!miss.col])
  dX<-dX[,miss.col]
  
  d<-cbind(dY,dX)
  train<-subset(d, !is.na(d[,Y]))
  predict<-subset(d,is.na(d[,Y]))
  
  
  mi<-which(is.na(d[,Y])) #missing index
  
  
  #Set up x and y variables for SL model
  X<-setdiff(names(d), Y)  
  
  
  fit<- SuperLearner(Y=as.numeric(subset(train, select=Y)[,1]), 
                     X=subset(train, select=X), 
                     family = gaussian(), SL.library=lib)
  
  
  # Generate predictions on the missing data set
  pred <- predict.SuperLearner(fit, newdata=subset(predict, select=X))
  d[mi,Y]<- as.data.frame(pred$pred)
  
  d<-cbind(d.temp,d,Xmiss)
  colnames(d)[ncol(d)]<-paste0(Y,".miss")
  
  return(d)
}




prepW<-function(study, Wvars, cont.vars=c("AGEDAYS")){
  # study=studylist[[1]]
  # Wvars=Wlist[[1]]
  # 
  # cont.vars=c("AGEDAYS", "BIRTHWT", "MMUACCM1")
  
  
  d<-as.data.frame(study)
  
  W<-subset(d, select=Wvars)
  table(is.na(W))
  for( i in 1:ncol(W)){
    cat(i, " ",colnames(W)[i],": ", class(W[,i]),"\n")
  }
  
  W <- apply(W, 2, as.character)
  W[W=="" | W=="." | W=="NA" | is.na(W)]<-"Missing"
  
  
  #Split out continious and factor variables
  cont.vars.index<-which(colnames(W) %in% cont.vars)
  contW<-W[,cont.vars.index]
  
  if(!is.null(ncol(contW))){
    suppressWarnings(contW <- apply(contW, 2, as.numeric))
  }else{
    suppressWarnings(contW<-as.numeric(contW))
  }
  
  factW<-W[,-cont.vars.index]
  factW <- apply(factW, 2, as.factor)
  
  table(is.na(factW))
  
  
  
  
  #Impute missing continious data
  #Add back in factor data for imputation
  data<-data.frame(contW, factW)
  table(is.na(data))
  for( i in 1:ncol(data)){
    cat(i, " ",colnames(data)[i],": ", class(data[,i]),"\n")
  }
  
  
  #Then SL impute continious variables
  for(i in 1:length(cont.vars)){
    suppressWarnings(data<-missing.data.SLimpute(Y=cont.vars[i], d=data, family="gaussian", lib=c("SL.mean","SL.glmnet")))
  }
  table(is.na(data))
  
  
  #Convert factors to indicators
  W<-design_matrix(data)
  
  #Remove near zero variance columns
  dim(W)
  preproc = caret::preProcess(W, method = c("zv", "nzv"))
  W = predict(preproc, W)
  rm(preproc)
  dim(W)
  
  #Drop empty factor levels
  W<-droplevels(W)
  
  return(W)
}




studyCV.tmle<-function(d, 
                       Y,
                       Avar, 
                       nstudies,
                       lib=lib, 
                       Wvars,
                       family,
                       CVstudies=T){
  
  # d=dat 
  # Y=Y
  # Avar=A
  # nstudies=nstudies
  # lib=SLlibrary
  # family=family
  # Wvars=colnames(w)
  # CVstudies=ifelse(nstudies==1,F,T)
  
  
  set.seed(12345)
  d<-as.data.frame(d)
  Y<-subset(d, select=Y)
  subjid<-subset(d, select="SUBJID")
  W<-subset(d, select=Wvars)
  W<-design_matrix(as.data.frame(W))
  
  if(nstudies>1){
    
    mixed.CVfolds<-createFolds(factor(d$STUDYID), k = 5, list = TRUE)
    
    
    X<-cbind(d[,Avar], W)
    colnames(X)[1]<-Avar
    Qsl<-SuperLearner(Y=Y[,1],
                      #X=cbind(d[,Avar], subset(d, select=Wvars)),
                      X=X,
                      SL.library = lib,
                      family=family,
                      cvControl = ifelse(CVstudies==T,
                                         list(V=5, validRows=mixed.CVfolds),
                                         list(V=5)))
    dY1<-dY0<-as.data.frame(X)
    dY1[,Avar]<-1
    dY0[,Avar]<-0
    Q<-cbind(predict(Qsl, newdata = dY1)$pred,predict(Qsl, newdata = dY0)$pred)
    head(Q)
    
    
    gsl<-SuperLearner(Y=subset(d, select=Avar)[,1],
                      X=X[,-1],
                      SL.library = lib,
                      family=family,
                      cvControl = ifelse(CVstudies==T,
                                         list(V=5, validRows=mixed.CVfolds),
                                         list(V=5)))
    
    g1W<-predict(gsl)$pred
    
    
    
    suppressWarnings(mixedCV.tmle.A<-tmle(Y=Y[,1], 
                         A=subset(d, select=Avar)[,1], 
                         W=X[,-1], 
                         Q=Q,
                         g1W=g1W,
                         family = family, 
                         verbose = F))
    
  }else{
    suppressWarnings(mixedCV.tmle.A<-tmle(Y=Y[,1], 
                         A=subset(d, select=Avar)[,1], 
                         W=W, 
                         family = family, 
                         Q.SL.library=lib,
                         g.SL.library = lib,
                         verbose = F,
                         id=subjid[,1]))  
    
  }
  if(family=="binomial"){
    return(c(unlist(mixedCV.tmle.A$estimates$ATE),unlist(mixedCV.tmle.A$estimates$RR)))
  }else{
    return(unlist(mixedCV.tmle.A$estimates$ATE))
  }
}


#---------------------------------------
# RiskFactorFunction.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Function to estimate ATE with TMLE
# across levels of a risk factor
#---------------------------------------

tmle_risk<-function(dat=d, 
                    Y="HAZ", 
                    W=Wvars, 
                    n.cat=2, 
                    A, 
                    Acuts=NULL, 
                    Alevels=NULL, 
                    reflevel=NULL, 
                    family="gaussian", 
                    SLlibrary=library, 
                    outputdf=res.df,
                    overall.dist=T,
                    sparseN=5,
                    adjusted=F){
  
  
<<<<<<< HEAD
                                   dat=dsub
                                   Y="stunt"
                                   W=colnames(dsub)[which(!(colnames(dsub) %in% c("STUDYID","COUNTRY","SUBJID","AGEDAYS","HAZ","stunt","sevstunt", Avar, paste0("miss_",Avar))))]
                                   n.cat=n.cat
                                   A=Avar
                                   Acuts=Acuts
                                   Alevels=Alevels
                                   reflevel=1
                                   family="binomial"
                                   SLlibrary="SL.glm"
                                   outputdf=NULL
                                   overall.dist=T
                                   sparseN=5
                                   adjusted=T

        
                                   
  # 
  # dat=as.data.frame(df[df$STUDYID=="ki1148112-iLiNS-DYAD-M",])
  # W=Wvars
  # A="PARITY"
  # n.cat=4
  # reflevel=1
  # outputdf=NULL
  # Y="wast"
  # family="binomial"
  # SLlibrary=lib
  # adjusted=T
  # sparseN=0
  # 
  # Acuts= c(1.1, 2.1, 3.1, 4.1)
  # Alevels=c("Firstborn", "Secondborn","Thirdborn","4th+ born")
  # 
  
=======
>>>>>>> 776799dbd659ca5b8d172b9522a9c2d80fa37eec
  #get study name
  study <-deparse(substitute(dat))
  
  #get number of studies 
  nstudies<-length(unique(dat$STUDYID))
  
  if(A %in% W){W<-W[-which(W %in% A)]}
  y<-subset(dat, select=Y)
  a<-subset(dat, select=A)
  dat$STUDYID<-as.factor(dat$STUDYID)
  studyid<-subset(dat, select="STUDYID")
  subjid<-subset(dat, select="SUBJID")
  
  if(overall.dist==F & class(a[,1])=="numeric"){
    Acuts<-as.numeric(quantile(a[,1], probs = c((1:(n.cat-1))/n.cat), na.rm=T))
    if(n.cat==2) Alevels<-c(paste0("<=",round(Acuts[1],3)),  paste0(">",round(Acuts[1],3)))
    if(n.cat==3) Alevels<-c(paste0("<=",round(Acuts[1],3)), paste0(round(Acuts[1],3),"-",round(Acuts[2],3)), paste0(">",round(Acuts[2],3)))
    if(n.cat==4) Alevels<-c(paste0("<=",round(Acuts[1],3)), paste0(round(Acuts[1],3),"-",round(Acuts[2],3)), paste0(round(Acuts[2],3),"-",round(Acuts[3],3)), paste0(">",round(Acuts[3],3)))
  }
  
  reference<-Alevels[reflevel]
  comparisons<-Alevels[-reflevel]
  
  
  summary(a[,1])
  
  table(findInterval(a[,1], Acuts, left.open=T))
  
  if(!is.null(Acuts)){
    a[,1]<-findInterval(a[,1], Acuts, left.open=T)
    a[,1]<-factor(a[,1])
    if(!is.null(Alevels)){levels(a[,1])<-Alevels[as.numeric(levels(a[,1]))+1]}
  }
  
  a[,1]<-as.factor(a[,1])
  print(table(a[,1]))
  
  w<-subset(dat, select=c(W))
    #remove sparse covariates
  if(adjusted==T){
    dim(w)
    w <- w[ , colSums(is.na(w)) == 0]
    #Drop factors with no variation
    w<-droplevels(w)
    w <- w[, sapply(w, nlevels) > 1 | sapply(w, is.numeric)]
    preproc = caret::preProcess(w, method = c("zv", "nzv"))
    w = predict(preproc, w)
    dim(w)
    print(colnames(w))
  }

  
  
  fulldat<-data.frame(y,a,studyid,subjid,w)
  fulldat<-fulldat[complete.cases(fulldat),]
  
  #Extract mean Y|A
  levelmeans<- fulldat %>% #fulldat[fulldat[,2]==levels(fulldat[,2])[1],] %>%
    group_by(.[[2]]) %>%
    do(as.data.frame(washb_mean(Y=.[[1]], id=1:length(.[[1]]), print = F))) %>% 
    as.data.frame %>% `rownames<-`(.[,1]) #%>% .[,-1]
  
  
  #Extract desired levels
  levelmeans<-levelmeans[1:n.cat,]
  
<<<<<<< HEAD
#Code for TMLE3
  
  #NOTE: Make sure I'm feeding in a factor, not numeric A, for the multinomial
  
  # nodes <- list(W=colnames(w),
  #               A=A,
  #               Y=Y)
  # 
  # lrnr_glm_fast <- make_learner(Lrnr_glm_fast)
  # lrnr_mean <- make_learner(Lrnr_mean)
  # learner_list <- list(Y=lrnr_mean, A=lrnr_glm_fast)
  # 
  # d<- data.table(fulldat)
  # tmle_fit_from_spec <- tmle3(tmle_TSM_all(),d, nodes, learner_list)
  # print(tmle_fit_from_spec)
  # 
  # tmle_fit_from_spec$summary
  # tmle_fit_from_spec$delta_summary
  # 
  # 
  #   
  # tmle_fit_PAF <- tmle3(tmle_PAR(baseline_level = 0),d, nodes, learner_list)
  # print(tmle_fit_PAF)
  # tmle_fit_PAF$delta_summary
  
=======
>>>>>>> 776799dbd659ca5b8d172b9522a9c2d80fa37eec
  
  res<-NULL
  for(i in comparisons){

    dat<-fulldat[fulldat[,2]==reference | fulldat[,2]==i,]
    # print(table(dat[,2], dat[,1]))

    print(table(dat[,2]))

    #Convert factor to binary indicator
    dat[,2]<-ifelse(dat[,2]==reference,0,1)

    table(dat[,1],dat[,2])
    if(family=="binomial"){print(c(table(dat[,1],dat[,2])))}
    sparse=F
    if(family=="binomial"){
      tab<-c(table(dat[,1],dat[,2]))
      if(tab[1]<sparseN+1 | tab[2]<sparseN+1  | tab[3]<sparseN+1  | tab[4]<sparseN+1 | is.na(tab[1]) | is.na(tab[2]) | is.na(tab[3]) | is.na(tab[4])){
        sparse=T
      }
    }



    if(sum(dat[,2]==0)>sparseN & sum(dat[,2]==1)>sparseN & sparse==F){ #Make sure there is enough support in the data
      w<-as.data.frame(dat[,5:ncol(dat)])
      if(ncol(w)==1){colnames(w)<-colnames(dat)[5]<-"colW"}

      if(adjusted==T){
        if(family=="binomial"){
          Wscreen <- hbgdki_prescreen(Y=dat[,1], Ws=droplevels(w), ncases=sum(dat[,1]))
          }else{
            Wscreen <- washb_prescreen(Y=dat[,1], Ws=droplevels(w))
            }
        if(length(Wscreen)>0){
        w<-subset(w, select=Wscreen)
        }else{
          dat$w1<-rep(1, nrow(dat))
          dat$w2<-rep(1, nrow(dat))
          w<-data.frame(w1=dat$w1, w2=dat$w2)
        }
        if(length(Wscreen)==1){ #add null covariate so glmnet works
        dat$w2 <- w$w2 <- rep(1, nrow(dat))
        }
      }

      fit<-studyCV.tmle(d=dat,
                        Y=Y,
                        Avar=A,
                        nstudies=nstudies,
                        lib=SLlibrary,
                        family=family,
                        Wvars=colnames(w),
                        CVstudies=ifelse(nstudies==1,F,T))

      out<-as.data.frame(fit)
      names(out)<-i
      out<-t(out)
      out<-data.frame(A,i,out,reference,compN=sum(dat[,2]==0), refN=sum(dat[,2]==1))

      if(family=="binomial"){
        out<-data.frame(out,t(c(table(dat[,1],dat[,2]))))
        colnames(out)<-c("variable","level","psi","var.psi","CI1","CI2","pvalue","RR", "RRCI1", "RRCI2","RRpvalue","log.RR","var.log.RR", "reference", "compN", "refN","d","c","b","a")
      }else{
        colnames(out)<-c("variable","level","psi","var.psi","CI1","CI2","pvalue", "reference", "compN", "refN")
      }


    }else{
      if(family=="binomial"){
        out<-data.frame(variable=A,level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, RR=NA,  RRCI1=NA, RRCI2=NA, RRpvalue=NA, log.RR=NA, var.log.RR=NA, reference=reference, compN=sum(dat[,2]==0), refN=sum(dat[,2]==1), d=tab[1], c=tab[2], b=tab[3], a=tab[4])

      }else{
        out<-data.frame(variable=A,level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, reference=reference, compN=sum(dat[,2]==0), refN=sum(dat[,2]==1))
      }
      rownames(out)<-i
    }
    res<-rbind(res,out)
  }

  
  if(family=="binomial"){
    refrow<-data.frame(res[1,1],reference,t(rep(NA,11)),reference,t(rep(NA,6)))
  }else{
    refrow<-data.frame(res[1,1],reference,t(rep(NA,5)),reference,t(rep(NA,2)))
  }
  colnames(refrow)<-colnames(res)
  res<-rbind(refrow,res)
  
  res<-cbind(study,res,levelmeans)
  
  if(family=="binomial"){
    colnames(res)<-c("study","variable","level","ATE","ATE.var","ATE.CI1","ATE.CI2", "ATE.Pval","RR","RR.CI1","RR.CI2", "RR.Pval", "logRR.psi","logRR.var","reference", "compN", "refN","a","b","c","d", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }else{
    colnames(res)<-c("study","variable","level","ATE","var","CI1","CI2", "Pval","reference", "compN", "refN", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }
  rownames(res)<-NULL
  res<-res[,-1] #Drop study label for dplyr groupby code
  
  
  if(!is.null(outputdf)){
    return(rbind(outputdf,res))
  }else{
    #return(list(res=res, PAFdat=PAFdat))
    return(res)
  }
}







tmle_subgroup<-function(dat=d, 
                        Y="HAZ", 
                        W=Wvars, 
                        n.cat=2, 
                        A, 
                        Acuts=NULL, 
                        Alevels=NULL, 
                        #reflevel=NULL, 
                        family="gaussian", 
                        SLlibrary=library, 
                        outputdf=res.df){
  
  
  
  
  #get study name
  study <-deparse(substitute(dat))
  
  
  #get number of studies 
  nstudies<-length(unique(dat$STUDYID))
  
  if(A %in% W){W<-W[-which(W %in% A)]}
  y<-subset(dat, select=Y)
  a<-subset(dat, select=A)
  tr<-subset(dat, select=tr)
  studyid<-subset(dat, select="STUDYID")
  
  summary(a[,1])
  table(findInterval(a[,1], Acuts))
  
  if(!is.null(Acuts)){
    a[,1]<-findInterval(a[,1], Acuts)
    a[,1]<-factor(a[,1])
    if(!is.null(Alevels)){levels(a[,1])<-Alevels[as.numeric(levels(a[,1]))+1]}
  }
  
  a[,1]<-as.factor(a[,1])
  print(table(a[,1]))
  
  w<-subset(dat, select=c(W))
  
  w<-design_matrix(w)
  fulldat<-data.frame(y,a,tr,studyid,w)
  fulldat<-fulldat[complete.cases(fulldat),]
  
  #Extract mean Y|A
  levelmeans<- fulldat %>%
    group_by(.[[2]]) %>%
    do(as.data.frame(washb_mean(Y=.[[1]], id=1:length(.[[1]]), print = F))) %>% 
    as.data.frame %>% `rownames<-`(.[,1]) 
  
  #Extract desired levels
  levelmeans<-levelmeans[1:n.cat,]
  print(levelmeans)
  
  res<-NULL
  for(i in Alevels){
    
    dat<-fulldat[fulldat[,2]==i,]
    
    cat(i,":\n")
    print(table(dat[,3]))
    
    
    sparse=F
    if(family=="binomial"){
      tab<-c(table(dat[,1],dat[,3]))
      if(tab[1]<6 | tab[2]<6  | tab[3]<6  | tab[4]<6 | is.na(tab[1]) | is.na(tab[2]) | is.na(tab[3]) | is.na(tab[4])){
        sparse=T
      }
    }
    
    
    if(sum(dat[,3]==0)>5 & sum(dat[,3]==1)>5 & sparse==F){ #Make sure there is enough support in the data
      w<-dat[,5:ncol(dat)]
      
      fit<-studyCV.tmle(d=dat, 
                        Y=Y,
                        Avar="tr", 
                        nstudies=nstudies,
                        lib=SLlibrary,
                        family=family,
                        Wvars=colnames(w),
                        CVstudies=ifelse(nstudies==1,F,T))
      
      out<-as.data.frame(fit)
      names(out)<-i
      out<-t(out)
      out<-data.frame(A,i,out,compN=sum(dat[,2]==0), refN=sum(dat[,2]==1))
      
      if(family=="binomial"){
        out<-data.frame(out,t(c(table(dat[,1],dat[,3]))))
        colnames(out)<-c("variable","level","psi","var.psi","CI1","CI2","pvalue","RR", "RRCI1", "RRCI2","RRpvalue","log.RR","var.log.RR","a","b","c","d", "compN", "refN")
      }else{
        colnames(out)<-c("variable","level","psi","var.psi","CI1","CI2","pvalue", "compN", "refN")
      }
      
      
    }else{
      if(family=="binomial"){
        out<-data.frame(variable=A,level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, RR=NA,  RRCI1=NA, RRCI2=NA, RRpvalue=NA, log.RR=NA, var.log.RR=NA, compN=sum(dat[,2]==0), refN=sum(dat[,2]==1), a=tab[1], b=tab[2], c=tab[3], d=tab[4])
        
      }else{
        out<-data.frame(variable=A,level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, compN=sum(dat[,3]==0), refN=sum(dat[,3]==1))
      }
      rownames(out)<-i
    }
    res<-rbind(res,out)
  }
  
  
  res<-cbind(study,res,levelmeans)
  
  if(family=="binomial"){
    colnames(res)<-c("study","variable","level","ATE","ATE.var","ATE.CI1","ATE.CI2", "ATE.Pval","RR","RR.CI1","RR.CI2", "RR.Pval", "logRR.psi","logRR.var", "compN", "refN","a","b","c","d", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }else{
    colnames(res)<-c("study","variable","level","ATE","var","CI1","CI2", "Pval", "compN", "refN", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }
  rownames(res)<-NULL
  res<-res[,-1] #Drop study label for dplyr groupby code
  
  if(!is.null(outputdf)){
    return(rbind(outputdf,res))
  }else{
    return(res)
  }
  
}




tmle_subgroup_ATE<-function(dat=d, 
                            Y="HAZ", 
                            W=Wvars, 
                            family="gaussian", 
                            SLlibrary=library, 
                            outputdf=res.df){
  

  
  #get study name
  study <-deparse(substitute(dat))
  
  
  #get number of studies 
  nstudies<-length(unique(dat$STUDYID))
  
  y<-subset(dat, select=Y)
  #a[,1]<-as.numeric(as.character(a[,1]))
  tr<-subset(dat, select=tr)
  studyid<-subset(dat, select="STUDYID")
  a<-data.frame(rep(1, nrow(dat)))
  w<-subset(dat, select=c(W))
  
  
  w<-design_matrix(w)
  fulldat<-data.frame(y,a,tr,studyid,w)
  fulldat<-fulldat[complete.cases(fulldat),]
  
  #Extract mean Y|A
  levelmeans<- fulldat %>% #fulldat[fulldat[,2]==levels(fulldat[,2])[1],] %>%
    group_by(tr) %>%
    do(as.data.frame(washb_mean(Y=.[[1]], id=1:length(.[[1]]), print = F))) %>% 
    as.data.frame %>% `rownames<-`(.[,1]) #%>% .[,-1]
  
  print(levelmeans)
  
  res<-NULL
  i<-1
  
  dat<-fulldat
  
  print(table(dat[,3]))
  
  
  sparse=F
  if(family=="binomial"){
    tab<-c(table(dat[,1],dat[,3]))
    if(tab[1]<6 | tab[2]<6  | tab[3]<6  | tab[4]<6 | is.na(tab[1]) | is.na(tab[2]) | is.na(tab[3]) | is.na(tab[4])){
      sparse=T
    }
  }
  
  
  if(sum(dat[,3]==0)>5 & sum(dat[,3]==1)>5 & sparse==F){ #Make sure there is enough support in the data
    w<-dat[,5:ncol(dat)]
    
    fit<-studyCV.tmle(d=dat, 
                      Y=Y,
                      Avar="tr", 
                      nstudies=nstudies,
                      lib=SLlibrary,
                      family=family,
                      Wvars=colnames(w),
                      CVstudies=ifelse(nstudies==1,F,T))
    
    out<-as.data.frame(fit)
    names(out)<-i
    out<-t(out)
    out<-data.frame("ATE",i,out,compN=sum(dat[,2]==0), refN=sum(dat[,2]==1))
    
    if(family=="binomial"){
      out<-data.frame(out,t(c(table(dat[,1],dat[,3]))))
      colnames(out)<-c("variable","level","psi","var.psi","CI1","CI2","pvalue","RR", "RRCI1", "RRCI2","RRpvalue","log.RR","var.log.RR","a","b","c","d", "compN", "refN")
    }else{
      colnames(out)<-c("variable","level","psi","var.psi","CI1","CI2","pvalue", "compN", "refN")
    }
    
    
  }else{
    if(family=="binomial"){
      out<-data.frame(variable="ATE",level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, RR=NA,  RRCI1=NA, RRCI2=NA, RRpvalue=NA, log.RR=NA, var.log.RR=NA, compN=sum(dat[,2]==0), refN=sum(dat[,2]==1), a=tab[1], b=tab[2], c=tab[3], d=tab[4])
      
    }else{
      out<-data.frame(variable="ATE",level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, compN=sum(dat[,3]==0), refN=sum(dat[,3]==1))
    }
    rownames(out)<-i
  }
  res<-out
  
  
  if(family=="binomial"){
    refrow<-data.frame(res[1,1],t(rep(NA,18)))
  }else{
    refrow<-data.frame(res[1,1],t(rep(NA,7)))
  }
  colnames(refrow)<-colnames(res)
  res<-rbind(refrow,res)
  
  res<-cbind(study,res,levelmeans)
  
  if(family=="binomial"){
    colnames(res)<-c("study","variable","level","ATE","ATE.var","ATE.CI1","ATE.CI2", "ATE.Pval","RR","RR.CI1","RR.CI2", "RR.Pval", "logRR.psi","logRR.var","a","b","c","d", "compN", "refN", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }else{
    colnames(res)<-c("study","variable","level","ATE","var","CI1","CI2", "Pval", "compN", "refN", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }
  rownames(res)<-NULL
  res<-res[,-1] #Drop study label for dplyr groupby code
  
  if(!is.null(outputdf)){
    return(rbind(outputdf,res))
  }else{
    return(res)
  }
  
}






#Pooled logistic functions for IRR:

# --------------------------------------
# Robust clustered SE function
# http://people.su.se/~ma/mcluster.R
# R (www.r-project.org) codes for computing multi-way clustered-standard errors
# Mahmood Arai, Jan 21, 2008. 
# See: Thompson (2006), Cameron, Gelbach and Miller (2006) and Petersen (2006).
#
# slightly modified to have it return the vcovCL object
# rather than the updated fit (since might need the VC matrix)
# --------------------------------------
cl   <- function(dat,fm, cluster){
  # dat: data used to fit the model
  # fm : model fit (object)
  # cluster : vector of cluster IDs
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}

# --------------------------------------
# function to estimate poisson models
# with robust SEs
# --------------------------------------
poissonRB <- function(fmla,dat,print=TRUE) {
  # poisson regression with robust SEs
  # fmla : formula for the model fit
  # dat  : data used to fit the model (has to include "SUBJID" for individuals)
  # print: print results ?
  
  # restrict to complete cases
  dat <- dat[complete.cases(dat[,c(all.vars(paste0(fmla)))]),] 
  
  fit <- glm(fmla,family=poisson,data=dat,model=FALSE,x=TRUE)
  
  dat <- na.omit(dat[ , c("SUBJID", all.vars(formula(fit)))])
  vcovCL <- cl(dat=dat,fm=fit,cluster=dat$SUBJID)
  rfit <- coeftest(fit, vcovCL)
  if(print==TRUE) {
    cat(paste("N obs=",nrow(dat)))
    print(rfit)
  }
  list(fit=fit,rfit=rfit,vcovCL=vcovCL)
}





