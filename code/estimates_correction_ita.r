# bootstrapping transition probabilities from panel SILC 
rm(list=ls())
lapply(list("data.table","tidyverse","VGAM","Formula"),require,character=T)
#------------------------------------------------------------------------------
yr <- "2004"
dir_in <- file.path("U:/NextCloud/Projects/IT_SILC/Data",yr,"data_in")
dir_out <- file.path("./data/data_out")
rds_file <- list.files(dir_in,full.names=T,pattern = "\\.RDS$")
dat <- readRDS(rds_file)
getwd()
mineta <- 50
maxeta <- 79
age_init <- seq(mineta-10,mineta+10,1)


# resampling using the IT-SILC structure - I keep the same number of people observed two, three and four times

sub2 <- dat[IDmax==2,]
sub3 <- dat[IDmax==3,]
sub4 <- dat[IDmax==4,] 


# dati <- setDT(dat[!is.na(RX010),])      

boot_fx <-function(){
  
  #-------data and models ---------------------------------- 
  
  dati <-  as.data.frame(rbind(
    sub2[sample(nrow(sub2), replace = T), ],
    sub3[sample(nrow(sub3), replace = T), ],
    sub4[sample(nrow(sub4), replace = T), ]))
  
  tr_format <- setDT(subset(dati,select = c("PB030","RX010","FROMD","TOD","PB150","PB010","DB040","HX100","weight","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")))
  names(tr_format) <- c("pid","age","from","to","gender","year","area","EQUI","weight","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")
  
  tr_format[,from2:=from,]
  tr_format[,to2:=to,]
  
  tr_format[,from:=factor(from2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  tr_format[,to:=factor(to2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  
  tr_format <- droplevels(tr_format[complete.cases(to),,])
  
  setkey(tr_format,"pid")
  tr_id <- tr_format[J(unique(pid)),mult="first"] 
  
  
  f1 <- formula(to ~  s(age) + from  + (edu_high + edu_low) + Centre + South)
  
  # Models 
  
  fit.m <- vgam(f1  
                ,family=multinomial(refLevel=2), data=droplevels(tr_format[tr_format$gender==1,]), trace=T,control=vgam.control(maxit=50))
  
  
  fit.f <- vgam(f1
                ,family=multinomial(refLevel=2), data=droplevels(tr_format[tr_format$gender==2,]), trace=T,control=vgam.control(maxit=50))
  
  #exp(coefvgam(fit.m,matrix=T))
  
  setkey(tr_format,"pid")
  tr_id <- droplevels(tr_format[J(unique(pid)),mult="first"]) 
  #---------------------------state space variables----------------------
  age <- paste(mineta:maxeta)
  hstatus <- c("Healthy","Disabled")
  variables <- list(age=age,hstatus=hstatus)
  
  # Generate state space
  tstates <- levels(interaction(variables,sep="::"))
  astates <- c("Dead")
  states <- c(tstates,astates)
  
  #------- probabilitites ---------------------------------------------------------------------------------------------------------
  # Data frame for prediction
  # Dummy data 
  
  tmp_probs <- data.frame(state=tstates)
  setDT(tmp_probs)[, c("from","age") := tstrsplit(state, "::", fixed=TRUE,keep=c(2,1),type.convert=TRUE),]
  tmp_probs[,from:=as.factor(from),]
  
 
  # ITA avg  ====================================================================================
  
  probs.m <- probs.f <- tmp_probs
  
  means_ita <- tr_format %>%  
    mutate(gender = factor(gender, c(1,2),c("Men","Women"))) %>% 
    group_by(gender) %>%  
    summarize(South = mean(South),
              Centre= mean(Centre),
              edu_low = mean(edu_low),
              edu_high = mean(edu_high),
              .groups = 'drop') %>% 
    select(c(South, Centre, edu_low,edu_high))
  
  
  probs.m <- crossing(probs.m,means_ita[1,])
  probs.f <- crossing(probs.f,means_ita[2,])
  
  probs.m_ita <- cbind(probs.m,predict(fit.m,probs.m,"response"))
  probs.f_ita <- cbind(probs.f,predict(fit.f,probs.f,"response"))
  
  #==============================================================================================
  transitions.m <- expand.grid(from=tstates,to=tstates)
  
  
  transitions <- function(dati_tmp){
    
    data_tp <- melt(setDT(dati_tmp),id.vars=c("state","age"),measure.vars = hstatus,variable.name = "state_to", value.name = "probs")
    data_tp[,to:=paste(age+1,state_to,sep="::"),]
    setnames(data_tp,"state","from")
    data_tp <- merge(transitions.m,data_tp[,c("from","to","probs"),with=F],by=c("from","to"),all.x=T)
    data_tp <- setDT(na.omit(data_tp))
  }
  
  tp_m_ita <- transitions(probs.m_ita)[,geo_edu := "ita"][]
  tp_f_ita <- transitions(probs.f_ita)[,geo_edu := "ita"][]
  

  Umat_m <- dcast(setDT(tp_m_ita[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  Umat_f <- dcast(setDT(tp_f_ita[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  
  Umat_m <- as.matrix(Umat_m,rownames="to")
  Umat_f <- as.matrix(Umat_f,rownames="to")
  
  # Mortality Corrention using LIFE TABLES ================================================================
  
  lt_female <- paste("./data/life_tables/Italia",as.numeric(yr)+2,"Femmine.csv",sep="_")
  
  lt_male <- paste("./data/life_tables/Italia",as.numeric(yr)+2,"Maschi.csv",sep="_")
  col_nm <- c("age", "lx", "dx", "qx", "Lx", "px", "ex")
  
  female <- read.csv2(lt_female,skip = 1, header=T, col.names = col_nm)
  male <- read.csv2(lt_male,skip = 1, header=T, col.names = col_nm)
  
  # General
  ages <- c(as.numeric(age))
  
  minage <- min(ages)-1
  
  ltf <- female[as.numeric(female$age)%in%ages,]
  
  surv_f <- c(ltf$px[1:(length(ages))])
  
  state.space <- list(setdiff(states,"Dead"),"Dead")
  
  ltm <- male[as.numeric(male$age)%in%ages,]
  
  surv_m <- ltm$px[1:(length(ages))]
  
  # Weights by sex
  
  weights_f <- prop.table(xtabs( ~ from,data = subset(tr_id, age %in% age_init & gender == 2 )))
  weights_f <- as.numeric(weights_f)
  
  weights_m <- prop.table(xtabs( ~ from,data = subset(tr_id, age %in% age_init & gender == 1 )))
  weights_m <- as.numeric(weights_m)
  
  # Correction mortality females
  
  # Get starting distribution 
  
  start.states <- paste(minage+1,hstatus,sep="::")
  
  start.distr <- numeric(length(state.space[[1]]))
  
  start.distr[match(start.states,state.space [[1]])] <- weights_f
  
  # Cycle
  
  for(which.age in ages[-length(ages)]) {
    
    which.states <- paste(which.age,hstatus,sep="::")
    
    surv.states_f <- colSums(Umat_f[,which.states]) 
    
    index <- (which.age - minage)
    new.surv <- surv_f[index]
    
    # Forecast to get distribution
    
    steps <- ( which.age - (minage + 1) )
    
    distr <- start.distr
    
    if(steps > 0) {
      
      for( j in  1 : (steps)   ) {
        
        distr <- Umat_f %*% distr
        
      }
    }
    
    
    weights <- distr[match(which.states,state.space [[1]])]
    
    weights <- weights_f*weights 
    
    weights <- weights/sum(weights)
    
    
    # Rescale
    
    factor <- sum(surv.states_f*weights)/new.surv
    
    tmp <- Umat_f[,which.states]/matrix(data=factor,nrow=dim(Umat_f)[1],ncol=length(which.states)) 
    
    # Check
    if(any( colSums(tmp) > 1 )) {
      
      ratio <- surv.states_f/new.surv
      
      ratio[ ratio %in% c(NA,0) ] <- 1
      
      Umat_f[, which.states ] <- Umat_f[, which.states ]/matrix(data=ratio,nrow=dim(Umat_f)[1],ncol=length(hstatus),byrow=T) 
      
    } else {
      
      Umat_f[, which.states] <- tmp 
    }
  } 
  
  
  
  
  # Males correction
  
  # Get starting distribution 
  
  start.states <- paste(minage+1,hstatus,sep="::")
  
  start.distr <- numeric(length(state.space[[1]]))
  
  start.distr[match(start.states,state.space [[1]])] <- weights_m
  
  # Cycle
  
  for(which.age in ages[-length(ages)]) {
    
    which.states <- paste(which.age,hstatus,sep="::")
    
    surv.states_m <- colSums(Umat_m[,which.states]) 
    
    index <- (which.age - minage)
    new.surv <- surv_m[index]
    
    # Forecast to get distribution
    
    steps <- ( which.age - (minage + 1) )
    
    distr <- start.distr
    
    if(steps > 0) {
      
      for( j in  1 : (steps)   ) {
        
        distr <- Umat_m %*% distr
        
      }
    }
    
    
    weights <- distr[match(which.states,state.space [[1]])]
    
    weights <- weights_m*weights 
    
    weights <- weights/sum(weights)
    
    
    # Rescale
    
    factor <- sum(surv.states_m*weights)/new.surv
    
    tmp <- Umat_m[,which.states]/matrix(data=factor,nrow=dim(Umat_m)[1],ncol=length(which.states)) 
    
    # Check
    if(any( colSums(tmp) > 1 )) {
      
      ratio <- surv.states_m/new.surv
      
      ratio[ ratio %in% c(NA,0) ] <- 1
      
      Umat_m[, which.states ] <- Umat_m[, which.states ]/matrix(data=ratio,nrow=dim(Umat_m)[1],ncol=length(hstatus),byrow=T) 
      
    } else {
      
      Umat_m[, which.states] <- tmp 
    }
  } 
  
  
  Expect <- function(Umat,eta_state) {
  N <- solve(diag(1,nrow(Umat))-Umat)
  expect <- sum(N[,eta_state])-0.5             
  expect
  }
  
  Fmat <- function(Umat){N <- solve(diag(1,nrow(Umat))-Umat)}
  
  NM <- Fmat(Umat_m)
  NF <- Fmat(Umat_f)
  
  
  LE_M  <- Expect(Umat_m,"50::Healthy")*weights_m[1] + Expect(Umat_m,"50::Disabled")*weights_m[2] 
  LE_F  <- Expect(Umat_f,"50::Healthy")*weights_f[1] + Expect(Umat_f,"50::Disabled")*weights_f[2] 
  
  DLE_M <- sum(c(sum(NM[1:30,"50::Healthy"]), sum(NM[1:30,"50::Disabled"])-0.5)*weights_m)  
  HLE_M <- sum(c(sum(NM[31:60,"50::Healthy"])-.5, sum(NM[31:60,"50::Disabled"]))*weights_m)   
                   
  DLE_F <- sum(c(sum(NF[1:30,"50::Healthy"]), sum(NF[1:30,"50::Disabled"])-0.5)*weights_f)  
  HLE_F <- sum(c(sum(NF[31:60,"50::Healthy"])-.5, sum(NF[31:60,"50::Disabled"]))*weights_f)   
  
  
  risultati <-c(LE_M,HLE_M,DLE_M,LE_F,HLE_F,DLE_F)
   
  
  return(risultati)
  
  
}



library(doParallel)
library(foreach)

# Parallelize =====================================================================================

mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
getDoParWorkers()
cl <- parallel::makeCluster(30)
doParallel::registerDoParallel(cl)

time_start <- Sys.time()

trials <- 500
expectancies <- foreach(i= icount(trials), .options.multicore=mcoptions, .combine='rbind',.packages=c('VGAM','Formula','data.table','tidyverse'),.errorhandling = 'remove',.verbose=T,.inorder=FALSE) %dopar% {
  boot_fx()
}

stopCluster(cl) 
closeAllConnections() 

time_end <- Sys.time()

gc()
ls()
dim(expectancies)

ci <- function(theta) {quantile(theta,probs = c(.025,.975),type=8)}

CICH <- round(apply(expectancies,2,ci),2)
time_start
time_end
names(CICH) <- c("LE_M","HLE_M","DLE_M","LE_F","HLE_F","DLE_F")
# Save results ===============================================================================


write.csv(CICH,file=paste0(dir_out,"/boot_results_lt_correct_ita_",yr,".csv"))

#write.csv(risultati,file=paste0(dir_out,"/results_lt_correct_ita_",yr,".csv"))

# matrices <- list(female = Umat_f,male = Umat_m) 
# 
# save(matrices,file=paste0(dir_out,"/Umats_lt_correct_ita_",yr,".rda"))
