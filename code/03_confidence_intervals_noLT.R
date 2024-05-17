rm(list=ls())
libraries <- list("data.table","VGAM","tidyverse")
lapply(libraries,require, character=T)

# Estimation of transition probabilities between health, disability and death states
# using Silc data 2012-15, survival correction matching survival probs with italian 
# life tables 2014. 
#------------------------------------------------------------------------------

anno <- sprintf("%02d",07:19)




dir_in <- file.path("U:/Projects/2022_07_hle_it_silc/dat_trformat/")

dir_out <- file.path("data/data_out/")


mineta <- 50
maxeta <- 79

age_init <- seq(mineta-10,mineta+10,1)

for (yr in anno){
  
  tr_file <- paste0("/SILC_panel_",yr,".rds")   
  
  dat <- readRDS(paste0(dir_in,tr_file)) 
  
  dat[,IDmax:=.N,by=PB030]

  sub2 <- dat[IDmax==2,]
  sub3 <- dat[IDmax==3,]
  sub4 <- dat[IDmax==4,] 
  sub5 <- dat[IDmax==5,] 
  sub6 <- dat[IDmax==6,] 
# dati <- dat

boot_fx <-function(){
  
  #-------data and models ----------------------------------
  
  dati <-  as.data.frame(rbind(
    sub2[sample(nrow(sub2), replace = T), ],
    sub3[sample(nrow(sub3), replace = T), ],
    sub4[sample(nrow(sub4), replace = T), ],
    sub5[sample(nrow(sub5), replace = T), ],
    sub6[sample(nrow(sub6), replace = T), ]))
  
  tr_format <- setDT(subset(dati,select = c("PB030","RX010","FROM","TO","PB150","PB010","DB040","HX100","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")))
  names(tr_format) <- c("pid","age","from","to","gender","year","area","EQUI","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")
  
  tr_format[,from2:=from,]
  tr_format[,to2:=to,]
  
  tr_format[,from:=factor(from2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  tr_format[,to:=factor(to2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  
  tr_format <- unique(droplevels(tr_format[complete.cases(to),,]))
  
  setkey(tr_format,"pid")
  tr_id <- tr_format[J(unique(pid)),mult="first"] 
  
  
  f1 <- formula(to ~  s(age) + from  + (edu_high + edu_low) + Centre + South)
  
  # Models 
  
  fit.m <- vgam(f1  
                ,family=multinomial(refLevel=1), data=droplevels(tr_format[tr_format$gender==1,]), trace=T,control=vgam.control(maxit=50))
  
  
  fit.f <- vgam(f1
                ,family=multinomial(refLevel=1), data=droplevels(tr_format[tr_format$gender==2,]), trace=T,control=vgam.control(maxit=50))
  
  
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
  
  
  
  # ITA avg  ==========================================
  
  probs.m <- probs.f <- tmp_probs
  
  means_ita <-
    tr_format[, lapply(.SD, function(x)
      mean(x, na.rm = T)), .SDcols = c("South", "Centre", "edu_low", "edu_high"), 
      by = "gender"]
  
  probs.m <- cbind(probs.m, means_ita[gender == 1, ])
  probs.f <- cbind(probs.f, means_ita[gender == 2, ])
  
  probs.m_ita <- cbind(probs.m, predict(fit.m, probs.m, "response"))
  probs.f_ita <- cbind(probs.f, predict(fit.f, probs.f, "response"))
  
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
  
  
  Expect <- function(Umat,eta_state) {
    N <- solve(diag(1,nrow(Umat))-Umat)
    expect <- sum(N[,eta_state])-0.5             
    expect
  }
  
  Fmat <- function(Umat){N <- solve(diag(1,nrow(Umat))-Umat)}
  
  NM <- Fmat(Umat_m)
  NF <- Fmat(Umat_f)
  
  #Init 
  weights_f <- prop.table(xtabs( ~ from,data = subset(tr_id, age %in% age_init & gender == 2 )))
  weights_f <- as.numeric(weights_f)
  
  weights_m <- prop.table(xtabs( ~ from,data = subset(tr_id, age %in% age_init & gender == 1 )))
  weights_m <- as.numeric(weights_m)
  
  LE_M  <- Expect(Umat_m,"50::Healthy")*weights_m[1] + Expect(Umat_m,"50::Disabled")*weights_m[2] 
  LE_F  <- Expect(Umat_f,"50::Healthy")*weights_f[1] + Expect(Umat_f,"50::Disabled")*weights_f[2] 
  
  
  
  
  DLE_M <- sum(c(sum(NM[rownames(NM) %like% "Disabled","50::Healthy"]), sum(NM[rownames(NM) %like% "Disabled","50::Disabled"])-0.5)*weights_m)  
  HLE_M <- sum(c(sum(NM[rownames(NM) %like% "Healthy","50::Healthy"])-.5, sum(NM[rownames(NM) %like% "Healthy","50::Disabled"]))*weights_m)   
  
  DLE_F <- sum(c(sum(NF[rownames(NF) %like% "Disabled","50::Healthy"]), sum(NF[rownames(NF) %like% "Disabled","50::Disabled"])-0.5)*weights_f)  
  HLE_F <- sum(c(sum(NF[rownames(NF) %like% "Healthy","50::Healthy"])-.5, sum(NF[rownames(NF) %like% "Healthy","50::Disabled"]))*weights_f)   
  
  
  risultati <-c(LE_M,HLE_M,DLE_M,LE_F,HLE_F,DLE_F)
  
  
  return(risultati)
  
  
}



library(doParallel)
library(foreach)
#
# # Parallelize =====================================================================================
#
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
getDoParWorkers()
cl <- parallel::makeCluster(30)
doParallel::registerDoParallel(cl)

time_start <- Sys.time()

trials <- 1000
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

save(list='expectancies',file=paste0(dir_out,"/boot_expectancies_no_lt_2023_ita_",yr,".rda"))

write.csv(CICH,file=paste0(dir_out,"/boot_results_no_lt_2023_ita_",yr,".csv"))

}

