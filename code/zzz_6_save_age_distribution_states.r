
rm(list=ls())
libraries <- list("data.table","VGAM","tidyverse")
lapply(libraries,require, character=T)

# Estimation of health distribution health, by age and sex
# this is for 5-year age groups, not including covariates
# not currently used.

# this could be used to compare prev w share or another source
#------------------------------------------------------------------------------


yrs <- 7:19

myr <- sprintf("%02d", as.numeric(yrs))


dir_in <- file.path("dat_trformat/")

dir_out <- file.path("./out/")

mineta <- 50
maxeta <- 79

ages <- mineta:maxeta

age_init <- seq(mineta-10,mineta+10,1)

dtout <- rep(list(NA),length(myr))

for (i in myr){
  
  tr_file <- paste0("/SILC_panel_",i,".rds")
  
  dat <- readRDS(paste0(dir_in,tr_file))
  
  dat <- setDT(dat[!is.na(RX020)&!is.na(PB150),])   
  
  tr_format <- setDT(subset(dat,select = c("PB030","RX010","FROM","TO","PB150","PB010","DB040","HX100","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")))
  names(tr_format) <- c("pid","age","from","to","gender","year","area","EQUI","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")
  
  tr_format[,from2:=from,]
  tr_format[,to2:=to,]
  
  tr_format[,from:=factor(from2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  tr_format[,to:=factor(to2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  
  tr_format <- unique(droplevels(tr_format[complete.cases(to),,]))
  tr_format[,age_g:=cut(age,seq(49,83,5),labels=paste(seq(50,75,5),seq(54,79,5),sep="-"),include.lowest = F,right=T)]
  
  table(tr_format$age,tr_format$age_g)
  # Health States Distribution by age and sex  
  
  weights_f <- prop.table(xtabs( ~ age_g + from,data = subset(tr_format,gender == 2 )),1)
  weights_f <- as.data.frame(weights_f)
  weights_f[,"year"] <- i
  weights_f[,"sex"] <- "F"
  
  weights_m <- prop.table(xtabs( ~ age_g + from,data = subset(tr_format,gender == 1 )),1)
  weights_m <- as.data.frame(weights_m)
  weights_m[,"year"] <- i
  weights_m[,"sex"] <- "M"

  out <- rbind(weights_f,weights_m)
  
  j <- as.numeric(i)- 6
  dtout[[j]] <- out  
}

out <- rbindlist(dtout)


write.csv(out, file=paste0(dir_out,"/age_states_07_19.csv"))

