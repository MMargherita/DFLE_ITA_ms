
#
rm(list=ls())
libraries <- list("data.table","VGAM","tidyverse")
lapply(libraries,require, character=T)

# Estimation of transition probabilities between health, disability and death states
# using Silc data 2012-15, survival correction matching survival probs with italian 
# life tables 2014. 
#-----------------------------------------------------------------


yrs <- 7:19

all_yrs <- sprintf("%02d", as.numeric(yrs))


dir_in <- file.path("dat_trformat/")

dir_out <- file.path("./out/")

men <- data.frame("H"=rep(NA,length(myr)),"D"=rep(NA,length(myr)),"year"=rep(NA,length(myr)))
women <- men

for (i in all_yrs){

tr_file <- paste0("/SILC_panel_",i,".rds")

dat <- readRDS(paste0(dir_in,tr_file))

mineta <- 50
maxeta <- 79

age_init <- seq(mineta-10,mineta+10,1)

dat <- setDT(dat[!is.na(RX020)&!is.na(PB150),])   

tr_format <- setDT(subset(dat,select = c("PB030","RX010","FROM","TO","PB150","PB010","DB040","HX100","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")))
names(tr_format) <- c("pid","age","from","to","gender","year","area","EQUI","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")

tr_format[,from2:=from,]
tr_format[,to2:=to,]

tr_format[,from:=factor(from2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
tr_format[,to:=factor(to2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]

tr_format <- unique(droplevels(tr_format[complete.cases(to),,]))

setkey(tr_format,"pid")
tr_id <- droplevels(tr_format[J(unique(pid)),mult="first"]) 

#Init 
weights_f <- prop.table(xtabs( ~ from,data = subset(tr_id, age %in% age_init & gender == 2 )))
weights_f <- as.numeric(weights_f)

weights_m <- prop.table(xtabs( ~ from,data = subset(tr_id, age %in% age_init & gender == 1 )))
weights_m <- as.numeric(weights_m)


rr <- as.numeric(i)-6
men[rr,1:2] <- weights_m 
women[rr,1:2] <- weights_f

men[rr,"year"] <-i
women[rr,"year"] <-i

}

men$sex <- "men"
women$sex <- "women"

out <- rbind(men,women)


write.csv(out, file=paste0(dir_out,"/init_07_19.csv"))

# Initial conditions used in decompositions are the stationary
# ones based on the assumption that the transition probabilities
# in age 50 were constant in ages below 50.

# source("code/00_functions.R")
# 
# for (yr in all_yrs){
#   
#   
#   # get age 50 transitions
#   
#   # calc init
#   init_constant()
#   
# }







