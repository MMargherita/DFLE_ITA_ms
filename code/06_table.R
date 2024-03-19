
# script to replicate the Table 2 (from Supplementary materials)
# containing, for each year and gender, the number of transitions observed among
# the states of interest in IT-SILC data

yrs <- 7:19

myr <- sprintf("%02d", as.numeric(yrs))


dir_in <- file.path("data/dat_trformat/")

dir_out <- file.path("data/data_out/")

mineta <- 50
maxeta <- 79

ages <- mineta:maxeta

age_init <- seq(mineta-10,mineta+10,1)

dtout <- rep(list(NA),length(myr))

for (i in myr){
  
  tr_file <- paste0("/SILC_panel_",i,".rds")
  
  dat <- readRDS(paste0(dir_in,tr_file))
  
  dat <- setDT(dat[!is.na(RX020)&!is.na(PB150),])   
  
  tr_format <- setDT(subset(dat,select = c("PB030","RX010","FROM","TO","PB150","PB010","DB040","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")))
  names(tr_format) <- c("pid","age","from","to","gender","year","area","edu","area3","edu_mid","edu_high","edu_low","Centre","South","North")
  
  tr_format[,from2:=from,]
  tr_format[,to2:=to,]
  
  tr_format[,from:=factor(from2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  tr_format[,to:=factor(to2,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  
  tr_format <- unique(droplevels(tr_format[complete.cases(to) & tr_format$from!="Dead",,]))
  
  nsize <- dim(tr_format)[1]
  
  weights_f <- xtabs(  ~ to + from,data = subset(tr_format,gender == 2 ))
  weights_f <- as.data.frame(weights_f)
  weights_f$N <- sum(weights_f$Freq) 
  weights_f[,"year"] <- i
  weights_f[,"sex"] <- "F"
  
  weights_m <- xtabs( ~ to + from,data = subset(tr_format,gender == 1 ))
  weights_m <- as.data.frame(weights_m)
  weights_m$N <- sum(weights_m$Freq) 
  weights_m[,"year"] <- i
  weights_m[,"sex"] <- "M"
  
  out <- rbind(weights_f,weights_m)
  
  j <- as.numeric(i)- 6
  dtout[[j]] <- out  
}

out <- rbindlist(dtout)

save(out, file=paste0(dir_out,"/descriptive_results_07_19.rda"))