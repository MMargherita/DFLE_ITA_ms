rm(list=ls())
yrs <- 7:19

myr <- sprintf("%02d", as.numeric(yrs)-1)

# read year-by year
for (i in myr){
  lt_female <- paste("data/life_tables/Italia",i,"Femmine.csv",sep="_")
  
  lt_male <- paste("data/life_tables/Italia",i,"Maschi.csv",sep="_")
  col_nm  <- c("age", "lx", "dx", "qx", "Lx", "px", "ex","x")
  
  female  <- read.csv2(lt_female, 
                       skip = 1, 
                       header = TRUE, 
                       col.names = col_nm)
  male    <- read.csv2(lt_male,
                       skip = 1,
                       header = TRUE,
                       col.names = col_nm)
  
  nc      <- seq_len(ncol(female) - 1)
  
  female  <- female[, nc]
  male    <- male[, nc]
  
  write.csv(female, file = paste0(paste("data/life_tables/Italia",i,"Women.csv",sep="_")))
  
  write.csv(male, file = paste0(paste("data/life_tables/Italia",i,"Men.csv",sep="_")))
}


