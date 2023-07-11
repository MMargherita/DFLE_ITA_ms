source("code/functions_decomp_ms_shared.R")
source("code/functions_decomp_ms_rate.R")
library(tidyverse)
load("data/data_out/new_Umats/Umats_lt_new_correct_ita_07.rda")

U <- matrices[[1]]
Ptibble <- from_U_to_trans_matrix(U) %>% 
  as_tibble()

Umarkov <- Ptibble2U_closed(Ptibble) %>%
  t()

Q <- U2Q(Umarkov, interval = 1)

Q2U <- function(Q, interval = 1){
  Q %>% 
    t() %>% 
    Q2Rtibble() %>% 
    Rtibble2Ptibble(interval = interval) %>% 
    Ptibble2U_closed()
}
U2Ptibble <- function(U){
  pi_HH <- extract_pi(U,from = "H", to = "H")
  pi_HU <- extract_pi(U,from = "H", to = "U")
  pi_UH <- extract_pi(U,from = "U", to = "H")
  pi_UU <- extract_pi(U,from = "U", to = "U")
  Ptibble <- tibble(HH=pi_HH,
                    HU=pi_HU,
                    UH=pi_UH,
                    UU=pi_UU)
  Ptibble
}
Q <-
  Umarkov %>% 
  U2Q(interval = 1) 

U_test <- 
  Q %>% 
  Q2U()

Umarkov %>% 
  t() %>% 
  U2Ptibble() 
U2Ptibble
extract_pi
Umarkov %>% 
  t() %>% head()

HHm <- Umarkov["H::50","H::51"]
HUm <- Umarkov["H::50","U::51"]

UUm <- Umarkov["U::50","U::51"]
UHm <- Umarkov["U::50","H::51"]
Umini <- matrix(c(HHm,HUm,UHm,UUm),2,byrow=TRUE)
Umini <- rbind(cbind(Umini,1 - rowSums(Umini)),0)
Umini[3,3] <- 1
library(expm)
Umini %>% logm() %>% expm()
Umini

Umini2 <- matrix(c(HHm,HUm,UHm+.01,UUm-.01),2,byrow=TRUE)
Umini2 <- rbind(cbind(Umini2,1 - rowSums(Umini2)),0)
Umini2[3,3] <- 1

Q1 <- logm(Umini)
Q2 <- logm(Umini2)


UminiH <- Umini
UminiH[,2] <- c(.3,.3,.4)
logm(UminiH)
