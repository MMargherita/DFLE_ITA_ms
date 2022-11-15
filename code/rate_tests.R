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
    Q2Rtibble() %>% 
    Rtibble2Ptibble(interval = interval) %>% 
    Ptibble2U_closed()
}

Q <-
  Umarkov %>% 
  U2Q(interval = 1) 

Q %>% 
  Q2Rtibble()

  