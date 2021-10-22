library(tidyverse)

# call useful functions
source("code/matrix_functions.R")

# Italy - 2012 -----------------------------------------------------------------

tp_mf_ita_12 <- readRDS("data/tp_mf_ita_2012.rds") %>% 
  mutate(age1 = as.numeric(str_sub(from, start = 1, end = 2)),
         FROM = str_sub(from, start = 5, end = 14),
         age2 = as.numeric(age1+1),
         FROM = case_when(FROM == "Disabled" ~ "U",
                          FROM == "Healthy" ~ "H"),
         TO = str_sub(to, start = 5, end = 14),
         TO = case_when(TO == "Disabled" ~ "U",
                        TO == "Healthy" ~ "H"),
         from = FROM,
         to = TO,
         edu = str_sub(geo_edu, start = 9, end = 13),
         edu = as.factor(ifelse(edu=="","tot",edu)),
         rep = "ita") %>% 
  select(gender,edu,rep,age1,from,age2,to,probs)



# Men --------------------------------------------------------------------------

# level of education: tot; gender: Men -----------------------------------------
tp_m_ita_12_60_tot <- tp_mf_ita_12 %>% 
  filter(gender == "Men",
         edu == "tot",
         age1 > 59)


# starting proportions in each state -------------------------------------------
# init <- c(0.75,0.25)  # ?
# names(init) <- c("H","U")


# Matrix calc ------------------------------------------------------------------

# Make the submatrices of U, the transient matrix 
HH <- pi2u(pivec = tp_m_ita_12_60_tot$probs[tp_m_ita_12_60_tot$from == "H" & tp_m_ita_12_60_tot$to == "H"],from = "H", to = "H")
HU <- pi2u(pivec = tp_m_ita_12_60_tot$probs[tp_m_ita_12_60_tot$from == "H" & tp_m_ita_12_60_tot$to == "U"],from = "H", to = "U")
UH <- pi2u(pivec = tp_m_ita_12_60_tot$probs[tp_m_ita_12_60_tot$from == "U" & tp_m_ita_12_60_tot$to == "H"],from = "U", to = "H")
UU <- pi2u(pivec = tp_m_ita_12_60_tot$probs[tp_m_ita_12_60_tot$from == "U" & tp_m_ita_12_60_tot$to == "U"],from = "U", to = "U")

# the transient matrix

# |-------|
# | HH UH |
# | HU UU |
# |-------|

U <- u2U(HH = HH, 
         HU = HU, 
         UH = UH, 
         UU = UU) 

# fundamental matrix
N <- U2N(U)
# it have conditional expected time spent in each state and age
# (conditional on survival and starting state!)
N60 <- N %>% 
  reshape2::melt(varnames = c("to","from"),
                 value.name = "time") %>% 
  mutate(to = as.character(to),
         from = as.character(from)) %>% 
  separate(col = "to", 
           sep = "::",
           into = c("to","age2"),
           convert = TRUE) %>% 
  separate(col = "from", 
           sep = "::",
           into = c("from","age1"),
           convert = TRUE) %>% 
  filter(age1 == 60,
         age2 > age1,
         from != "D",
         to != "D")

# calculate DFLE and DLE from it:
HLE_m_ita_12_60_tot <-
  N60 %>% 
  group_by(from, to) %>% 
  summarize(Ex_cond = sum(time)) #%>%
  # mutate(init = ifelse(from == "H", init[1], init[2]),
  #        Ex = Ex_cond * init) %>%
  # group_by(to) %>%
  # summarize(Ex = sum(Ex))




# Women ------------------------------------------------------------------------


# level of education: tot; gender: Women ---------------------------------------
tp_f_ita_12_60_tot <- tp_mf_ita_12 %>% 
  filter(gender == "Women",
         edu == "tot",
         age1 > 59)


# starting proportions in each state -------------------------------------------
# init <- c(0.7,0.3)  # ?
# names(init) <- c("H","U")


# Matrix calc ------------------------------------------------------------------

# Make the submatrices of U, the transient matrix 
HH <- pi2u(pivec = tp_f_ita_12_60_tot$probs[tp_f_ita_12_60_tot$from == "H" & tp_f_ita_12_60_tot$to == "H"],from = "H", to = "H")
HU <- pi2u(pivec = tp_f_ita_12_60_tot$probs[tp_f_ita_12_60_tot$from == "H" & tp_f_ita_12_60_tot$to == "U"],from = "H", to = "U")
UH <- pi2u(pivec = tp_f_ita_12_60_tot$probs[tp_f_ita_12_60_tot$from == "U" & tp_f_ita_12_60_tot$to == "H"],from = "U", to = "H")
UU <- pi2u(pivec = tp_f_ita_12_60_tot$probs[tp_f_ita_12_60_tot$from == "U" & tp_f_ita_12_60_tot$to == "U"],from = "U", to = "U")


# the transient matrix

# |-------|
# | HH UH |
# | HU UU |
# |-------|

U <- u2U(HH = HH, 
         HU = HU, 
         UH = UH, 
         UU = UU) 

# fundamental matrix
N <- U2N(U)
# it have conditional expected time spent in each state and age
# (conditional on survival and starting state!)
N60 <- N %>% 
  reshape2::melt(varnames = c("to","from"),
                 value.name = "time") %>% 
  mutate(to = as.character(to),
         from = as.character(from)) %>% 
  separate(col = "to", 
           sep = "::",
           into = c("to","age2"),
           convert = TRUE) %>% 
  separate(col = "from", 
           sep = "::",
           into = c("from","age1"),
           convert = TRUE) %>% 
  filter(age1 == 60,
         age2 > age1,
         from != "D",
         to != "D")

# calculate DFLE and DLE from it:
HLE_f_ita_12_60_tot <-
  N60 %>% 
  group_by(from, to) %>% 
  summarize(Ex_cond = sum(time)) # %>%
  # mutate(init = ifelse(from == "H", init[1], init[2]),
  #        Ex = Ex_cond * init) %>%
  # group_by(to) %>%
  # summarize(Ex = sum(Ex))
