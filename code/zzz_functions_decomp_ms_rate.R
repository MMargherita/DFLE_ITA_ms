# TR: Note, this excludes age 79, and we need to investigate why.
# suspecting -n line (occurs twice) is the culprit


# move away from the probability space
#source("code/functions_decomp_ms_shared.R")

u2U_closed <- function(HH, HU, UH, UU){
  out <- rbind(
    cbind(HH, UH),
    cbind(HU, UU))
  
  out <- cbind(rbind(out, 1 - colSums(out)),0)
  colnames(out)[ncol(out)] <- "D::Inf"
  rownames(out)[nrow(out)] <- "D::Inf"
  out[nrow(out),ncol(out)] <- 1
  out
}


Ptibble2U_closed <- function(Ptibble, interval = 1, start_age = 50){
  n <- nrow(Ptibble) + 1
  HH <- Ptibble %>% 
    pull(HH) %>% 
    pi2u("H","H", start_age = start_age, interval = interval) %>% 
    '['(-1,-n) # hard coded for this example. 
  # Could be dealt with in pi2u() more generally
  HU <- Ptibble %>% 
    pull(HU) %>% 
    pi2u("H","U", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  UU <- Ptibble %>% 
    pull(UU) %>% 
    pi2u("U","U", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  UH <- Ptibble %>% 
    pull(UH) %>% 
    pi2u("U","H", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  
  U <- u2U_closed(HH, HU, UH, UU)
  U
}

U2Q <- function(U, interval = 1){
  Q <-
    U %>% 
    expm::logm() %>% 
    zapsmall() %>% 
    '/'(interval)
  dimnames(Q) <- dimnames(U)
  Q
}

Q2Rtibble <- function(Q){
  Q %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "to") %>% 
    pivot_longer(-1, names_to = "from", values_to = "R") %>% 
    separate(to, 
             into = c("state_to", "age_to"), 
             sep = "::", 
             convert = TRUE) %>% 
    separate(from, 
             into = c("state_from", "age_from"), 
             sep = "::",
             convert = TRUE) %>% 
    filter(age_to == (age_from + 1) |
             is.infinite(age_to),
           !is.infinite(age_from)) %>% 
    select(-age_to) %>% 
    mutate(transition = paste0(state_from, state_to), 
           .keep = "unused") %>% 
    pivot_wider(names_from = transition,
                values_from = R) %>% 
    rename(age = age_from)
}

Ptibble2Rtibble <- function(Ptibble, interval = 1, start_age = 50){
  Ptibble %>% 
    Ptibble2U_closed(interval = interval,
                     start_age = start_age) %>% 
    U2Q() %>% 
    Q2Rtibble() %>% 
    column_to_rownames("age")
}

complete_partial_Rtibble <- function(partial_Rtibble){
  partial_Rtibble <- as.data.frame(partial_Rtibble)
  
  all_from_to <- c("HH","HU","HD","UH","UU","UD")
  Missing <- setdiff(all_from_to, names(partial_Rtibble))
  if (length(Missing) == 0){
    return(partial_Rtibble)
  }
  partial_Rtibble[Missing] <- NA
  
  # ----------------------------- #
  # ensure self-arrows are negative
  partial_Rtibble$HH <- 
    partial_Rtibble %>% 
    pull(HH) %>% 
    '*'(sign(.)) %>% 
    '*'(-1)
  partial_Rtibble$UU <- 
    partial_Rtibble %>% 
    pull(UU) %>% 
    '*'(sign(.)) %>% 
    '*'(-1)
  # ----------------------------- #
  
  
  partial_Rtibble %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("age") %>% 
    pivot_longer(-age,names_to = "from_to", values_to = "p") %>% 
    mutate(from = substr(from_to, 1, 1)) %>% 
    group_by(from, age) %>% 
    mutate(p = if_else(is.na(p), 0 - sum(p, na.rm = TRUE), p)) %>% 
    ungroup() %>% 
    select(-from) %>% 
    pivot_wider(names_from = from_to, values_from = p) %>% 
    column_to_rownames(var="age")
}

Rtibble2Q <- function(Rtibble, interval = 1, start_age = 50){
  n <- nrow(Rtibble) + 1
  HH <- Rtibble %>% 
    pull("HH") %>% 
    pi2u(from = "H", 
         to = "H", 
         start_age = start_age, 
         interval = interval) %>% 
    '['(-1,-n)
  
  HU <- Rtibble %>% 
    pull(HU) %>% 
    pi2u("H","U", 
         start_age = start_age, 
         interval = interval) %>% 
    '['(-1,-n)
  
  UU <- Rtibble %>% 
    pull(UU) %>% 
    pi2u("U","U", 
         start_age = start_age, 
         interval = interval) %>% 
    '['(-1,-n)
  
  UH <- Rtibble %>% 
    pull(UH) %>% 
    pi2u("U","H", 
         start_age = start_age, 
         interval = interval) %>% 
    '['(-1,-n)
  
  Q <- rbind(
    cbind(HH, UH),
    cbind(HU, UU))
  
  Q <- cbind(rbind(Q, -colSums(Q)),0)
  colnames(Q)[ncol(Q)] <- "D::Inf"
  rownames(Q)[nrow(Q)] <- "D::Inf"
  Q
}

Rtibble2Ptibble <- function(Rtibble, 
                            start_age = 50,
                            interval = 1){
  Rtibble %>% 
    Rtibble2Q(start_age = start_age,
              interval = interval) %>% 
    expm::expm() %>% 
    '*'(interval) %>% 
    as.data.frame() %>% 
    rownames_to_column("to") %>% 
    pivot_longer(-1, names_to = "from", values_to = "P") %>% 
    separate(to, 
             into = c("state_to", "age_to"), 
             sep = "::", 
             convert = TRUE) %>% 
    separate(from, 
             into = c("state_from", "age_from"), 
             sep = "::",
             convert = TRUE) %>% 
    filter(age_to == age_from |
             is.infinite(age_to),
           !is.infinite(age_from)) %>% 
    select(-age_to) %>% 
    mutate(transition = paste0(state_from, state_to), 
           .keep = "unused") %>% 
    pivot_wider(names_from = transition,
                values_from = P) %>% 
    rename(age = age_from)
}

partialR_vec_to_ex <- function(vec_with_names, 
                               init = c(.8,.2), 
                               state = "H",
                               age = 50,
                               anti_function = function(x){x}){
  vec_with_names %>% 
    anti_function() %>% 
    # also works for Rates
    vec_to_partial_Ptibble() %>% 
    complete_partial_Rtibble() %>% 
    Rtibble2Ptibble() %>% 
    as.matrix() %>% 
    Expect_1(age = age, init = init, state = state)
}

# load("data/data_out/new_Umats/Umats_lt_new_correct_ita_07.rda")


# Umat1 <- matrices$female
# Umat2 <- matrices$male
# 
# # # get pieces to treat as inputs
# 
# R1 <- from_U_to_trans_matrix(Umat1) %>% 
#       as_tibble() %>% 
#       complete_partial_Ptibble() %>% 
#       Ptibble2Rtibble(start_age = 50,
#                       interval = 1)
# R2 <- from_U_to_trans_matrix(Umat2) %>% 
#   as_tibble() %>% 
#   complete_partial_Ptibble() %>% 
#   Ptibble2Rtibble(start_age = 50,
#                   interval = 1)
# 


# # here contributions assigned to HD, HU, UH, UD only
# cc_r_attrition <-
#   horiuchi2(func = partialR_vec_to_ex, 
#             # define vectors in situ
#             pars1 = R1 %>% 
#               select(-HH, -UU) %>% 
#               partial_Ptibble_to_vec(), 
#             pars2 = R2 %>% 
#               select(-HH, -UU) %>% 
#               partial_Ptibble_to_vec(), 
#             # how many interpolation steps?
#             N = 20) %>% 
#   vec_to_partial_Ptibble() %>% 
#   rownames_to_column("age") %>% 
#   pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>% 
#   mutate(variant = "rate attrition")
# 
# 
# cc_r_attrition %>% 
#   mutate(age = as.integer(age) + 50) %>% 
#   ggplot(aes(x = age, y = cc, fill = from_to))+
#   geom_col() +
#   labs(title = "Men worse than women on mortality, but *much* better on health transitions!")
#   
# cc_r_attrition %>% 
#   count(from_to, wt = cc, name = "contribution")%>% 
#   ggplot(aes(x = from_to, y = contribution, fill = from_to))+
#   geom_col()
# 
# cc_r_attrition$cc %>% sum()

