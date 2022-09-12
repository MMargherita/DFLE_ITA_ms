# function to extract the non-null probabilities from U
# (default: of transition to being "H" to being "H" (staying "H"))
extract_pi <- function(U, from = "H", to = "H"){
  
  # identify the part of the matrix of the from and to of interest
  from_cols   <- grepl(colnames(U), pattern = from)
  to_cols     <-  grepl(rownames(U), pattern = to) 
  U_block     <- U[to_cols, from_cols]
  # take the subdiagonal of the matrix 
  # without the last col (last transition it is always to the abs. one )
  diag(U_block[-1,-ncol(U)])
  
}


# extract the non-null probabilities from U of each possible transition
from_U_to_trans_matrix <- function(U){
  
  pi_HH <- extract_pi(U,from = "Healthy", to = "Healthy")
  pi_HU <- extract_pi(U,from = "Healthy", to = "Disabled")
  pi_UH <- extract_pi(U,from = "Disabled", to = "Healthy")
  pi_UU <- extract_pi(U,from = "Disabled", to = "Disabled")
  
  trans_matrix <- cbind(HH=pi_HH,HU=pi_HU,UH=pi_UH,UU=pi_UU)
  # trans_matrix has each transition on cols by each age on rows
  return(trans_matrix)
  
}



# from the transition probabilities vectors to the transition matrix 
# between two states
pi2u <- function(pivec, from ="H",to = "H",start_age = 50,interval = 1) {
  out           <- cbind(rbind(0, diag(pivec)), 0)
  n             <- length(pivec)
  ages          <- ((0:n) * interval) + start_age
  from_names    <- paste(from, ages, sep = "::")
  to_names      <- paste(to, ages, sep = "::")
  dimnames(out) <- list(to_names, from_names)
  out
}


# bind each transition matrix of the transition between two states
u2U <- function(HH, HU, UH, UU){
  rbind(
    cbind(HH, UH),
    cbind(HU, UU))
}


# from the transition matrix U to the fundamental matrix N
U2N <- function(U, interval = 1) {
  I   <- diag(nrow(U))
  Nsx <- solve(I - U) * interval
  dimnames(Nsx) <- dimnames(U)
  Nsx <- Nsx-(I*interval)/2
  Nsx
}


# get the expectancy from the U matrix
Expect <- function(U, age = 50, state = "H", init = c(H = .8,U = .2)){
  
  N <- U2N(U)
  
  state_rows  = grepl(rownames(N), pattern = state)
  origin_cols = grepl(colnames(N), pattern = as.character(age))
  N_block     = N[state_rows, origin_cols]
  
  # beware this assumes the order is HD in columns and rows
  sum(t(N_block) * init) # -0.5
}


# get the expectancy from the trans_matrix
Expect_1 <- function(trans_matrix,age = 50, state = "H",init = c(H = .8,U = .2)){
  
  HH <- pi2u(pivec = trans_matrix[, "HH"], from = "H", to = "H")
  HU <- pi2u(pivec = trans_matrix[, "HU"], from = "H", to = "U")
  UH <- pi2u(pivec = trans_matrix[, "UH"], from = "U", to = "H")
  UU <- pi2u(pivec = trans_matrix[, "UU"], from = "U", to = "U")
  
  U <- u2U(HH=HH,HU=HU,UH=UH,UU=UU)
  Expect(U, init = init, age = age, state = state)
  
}

# some standards for jumping between named vec and standard Ptibble
# just because it makes life easier
partial_Ptibble_to_vec <- function(partial_Ptibble){
  # trick for reshaping is to name things
  n <- nrow(partial_Ptibble)
  
  outl <-
    partial_Ptibble %>% 
    mutate(age = 0:(n-1)) %>% 
    pivot_longer(-age, names_to = "from_to", values_to = "p") %>% 
    mutate(from_to = paste(age, from_to))  %>% 
    as.list()
  
  out <- outl[["p"]]
  names(out) <- outl[["from_to"]]  
  out
}

# this is premised on having a named vector of the form
# "0 HH" where we have {age, space, fromto}
vec_to_partial_Ptibble <- function(vec_with_names){
  vec_with_names %>% 
    as.data.frame() %>% 
    rownames_to_column("from_to") %>% 
    separate(from_to, into = c("age", "from_to"), sep = " ") %>% 
    pivot_wider(names_from = from_to, values_from = ".") %>% 
    column_to_rownames(var = "age") 
  
}

# and for jumping from a named vec to a given expectancy
partial_vec_to_ex <- function(vec_with_names, age = 50, init = c(.8,.2), state = "H"){
  vec_with_names %>% 
    vec_to_partial_Ptibble() %>% 
    complete_partial_Ptibble() %>% 
    Expect_1(age = age,
             init = init,
             state = state)
}
# now we need function machinery for partial information settings:
complete_partial_Ptibble <- function(partial_Ptibble){
  partial_Ptibble <- as.data.frame(partial_Ptibble)
  
  all_from_to <- c("HH","HU","HD","UH","UU","UD")
  Missing <- setdiff(all_from_to, names(partial_Ptibble))
  
  if (length(Missing) == 0){
    return(partial_Ptibble)
  }
  partial_Ptibble[Missing] <- NA
  
  n <- nrow(partial_Ptibble)
  rownames(partial_Ptibble) <- 0:(n-1)
  
  partial_Ptibble %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("age") %>% 
    pivot_longer(-age,names_to = "from_to", values_to = "p") %>% 
    mutate(from = substr(from_to, 1, 1)) %>% 
    group_by(from, age) %>% 
    mutate(p = if_else(is.na(p), 1 - sum(p, na.rm = TRUE), p)) %>% 
    ungroup() %>% 
    select(-from) %>% 
    pivot_wider(names_from = from_to, values_from = p) %>% 
    column_to_rownames(var="age")
}

# slightly modified horiuchi from DemoDecomp: this one preserves
# names in the internal matrices, which we need in order to reconstitute
# Ptibble on the fly..
horiuchi2 <- function(func, pars1, pars2, N, ...){ 
  d     <- pars2 - pars1
  n     <- length(pars1)
  delta <- d / N
  grad  <- matrix(rep(0.5:(N - 0.5)/N, n), byrow = TRUE, ncol = N,
                  dimnames = list(names(pars1), 1:N))
  x     <- pars1 + d * grad
  cc    <- matrix(0, nrow = n, ncol = N,
                  dimnames = list(names(pars1), 1:N))
  for (j in 1:N) {
    DD <- diag(delta/2)
    for (i in 1:n) {
      cc[i, j] <- 
        func((x[, j] + DD[, i]), ...) - 
        func((x[, j] - DD[, i]), ...)
    }
  }
  rowSums(cc)
}