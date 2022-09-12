
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
  pi_HD <- extract_pi(U,from = "Healthy", to = "Disabled")
  pi_DH <- extract_pi(U,from = "Disabled", to = "Healthy")
  pi_DD <- extract_pi(U,from = "Disabled", to = "Disabled")
  
  trans_matrix <- cbind(HH=pi_HH,HD=pi_HD,DH=pi_DH,DD=pi_DD)
  # trans_matrix has each transition on cols by each age on rows
  return(trans_matrix)
  
}


# from trans matrix to conditional_vec
# from the transition matrix, calculate the conditional probabilities
# and store all the probabilities needed as we can use it in the
# decomposition functions from DemoDecomp
# (see order*)
conditionalize_trans_matrix_to_vec <- function(trans_matrix, init){
  
  # transition probabilities from state "H"
  fromH <- trans_matrix[,c("HH","HD")]
  # survival probabilities from state "H"
  HS = rowSums(fromH)
  
  # transition probabilities from state "D"
  fromD <- trans_matrix[,c("DH","DD")]
  # survival probabilities from state "H"
  DS = rowSums(fromD)
  
  # construct conditional probabilities by:
  # dividing transition probability to remaing in the state
  # and the survival probability from that state
  H_conditioned <- cbind(HS = HS, HH_given_HS = trans_matrix[,"HH"] / HS)
  D_conditioned <- cbind(DS = DS, DD_given_DS = trans_matrix[,"DD"] / DS)
  
  # store it ordered* as:
  # survival probability from state "H"
  # probability of remaining in the "H" state, conditional on surviving
  # survival probability from state "D"
  # probability of remaining in the "D" state, conditional on surviving
  # initial prevalence of "H" state
  c(c(H_conditioned), c(D_conditioned), init[1])
  
}


# function to get the ordered* parameters for the decomposition
# directly from the U matrix
from_U_to_conditional_vec <- function(U,init){
  
  trans_matrix <- from_U_to_trans_matrix(U)
  conditional_vec <- conditionalize_trans_matrix_to_vec(trans_matrix,init)
  conditional_vec
  
}





# Expectancy functions -----

# from conditional_vec to parameters
convert_conditional_to_pars <- function(conditional_vec){
  
  n               <- length(conditional_vec)
  
  # select the initial prevalence from conditional_vec
  init            <- conditional_vec[n]
  init            <- c(init, 1 - init)
  
  # select the probabilities from conditional_vec
  conditional_matrix           <- conditional_vec[-n]
  # get the length of the probabilities vector
  n                            <- n - 1
  # store the different probabilities in cols
  dim(conditional_matrix)      <- c(n / 4, 4)
  # assign col names (N.B. order!)
  colnames(conditional_matrix) <- c("HS","HH_given_HS","DS","DD_given_DS")
  
  # set an empty transition matrix
  trans_matrix <- matrix(ncol = 4, nrow = n / 4,
                         dimnames = list(NULL, c("HH","HD","DH","DD")))
  
  # now undo the conditional transformations
  HH <- conditional_matrix[,"HH_given_HS"] * conditional_matrix[,"HS"]
  HD <- (1 - conditional_matrix[,"HH_given_HS"]) * conditional_matrix[,"HS"]
  
  DD <- conditional_matrix[,"DD_given_DS"] * conditional_matrix[,"DS"]
  DH <- (1 - conditional_matrix[,"DD_given_DS"]) * conditional_matrix[,"DS"]
  
  # store it in the trans_matrix
  trans_matrix <- cbind(HH=HH,HD=HD,DH=DH,DD=DD)
  # and give trans_matrix and init as output in "list" form
  list(trans_matrix = trans_matrix, init = init)
  
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
u2U <- function(HH, HD, DH, DD){
  rbind(
    cbind(HH, DH),
    cbind(HD, DD))
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
Expect <- function(U, age = 50, state = "H", init = c(H = .8,D = .2)){
  
  N <- U2N(U)
  
  state_rows  = grepl(rownames(N), pattern = state)
  origin_cols = grepl(colnames(N), pattern = as.character(age))
  N_block     = N[state_rows, origin_cols]
  
  # beware this assumes the order is HD in columns and rows
  sum(t(N_block) * init) # -0.5
}


# get the expectancy from the trans_matrix
Expect_1 <- function(trans_matrix,age = 50, state = "H",init = c(H = .8,D = .2)){
  
  HH <- pi2u(pivec = trans_matrix[, "HH"], from = "H", to = "H")
  HD <- pi2u(pivec = trans_matrix[, "HD"], from = "H", to = "D")
  DH <- pi2u(pivec = trans_matrix[, "DH"], from = "D", to = "H")
  DD <- pi2u(pivec = trans_matrix[, "DD"], from = "D", to = "D")
  
  U <- u2U(HH=HH,HD=HD,DH=DH,DD=DD)
  Expect(U,init=init,age = age, state = state)
  
}



# get the expectancy from the conditional_vec
Expect_from_cond_vec <- function(conditional_vec,age=50,state="H"){
  
  pars <- convert_conditional_to_pars(conditional_vec)
  Expect_1(pars[["trans_matrix"]],init=pars[["init"]],age = age, state = state)
  
}
# this the function we actually decompose!


# function to organize the results of the decomposition
organize_decomp_results <- function(cont_vec){
  n               <- length(cont_vec)
  
  init            <- cont_vec[n]
  
  conditional_matrix <- cont_vec[-n]
  n               <- n - 1
  dim(conditional_matrix) <- c(n / 4, 4)
  colnames(conditional_matrix) <- c("HS","HH_given_HS","DS","DD_given_DS")
  list(conditional_matrix = conditional_matrix, init = init)
}
