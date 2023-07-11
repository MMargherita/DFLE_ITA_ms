source("code/functions_decomp_ms_shared.R")



# from trans matrix to conditional_vec
# from the transition matrix, calculate the conditional probabilities
# and store all the probabilities needed as we can use it in the
# decomposition functions from DemoDecomp
# (see order*)
conditionalize_trans_matrix_to_vec <- function(trans_matrix, init){
  
  # transition probabilities from state "H"
  fromH <- trans_matrix[,c("HH","HU")]
  # survival probabilities from state "H"
  HS = rowSums(fromH)
  
  # transition probabilities from state "D"
  fromU <- trans_matrix[,c("UH","UU")]
  # survival probabilities from state "H"
  US = rowSums(fromU)
  
  # construct conditional probabilities by:
  # dividing transition probability to remaing in the state
  # and the survival probability from that state
  H_conditioned <- cbind(HS = HS, HH_given_HS = trans_matrix[,"HH"] / HS)
  U_conditioned <- cbind(US = US, UU_given_US = trans_matrix[,"UU"] / US)
  
  # store it ordered* as:
  # survival probability from state "H"
  # probability of remaining in the "H" state, conditional on surviving
  # survival probability from state "U"
  # probability of remaining in the "U" state, conditional on surviving
  # initial prevalence of "H" state
  c(c(H_conditioned), c(U_conditioned), init[1])
  
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
  colnames(conditional_matrix) <- c("HS","HH_given_HS","US","UU_given_US")
  
  # set an empty transition matrix
  trans_matrix <- matrix(ncol = 4, nrow = n / 4,
                         dimnames = list(NULL, c("HH","HU","UH","UU")))
  
  # now undo the conditional transformations
  HH <- conditional_matrix[,"HH_given_HS"] * conditional_matrix[,"HS"]
  HU <- (1 - conditional_matrix[,"HH_given_HS"]) * conditional_matrix[,"HS"]
  
  UU <- conditional_matrix[,"UU_given_US"] * conditional_matrix[,"US"]
  UH <- (1 - conditional_matrix[,"UU_given_US"]) * conditional_matrix[,"US"]
  
  # store it in the trans_matrix
  trans_matrix <- cbind(HH=HH,HU=HU,UH=UH,UU=UU)
  # and give trans_matrix and init as output in "list" form
  list(trans_matrix = trans_matrix, init = init)
  
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
  colnames(conditional_matrix) <- c("HS","HH_given_HS","US","UU_given_US")
  list(conditional_matrix = conditional_matrix, init = init)
}
