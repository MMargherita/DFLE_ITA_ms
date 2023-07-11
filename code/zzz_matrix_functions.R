# Make a single U submatrix from a pi (transfer probs) vector
pi2u <- function(pivec, 
                 from,
                 to,
                 start_age = 60,
                 interval = 1) {
  out           <- cbind(rbind(0, diag(pivec)), 0)
  ages          <- 60:80
  from_names    <- paste(from,ages[-length(ages)],sep="::")
  to_names      <- paste(to,ages[-length(ages)],sep="::")
  dimnames(out) <- list(to_names, from_names)
  out
}


# Compose u blocks into U (2 transient states assumed)
u2U <- function(HH, HU, UH, UU){
  rbind(
    cbind(HH, UH),
    cbind(HU, UU))
}


# convert transient dynamics into outcomes: the fundamental matrix, N
U2N <- function(U, interval = 1) {
  I   <- diag(nrow(U))
  Nsx <- solve(I - U) * interval
  dimnames(Nsx) <- dimnames(U)
  Nsx
}