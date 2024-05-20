# dir("data/data_out")


# load shared functions
source("code/00_functions.R")

# set period
period_left  <- c("07","11","16")
period_right <- c("09","15","19")



# s = "men"
# p = 1
# iter = 1

iter_list <- list()

# Decompose DFLE ----
for (s in c("men", "women")) {
  for (p in 1:3) {
    Umat_left_name  <- paste0("sex_matrix_lt_2023_ita_", period_left[p], ".rds")
    Umat_left       <- readRDS(paste0("data/data_out/",Umat_left_name))
    Umat_right_name <- paste0("sex_matrix_lt_2023_ita_", period_right[p], ".rds")
    Umat_right      <- readRDS(paste0("data/data_out/",Umat_right_name))
    for (iter in 1:1000) {

  
    Umat_lefti       <- Umat_left %>% filter(sex == s & i == iter) %>% select(-c(sex,i))
    Umat_lefti       <- Umat_lefti %>% column_to_rownames(var="to")
    
    Umat_righti      <- Umat_right %>% filter(sex == s & i == iter) %>% select(-c(sex,i))
    Umat_righti      <- Umat_righti %>% column_to_rownames(var="to")
    
    P_left <- from_U_to_trans_matrix(Umat_lefti) |>
      as_tibble() |>
      complete_partial_Ptibble() |>
      select(-HH,-UU)
    
    P_right <- from_U_to_trans_matrix(Umat_righti) |>
      as_tibble() |>
      complete_partial_Ptibble() |>
      select(-HH,-UU)
  
    cc_p_attrition <-
      horiuchi2(
        func = partial_vec_to_ex,
        pars1 = P_left %>%
          partial_Ptibble_to_vec(),
        pars2 = P_right %>%
          partial_Ptibble_to_vec(),
        N = 20,
        state="H"
      ) %>%
      vec_to_partial_Ptibble() %>%
      rownames_to_column("age") %>%
      pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
      mutate(
        variant = "prob attrition",
        iter = iter,
        s = s,
        period_left = period_left[p],
        period_right = period_right[p]
      )
   
   # save iteration
    iter.name <-
      paste0(period_left[p], "_", period_right[p], "_", s, "_", iter)

    iter_list[[iter.name]] <- cc_p_attrition
    }
  }
}

cc_out <- bind_rows(iter_list)
write_csv(cc_out, file = "cc_iter.csv")
