

# load shared functions
source("code/00_functions.R")

# load Umats for 07, 15, 16, 19
# first period decomp for 2007 - 2009
load("data/data_out/Umats_07.rda")
Umat_f_07 <- matrices$female
Umat_m_07 <- matrices$male

load("data/data_out/Umats_09.rda")
Umat_f_09 <- matrices$female
Umat_m_09 <- matrices$male

# second period decomp for 2011 - 2015
load("data/data_out/Umats_11.rda")
Umat_f_11 <- matrices$female
Umat_m_11 <- matrices$male

load("data/data_out/Umats_15.rda")
Umat_f_15 <- matrices$female
Umat_m_15 <- matrices$male

# third period decomp for 2016 - 2019
load("data/data_out/Umats_16.rda")
Umat_f_16 <- matrices$female
Umat_m_16 <- matrices$male

load("data/data_out/Umats_19.rda")
Umat_f_19 <- matrices$female
Umat_m_19 <- matrices$male

period_left  <- c("07","11","16")
period_right <- c("09","15","19")

for (s in c("m", "f")) {
  for (p in 1:3) {
    sex <- ifelse(sex == "m", "male", "female")
    
    Umat_left_name  <- paste0("Umats_", period_left[p], ".rda")
    Umat_left       <- local(get(load(file.path("data/data_out/data_out",Umat_left_name))))
    Umat_left       <- Umat_left[[sex]]
    
    Umat_right_name <- paste0("Umats_", period_right[p], ".rda")
    Umat_right      <- local(get(load(file.path("data/data_out/data_out",Umat_right_name))))
    Umat_right      <- Umat_right[[sex]]
    
    P_left <- from_U_to_trans_matrix(Umat_left) |>
      as_tibble() |>
      complete_partial_Ptibble() |>
      select(-HH,-UU)
    
    P_right <- from_U_to_trans_matrix(Umat_right) |>
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
        N = 20
      ) %>%
      vec_to_partial_Ptibble() %>%
      rownames_to_column("age") %>%
      pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
      mutate(
        variant = "prob attrition",
        sex = sex,
        period_left = period_left[p],
        period_right = period_right[p]
      )
    file.name <-
      paste0("cc_", period_left[p], "_", period_right[p], "_", sex, ".rda")
    save(cc_p_attrition, file = file.path("data/data_out/data_out", file.name))
    
  }
}

# decompose again using H:
for (sex in c("m", "f")) {
  for (p in 1:3) {
    sex <- ifelse(s == "m", "male", "female")
    
    Umat_left_name  <- paste0("Umats_", period_left[p], ".rda")
    Umat_left       <- local(get(load(file.path("data/data_out",Umat_left_name))))
    Umat_left       <- Umat_left[[sex]]
    
    Umat_right_name <- paste0("Umats_", period_right[p], ".rda")
    Umat_right      <- local(get(load(file.path("data/data_out",Umat_right_name))))
    Umat_right      <- Umat_right[[sex]]
    
    P_left <- from_U_to_trans_matrix(Umat_left) |>
      as_tibble() |>
      complete_partial_Ptibble() |>
      select(-HH,-UU)
    
    P_right <- from_U_to_trans_matrix(Umat_right) |>
      as_tibble() |>
      complete_partial_Ptibble() |>
      select(-HH,-UU)
    
    cc_p_attrition <-
      horiuchi2(
        func = partial_vec_to_H,
        pars1 = P_left %>%
          partial_Ptibble_to_vec(),
        pars2 = P_right %>%
          partial_Ptibble_to_vec(),
        N = 20
      ) %>%
      vec_to_partial_Ptibble() %>%
      rownames_to_column("age") %>%
      pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
      mutate(
        variant = "prob attrition",
        sex = sex,
        period_left = period_left[p],
        period_right = period_right[p]
      )
    file.name <-
      paste0("cc_", period_left[p], "_", period_right[p], "_", sex, "_H.rda")
    save(cc_p_attrition, file = file.path("data/data_out", file.name))
    
  }
}
