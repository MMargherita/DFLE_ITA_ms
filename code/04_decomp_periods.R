# dir("data/data_out")


# load shared functions
source("code/00_functions.R")

# set period
period_left  <- c("07","11","16")
period_right <- c("09","15","19")



# decompose H ----
for (s in c("m", "f")) {
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
# 
# 
# 
# females_07 <- local(get(load("data/data_out/cc_07_09_female_H.rda")))
# males_07 <- local(get(load("data/data_out/cc_07_09_male_H.rda")))
# females_11 <- local(get(load("data/data_out/cc_11_15_female_H.rda")))
# males_11 <- local(get(load("data/data_out/cc_11_15_male_H.rda")))
# females_16 <- local(get(load("data/data_out/cc_16_19_female_H.rda")))
# males_16 <- local(get(load("data/data_out/cc_16_19_male_H.rda")))
# 
# 
# cc_p_attrition <- bind_rows(females_07, males_07,
#                             females_11, males_11,
#                             females_16, males_16)
# 
# cc_p_attrition  %>%
#   mutate(period_left =factor(period_left,
#                              levels = c("07","11","16"),
#                              labels = c("2007-2009","2011-2015","2016-2019")),
#          from_to = factor(from_to,
#                           levels = c("HU","UH",
#                                      "HD","UD"),
#                           labels = c("Disability-Free -> With Disability",
#                                      "With Disability -> Disability-Free",
#                                      "Disability-Free -> Death",
#                                      "With Disability -> Death"))) %>%
#   group_by(sex, period_left, from_to) %>%
#   summarise(cc = sum(cc)) %>%
#   ungroup() %>%
#   ggplot(aes(x = period_left, y = cc, fill = from_to))+
#   geom_col()+
#   facet_grid(rows=vars(sex))+
#   scale_fill_manual(values=c("#0a2777","#166e90","#7fcd9f","#deee0c"))+
#   geom_hline(yintercept=0)+
#   labs(title = "Contributions to the change in H of the transitions")+
#   guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
#   theme_bw() +
#   theme(axis.text.x = element_text(size=18,face="bold"),
#         axis.text.y = element_text(size=18,face="bold"),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         legend.text=element_text(size=18,face="bold"),
#         legend.title=element_blank(),
#         plot.title = element_text(size=18, face="bold",hjust = 0.5),
#         text = element_text(size = 18,face="bold"),
#         legend.position = "bottom",
#         panel.grid.minor = element_line(color = "grey90",
#                                         linewidth = 1.2),
#         panel.grid.major = element_line(color = "grey90",
#                                         linewidth = 1.2))






# Decompose DFLE ----
for (s in c("m", "f")) {
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
        sex = sex,
        period_left = period_left[p],
        period_right = period_right[p]
      )
    file.name <-
      paste0("cc_", period_left[p], "_", period_right[p], "_", sex, "_DFLE.rda")
    save(cc_p_attrition, file = file.path("data/data_out", file.name))
    
  }
}






# Decompose DLE ----
for (s in c("m", "f")) {
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
        func = partial_vec_to_ex,
        pars1 = P_left %>%
          partial_Ptibble_to_vec(),
        pars2 = P_right %>%
          partial_Ptibble_to_vec(),
        N = 20,
        state="U"
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
      paste0("cc_", period_left[p], "_", period_right[p], "_", sex, "_DLE.rda")
    save(cc_p_attrition, file = file.path("data/data_out", file.name))
    
  }
}


# females_07 <- local(get(load("data/data_out/cc_07_09_female_DLE.rda")))
# males_07 <- local(get(load("data/data_out/cc_07_09_male_DLE.rda")))
# females_11 <- local(get(load("data/data_out/cc_11_15_female_DLE.rda")))
# males_11 <- local(get(load("data/data_out/cc_11_15_male_DLE.rda")))
# females_16 <- local(get(load("data/data_out/cc_16_19_female_DLE.rda")))
# males_16 <- local(get(load("data/data_out/cc_16_19_male_DLE.rda")))
# 
# 
# cc_p_attrition <- bind_rows(females_07, males_07,
#                             females_11, males_11,
#                             females_16, males_16)
# 
# cc_p_attrition  %>%
#   mutate(period_left =factor(period_left,
#                              levels = c("07","11","16"),
#                              labels = c("2007-2009","2011-2015","2016-2019")),
#          from_to = factor(from_to,
#                           levels = c("HU","UH",
#                                      "HD","UD"),
#                           labels = c("Disability-Free -> With Disability",
#                                      "With Disability -> Disability-Free",
#                                      "Disability-Free -> Death",
#                                      "With Disability -> Death"))) %>%
#   group_by(sex, period_left, from_to) %>%
#   summarise(cc = sum(cc)) %>%
#   ungroup() %>%
#   ggplot(aes(x = period_left, y = cc, fill = from_to))+
#   geom_col()+
#   facet_grid(rows=vars(sex))+
#   scale_fill_manual(values=c("#0a2777","#166e90","#7fcd9f","#deee0c"))+
#   geom_hline(yintercept=0)+
#   labs(title = "Contributions to the change in DLE of the transitions")+
#   guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
#   theme_bw() +
#   theme(axis.text.x = element_text(size=18,face="bold"),
#         axis.text.y = element_text(size=18,face="bold"),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         legend.text=element_text(size=18,face="bold"),
#         legend.title=element_blank(),
#         plot.title = element_text(size=18, face="bold",hjust = 0.5),
#         text = element_text(size = 18,face="bold"),
#         legend.position = "bottom",
#         panel.grid.minor = element_line(color = "grey90",
#                                         linewidth = 1.2),
#         panel.grid.major = element_line(color = "grey90",
#                                         linewidth = 1.2))





# Contrib to changes from LE: ----
# sum contribution of DFLE and DLE







# Plot with all the indicators -------------------------------------------------
# plot in which the col as gender and rows the diff indicators H, DFLE, DLE, LE
# (and the diff period in the x, check labels)



# DLE
females_07_DLE <- local(get(load("data/data_out/cc_07_09_female_DLE.rda")))
males_07_DLE <- local(get(load("data/data_out/cc_07_09_male_DLE.rda")))
females_11_DLE <- local(get(load("data/data_out/cc_11_15_female_DLE.rda")))
males_11_DLE <- local(get(load("data/data_out/cc_11_15_male_DLE.rda")))
females_16_DLE <- local(get(load("data/data_out/cc_16_19_female_DLE.rda")))
males_16_DLE <- local(get(load("data/data_out/cc_16_19_male_DLE.rda")))

cc_p_attrition_DLE <- bind_rows(females_07_DLE, males_07_DLE,
                                females_11_DLE, males_11_DLE,
                                females_16_DLE, males_16_DLE) %>% 
  mutate(index = "DLE")


# DFLE
females_07_DFLE <- local(get(load("data/data_out/cc_07_09_female_DFLE.rda")))
males_07_DFLE <- local(get(load("data/data_out/cc_07_09_male_DFLE.rda")))
females_11_DFLE <- local(get(load("data/data_out/cc_11_15_female_DFLE.rda")))
males_11_DFLE <- local(get(load("data/data_out/cc_11_15_male_DFLE.rda")))
females_16_DFLE <- local(get(load("data/data_out/cc_16_19_female_DFLE.rda")))
males_16_DFLE <- local(get(load("data/data_out/cc_16_19_male_DFLE.rda")))

cc_p_attrition_DFLE <- bind_rows(females_07_DFLE, males_07_DFLE,
                                females_11_DFLE, males_11_DFLE,
                                females_16_DFLE, males_16_DFLE) %>% 
  mutate(index = "DFLE")


# LE
cc_p_attrition_LE <- bind_rows(cc_p_attrition_DFLE, cc_p_attrition_DLE) %>% 
  group_by(age, sex, period_left, from_to) %>% 
  summarise(cc=sum(cc)) %>% 
  ungroup() %>% 
  mutate(index = "LE")
  

rm(females_07_DLE, males_07_DLE,
   females_11_DLE, males_11_DLE,
   females_16_DLE, males_16_DLE,
   females_07_DFLE, males_07_DFLE,
   females_11_DFLE, males_11_DFLE,
   females_16_DFLE, males_16_DFLE)




# H
females_07_H <- local(get(load("data/data_out/cc_07_09_female_H.rda")))
males_07_H <- local(get(load("data/data_out/cc_07_09_male_H.rda")))
females_11_H <- local(get(load("data/data_out/cc_11_15_female_H.rda")))
males_11_H <- local(get(load("data/data_out/cc_11_15_male_H.rda")))
females_16_H <- local(get(load("data/data_out/cc_16_19_female_H.rda")))
males_16_H <- local(get(load("data/data_out/cc_16_19_male_H.rda")))

cc_p_attrition_H <- bind_rows(females_07_H, males_07_H,
                              females_11_H, males_11_H,
                              females_16_H, males_16_H) %>% 
  mutate(index = "H")

rm(females_07_H, males_07_H,
   females_11_H, males_11_H,
   females_16_H, males_16_H)




# all together
cc_p_attrition <- bind_rows(cc_p_attrition_H,
                            cc_p_attrition_DFLE, cc_p_attrition_DLE,
                            cc_p_attrition_LE)

rm(cc_p_attrition_H,
  cc_p_attrition_DFLE, cc_p_attrition_DLE,
  cc_p_attrition_LE)


# PLOT 

cc_p_attrition  %>%
  mutate(period_left =factor(period_left,
                             levels = c("07","11","16"),
                             labels = c("2007-2009","2011-2015","2016-2019")),
         from_to = factor(from_to,
                          levels = c("HU","UH",
                                     "HD","UD"),
                          labels = c("Disability-Free -> With Disability",
                                     "With Disability -> Disability-Free",
                                     "Disability-Free -> Death",
                                     "With Disability -> Death")),
         index = factor(index,
                        levels = c("H", "DFLE", "DLE", "LE"),
                        labels = c("H", "DFLE", "DLE", "LE"))) %>%
  group_by(sex, period_left, from_to, index) %>%
  summarise(cc = sum(cc)) %>%
  ungroup() %>%
  ggplot(aes(x = period_left, y = cc, fill = from_to))+
  geom_col()+
  facet_grid(rows = vars(index),
             cols = vars(sex))+ # scales = "free_y"
  scale_fill_manual(values = c("#0a2777","#166e90","#7fcd9f","#deee0c"))+
  geom_hline(yintercept = 0)+
  labs(title = "Contributions from the changes in the transitions to the indicators")+
  guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=18,face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(size=18, face="bold",hjust = 0.5),
        text = element_text(size = 18,face="bold"),
        legend.position = "bottom",
        panel.grid.minor = element_line(color = "grey90",
                                        linewidth = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        linewidth = 1.2))


