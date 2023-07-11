library(tidyverse)

# load shared functions
source("code/00_functions.R")

# load Umats for 07, 15, 16, 19
# first period decomp for 2007 - 2009
load("data/Umats_07.rda")
Umat_f_07 <- matrices$female
Umat_m_07 <- matrices$male

load("data/Umats_09.rda")
Umat_f_09 <- matrices$female
Umat_m_09 <- matrices$male

# second period decomp for 2011 - 2015
load("data/Umats_11.rda")
Umat_f_11 <- matrices$female
Umat_m_11 <- matrices$male

load("data/Umats_15.rda")
Umat_f_15 <- matrices$female
Umat_m_15 <- matrices$male

# third period decomp for 2016 - 2019
load("data/Umats_16.rda")
Umat_f_16 <- matrices$female
Umat_m_16 <- matrices$male

load("data/Umats_19.rda")
Umat_f_19 <- matrices$female
Umat_m_19 <- matrices$male

period_left  <- c("07","11","16")
period_right <- c("09","15","19")

for (s in c("m", "f")) {
  for (p in 1:3) {
    sex <- ifelse(sex == "m", "male", "female")
    
    Umat_left_name  <- paste0("Umats_", period_left[p], ".rda")
    Umat_left       <- local(get(load(Umat_left_name)))
    Umat_left       <- Umat_left[[sex]]
    
    Umat_right_name <- paste0("Umats_", period_right[p], ".rda")
    Umat_right      <- local(get(load(Umat_right_name)))
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
    save(cc_p_attrition, file = file.path("data/data_out", file.name))
    
  }
}

cc_p_attrition_mf_07_15 %>%
  mutate(age = as.factor(as.integer(age) + 50),
         from_to = factor(from_to,
                          levels = c("HU","UH",
                                     "HD","UD"),
                          labels = c("Disability-Free -> With Disability",
                                     "With Disability -> Disability-Free",
                                     "Disability-Free -> Death",
                                     "With Disability -> Death"))) %>%
  ggplot(aes(x = age, y = cc, fill = from_to))+
  geom_col()+
  facet_grid(cols=vars(sex))+
  scale_x_discrete(breaks = c("50","55","60","65","70","75"))+
  scale_y_continuous(limits = c(-0.07,0.1),
                     breaks = seq(-0.1:0.2,by=0.05))+
  scale_fill_viridis_d(begin = 0.1, end = 0.9,option = "G",
                       direction = -1)+
  geom_hline(yintercept=0)+
  labs(title = "Contributions to the change in DFLE (2007-2015) of the transitions")+
  guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        axis.title.y = element_blank(),
        legend.text=element_text(size=18,face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(size=18, face="bold",hjust = 0.5),
        text = element_text(size = 18,face="bold"),
        legend.position = "bottom",
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))
ggsave(filename = "plot/decomp_p_age_MF_07_15.png",
       device = "png", dpi = 300,
       width = 15, height = 6)
ggsave(filename = "plot/decomp_p_age_MF_07_15.pdf",
       device = "pdf", dpi = 300,
       width = 12, height = 6)


# PLOT ATTR 16-19 ----
# rm(list=ls())

# load("code/cc_p_attrition_m_16_19.RDa")
gap_m <- round(cc_p_attrition_m_16_19$cc %>% sum(),2) # 0.93 MEN
# load("code/cc_p_attrition_f_16_19.RDa")
gap_f <- round(cc_p_attrition_f_16_19$cc %>% sum(),2) # +1.58 WOMEN

# merge men and women
cc_p_attrition_m_16_19 <- cc_p_attrition_m_16_19 %>% 
  mutate(sex="Men")
cc_p_attrition_f_16_19 <- cc_p_attrition_f_16_19 %>% 
  mutate(sex="Women")
cc_p_attrition_mf_16_19 <- rbind(cc_p_attrition_m_16_19,
                                 cc_p_attrition_f_16_19)

cc_p_attrition_mf_16_19 %>%
  mutate(age = as.factor(as.integer(age) + 50),
         from_to = factor(from_to,
                          levels = c("HU","UH",
                                     "HD","UD"),
                          labels = c("Disability-Free -> With Disability",
                                     "With Disability -> Disability-Free",
                                     "Disability-Free -> Death",
                                     "With Disability -> Death"))) %>%
  ggplot(aes(x = age, y = cc, fill = from_to))+
  geom_col()+
  facet_grid(cols=vars(sex))+
  scale_x_discrete(breaks = c("50","55","60","65","70","75"))+
  scale_y_continuous(limits = c(-0.07,0.1),
                     breaks = seq(-0.1:0.2,by=0.05))+
  scale_fill_viridis_d(begin = 0.1, end = 0.9,option = "G",
                       direction = -1)+
  geom_hline(yintercept=0)+
  labs(title = "Contributions to the change in DFLE (2016-2019) of the transitions")+
  guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        axis.title.y = element_blank(),
        legend.text=element_text(size=18,face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(size=18, face="bold",hjust = 0.5),
        text = element_text(size = 18,face="bold"),
        legend.position = "bottom",
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))
ggsave(filename = "plot/decomp_p_age_MF_16_19.png",
       device = "png", dpi = 300,
       width = 15, height = 6)
ggsave(filename = "plot/decomp_p_age_MF_16_19.pdf",
       device = "pdf", dpi = 300,
       width = 12, height = 6)



# PLOT ATTR 07-19 ----
# rm(list=ls())

# load("code/cc_p_attrition_m_07_19.RDa")
gap_m <- round(cc_p_attrition_m_07_19$cc %>% sum(),2) # 2.95 MEN
# load("code/cc_p_attrition_f_07_19.RDa")
gap_f <- round(cc_p_attrition_f_07_19$cc %>% sum(),2) # 4.43 WOMEN

# merge men and women
cc_p_attrition_m_07_19 <- cc_p_attrition_m_07_19 %>% 
  mutate(sex="Men")
cc_p_attrition_f_07_19 <- cc_p_attrition_f_07_19 %>% 
  mutate(sex="Women")
cc_p_attrition_mf_07_19 <- rbind(cc_p_attrition_m_07_19,
                                 cc_p_attrition_f_07_19)

cc_p_attrition_mf_07_19 %>%
  mutate(age = as.factor(as.integer(age) + 50),
         from_to = factor(from_to,
                          levels = c("HU","UH",
                                     "HD","UD"),
                          labels = c("Disability-Free -> With Disability",
                                     "With Disability -> Disability-Free",
                                     "Disability-Free -> Death",
                                     "With Disability -> Death"))) %>%
  ggplot(aes(x = age, y = cc, fill = from_to))+
  geom_col()+
  facet_grid(cols=vars(sex))+
  scale_x_discrete(breaks = c("50","55","60","65","70","75"))+
  scale_y_continuous(limits = c(-0.07,0.2),
                     breaks = seq(-0.1:0.2,by=0.05))+
  scale_fill_viridis_d(begin = 0.1, end = 0.9,option = "G",
                       direction = -1)+
  geom_hline(yintercept=0)+
  labs(title = "Contributions to the change in DFLE (2007-2019) of the transitions")+
  guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        axis.title.y = element_blank(),
        legend.text=element_text(size=18,face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(size=18, face="bold",hjust = 0.5),
        text = element_text(size = 18,face="bold"),
        legend.position = "bottom",
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))
ggsave(filename = "plot/decomp_p_age_MF_07_19.png",
       device = "png", dpi = 300,
       width = 15, height = 6)
ggsave(filename = "plot/decomp_p_age_MF_07_19.pdf",
       device = "pdf", dpi = 300,
       width = 12, height = 6)




# RATES ------

source("code/functions_decomp_ms_rate.R")

# Female 07-15 -----
R07_f_07 <- from_U_to_trans_matrix(Umat_f_07) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)
R15_f_15 <- from_U_to_trans_matrix(Umat_f_15) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)


# here contributions assigned to HD, HU, UH, UD only
cc_r_attrition_f_07_15 <-
  horiuchi2(func = partialR_vec_to_ex,
            pars1 = R07_f_07 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            pars2 = R15_f_15 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            N = 20) %>%
  vec_to_partial_Ptibble() %>%
  rownames_to_column("age") %>%
  pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
  mutate(variant = "rate attrition")



save(cc_r_attrition_f_07_15,
     file="data/cc_r_attrition_f_07_15.RDa")



# Female 16-19 -----
R_f_16 <- from_U_to_trans_matrix(Umat_f_16) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)
R_f_19 <- from_U_to_trans_matrix(Umat_f_19) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)


# here contributions assigned to HD, HU, UH, UD only
cc_r_attrition_f_16_19 <-
  horiuchi2(func = partialR_vec_to_ex,
            pars1 = R_f_16 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            pars2 = R_f_19 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            N = 20) %>%
  vec_to_partial_Ptibble() %>%
  rownames_to_column("age") %>%
  pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
  mutate(variant = "rate attrition")


save(cc_r_attrition_f_16_19,
     file="data/cc_r_attrition_f_16_19.RDa")


# Female 07-19 -----
R_f_07 <- from_U_to_trans_matrix(Umat_f_07) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)
R_f_19 <- from_U_to_trans_matrix(Umat_f_19) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)


# here contributions assigned to HD, HU, UH, UD only
cc_r_attrition_f_07_19 <-
  horiuchi2(func = partialR_vec_to_ex,
            pars1 = R_f_07 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            pars2 = R_f_19 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            N = 20) %>%
  vec_to_partial_Ptibble() %>%
  rownames_to_column("age") %>%
  pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
  mutate(variant = "rate attrition")


save(cc_r_attrition_f_07_19,
     file="data/cc_r_attrition_f_07_19.RDa")



# Male 07-15 -----

# get pieces to treat as inputs

R_m_07 <- from_U_to_trans_matrix(Umat_m_07) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)
R_m_15 <- from_U_to_trans_matrix(Umat_m_15) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)


# here contributions assigned to HD, HU, UH, UD only
cc_r_attrition_m_07_15 <-
  horiuchi2(func = partialR_vec_to_ex,
            pars1 = R_m_07 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            pars2 = R_m_15 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            N = 20) %>%
  vec_to_partial_Ptibble() %>%
  rownames_to_column("age") %>%
  pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
  mutate(variant = "rate attrition")



save(cc_r_attrition_m_07_15,
     file="data/cc_r_attrition_m_07_15.RDa")



# Male 16-19 -----
R_m_16 <- from_U_to_trans_matrix(Umat_m_16) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)
R_m_19 <- from_U_to_trans_matrix(Umat_m_19) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)


# here contributions assigned to HD, HU, UH, UD only
cc_r_attrition_m_16_19 <-
  horiuchi2(func = partialR_vec_to_ex,
            pars1 = R_m_16 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            pars2 = R_m_19 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            N = 20) %>%
  vec_to_partial_Ptibble() %>%
  rownames_to_column("age") %>%
  pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
  mutate(variant = "rate attrition")


save(cc_r_attrition_m_16_19,file="data/cc_r_attrition_m_16_19.RDa")



# Male 07-19 -----
R_m_07 <- from_U_to_trans_matrix(Umat_m_07) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)
R_m_19 <- from_U_to_trans_matrix(Umat_m_19) %>%
  as_tibble() %>%
  complete_partial_Ptibble() %>%
  Ptibble2Rtibble(start_age = 50,
                  interval = 1)


# here contributions assigned to HD, HU, UH, UD only
cc_r_attrition_m_07_19 <-
  horiuchi2(func = partialR_vec_to_ex,
            pars1 = R_m_07 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            pars2 = R_m_19 %>%
              select(-HH, -UU) %>%
              partial_Ptibble_to_vec(),
            N = 20) %>%
  vec_to_partial_Ptibble() %>%
  rownames_to_column("age") %>%
  pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>%
  mutate(variant = "rate attrition")


save(cc_r_attrition_m_07_19,file="data/cc_r_attrition_m_07_19.RDa")






# PLOT RATES 07-15--------
# rm(list=ls())

# load("code/cc_r_attrition_m_07_15.RDa")
gap_m <- round(cc_r_attrition_m_07_15$cc %>% sum(),2) # -0.58 MEN
# load("code/cc_r_attrition_f_07_15.RDa")
gap_f <- round(cc_r_attrition_f_07_15$cc %>% sum(),2) # +0.59 WOMEN

# merge men and women
cc_r_attrition_m_07_15 <- cc_r_attrition_m_07_15 %>% 
  mutate(sex="Men")
cc_r_attrition_f_07_15 <- cc_r_attrition_f_07_15 %>% 
  mutate(sex="Women")
cc_r_attrition_mf_07_15 <- rbind(cc_r_attrition_m_07_15,
                                 cc_r_attrition_f_07_15)

cc_r_attrition_mf_07_15 %>%
  mutate(age = as.factor(as.integer(age) + 50),
         from_to = factor(from_to,
                          levels = c("HU","UH",
                                     "HD","UD"),
                          labels = c("Disability-Free -> With Disability",
                                     "With Disability -> Disability-Free",
                                     "Disability-Free -> Death",
                                     "With Disability -> Death"))) %>%
  ggplot(aes(x = age, y = cc, fill = from_to))+
  geom_col()+
  facet_grid(cols=vars(sex))+
  scale_x_discrete(breaks = c("50","55","60","65","70","75"))+
  scale_y_continuous(limits = c(-0.11,0.1),
                     breaks = seq(-0.1:0.2,by=0.05))+
  scale_fill_viridis_d(begin = 0.1, end = 0.9,option = "G",
                       direction = -1)+
  geom_hline(yintercept=0)+
  labs(title = "Contributions to the change in DFLE (2007-2015) of the transitions")+
  guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        axis.title.y = element_blank(),
        legend.text=element_text(size=18,face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(size=18, face="bold",hjust = 0.5),
        text = element_text(size = 18,face="bold"),
        legend.position = "bottom",
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))
ggsave(filename = "plot/decomp_r_age_MF_07_15.png",
       device = "png", dpi = 300,
       width = 15, height = 6)
ggsave(filename = "plot/decomp_r_age_MF_07_15.pdf",
       device = "pdf", dpi = 300,
       width = 12, height = 6)



# PLOT RATES 16_19 ------
# load("code/cc_r_attrition_m_16_19.RDa")
gap_m_16_19 <- round(cc_r_attrition_m_16_19$cc %>% sum(),2) # +0.93 MEN
# load("code/cc_r_attrition_f_16_19.RDa")
gap_f_16_19 <- round(cc_r_attrition_f_16_19$cc %>% sum(),2) # +1.58 WOMEN

# merge men and women
cc_r_attrition_m_16_19 <- cc_r_attrition_m_16_19 %>% 
  mutate(sex="Men")
cc_r_attrition_f_16_19 <- cc_r_attrition_f_16_19 %>% 
  mutate(sex="Women")
cc_r_attrition_mf_16_19 <- rbind(cc_r_attrition_m_16_19,
                                 cc_r_attrition_f_16_19)

cc_r_attrition_mf_16_19 %>%
  mutate(age = as.factor(as.integer(age) + 50),
         from_to = factor(from_to,
                          levels = c("HU","UH",
                                     "HD","UD"),
                          labels = c("Disability-Free -> With Disability",
                                     "With Disability -> Disability-Free",
                                     "Disability-Free -> Death",
                                     "With Disability -> Death"))) %>%
  ggplot(aes(x = age, y = cc, fill = from_to))+
  geom_col()+
  facet_grid(cols=vars(sex))+
  scale_x_discrete(breaks = c("50","55","60","65","70","75"))+
  scale_y_continuous(limits = c(-0.11,0.12),
                     breaks = seq(-0.1:0.2,by=0.05))+
  scale_fill_viridis_d(begin = 0.1, end = 0.9,option = "G",
                       direction = -1)+
  geom_hline(yintercept=0)+
  labs(title = "Contributions to the change in DFLE (2016-2019) of the transitions")+
  guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        axis.title.y = element_blank(),
        legend.text=element_text(size=18,face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(size=18, face="bold",hjust = 0.5),
        text = element_text(size = 18,face="bold"),
        legend.position = "bottom",
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))
ggsave(filename = "plot/decomp_r_age_MF_16_19.png",
       device = "png", dpi = 300,
       width = 19, height = 6)
ggsave(filename = "plot/decomp_r_age_MF_16_19.pdf",
       device = "pdf", dpi = 300,
       width = 12, height = 6)




# PLOT RATES 07_19 ------
# load("code/cc_r_attrition_m_07_19.RDa")
gap_m_07_19 <- round(cc_r_attrition_m_07_19$cc %>% sum(),2) # +2.95 MEN
# load("code/cc_r_attrition_f_07_19.RDa")
gap_f_07_19 <- round(cc_r_attrition_f_07_19$cc %>% sum(),2) # +4.43 WOMEN

# merge men and women
cc_r_attrition_m_07_19 <- cc_r_attrition_m_07_19 %>% 
  mutate(sex="Men")
cc_r_attrition_f_07_19 <- cc_r_attrition_f_07_19 %>% 
  mutate(sex="Women")
cc_r_attrition_mf_07_19 <- rbind(cc_r_attrition_m_07_19,
                                 cc_r_attrition_f_07_19)

cc_r_attrition_mf_07_19 %>%
  mutate(age = as.factor(as.integer(age) + 50),
         from_to = factor(from_to,
                          levels = c("HU","UH",
                                     "HD","UD"),
                          labels = c("Disability-Free -> With Disability",
                                     "With Disability -> Disability-Free",
                                     "Disability-Free -> Death",
                                     "With Disability -> Death"))) %>% 
  ggplot(aes(x = age, y = cc, fill = from_to))+
  geom_col()+
  facet_grid(cols=vars(sex))+
  scale_x_discrete(breaks = c("50","55","60","65","70","75"))+
  scale_y_continuous(limits = c(-0.15,0.27),
                     breaks = seq(-0.1:0.2,by=0.05))+
  scale_fill_viridis_d(begin = 0.1, end = 0.9,option = "G",
                       direction = -1)+
  geom_hline(yintercept=0)+
  labs(title = "Contributions to the change in DFLE (2007-2019) of the transitions")+
  guides(fill=guide_legend(nrow=2,byrow=T,reverse=F))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        axis.title.y = element_blank(),
        legend.text=element_text(size=18,face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(size=18, face="bold",hjust = 0.5),
        text = element_text(size = 18,face="bold"),
        legend.position = "bottom",
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))
ggsave(filename = "plot/decomp_r_age_MF_07_19.png",
       device = "png", dpi = 300,
       width = 19, height = 6)
ggsave(filename = "plot/decomp_r_age_MF_07_19.pdf",
       device = "pdf", dpi = 300,
       width = 12, height = 6)

