source("code/00_functions.R")

# Plot of expectancy trend estimates with confidence intervals -----------------
all_yrs <- sprintf("%02d",07:19)


expect_ci <- NA


for (yr in all_yrs){
  #load expectancies
  expect_yr <- read.csv(paste0("data/data_out/expectancies_",yr,".csv")) %>% 
    mutate(year=yr)
  names(expect_yr) <- c("LE_M","HLE_M","DLE_M","LE_F","HLE_F","DLE_F","year")
  expect_yr <- expect_yr %>%
    pivot_longer(!year, names_to = "ind", values_to = "est") %>% 
  
  #load expectancies's confidence intervals (lower and upper estimates)
  expect_ci_yr <- read.csv(paste0("data/data_out/expectancies_CI_",yr,".csv")) %>% 
    mutate(year=yr)
  names(expect_ci_yr) <- c("LE_M","HLE_M","DLE_M","LE_F","HLE_F","DLE_F","year")
  
  expect_ci_yr_low_est <- expect_ci_yr[1,-1]
  expect_ci_yr_low_est <- expect_ci_yr_low_est %>% 
    pivot_longer(!year, names_to = "ind", values_to = "low_est")
  
  expect_ci_yr_up_est <- expect_ci_yr[1,-1]
  expect_ci_yr_up_est <- expect_ci_yr_up_est %>% 
    pivot_longer(!year, names_to = "ind", values_to = "up_est")
  
  
  #join the point estimates and low/up ones
  expect_ci_yr <- full_join(expect_yr,expect_ci_yr_low_est,expect_ci_yr_up_est,
                            by="year") %>% 
    mutate(gender=ifelse(ind=="LE_F"|ind=="DFLE_F"|ind=="DLE_F"|ind=="H_F",
                                                           "W","M"),
           ind=str_sub(ind, end=-3),
           est=round(est,2),
           year=year+2000) %>% 
  
  #bind each year
  expect_ci <- rbind(expect_ci,expect_ci_yr)
  
}

# Figure 2
# plot of DFLE and DLE trends
expect_ci %>% 
  filter(ind %in% c("DFLE","DLE")) %>% 
  ggplot(aes(x=year,y=est,
             color=gender))+
  geom_line(size=2.5) +
  geom_point(size=1)+
  geom_errorbar(aes(ymin=low_est, ymax=up_est),
                width = 0, size=2.5) +
  facet_grid(cols = vars(ind))+
  theme_bw() +
  theme(text = element_text(size=20))+
  scale_color_viridis_d(begin = 0.2, end = 0.8, option = "D",
                        guide=guide_legend(reverse=TRUE)) +
  scale_y_continuous(limits = c(0,25),
                     breaks = seq(0,25,by=5))+
  scale_x_continuous(limits = c(2007,2019),
                     breaks = seq(2007,2019,by=2))+
  labs(x ="year", y = "DFLE")+
  ggtitle("Disability-free life expectancy at age 50")+
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        legend.text=element_text(size=18,face="bold"),
        text = element_text(size = 18,face="bold"),
        legend.position = c(0.95, 0.9),
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))

# to be noted that, because of data and comparability issues
# (further elaborated in the limitations section of the paper)
# the estimates of the indicators between 2009 and 2011 and
# between 2015 and 2016 are not reliable and, for this reason are then 
# replaced with dotted lines in the figure included in the paper



# Figure 3
# Plot of Decomposition --------------------------------------------------------
period_left  <- c("07","11","16")
period_right <- c("09","15","19")

decomp_list <- list()
for (s in c("m", "f")) {
  for (p in 1:3) {
    file.name <-
      paste0("cc_", period_left[p], "_", period_right[p], "_", sex, ".rda")
    dat_in <- local(get(load(file.path("data/data_out", file.name)))) |> 
      mutate(gender = ifelse(sex == "male","Men","Women"),
             period = paste(period_left,period_right,sep="_"))
    decomp_list[[paste0(s,p)]] <- dat_in
  }
}

cc_p_attrition_mf_per <- bind_rows(decomp_list)

cc_p_attrition_mf_per  %>%
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
  facet_grid(cols=vars(period),
             rows=vars(gender))+
  scale_x_discrete(breaks = c("50","55","60","65","70","75"))+
  scale_y_continuous(limits = c(-0.07,0.17),
                     breaks = seq(-0.05:0.15, by=0.05))+
  scale_fill_manual(values=c("#0a2777","#166e90","#7fcd9f","#deee0c"))+
  geom_hline(yintercept=0)+
  labs(title = "Contributions to the change in DFLE of the transitions")+
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







# Figure 4 (from Supplementary materials)
# Plot of transitions compared to qx IstAT -------------------------------------

# example of comparing the probabilities using trasitions of2013 
load(file="data/data_out/Umats_13.rda")
Umat_m_13 <- matrices$male
Umat_f_13 <- matrices$female


source("code/00_functions.R")

tp_fun <- function(Umat_m,Umat_f){
  
  tp_m <- as.data.frame(from_U_to_trans_matrix(Umat_m)) %>% 
    rename(DF_DF = HH,
           DF_WD = HU,
           WD_WD = UU,
           WD_DF = UH) %>% 
    mutate(age1 = seq(50,78),
           age2 = age1+1,
           DF_D = 1-(DF_DF+DF_WD),
           WD_D = 1-(WD_WD+WD_DF)) %>% 
    select(age1, age2,
           DF_DF,DF_WD,
           WD_WD, WD_DF,
           DF_D,WD_D) %>% 
    mutate(gender="Men")
  
  
  tp_f <- as.data.frame(from_U_to_trans_matrix(Umat_f)) %>% 
    rename(DF_DF = HH,
           DF_WD = HU,
           WD_WD = UU,
           WD_DF = UH) %>% 
    mutate(age1 = seq(50,78),
           age2 = age1+1,
           DF_D = 1-(DF_DF+DF_WD),
           WD_D = 1-(WD_WD+WD_DF)) %>% 
    select(age1, age2,
           DF_DF,DF_WD,
           WD_WD, WD_DF,
           DF_D,WD_D)%>% 
    mutate(gender="Women")
  
  tp_mf <- rbind(tp_f,tp_m)
}



tp_mf_13 <- tp_fun(Umat_m_13,Umat_f_13) %>% 
  mutate(year=2013)




# data Istat
data_istat04_19 <- read_csv("data/data_istat_04-19.csv", 
                            col_types = cols(`Flag Codes` = col_skip(), 
                                             Flags = col_skip()))[c(3,6,7,10,11)]
names(data_istat04_19) <- c("fun","sex","age","year","value")

# only 2012
data_istat12 <- data_istat04_19 %>% 
  mutate(age1 = as.numeric(str_sub(age, start = 2, end = 3)),
         age2 = age1+1,
         from = "T",
         to = "D",
         gender = as.factor(case_when(sex == "femmine" ~ "Women",
                                      sex == "maschi" ~ "Men")),
         fun = case_when(fun == "SURVIVORS" ~ "lx",
                         fun == "DEATHS" ~ "dx",
                         fun == "PROBDEATH" ~ "qx_1000",
                         fun == "PYLIVED" ~ "Lx",
                         fun == "LIFEXP" ~ "ex")) %>% 
  pivot_wider(names_from = fun, values_from = value) %>% 
  mutate(qx = qx_1000/1000,
         probability=qx,
         year=as.factor(year),
         type="ISTAT",
         transition="qx") %>% 
  filter(age1>49&age1<80,
         year %in% c("2012")) %>% 
  select(age1,age2,gender,year,transition,probability,type)


# tp_silc 
tp_mf_13 <- tp_mf_13 %>%
  pivot_longer(cols=c("DF_DF","DF_WD","WD_WD","WD_DF","DF_D","WD_D"),
               names_to = "transition",
               values_to = "probability") %>% 
  mutate(year=as.factor(year),
         transition = factor(transition,
                             levels = c("DF_DF","DF_WD","DF_D",
                                        "WD_DF","WD_WD","WD_D"),
                             labels = c("Disability-Free -> Disability-Free",
                                        "Disability-Free -> With Disability",
                                        "Disability-Free -> Death",
                                        "With Disability -> Disability-Free",
                                        "With Disability -> With Disability",
                                        "With Disability -> Death")),
         type="SILC") %>%
  dplyr::filter(transition %in% c("With Disability -> Death",
                                  "Disability-Free -> Death")) %>% 
  mutate(year=case_when(year=="2013"~"2012"))  # align with Istat



# together
qx_silc_istat_13 <- rbind(data_istat12,tp_mf_13)

# graphically 
qx_silc_istat_13 %>% 
  dplyr::filter(age1<79) %>% 
  mutate(transition=as.factor(transition),
         transition=factor(transition,
                           levels = c("qx",
                                      "With Disability -> Death",
                                      "Disability-Free -> Death"),
                           labels = c("qx from Istat",
                                      "With Disability -> Death",
                                      "Disability-Free -> Death"))) %>% 
  ggplot(aes(x = age1,
             y = probability)) +
  geom_line(size=1.2,
            aes(colour = transition,
                linetype = transition)) +
  facet_grid(cols = vars(gender)) +
  scale_linetype_manual(values=c("solid","twodash", "twodash"))+
  scale_color_manual(values=c("#3d0099","#a366ff","#ff0000"))+ #"#990000","#6600ff","#ff6666"
  scale_x_continuous(limits = c(50,78),
                     breaks = seq(50,78,by=5))+
  scale_y_continuous(limits = c(0,0.05),
                     breaks = seq(0,0.05,by=0.01))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        text = element_text(size = 18,face="bold"),
        strip.text.x = element_text(size = 18),
        legend.title=element_blank(),
        legend.position = c(0.85, 0.9),
        legend.text=element_text(size=18,face="bold"),
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))



# unadjasted
load(file="data/data_out/Umats_13.rda")
Umat_m_13_unadj <- matrices$male
Umat_f_13_unadj <- matrices$female

source("code/00_functions.R")

tp_fun <- function(Umat_m,Umat_f){
  
  tp_m <- as.data.frame(from_U_to_trans_matrix(Umat_m)) %>% 
    rename(DF_DF = HH,
           DF_WD = HU,
           WD_WD = UU,
           WD_DF = UH) %>% 
    mutate(age1 = seq(50,78),
           age2 = age1+1,
           DF_D = 1-(DF_DF+DF_WD),
           WD_D = 1-(WD_WD+WD_DF)) %>% 
    select(age1, age2,
           DF_DF,DF_WD,
           WD_WD, WD_DF,
           DF_D,WD_D) %>% 
    mutate(gender="Men")
  
  
  tp_f <- as.data.frame(from_U_to_trans_matrix(Umat_f)) %>% 
    rename(DF_DF = HH,
           DF_WD = HU,
           WD_WD = UU,
           WD_DF = UH) %>% 
    mutate(age1 = seq(50,78),
           age2 = age1+1,
           DF_D = 1-(DF_DF+DF_WD),
           WD_D = 1-(WD_WD+WD_DF)) %>% 
    select(age1, age2,
           DF_DF,DF_WD,
           WD_WD, WD_DF,
           DF_D,WD_D)%>% 
    mutate(gender="Women")
  
  tp_mf <- rbind(tp_f,tp_m)
}



tp_mf_13_unadj <- tp_fun(Umat_m_13_unadj,Umat_f_13_unadj) %>% 
  mutate(year=2013)

tp_mf_13_unadj <- tp_mf_13_unadj %>%
  pivot_longer(cols=c("DF_DF","DF_WD","WD_WD","WD_DF","DF_D","WD_D"),
               names_to = "transition",
               values_to = "probability") %>% 
  mutate(year=as.factor(year),
         transition = factor(transition,
                             levels = c("DF_DF","DF_WD","DF_D",
                                        "WD_DF","WD_WD","WD_D"),
                             labels = c("Disability-Free -> Disability-Free",
                                        "Disability-Free -> With Disability",
                                        "Disability-Free -> Death",
                                        "With Disability -> Disability-Free",
                                        "With Disability -> With Disability",
                                        "With Disability -> Death")),
         type="SILC") %>%
  dplyr::filter(transition %in% c("With Disability -> Death",
                                  "Disability-Free -> Death")) %>% 
  mutate(year=case_when(year=="2013"~"2012"))  # align with Istat



# together with istat
qx_silc_unadj_istat_13 <- rbind(data_istat12,tp_mf_13_unadj)

# graphically 
qx_silc_unadj_istat_13 %>% 
  dplyr::filter(age1<79) %>% 
  mutate(transition=as.factor(transition),
         transition=factor(transition,
                           levels = c("qx",
                                      "With Disability -> Death",
                                      "Disability-Free -> Death"),
                           labels = c("qx from Istat",
                                      "With Disability -> Death",
                                      "Disability-Free -> Death"))) %>% 
  ggplot(aes(x = age1,
             y = probability)) +
  geom_line(size=1.2,
            aes(colour = transition,
                linetype = transition)) +
  facet_grid(cols = vars(gender)) +
  scale_linetype_manual(values=c("solid","twodash", "twodash"))+
  scale_color_manual(values=c("#3d0099","#a366ff","#ff0000"))+ #"#990000","#6600ff","#ff6666"
  scale_x_continuous(limits = c(50,78),
                     breaks = seq(50,78,by=5))+
  scale_y_continuous(limits = c(0,0.05),
                     breaks = seq(0,0.05,by=0.01))+
  theme_bw() +
  theme(axis.text.x = element_text(size=18,face="bold"),
        axis.text.y = element_text(size=18,face="bold"),
        text = element_text(size = 18,face="bold"),
        strip.text.x = element_text(size = 18),
        legend.title=element_blank(),
        legend.position = c(0.85, 0.9),
        legend.text=element_text(size=18,face="bold"),
        panel.grid.minor = element_line(color = "grey90",
                                        size = 1.2),
        panel.grid.major = element_line(color = "grey90",
                                        size = 1.2))






# Figure 5 (from Supplementary materials)
# Plot of transitions probabilities over time ----------------------------------

source("code/00_functions.R")

tp_gender_year <- function(Umat_m,Umat_f,year){
  
  tp_m <- as.data.frame(from_U_to_trans_matrix(Umat_m)) %>% 
    rename(DF_DF = HH,
           DF_WD = HU,
           WD_WD = UU,
           WD_DF = UH) %>% 
    mutate(age1 = seq(50,78),
           age2 = age1+1,
           DF_D = 1-(DF_DF+DF_WD),
           WD_D = 1-(WD_WD+WD_DF)) %>% 
    select(age1, age2,
           DF_DF,DF_WD,
           WD_WD, WD_DF,
           DF_D,WD_D) %>% 
    mutate(gender="Men")
  
  
  tp_f <- as.data.frame(from_U_to_trans_matrix(Umat_f)) %>% 
    rename(DF_DF = HH,
           DF_WD = HU,
           WD_WD = UU,
           WD_DF = UH) %>% 
    mutate(age1 = seq(50,78),
           age2 = age1+1,
           DF_D = 1-(DF_DF+DF_WD),
           WD_D = 1-(WD_WD+WD_DF)) %>% 
    select(age1, age2,
           DF_DF,DF_WD,
           WD_WD, WD_DF,
           DF_D,WD_D)%>% 
    mutate(gender="Women")
  
  tp_mf <- rbind(tp_f,tp_m)
  tp_mf_y <- cbind(rbind(tp_f,tp_m),rep(year,nrow(tp_mf)))
  names(tp_mf_y) <- c(names(tp_mf),"year")
  
  
  tp_mf_y <- tp_mf_y %>%
    pivot_longer(cols=c("DF_DF","DF_WD","WD_WD","WD_DF","DF_D","WD_D"),
                 names_to = "transition",
                 values_to = "probability") %>% 
    mutate(year=as.factor(year),
           transition = factor(transition,
                               levels = c("DF_DF","DF_WD","DF_D",
                                          "WD_WD","WD_DF","WD_D"),
                               labels = c("Disability-Free ->\n Disability-Free",
                                          "Disability-Free ->\n With Disability",
                                          "Disability-Free ->\n Death",
                                          "With Disability ->\n With Disability",
                                          "With Disability ->\n Disability-Free",
                                          "With Disability ->\n Death"))) %>%
    dplyr::filter(!transition %in% c("With Disability ->\n Death",
                                     "Disability-Free ->\n Death"))
  
  return(tp_mf_y)
  
}



tp_mf_y <- NA

for (yr in all_yrs){
  Umat_yr <- load(paste0("data/data_out/Umats_",yr,".csv")) %>% 
    mutate(year=yr)
  Umat_yr_m <- matrices$male
  Umat_yr_f <- matrices$female
  
  tp_mf_y_yr <- as.data.frame(tp_gender_year(Umat_m=Umat_yr_m,
                                             Umat_f=Umat_yr_f,
                                             year=yr))
  #bind each year
  tp_mf_y <- rbind(tp_mf_y,tp_mf_y_yr)
  
}



# plot
tp_mf_y %>%
  ggplot(aes(x = age1,
             y = probability)) +
  geom_line(size=0.8,
            aes(color = year)) +
  facet_grid(rows=vars(transition),
             cols=vars(gender))+
  scale_colour_viridis_d(option="B",direction=1, begin=0, end=1)+
  scale_x_continuous(breaks = seq(50, 80,by=5))+
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1,by=0.25))+
  theme_bw() +
  theme(text = element_text(size=18),
        legend.text = element_text(size=18))+
  labs(x ="Age", y = "Probability")+
  ggtitle("Transition probabilities")
