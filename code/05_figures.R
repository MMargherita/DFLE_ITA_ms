


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