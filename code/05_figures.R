
# all years together! ----
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
                                     "With Disability -> Death")),
         gap_2 = paste("change between\n2016 and 2019\n",
                       "= ",gap_m, "years")) %>%
  ggplot(aes(x = age, y = cc, fill = from_to))+
  geom_col()+
  facet_grid(cols=vars(period),
             rows=vars(gender))+
  scale_x_discrete(breaks = c("50","55","60","65","70","75"))+
  scale_y_continuous(limits = c(-0.07,0.17),
                     breaks = seq(-0.05:0.15, by=0.05))+
  # scale_fill_viridis_d(begin = 0.1, end = 0.9,option = "G",
  #                      direction = -1)+
  scale_fill_manual(values=c("#0a2777","#166e90","#7fcd9f","#deee0c"))+
  geom_hline(yintercept=0)+
  labs(title = "Contributions to the change in DFLE of the transitions")+
  # geom_text(aes(label = gap_2,
  #               x = min(cc_p_attrition$age),
  #               y = min(cc_p_attrition$cc)),
  #           hjust = -0.35, vjust = -3,
  #           color = "black", size = 6)+
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