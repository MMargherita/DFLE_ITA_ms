library(tidyverse)
library(readr)


# qx from istat ----------------------------------------------------------------

data_istat04_16 <- read_csv("data/data_istat_04-16.csv", 
                            col_types = cols(`Flag Codes` = col_skip(), 
                                             Flags = col_skip()))


data_istat04_16 <- data_istat04_16[c(2,6,7,10,11)]
names(data_istat04_16) <- c("rep","sex","age","year","qx")

data_istat04_16 <- data_istat04_16 %>% 
  mutate(age1 = as.numeric(str_sub(age, start = 2, end = 3)),
         age2 = age1+1,
         from = "T",
         to = "D",
         rep = as.factor(case_when(rep == "Nord" ~ "North",
                                   rep == "Centro" ~ "Centre",
                                   rep == "Mezzogiorno" ~ "South")),
         rep = factor(rep,
                      levels = c("North","Centre","South","ita"),
                      labels = c("North","Centre","South","ita")),
         gender = as.factor(case_when(sex == "femmine" ~ "Women",
                                      sex == "maschi" ~ "Men")),
         probs = qx/1000) %>%
  select(year,gender,rep,age1,from,age2,to,probs) %>% 
  filter(age1>59&age1<80,
         year %in% c(2008:2011))




# estimates from Angelo --------------------------------------------------------

#north
tp_mf_North_08 <- readRDS("data/tp_mf_North_2008.rds")  %>% 
  filter(geo_edu=="North") %>% 
  mutate(age1 = as.numeric(str_sub(from, start = 1, end = 2)),
         FROM = str_sub(from, start = 5, end = 14),
         age2 = as.numeric(age1+1),
         FROM = case_when(FROM == "Disabled" ~ "U",
                          FROM == "Healthy" ~ "H"),
         TO = str_sub(to, start = 5, end = 14),
         TO = case_when(TO == "Disabled" ~ "U",
                        TO == "Healthy" ~ "H"),
         from = FROM,
         to = TO,
         rep = "North",
         year = 2008) %>% 
  filter(age1 > 59,
         age2 < 80) %>% 
  select(year,gender,rep,age1,from,age2,to,probs)

#just death probs
qx_mf_North_08 <- tp_mf_North_08 %>% 
  group_by(gender,rep,age1,from) %>% 
  mutate(probs = 1-(sum(probs)),
         to = "D") %>% 
  select(year,gender,rep,age1,from,age2,to,probs) %>% 
  unique() %>% 
  ungroup()


# centre

# tp 
tp_mf_Centre_08 <- readRDS("data/tp_mf_Centre_2008.rds")  %>% 
  filter(geo_edu=="Centre") %>% 
  mutate(age1 = as.numeric(str_sub(from, start = 1, end = 2)),
         FROM = str_sub(from, start = 5, end = 14),
         age2 = as.numeric(age1+1),
         FROM = case_when(FROM == "Disabled" ~ "U",
                          FROM == "Healthy" ~ "H"),
         TO = str_sub(to, start = 5, end = 14),
         TO = case_when(TO == "Disabled" ~ "U",
                        TO == "Healthy" ~ "H"),
         from = FROM,
         to = TO,
         rep = "Centre",
         year = 2008) %>% 
  filter(age1 > 59,
         age2 < 80) %>% 
  select(year,gender,rep,age1,from,age2,to,probs)

#just death probs
qx_mf_Centre_08 <- tp_mf_Centre_08 %>% 
  group_by(gender,rep,age1,from) %>% 
  mutate(probs = 1-(sum(probs)),
         to = "D") %>% 
  select(year,gender,rep,age1,from,age2,to,probs) %>% 
  unique() %>% 
  ungroup()


# South

# tp 
tp_mf_South_08 <- readRDS("data/tp_mf_South_2008.rds")  %>% 
  filter(geo_edu=="South") %>% 
  mutate(age1 = as.numeric(str_sub(from, start = 1, end = 2)),
         FROM = str_sub(from, start = 5, end = 14),
         age2 = as.numeric(age1+1),
         FROM = case_when(FROM == "Disabled" ~ "U",
                          FROM == "Healthy" ~ "H"),
         TO = str_sub(to, start = 5, end = 14),
         TO = case_when(TO == "Disabled" ~ "U",
                        TO == "Healthy" ~ "H"),
         from = FROM,
         to = TO,
         rep = "South",
         year = 2008) %>% 
  filter(age1 > 59,
         age2 < 80) %>% 
  select(year,gender,rep,age1,from,age2,to,probs)

#just death probs
qx_mf_South_08 <- tp_mf_South_08 %>% 
  group_by(gender,rep,age1,from) %>% 
  mutate(probs = 1-(sum(probs)),
         to = "D") %>% 
  select(year,gender,rep,age1,from,age2,to,probs) %>% 
  unique() %>% 
  ungroup()



# merge the datasets -----------------------------------------------------------
data_trans_death <- rbind(data_istat04_16,
                          qx_mf_North_08,
                          qx_mf_Centre_08,
                          qx_mf_South_08) %>%
  mutate(transition = case_when(
    from == "H" & to == "D" ~ "H->D",
    from == "U" & to == "D" ~ "U->D",
    from == "T" ~ "qx_istat"),
    transition = factor(transition,
                        levels = c("U->D","H->D","qx_istat"),
                        labels = c("U->D","H->D","qx_istat")))




# graphically ------------------------------------------------------------------
data_trans_death %>%
  ggplot(aes(x = age1,
             y = probs)) +
  geom_line(size=1.2,aes(linetype=as.factor(year),color=transition)) +
  facet_grid(rows = vars(rep),
             cols = vars(gender)) +
  theme_minimal() +
  theme(text = element_text(size=18))+
  ggtitle("Transition probability to death from ITSILC compared to qx from ISTAT - 2008")


# ggsave(filename = "plot/tp_death_qx_08.png",
#        device = "png", dpi = 200,
#        width = 15, height = 8)
