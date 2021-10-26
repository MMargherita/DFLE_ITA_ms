library(tidyverse)


tp_mf_ita <- readRDS("data/tp_mf_ita_2012.rds") %>% 
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
         edu = str_sub(geo_edu, start = 9, end = 13),
         edu = as.factor(ifelse(edu=="","tot",edu)),
         edu = factor(edu, levels = c("high","mid","low","tot"),
                      labels = c("high","mid","low","tot")),
         rep = "ita") %>% 
  select(gender,edu,rep,age1,from,age2,to,probs)

death_prob <- tp_mf_ita %>% 
  group_by(gender,edu,rep,age1,from) %>% 
  mutate(death_probs = 1-(sum(probs))) %>% 
  select(gender,edu,rep,age1,from,age2,to,death_probs) %>% 
  unique()

death_prob <- death_prob %>% 
  mutate(probs = death_probs,
         to = "D") %>% 
  select(gender,edu,rep,age1,from,age2,to,probs)

tp_mf_ita <- rbind(tp_mf_ita,death_prob)


# explorative plot
tp_mf_ita %>% 
  filter(age1>59) %>%
  mutate(transition = case_when(
    from == "H" & to == "H" ~ "H->H",
    from == "U" & to == "H" ~ "U->H",
    from == "H" & to == "U" ~ "H->U",
    from == "U" & to == "U" ~ "U->U",
    from == "H" & to == "D" ~ "H->D",
    from == "U" & to == "D" ~ "U->D")) %>%
  ggplot(aes(x = age1,
             y = probs,
             color = transition, 
             group = transition)) +
  geom_line(size=1.2) +
  facet_grid(rows = vars(edu),
             cols = vars(gender))+
  ylim(0,1)+
  theme_minimal() +
  theme(text = element_text(size=18))+
  ggtitle("Transition probability ITSILC - 2012")

# ggsave(filename = "plot/tp_ita_12.png", device = "png", dpi = 200,
#        width = 15, height = 8)



# centre -----------------------------------------------------------------------

tp_mf_ce <- readRDS("data/tp_mf_Centre_2012.rds") %>% 
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
         edu = str_sub(geo_edu, start = 12, end = 16),
         edu = as.factor(ifelse(edu=="","tot",edu)),
         edu = factor(edu, levels = c("high","mid","low","tot"),
                      labels = c("high","mid","low","tot")),
         rep = as.factor("centre")) %>% 
  select(gender,edu,rep,age1,from,age2,to,probs)

death_prob <- tp_mf_ce %>% 
  group_by(gender,edu,rep,age1,from) %>% 
  mutate(death_probs = 1-(sum(probs))) %>% 
  select(gender,edu,rep,age1,from,age2,to,death_probs) %>% 
  unique()

death_prob <- death_prob %>% 
  mutate(probs = death_probs,
         to = "D") %>% 
  select(gender,edu,rep,age1,from,age2,to,probs)

tp_mf_ce <- rbind(tp_mf_ce,death_prob)


# explorative plot
tp_mf_ce %>% 
  filter(age1>59) %>%
  mutate(transition = case_when(
    from == "H" & to == "H" ~ "H->H",
    from == "U" & to == "H" ~ "U->H",
    from == "H" & to == "U" ~ "H->U",
    from == "U" & to == "U" ~ "U->U",
    from == "H" & to == "D" ~ "H->D",
    from == "U" & to == "D" ~ "U->D")) %>%
  ggplot(aes(x = age1,
             y = probs,
             color = transition, 
             group = transition)) +
  geom_line(size=1.2) +
  facet_grid(rows = vars(edu),
             cols = vars(gender))+
  ylim(0,1)+
  theme_minimal() +
  theme(text = element_text(size=18))+
  ggtitle("Transition probability ITSILC - Centre - 2012")

# ggsave(filename = "plot/tp_centre_12.png", device = "png", dpi = 200,
#        width = 15, height = 8)


