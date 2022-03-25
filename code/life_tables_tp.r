rm(list=ls())
getwd()
library(data.table)
dd <- file.path("./data/life_tables")
files <- list.files(dd,full.names=T,pattern = "\\.csv$")

dat <- lapply(files, function(x) read.csv2(x, header = T,skip = 1,sep=";",dec=",")) %>% 
  set_names(files) %>% 
  rbindlist(idcol = "origin") 

dat <- dat %>%
    rename_all( ~  c("origin","age", "lx", "dx", "qx", "Lx", "px", "ex")) %>% 
    filter(age %in% 50:79) %>% 
    mutate(origin = str_replace(origin, "./data/life_tables/",""),
           origin = str_replace(origin, ".csv","")) %>% 
    separate(origin, c("area", "sex","year"))

dat <- dat %>%
    mutate(probs = qx/1000,
           area = factor(area, c("Nord","Centro", "Mezzogiorno", "Italia"),
                               c("North","Centre","South","Italy")),
           gender = factor(sex, c("Maschi","Femmine"),c("Men","Women")),
           dsource = paste0("LT_",year),
           from = "Pop",
           to = "Dead") %>% 
    select(age,from,to,qx,probs,gender,area,dsource) 

# get tp

dd <- file.path("./data")
files <- list.files(dd,full.names=T,pattern = "\\.RDS$")

dt <- lapply(files, function(x) readRDS(x)) %>%   rbindlist() 

dt <- dt %>%
  separate(from, c("age","from")) %>% 
  separate(to, c("age_to","to")) %>% 
  filter(age %in% 50:79) %>% 
  select(age,from,to,probs,gender,geo_edu)

dq <- dt %>% 
group_by(geo_edu, gender, age, from) %>% 
  mutate(probs = 1 - sum(probs),
         qx = probs*1000,
         to = "Dead",
         dsource = "IT_SILC") %>% unique()

dmort <- dq %>% 
  filter(geo_edu %in% c("North", "Centre","South","ita")) %>% 
  rename(area = geo_edu) %>% 
  mutate(area = factor(area,c("North", "Centre","South","ita"), c("North", "Centre","South","Italy")),
         age = as.numeric(age))

dati <- bind_rows(dmort,dat)

library(ggplot2)

dati %>% ggplot(aes(x = age,
           y = probs,
           color = from, 
           group = from)) +
  geom_line(size=1.2,aes(linetype = from)) +
  facet_grid(rows = vars(area) ,cols = vars(gender))+
  #lim(0,1)+
  theme_minimal() +
  theme(text = element_text(size = 18))+
  ggtitle("Death probability IT SILC vs LT - 2012")
