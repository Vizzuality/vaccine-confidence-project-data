setwd("/Users/gretacvega/Documents/GitHub/vcp/")
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)

dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[3], "/Data/")
f = dir(data_path)
dd = read.csv(paste0(data_path,f))
head(dd)

countries = unique(d$Country)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  
  d_overall =  d %>% 
    filter(Country == country) %>% 
    group_by(VaxImp) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    select(VaxImp, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxImp") %>% 
    t() 

  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know/ NR` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)


write.csv(res_df, "outputs/2018_EU_map.csv")
