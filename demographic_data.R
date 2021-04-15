library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
d = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/VCP/2018_EU/Data/eu28_merged.csv")

countries = unique(d$Country)
res_list = list()
for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  d_sex = d %>% 
    filter(Country == country) %>% 
    group_by(Sex, VaxImp) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ifelse(Sex=="Female", ss*100/sum(dc$Weight[dc$Sex=="Female"]), ss*100/sum(dc$Weight[dc$Sex=="Male"]))) %>% 
    filter(VaxImp == "Strongly agree") %>% 
    select(Sex, weighted_perc) %>% 
    rename("variable" = "Sex")
  d_overall =  d %>% 
    filter(Country == country) %>% 
    group_by(VaxImp) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    filter(VaxImp == "Strongly agree") %>% 
    select(weighted_perc) %>% 
    mutate(variable = "overall") 
  d_age =  d %>% 
    filter(Country == country) %>%
    group_by(Age_cat,VaxImp ) %>% 
    dplyr::summarise(ss = sum(Weight) ) %>% 
    mutate(weighted_perc = ifelse(Age_cat=="25-34", ss*100/sum(dc$Weight[dc$Age_cat=="25-34"]), 
                                  ifelse(Age_cat=="45-54", ss*100/sum(dc$Weight[dc$Age_cat=="45-54"]),
                                         ifelse(Age_cat=="35-44", ss*100/sum(dc$Weight[dc$Age_cat=="35-44"]),
                                                ifelse(Age_cat=="65+", ss*100/sum(dc$Weight[dc$Age_cat=="65+"]),
                                                       ifelse(Age_cat=="55-64", ss*100/sum(dc$Weight[dc$Age_cat=="55-64"]),
                                                              ss*100/sum(dc$Weight[dc$Age_cat=="18-24"])
                                                       )))))) %>% 
    filter(VaxImp == "Strongly agree") %>% 
    select(Age_cat, weighted_perc) %>% 
    rename("variable" = "Age_cat")
  
  res = rbind(d_overall, d_sex, d_age) %>% 
    dcast(.~variable, value.var = "weighted_perc") %>% 
    select(-".")  
  res2=res[,order(ncol(res):1)]
  
  res_list[[i]] = res2
  names(res_list)[i]=country
}

res_df = ldply(res_list, .id = "Country")

dim(res_df) 
head(res_df)  
summary(res_df)
write.csv(res_df, "euro2018_dem_data.csv", row.names = FALSE)
