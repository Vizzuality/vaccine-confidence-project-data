setwd("/Users/gretacvega/Documents/GitHub/vcp/")
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)

dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[7], "/Data/")
f = dir(data_path)
d = read.csv(paste0(data_path,f))
head(dd)
names(dd)

#general vaccine
countries = unique(d$COUNTRY)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(COUNTRY == country)
  
  d_overall =  d %>% 
    filter(COUNTRY == country) %>% 
    group_by(VACIMP) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(WGT)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$WGT)) %>% 
    select(VACIMP, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VACIMP") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `DNK` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

map_id = "U8imI"
new_map = dw_copy_chart(map_id) 
new_map_id = new_map$content$publicId

describe_list = new_map$content$metadata$describe
describe_list$`source-name` = study_source_name
describe_list$`source-url` = study_source_url

dw_data_to_chart(x = res_df, chart_id = new_map_id)
dw_edit_chart(new_map_id, 
              title = "Proportion of the population who strongly agrees that vaccines are important<br>",
              describe = describe_list)
dw_publish_chart(chart_id = new_map_id)


#MMR
countries = unique(d$COUNTRY)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(COUNTRY == country)
  
  d_overall =  d %>% 
    filter(COUNTRY == country) %>% 
    group_by(VACIMPMMR) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(WGT)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$WGT)) %>% 
    select(VACIMPMMR, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VACIMPMMR") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `DNK` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

map_id = "U8imI"
new_map = dw_copy_chart(map_id) 
new_map_id = new_map$content$publicId

describe_list = new_map$content$metadata$describe
describe_list$`source-name` = study_source_name
describe_list$`source-url` = study_source_url

dw_data_to_chart(x = res_df, chart_id = new_map_id)
dw_edit_chart(new_map_id, 
              title = "Proportion of the population who strongly agrees that MMR vaccines are important<br>",
              describe = describe_list)
dw_publish_chart(chart_id = new_map_id)


#flu
countries = unique(d$COUNTRY)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(COUNTRY == country)
  
  d_overall =  d %>% 
    filter(COUNTRY == country) %>% 
    group_by(VACIMPFLU) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(WGT)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$WGT)) %>% 
    select(VACIMPFLU, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VACIMPFLU") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `DNK` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

map_id = "U8imI"
new_map = dw_copy_chart(map_id) 
new_map_id = new_map$content$publicId

describe_list = new_map$content$metadata$describe
describe_list$`source-name` = study_source_name
describe_list$`source-url` = study_source_url

dw_data_to_chart(x = res_df, chart_id = new_map_id)
dw_edit_chart(new_map_id, 
              title = "Proportion of the population who strongly agrees that flu vaccines are important<br>",
              describe = describe_list)
dw_publish_chart(chart_id = new_map_id)