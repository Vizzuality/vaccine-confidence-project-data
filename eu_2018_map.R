setwd("/Users/gretacvega/Documents/GitHub/vcp/")
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)

dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[3], "/Data/")
f = dir(data_path)
d = read.csv(paste0(data_path,f))
head(dd)
names(dd)

#general vaccine map importance
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


# General vaccine safety map
countries = unique(d$Country)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  
  d_overall =  d %>% 
    filter(Country == country) %>% 
    group_by(VaxSaf) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    select(VaxSaf, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxSaf") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know/ NR` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

map_id = "U8imI"
new_map = dw_copy_chart(map_id) 
new_map_id = new_map$content$publicId


dw_data_to_chart(x = res_df, chart_id = new_map_id)
dw_edit_chart(new_map_id, 
              title = "Proportion of the population who strongly agrees that vaccines are important<br>")
dw_publish_chart(chart_id = new_map_id)

# General vaccine effectiveness map
countries = unique(d$Country)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  
  d_overall =  d %>% 
    filter(Country == country) %>% 
    group_by(VaxEff) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    select(VaxEff, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxEff") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know/ NR` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

map_id = "U8imI"
new_map = dw_copy_chart(map_id) 
new_map_id = new_map$content$publicId


dw_data_to_chart(x = res_df, chart_id = new_map_id)
dw_edit_chart(new_map_id, 
              title = "Proportion of the population who strongly agrees that vaccines are effective<br>")
dw_publish_chart(chart_id = new_map_id)



# MMR map importance
countries = unique(d$Country)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  
  d_overall =  d %>% 
    filter(Country == country) %>% 
    group_by(VaxImpMMR) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    select(VaxImpMMR, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxImpMMR") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know/ NR` + `Tend to agree` + `Tend to disagree`) %>% 
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

# MMR map Safety
countries = unique(d$Country)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  
  d_overall =  d %>% 
    filter(Country == country) %>% 
    group_by(VaxSafMMR) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    select(VaxSafMMR, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxSafMMR") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know/ NR` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

map_id = "U8imI"
new_map = dw_copy_chart(map_id) 
new_map_id = new_map$content$publicId



dw_data_to_chart(x = res_df, chart_id = new_map_id)
dw_edit_chart(new_map_id, 
              title = "Proportion of the population who strongly agrees that MMR vaccines are safe<br>")
dw_publish_chart(chart_id = new_map_id)



#flu importance map
names(d)
countries = unique(d$Country)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  
  d_overall =  d %>% 
    filter(Country == country) %>% 
    group_by(VaxImpFlu) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    select(VaxImpFlu, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxImpFlu") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know/ NR` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

# U8imI
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

#flu safety map
countries = unique(d$Country)
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  
  d_overall =  d %>% 
    filter(Country == country) %>% 
    group_by(VaxSafFlu) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    select(VaxSafFlu, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxSafFlu") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know/ NR` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

# U8imI
map_id = "U8imI"
new_map = dw_copy_chart(map_id) 
new_map_id = new_map$content$publicId



dw_data_to_chart(x = res_df, chart_id = new_map_id)
dw_edit_chart(new_map_id, 
              title = "Proportion of the population who strongly agrees that flu vaccines are safe<br>",
              describe = describe_list)
dw_publish_chart(chart_id = new_map_id)
