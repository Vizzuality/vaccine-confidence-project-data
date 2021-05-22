# this script is for the datasets that have demographic information
# this script uses information from countries and per study creates 3 tables one per each indicator: efficiency, safety and importance
#overall, age and gender
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(DatawRappr)
# connect to the API and authenticate
token = read.table(".env", sep = "=") %>% 
  filter(V1 == "DATAWRAPPER_TOKEN") %>% 
  select(V2)
datawrapper_auth(api_key = token[1,1])

setwd("/Users/gretacvega/Documents/GitHub/vcp/")
#dir_list = dir("data/VCP")[-10]
dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")


data_path = paste0("data/VCP/", dir_list[3], "/Data/")
f = dir(data_path)
dd = read.csv(paste0(data_path,f))

study_source_name = "State of vaccine confidence in the EU 2018"
study_source_url = "https://ec.europa.eu/health/sites/default/files/vaccination/docs/2018_vaccine_confidence_en.pdf"

table_titles = c("imp" = "Proportion of population who strongly agree vaccines are important in 2018", 
                 "eff" = "Proportion of population who strongly agree vaccines are efficient in 2018", 
                 "saf" = "Proportion of population who strongly agree vaccines are safe in 2018")

ind_list = c("imp","eff","saf")

# select one indicator
for (ind in c("eff","saf")){#"imp",
sel_ind = ind
col_ind = c("VaxImp", "VaxEff", "VaxSaf")
names(col_ind) = c("imp","eff","saf")

d = dd %>% #i want to keep the necessary columns and change the name of the one of the indicator to "ind". 
  select(Country, Sex, Weight, Age_cat, col_ind[ind])
names(d)[5] = "indicator"
  
countries = unique(d$Country)
res_list = list()
for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  d_sex = d %>% 
    filter(Country == country) %>%  
    group_by(Sex, indicator) %>% 
    #group_by(Sex, VaxImp) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ifelse(Sex=="Female", 
                                  ss*100/sum(dc$Weight[dc$Sex=="Female"]), 
                                  ss*100/sum(dc$Weight[dc$Sex=="Male"]))) %>% 
    #filter(VaxImp == "Strongly agree") %>% 
    filter(indicator == "Strongly agree") %>%
    select(Sex, weighted_perc) %>% 
    rename("variable" = "Sex")
  d_overall =  d %>% 
    filter(Country == country) %>% 
    #group_by(VaxImp) %>% 
    group_by(indicator) %>% 
    summarise(ss = sum(Weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$Weight)) %>% 
    #filter(VaxImp == "Strongly agree") %>% 
    filter(indicator == "Strongly agree") %>% 
    select(weighted_perc) %>% 
    mutate(variable = "overall") 
  d_age =  d %>% 
    filter(Country == country) %>%
    #group_by(Age_cat,VaxImp ) %>% 
    group_by(Age_cat, indicator ) %>% 
    dplyr::summarise(ss = sum(Weight) ) %>% 
    mutate(weighted_perc = ifelse(Age_cat=="25-34", ss*100/sum(dc$Weight[dc$Age_cat=="25-34"]), 
                                  ifelse(Age_cat=="45-54", ss*100/sum(dc$Weight[dc$Age_cat=="45-54"]),
                                         ifelse(Age_cat=="35-44", ss*100/sum(dc$Weight[dc$Age_cat=="35-44"]),
                                                ifelse(Age_cat=="65+", ss*100/sum(dc$Weight[dc$Age_cat=="65+"]),
                                                       ifelse(Age_cat=="55-64", ss*100/sum(dc$Weight[dc$Age_cat=="55-64"]),
                                                              ss*100/sum(dc$Weight[dc$Age_cat=="18-24"])
                                                       )))))) %>% 
    filter(indicator == "Strongly agree") %>%
    #filter(VaxImp == "Strongly agree") %>% 
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

# publish to datawrapper
table_id = "5xLOT"
new_table = dw_copy_chart(table_id) 
new_table_id = new_table$content$publicId

describe_list = new_table$content$metadata$describe
describe_list$`source-name` = study_source_name
describe_list$`source-url` = study_source_url

dw_data_to_chart(x = res_df, chart_id = new_table_id)
dw_edit_chart(new_table_id, 
              title = table_titles[sel_ind],
              describe = describe_list)
dw_publish_chart(chart_id = new_table_id)
}
