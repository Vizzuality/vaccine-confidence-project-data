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

data_path = paste0("data/VCP/", dir_list[7], "/Data/")
f = dir(data_path)
dd = read.csv(paste0(data_path,f))


#General vaccine confidence
study_source_name = "State of vaccine confidence in the EU and UK 2020"
study_source_url = "https://ec.europa.eu/health/vaccination/confidence_en"

table_titles = c("imp" = "Proportion of population who strongly agree vaccines are important in 2020", 
                 "eff" = "Proportion of population who strongly agree vaccines are efficient in 2020", 
                 "saf" = "Proportion of population who strongly agree vaccines are safe in 2020")

ind_list = c("imp","eff","saf")

# select one indicator
for (ind in c("imp", "eff", "saf")){#
  sel_ind = ind
  col_ind = c("VACIMP", "VACEFF", "VACSAF")
  names(col_ind) = c("imp","eff","saf")
  
  d = dd %>% #i want to keep the necessary columns and change the name of the one of the indicator to "ind". 
    select(COUNTRY, SEX, WGT, AGE, col_ind[ind])
  names(d)[5] = "indicator"
  countries = unique(d$COUNTRY)
  res_list = list()
  for (i in 1:length(countries)){
    country = countries[i]
    dc = d %>% 
      filter(COUNTRY == country)
    d_sex = d %>% 
      filter(COUNTRY == country) %>%  
      group_by(SEX, indicator) %>% 
      #group_by(SEX, VaxImp) %>% 
      summarise(ss = sum(WGT)) %>% 
      mutate(weighted_perc = ifelse(SEX=="Female", 
                                    ss*100/sum(dc$WGT[dc$SEX=="Female"]), 
                                    ss*100/sum(dc$WGT[dc$SEX=="Male"]))) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree") %>%
      select(SEX, weighted_perc) %>% 
      rename("variable" = "SEX")
    d_overall =  d %>% 
      filter(COUNTRY == country) %>% 
      #group_by(VaxImp) %>% 
      group_by(indicator) %>% 
      summarise(ss = sum(WGT)) %>% 
      mutate(weighted_perc = ss*100/sum(dc$WGT)) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree") %>% 
      select(weighted_perc) %>% 
      mutate(variable = "overall") 
    d_age =  d %>% 
      filter(COUNTRY == country) %>%
      #group_by(AGE,VaxImp ) %>% 
      group_by(AGE, indicator ) %>% 
      dplyr::summarise(ss = sum(WGT) ) %>% 
      mutate(weighted_perc = ifelse(AGE=="25-34", ss*100/sum(dc$WGT[dc$AGE=="25-34"]), 
                                    ifelse(AGE=="45-54", ss*100/sum(dc$WGT[dc$AGE=="45-54"]),
                                           ifelse(AGE=="35-44", ss*100/sum(dc$WGT[dc$AGE=="35-44"]),
                                                  ifelse(AGE=="65+", ss*100/sum(dc$WGT[dc$AGE=="65+"]),
                                                         ifelse(AGE=="55-64", ss*100/sum(dc$WGT[dc$AGE=="55-64"]),
                                                                ss*100/sum(dc$WGT[dc$AGE=="18-24"])
                                                         )))))) %>% 
      filter(indicator == "Strongly agree") %>%
      #filter(VaxImp == "Strongly agree") %>% 
      select(AGE, weighted_perc) %>% 
      rename("variable" = "AGE")
    
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


#MMR
study_source_name = "State of vaccine confidence in the EU and UK 2020"
study_source_url = "https://ec.europa.eu/health/vaccination/confidence_en"

table_titles = c("imp" = "Proportion of population who strongly agree MMR vaccines are important in 2020", 
                 #"eff" = "Proportion of population who strongly agree vaccines are efficient in 2020", 
                 "saf" = "Proportion of population who strongly agree MMR vaccines are safe in 2020")

ind_list = c("imp","saf")

# select one indicator
for (ind in c("imp", "saf")){#
  sel_ind = ind
  col_ind = c("VACIMPMMR", "VACSAFMMR")
  names(col_ind) = c("imp","saf")
  
  d = dd %>% #i want to keep the necessary columns and change the name of the one of the indicator to "ind". 
    select(COUNTRY, SEX, WGT, AGE, col_ind[ind])
  names(d)[5] = "indicator"
  countries = unique(d$COUNTRY)
  res_list = list()
  for (i in 1:length(countries)){
    country = countries[i]
    dc = d %>% 
      filter(COUNTRY == country)
    d_sex = d %>% 
      filter(COUNTRY == country) %>%  
      group_by(SEX, indicator) %>% 
      #group_by(SEX, VaxImp) %>% 
      summarise(ss = sum(WGT)) %>% 
      mutate(weighted_perc = ifelse(SEX=="Female", 
                                    ss*100/sum(dc$WGT[dc$SEX=="Female"]), 
                                    ss*100/sum(dc$WGT[dc$SEX=="Male"]))) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree") %>%
      select(SEX, weighted_perc) %>% 
      rename("variable" = "SEX")
    d_overall =  d %>% 
      filter(COUNTRY == country) %>% 
      #group_by(VaxImp) %>% 
      group_by(indicator) %>% 
      summarise(ss = sum(WGT)) %>% 
      mutate(weighted_perc = ss*100/sum(dc$WGT)) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree") %>% 
      select(weighted_perc) %>% 
      mutate(variable = "overall") 
    d_age =  d %>% 
      filter(COUNTRY == country) %>%
      #group_by(AGE,VaxImp ) %>% 
      group_by(AGE, indicator ) %>% 
      dplyr::summarise(ss = sum(WGT) ) %>% 
      mutate(weighted_perc = ifelse(AGE=="25-34", ss*100/sum(dc$WGT[dc$AGE=="25-34"]), 
                                    ifelse(AGE=="45-54", ss*100/sum(dc$WGT[dc$AGE=="45-54"]),
                                           ifelse(AGE=="35-44", ss*100/sum(dc$WGT[dc$AGE=="35-44"]),
                                                  ifelse(AGE=="65+", ss*100/sum(dc$WGT[dc$AGE=="65+"]),
                                                         ifelse(AGE=="55-64", ss*100/sum(dc$WGT[dc$AGE=="55-64"]),
                                                                ss*100/sum(dc$WGT[dc$AGE=="18-24"])
                                                         )))))) %>% 
      filter(indicator == "Strongly agree") %>%
      #filter(VaxImp == "Strongly agree") %>% 
      select(AGE, weighted_perc) %>% 
      rename("variable" = "AGE")
    
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

#flu
study_source_name = "State of vaccine confidence in the EU and UK 2020"
study_source_url = "https://ec.europa.eu/health/vaccination/confidence_en"

table_titles = c("imp" = "Proportion of population who strongly agree flu vaccines are important in 2020", 
                 #"eff" = "Proportion of population who strongly agree vaccines are efficient in 2020", 
                 "saf" = "Proportion of population who strongly agree flu vaccines are safe in 2020")

ind_list = c("imp","saf")

# select one indicator
for (ind in c("imp", "saf")){#
  sel_ind = ind
  col_ind = c("VACIMPFLU", "VACSAFFLU")
  names(col_ind) = c("imp","saf")
  
  d = dd %>% #i want to keep the necessary columns and change the name of the one of the indicator to "ind". 
    select(COUNTRY, SEX, WGT, AGE, col_ind[ind])
  names(d)[5] = "indicator"
  countries = unique(d$COUNTRY)
  res_list = list()
  for (i in 1:length(countries)){
    country = countries[i]
    dc = d %>% 
      filter(COUNTRY == country)
    d_sex = d %>% 
      filter(COUNTRY == country) %>%  
      group_by(SEX, indicator) %>% 
      #group_by(SEX, VaxImp) %>% 
      summarise(ss = sum(WGT)) %>% 
      mutate(weighted_perc = ifelse(SEX=="Female", 
                                    ss*100/sum(dc$WGT[dc$SEX=="Female"]), 
                                    ss*100/sum(dc$WGT[dc$SEX=="Male"]))) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree") %>%
      select(SEX, weighted_perc) %>% 
      rename("variable" = "SEX")
    d_overall =  d %>% 
      filter(COUNTRY == country) %>% 
      #group_by(VaxImp) %>% 
      group_by(indicator) %>% 
      summarise(ss = sum(WGT)) %>% 
      mutate(weighted_perc = ss*100/sum(dc$WGT)) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree") %>% 
      select(weighted_perc) %>% 
      mutate(variable = "overall") 
    d_age =  d %>% 
      filter(COUNTRY == country) %>%
      #group_by(AGE,VaxImp ) %>% 
      group_by(AGE, indicator ) %>% 
      dplyr::summarise(ss = sum(WGT) ) %>% 
      mutate(weighted_perc = ifelse(AGE=="25-34", ss*100/sum(dc$WGT[dc$AGE=="25-34"]), 
                                    ifelse(AGE=="45-54", ss*100/sum(dc$WGT[dc$AGE=="45-54"]),
                                           ifelse(AGE=="35-44", ss*100/sum(dc$WGT[dc$AGE=="35-44"]),
                                                  ifelse(AGE=="65+", ss*100/sum(dc$WGT[dc$AGE=="65+"]),
                                                         ifelse(AGE=="55-64", ss*100/sum(dc$WGT[dc$AGE=="55-64"]),
                                                                ss*100/sum(dc$WGT[dc$AGE=="18-24"])
                                                         )))))) %>% 
      filter(indicator == "Strongly agree") %>%
      #filter(VaxImp == "Strongly agree") %>% 
      select(AGE, weighted_perc) %>% 
      rename("variable" = "AGE")
    
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
