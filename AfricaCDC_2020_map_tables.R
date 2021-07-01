library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(foreign)
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[6], "/Data/")
f = dir(data_path)
dd = read.spss(paste0(data_path,f), to.data.frame=TRUE)


#"B1r1"                    "B1r2"                    "B1r3"                    "B1r4"                    "B1r5" have the answers, Questions are coded in the same order they appear in the questionnaire – the variable label will also specify the relevant wording.

ddd = dd %>% 
  select(Adm1, Gender, Coded_Respondent_Age, B1r1, B1r3,B1r4, Weight_IvoryCoast) 
names(ddd) = c("Country", "sex", "Age", "VaxImp", "VaxSaf", "VaxEff", "weight")
head(ddd)


# map
countries = as.character(unique(ddd$Country))
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = ddd %>% 
    filter(Country == country)
  
  d_overall =  ddd %>% 
    filter(Country == country) %>% 
    group_by(VaxImp) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
    select(VaxImp, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxImp") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = ifelse(is.na(`Dont know`),0, `Dont know`) + ifelse(is.na(`Tend to Agree`),0, `Tend to Agree`) + ifelse(is.na(`Tend to Disagree`),0, `Tend to Disagree`) + ifelse(is.na(`Refused`),0, `Refused`)) %>% 
  select(Country, `Strongly Agree`, Neither, `Strongly Disagree`)

replacement = c("7 Cote d Ivoire" = "Côte d'Ivoire",
                "5 South Africa" = "South Africa",
                "6 Niger" = "Niger",
                "3 Nigeria" = "Nigeria",
                "9 DR Congo" = "Democratic Republic of Congo",
                "4 Sudan" = "Sudan",
                "8 Gabon"  = "Gabon",
                "12 Kenya"   = "Kenya", 
                "11 Burkina Faso" = "Burkina Faso",
                "15 Malawi" = "Malawi",
                "14 Senegal" = "Senegal",
                "13 Uganda" = "Uganda",
                "10 Ethiopia" = "Ethiopia",
                "1 Morocco" = "Morocco",
                "2 Tunisia" = "Tunisia")
res_df$Country = mapvalues(res_df$Country, from = names(replacement), to = replacement)


write.csv(res_df, "outputs/2020_AfricaCDC_general_map.csv")

#tables
head(ddd)
unique(ddd$Age)


study_source_name = "Report for Africa CDC."
study_source_url = ""

table_titles = c("imp" = "Proportion of population who strongly agree vaccines are important in 2020", 
                 "eff" = "Proportion of population who strongly agree vaccines are effective in 2020", 
                 "saf" = "Proportion of population who strongly agree vaccines are safe in 2020")

ind_list = c("imp","eff", "saf")
dd = ddd

for (ind in c("imp","eff", "saf")){#
  sel_ind = ind
  col_ind = c("VaxImp","VaxEff", "VaxSaf")
  names(col_ind) = c("imp","eff","saf")
  
  d = dd %>% #i want to keep the necessary columns and change the name of the one of the indicator to "ind". 
    select(Country, sex, weight, Age, col_ind[ind])
  names(d)[5] = "indicator"
  countries = as.character(unique(d$Country))
  res_list = list()
  
  for (i in 1:length(countries)){
    country = countries[i]
    dc = d %>% 
      filter(Country == country)
    d_sex = d %>% 
      filter(Country == country) %>%  
      group_by(sex, indicator) %>% 
      #group_by(sex, VaxImp) %>% 
      summarise(ss = sum(weight)) %>% 
      mutate(weighted_perc = ifelse(sex=="Female", 
                                    ss*100/sum(dc$weight[dc$sex=="Female"]), 
                                    ss*100/sum(dc$weight[dc$sex=="Male"]))) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly Agree") %>%
      select(sex, weighted_perc) %>% 
      rename("variable" = "sex")
    d_overall =  d %>% 
      filter(Country == country) %>% 
      #group_by(VaxImp) %>% 
      group_by(indicator) %>% 
      summarise(ss = sum(weight)) %>% 
      mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly Agree") %>% 
      select(weighted_perc) %>% 
      mutate(variable = "overall") 
    d_age =  d %>% 
      filter(Country == country) %>%
      #group_by(Age,VaxImp ) %>% 
      group_by(Age, indicator ) %>% 
      dplyr::summarise(ss = sum(weight) ) %>% 
      mutate(weighted_perc = ifelse(Age=="25-34", ss*100/sum(dc$weight[dc$Age=="25-34"]), 
                                    ifelse(Age=="45-54", ss*100/sum(dc$weight[dc$Age=="45-54"]),
                                           ifelse(Age=="35-44", ss*100/sum(dc$weight[dc$Age=="35-44"]),
                                                  ifelse(Age=="55+", ss*100/sum(dc$weight[dc$Age=="55+"]),
                                                         ifelse(Age=="18-24", ss*100/sum(dc$weight[dc$Age=="18-24"]),NA)))))) %>% 
      filter(indicator == "Strongly Agree") %>%
      #filter(VaxImp == "Strongly agree") %>% 
      select(Age, weighted_perc) %>% 
      rename("variable" = "Age")
    
    res = rbind(d_overall, d_sex, d_age) %>% 
      dcast(.~variable, value.var = "weighted_perc") %>% 
      select(-".")  
    res2=res[,order(ncol(res):1)]
    
    res_list[[i]] = res2
    names(res_list)[i]=country
  }
  res_df = ldply(res_list, .id = "Country") 
  res_df$Country = mapvalues(res_df$Country, from = names(replacement), to = replacement)  
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


# covid19
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[6], "/Data/")
f = dir(data_path)
dd = read.spss(paste0(data_path,f), to.data.frame=TRUE)


#"B1r1"                    "B1r2"                    "B1r3"                    "B1r4"                    "B1r5" have the answers, Questions are coded in the same order they appear in the questionnaire – the variable label will also specify the relevant wording.

ddd = dd %>% 
  select(Adm1, Gender, Coded_Respondent_Age, B3s1, B3s2,B3s3, Weight_IvoryCoast) 
names(ddd) = c("Country", "sex", "Age", "VaxSaf","VaxImp",  "VaxEff", "weight")
head(ddd)

# map
countries = as.character(unique(ddd$Country))
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = ddd %>% 
    filter(Country == country)
  
  d_overall =  ddd %>% 
    filter(Country == country) %>% 
    group_by(VaxImp) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
    select(VaxImp, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxImp") %>% 
    t() 
  
  
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = ifelse(is.na(`Dont know`),0, `Dont know`) + ifelse(is.na(`Tend to Agree`),0, `Tend to Agree`) + ifelse(is.na(`Tend to Disagree`),0, `Tend to Disagree`) + ifelse(is.na(`Refused`),0, `Refused`)) %>% 
  select(Country, `Strongly Agree`, Neither, `Strongly Disagree`)

replacement = c("7 Cote d Ivoire" = "Côte d'Ivoire",
                "5 South Africa" = "South Africa",
                "6 Niger" = "Niger",
                "3 Nigeria" = "Nigeria",
                "9 DR Congo" = "Democratic Republic of Congo",
                "4 Sudan" = "Sudan",
                "8 Gabon"  = "Gabon",
                "12 Kenya"   = "Kenya", 
                "11 Burkina Faso" = "Burkina Faso",
                "15 Malawi" = "Malawi",
                "14 Senegal" = "Senegal",
                "13 Uganda" = "Uganda",
                "10 Ethiopia" = "Ethiopia",
                "1 Morocco" = "Morocco",
                "2 Tunisia" = "Tunisia")
res_df$Country = mapvalues(res_df$Country, from = names(replacement), to = replacement)


write.csv(res_df, "outputs/2020_AfricaCDC_covid_map.csv")


study_source_name = "Report for Africa CDC."
study_source_url = ""

table_titles = c("imp" = "Proportion of population who strongly agree a COVID-19 vaccine is important in 2020", 
                 "eff" = "Proportion of population who strongly agree a COVID-19 vaccine is effective in 2020", 
                 "saf" = "Proportion of population who strongly agree a COVID-19 vaccine is safe in 2020")

ind_list = c("imp","eff", "saf")
dd = ddd

for (ind in c("imp","eff", "saf")){#
  sel_ind = ind
  col_ind = c("VaxImp","VaxEff", "VaxSaf")
  names(col_ind) = c("imp","eff","saf")
  
  d = dd %>% #i want to keep the necessary columns and change the name of the one of the indicator to "ind". 
    select(Country, sex, weight, Age, col_ind[ind])
  names(d)[5] = "indicator"
  countries = as.character(unique(d$Country))
  res_list = list()
  
  for (i in 1:length(countries)){
    country = countries[i]
    dc = d %>% 
      filter(Country == country)
    d_sex = d %>% 
      filter(Country == country) %>%  
      group_by(sex, indicator) %>% 
      #group_by(sex, VaxImp) %>% 
      summarise(ss = sum(weight)) %>% 
      mutate(weighted_perc = ifelse(sex=="Female", 
                                    ss*100/sum(dc$weight[dc$sex=="Female"]), 
                                    ss*100/sum(dc$weight[dc$sex=="Male"]))) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly Agree") %>%
      select(sex, weighted_perc) %>% 
      rename("variable" = "sex")
    d_overall =  d %>% 
      filter(Country == country) %>% 
      #group_by(VaxImp) %>% 
      group_by(indicator) %>% 
      summarise(ss = sum(weight)) %>% 
      mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly Agree") %>% 
      select(weighted_perc) %>% 
      mutate(variable = "overall") 
    d_age =  d %>% 
      filter(Country == country) %>%
      #group_by(Age,VaxImp ) %>% 
      group_by(Age, indicator ) %>% 
      dplyr::summarise(ss = sum(weight) ) %>% 
      mutate(weighted_perc = ifelse(Age=="25-34", ss*100/sum(dc$weight[dc$Age=="25-34"]), 
                                    ifelse(Age=="45-54", ss*100/sum(dc$weight[dc$Age=="45-54"]),
                                           ifelse(Age=="35-44", ss*100/sum(dc$weight[dc$Age=="35-44"]),
                                                  ifelse(Age=="55+", ss*100/sum(dc$weight[dc$Age=="55+"]),
                                                         ifelse(Age=="18-24", ss*100/sum(dc$weight[dc$Age=="18-24"]),NA)))))) %>% 
      filter(indicator == "Strongly Agree") %>%
      #filter(VaxImp == "Strongly agree") %>% 
      select(Age, weighted_perc) %>% 
      rename("variable" = "Age")
    
    res = rbind(d_overall, d_sex, d_age) %>% 
      dcast(.~variable, value.var = "weighted_perc") %>% 
      select(-".")  
    res2=res[,order(ncol(res):1)]
    
    res_list[[i]] = res2
    names(res_list)[i]=country
  }
  res_df = ldply(res_list, .id = "Country") 
  res_df$Country = mapvalues(res_df$Country, from = names(replacement), to = replacement)  
  # publish to datawrapper
  table_id = "rBrvn"
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



