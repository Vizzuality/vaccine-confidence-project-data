library(plyr)
library(dplyr)
library(reshape2)
library(foreign)
library(tidyverse)
library(DatawRappr)
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[8], "/Data/")
f = dir(data_path)
d = read.spss(paste0(data_path,f), to.data.frame=TRUE)
# """Q11r1""           ""Q11r2""           ""Q11r3""           ""Q11r4""           ""Q11r5""           ""Q12r1""           ""Q12r2""           ""Q12r3""           ""Q12r4""           ""Q12r5""           ""Q12r6""           ""Q12r7""           ""Q12r8""           ""Q12r9""           ""Q12r10""          ""Q12r11""          ""Q13r1""           ""Q13r2""           ""Q13r3""           ""Q13r4""           ""Q13r5""           ""Q14r1""           ""Q14r2""           ""Q14r3""           ""Q14r4""           ""Q14r5""           ""Q14r6""           ""Q15a""  have the answers Questions are coded in the same order they appear in the questionnaire – the variable label will also specify the relevant wording."
#  select(country, D1, D2code,  Q11r1,Q11r2, Q11r3, Q11r4, Q11r5, Q12r1, Q12r2, Q12r3, Q12r4, Q12r5, Q12r6, Q12r7, Q12r8,  Q12r9, Q12r10, Q12r11, Q13r1, Q13r2, Q13r3, Q13r4, Q13r5, Q14r1, Q14r2, Q14r3, Q14r4, Q14r5,Q14r6, Q15a, Weight_Combined ) 

#general vaccine Q13


ddd = d %>% 
  select(country, D1, D2code, Q13r1,  Q13r3, Q13r4,  Weight_Combined ) %>% 
  na.omit

names(ddd) = c("Country", "sex" ,"Age", "VaxImp", "VaxSaf", "VaxEff", "weight")

# map importance
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
  mutate(Neither = `Do not know` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)
write.csv(res_df, "outputs/2020_Janssen_map.csv")

#map safety

countries = as.character(unique(ddd$Country))
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = ddd %>% 
    filter(Country == country)
  
  d_overall =  ddd %>% 
    filter(Country == country) %>% 
    group_by(VaxSaf) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
    select(VaxSaf, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxSaf") %>% 
    t() 
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

table_id = "1ZCDR"
new_table = dw_copy_chart(table_id) 
new_table_id = new_table$content$publicId



dw_data_to_chart(x = res_df, chart_id = new_table_id)
dw_edit_chart(new_table_id, 
              title = "Proportion of the population who strongly agrees that vaccines are safe")
dw_publish_chart(chart_id = new_table_id)


#map effective
countries = as.character(unique(ddd$Country))
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = ddd %>% 
    filter(Country == country)
  
  d_overall =  ddd %>% 
    filter(Country == country) %>% 
    group_by(VaxEff) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
    select(VaxEff, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxEff") %>% 
    t() 
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

table_id = "1ZCDR"
new_table = dw_copy_chart(table_id) 
new_table_id = new_table$content$publicId



dw_data_to_chart(x = res_df, chart_id = new_table_id)
dw_edit_chart(new_table_id, 
              title = "Proportion of the population who strongly agrees that vaccines are effective")
dw_publish_chart(chart_id = new_table_id)


#tables
age_replacement = c("25-34 years" = "25-34",
                    "35-44 years" = "35-44",         
                    "18-24 years" = "18-24",        
                    "65+ years" = "65+",
                    "45-54 years" = "45-54",
                    "55-64 years" = "55-64"
                    
)

ddd$Age = mapvalues(ddd$Age, from = names(age_replacement), to = age_replacement)

study_source_name = "Global COVID-19 vaccine acceptance and its socio-demographic and emotional determinants: a multi-country cross-sectional study."
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
      filter(indicator == "Strongly agree") %>%
      select(sex, weighted_perc) %>% 
      rename("variable" = "sex")
    d_overall =  d %>% 
      filter(Country == country) %>% 
      #group_by(VaxImp) %>% 
      group_by(indicator) %>% 
      summarise(ss = sum(weight)) %>% 
      mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree") %>% 
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
                                                  ifelse(Age=="65+", ss*100/sum(dc$weight[dc$Age=="65+"]),
                                                         ifelse(Age=="55-64", ss*100/sum(dc$weight[dc$Age=="55-64"]),ifelse(Age=="18-24", ss*100/sum(dc$weight[dc$Age=="18-24"]),NA))))))) %>% 
      filter(indicator == "Strongly agree") %>%
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
  res_df = ldply(res_list, .id = "Country") %>% 
    select(Country,  overall,     Male,   Female,      `65+`,    `55-64`,    `45-54`, `35-44`,    `25-34`,    `18-24`)
  
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





#covid vaccine Q11 Q12


library(plyr)
library(dplyr)
library(reshape2)
library(foreign)
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[8], "/Data/")
f = dir(data_path)
d = read.spss(paste0(data_path,f), to.data.frame=TRUE)
#  select(country, D1, D2code,  Q11r1,Q11r2, Q11r3, Q11r4, Q11r5, Q12r1, Q12r2, Q12r3, Q12r4, Q12r5, Q12r6, Q12r7, Q12r8,  Q12r9, Q12r10, Q12r11, Q13r1, Q13r2, Q13r3, Q13r4, Q13r5, Q14r1, Q14r2, Q14r3, Q14r4, Q14r5,Q14r6, Q15a, Weight_Combined ) 

ddd = d %>% 
  select(country, D1, D2code, Q11r2, Q11r3, Q11r4,  Weight_Combined ) %>% 
  na.omit

names(ddd) = c("Country", "sex" ,"Age", "VaxSaf", "VaxImp",  "VaxEff", "weight")

#map importance covid 19
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
  mutate(Neither = `Do not know` + `Agree` + `Disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)
write.csv(res_df, "outputs/2020_Janssen_covid_map.csv")

# map covid safety
countries = as.character(unique(ddd$Country))
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = ddd %>% 
    filter(Country == country)
  
  d_overall =  ddd %>% 
    filter(Country == country) %>% 
    group_by(VaxSaf) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
    select(VaxSaf, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxSaf") %>% 
    t() 
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know` + `Agree` + `Disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)


table_id = "1ZCDR"
new_table = dw_copy_chart(table_id) 
new_table_id = new_table$content$publicId



dw_data_to_chart(x = res_df, chart_id = new_table_id)
dw_edit_chart(new_table_id, 
              title = "Proportion of the population who strongly agrees that a COVID-19 vaccine is safe")
dw_publish_chart(chart_id = new_table_id)

#map covid effectiveness

countries = as.character(unique(ddd$Country))
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = ddd %>% 
    filter(Country == country)
  
  d_overall =  ddd %>% 
    filter(Country == country) %>% 
    group_by(VaxEff) %>% 
    #group_by(indicator) %>% 
    summarise(ss = sum(weight)) %>% 
    mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
    select(VaxEff, weighted_perc) %>% 
    remove_rownames %>% 
    column_to_rownames(var="VaxEff") %>% 
    t() 
  res_list[[i]] = d_overall
  names(res_list)[i]=country
}
res_df = ldply(res_list, .id = "Country") %>% 
  mutate(Neither = `Do not know` + `Agree` + `Disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)


table_id = "1ZCDR"
new_table = dw_copy_chart(table_id) 
new_table_id = new_table$content$publicId



dw_data_to_chart(x = res_df, chart_id = new_table_id)
dw_edit_chart(new_table_id, 
              title = "Proportion of the population who strongly agrees that a COVID-19 vaccine is effective")
dw_publish_chart(chart_id = new_table_id)

#tables
age_replacement = c("25-34 years" = "25-34",
                    "35-44 years" = "35-44",         
                    "18-24 years" = "18-24",        
                    "65+ years" = "65+",
                    "45-54 years" = "45-54",
                    "55-64 years" = "55-64"
                    
)

ddd$Age = mapvalues(ddd$Age, from = names(age_replacement), to = age_replacement)

study_source_name = "Global COVID-19 vaccine acceptance and its socio-demographic and emotional determinants: a multi-country cross-sectional study."
study_source_url = ""

table_titles = c("imp" = "Proportion of population who strongly agree a COVID-19 vaccine is important in 2020", 
                 "eff" = "Proportion of population who strongly agree a COVID-19 vaccine is  effective in 2020", 
                 "saf" = "Proportion of population who strongly agree a COVID-19 vaccine is  safe in 2020")

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
      filter(indicator == "Strongly agree") %>%
      select(sex, weighted_perc) %>% 
      rename("variable" = "sex")
    d_overall =  d %>% 
      filter(Country == country) %>% 
      #group_by(VaxImp) %>% 
      group_by(indicator) %>% 
      summarise(ss = sum(weight)) %>% 
      mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree") %>% 
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
                                                  ifelse(Age=="65+", ss*100/sum(dc$weight[dc$Age=="65+"]),
                                                         ifelse(Age=="55-64", ss*100/sum(dc$weight[dc$Age=="55-64"]),ifelse(Age=="18-24", ss*100/sum(dc$weight[dc$Age=="18-24"]),NA))))))) %>% 
      filter(indicator == "Strongly agree") %>%
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
  res_df = ldply(res_list, .id = "Country") %>% 
    select(Country,  overall,     Male,   Female,      `65+`,    `55-64`,    `45-54`, `35-44`,    `25-34`,    `18-24`)
  
  # publish to datawrapper
  table_id = "VHfkY"
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







