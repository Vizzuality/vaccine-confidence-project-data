library(plyr)
library(dplyr)
library(reshape2)
library(foreign)
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[2], "/Data/")
f = dir(data_path)
dd = read.csv(paste0(data_path,f))

#V1a, V1b,  V1c, V1d

ddd = dd %>% 
  select(Country, D3, D5, V1a, V1b,  V1c, MALI_WEIGHT, MAURITANIA_WEIGHT, NIGER_WEIGHTb, NIGERIA_WEIGHT, SENEGAL_WEIGHT, BURKINA_WEIGHT)

names(ddd) = c("Country", "sex" ,"Age", "VaxImp", "VaxSaf", "VaxEff", "MALI_WEIGHT", "MAURITANIA_WEIGHT", "NIGER_WEIGHTb", "NIGERIA_WEIGHT", "SENEGAL_WEIGHT", "BURKINA_WEIGHT" )
w_vector = c("MALI_WEIGHT", "MAURITANIA_WEIGHT", "NIGER_WEIGHTb", "NIGERIA_WEIGHT", "SENEGAL_WEIGHT", "BURKINA_WEIGHT")
names(w_vector) = c("Mali", "Mauritania", "Niger", "Nigeria", "Senegal", "Burkina Faso")


# map
countries = as.character(unique(d$Country))
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = ddd %>% 
    filter(Country == country) %>% 
    select("Country", "sex" ,"Age", "VaxImp", "VaxSaf", "VaxEff", w_vector[country])
  names(dc)[7] = "weight"
  d_overall =  dc %>% 
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
res_df = ldply(res_list, .id = "Country") 
names(res_df) = c("Country",                  "Do not know/ no response", "Somewhat agree", 
                  "Somewhat disagree",      "Strongly agree",         "Strongly disagree" )
res_df = res_df %>%   
  mutate(Neither = `Do not know/ no response` + `Somewhat agree` + `Somewhat disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

write.csv(res_df, "outputs/2016_Sahel_map.csv")



#tables
study_source_name = "Alex de Figueiredo, PhD thesis. (Chapter 4). 2018."
study_source_url = "https://spiral.imperial.ac.uk/bitstream/10044/1/69771/1/De-Figueiredo-C-2018-PhD-Thesis.pdf"

table_titles = c("imp" = "Proportion of population who strongly agree vaccines are important in 2016", 
                 "eff" = "Proportion of population who strongly agree vaccines are effective in 2016", 
                 "saf" = "Proportion of population who strongly agree vaccines are safe in 2016")

ind_list = c("imp","eff", "saf")
dd = ddd

for (ind in c("imp","eff", "saf")){#
  sel_ind = ind
  col_ind = c("VaxImp","VaxEff", "VaxSaf")
  names(col_ind) = c("imp","eff","saf")
  
  d = dd %>% #i want to keep the necessary columns and change the name of the one of the indicator to "ind". 
    select(Country, sex,  Age, col_ind[ind], MALI_WEIGHT, MAURITANIA_WEIGHT, NIGER_WEIGHTb, NIGERIA_WEIGHT, SENEGAL_WEIGHT, BURKINA_WEIGHT)
  names(d)[4] = "indicator"
  countries = as.character(unique(d$Country))
  res_list = list()
  
  for (i in 1:length(countries)){
    country = countries[i]
    dc = d %>% 
      filter(Country == country) %>% 
      select("Country", "sex" ,"Age", indicator, w_vector[country])
    names(dc)[5] = "weight"
    

    d_sex = dc %>%  
      group_by(sex, indicator) %>% 
      #group_by(sex, VaxImp) %>% 
      summarise(ss = sum(weight)) %>% 
      mutate(weighted_perc = ifelse(sex=="Female", 
                                    ss*100/sum(dc$weight[dc$sex=="Female"]), 
                                    ss*100/sum(dc$weight[dc$sex=="Male"]))) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree\t") %>%
      select(sex, weighted_perc) %>% 
      rename("variable" = "sex")
    d_overall =  dc %>% 
      #group_by(VaxImp) %>% 
      group_by(indicator) %>% 
      summarise(ss = sum(weight)) %>% 
      mutate(weighted_perc = ss*100/sum(dc$weight)) %>% 
      #filter(VaxImp == "Strongly agree") %>% 
      filter(indicator == "Strongly agree\t") %>% 
      select(weighted_perc) %>% 
      mutate(variable = "overall") 
    d_age =  dc %>%
      #group_by(Age,VaxImp ) %>% 
      group_by(Age, indicator ) %>% 
      dplyr::summarise(ss = sum(weight) ) %>% 
      mutate(weighted_perc = ifelse(Age=="30-39", ss*100/sum(dc$weight[dc$Age=="30-39"]), 
                                    ifelse(Age=="50-59", ss*100/sum(dc$weight[dc$Age=="50-59"]),
                                           ifelse(Age=="40-49", ss*100/sum(dc$weight[dc$Age=="40-49"]),
                                                  ifelse(Age=="65+", ss*100/sum(dc$weight[dc$Age=="65+"]),
                                                         ifelse(Age=="60-64", ss*100/sum(dc$weight[dc$Age=="60-64"]),
                                                                ifelse(Age=="20-29", ss*100/sum(dc$weight[dc$Age=="20-29"]),ifelse(Age=="18-19", ss*100/sum(dc$weight[dc$Age=="18-19"]),NA)))))))) %>% 
      filter(indicator == "Strongly agree\t") %>%
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
    #select(Country,  overall,     Male,   Female,      `65+`,    `55-64`,    `45-54`, `35-44`,    `25-34`,    `18-24`)

  # publish to datawrapper
  table_id = "n0TzH"
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
