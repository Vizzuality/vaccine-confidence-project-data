library(plyr)
library(dplyr)
library(reshape2)
library(foreign)
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[1], "/Data/")
f = dir(data_path)
dd = read.spss(paste0(data_path,f), to.data.frame=TRUE)
#"""d1""          ""d2""  ""d3""          ""d4""          ""d5""          ""d6"" have the demographic questions ""q9.1""        ""q9.2""        ""q9.3""        ""q9.4"" have the answers to our questions"


d = dd %>% 
  select(country, d1, d2, q9.1,q9.2, q9.3, weight) %>% 
  na.omit

names(d) = c("Country", "sex" ,"Age", "VaxImp", "VaxSaf", "VaxEff", "weight")
head(d)





# map
countries = as.character(unique(d$Country))
res_list = list()

for (i in 1:length(countries)){
  country = countries[i]
  dc = d %>% 
    filter(Country == country)
  
  d_overall =  d %>% 
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
  mutate(Neither = `Do not know / no response` + `Tend to agree` + `Tend to disagree`) %>% 
  select(Country, `Strongly agree`, Neither, `Strongly disagree`)

write.csv(res_df, "outputs/2015_EOY_map.csv")



replacement = c("Dr congo" = "Democratic Republic of Congo",
                "Egypt" =  "Arab Republic of Egypt",
                "Iran" = "Islamic Republic of Iran",
                "Ivory Coast" = "Côte d'Ivoire",
                "Kyrgyzstan" = "Kyrgyz Republic",
                "Laos" = "Lao People's Democratic Republic",
                "Palestine" = "West Bank and Gaza",
                "Republic of Congo" = "Congo",
                "Russia" = "Russian Federation",
                "Slovakia"   = "Slovak Republic",
                "Korea" = "Republic of Korea",
                "Swaziland"      = "Eswatini",
                "Syria" = "Syrian Arab Republic",
                "UK"  = "United Kingdom",
                "USA" = "United States of America",
                "Venezuela" = "R. B. de Venezuela",
                "Yemen" = "Republic of Yemen",
                "Gambia" = "The Gambia",
                "Macedonia" = "North Macedonia",
                "Bosnia & Herzegovina" = "Bosnia and Herzegovina")

d$Country = mapvalues(d$Country, from = names(replacement), to = replacement)

age_replacement = c("25 – 34" = "25-34",
                    "35 – 44" = "35-44",         
                    "18 – 24" = "18-24",        
                    "65+" = "65+",
                    "45 – 54" = "45-54",
                    "55 – 64" = "55-64"
  
)
d$Age = mapvalues(d$Age, from = names(age_replacement), to = age_replacement)

#tables
study_source_name = "State of Vaccine Confidence. EBioMedicine, 2016"
study_source_url = "https://www.thelancet.com/journals/ebiom/article/PIIS2352-3964(16)30398-X/fulltext"

table_titles = c("imp" = "Proportion of population who strongly agree vaccines are important in 2015", 
                 "eff" = "Proportion of population who strongly agree vaccines are effective in 2015", 
                 "saf" = "Proportion of population who strongly agree vaccines are safe in 2015")

ind_list = c("imp","eff", "saf")
dd = d
# select one indicator
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





