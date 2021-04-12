library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
d = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/mmc2/model_fit_safe-Table 1.csv")
head(d)

#data wrangling to create tiny-charts that show the timeseries with the mean for each country
mean_d = d %>% 
  #filter(response == "strongly agree") %>% 
  select(country.or.territory, time, response, mean) %>% 
  dcast(country.or.territory ~  response + time, value.var = "mean")
mean_d[1:10,1:10]
diff(unique(d$time))

write.csv(mean_d,"/Users/gretacvega/Documents/GitHub/vcp/data/model_fit_safe_mean.csv")
names(d)

#data wrangling to show a line per country
# first column date, following columns a column per country
country_d_strong_agree = d %>%  
  filter(response == "strongly agree") %>% 
  select(country.or.territory, time,  mean) %>% 
  dcast( time ~country.or.territory , value.var = "mean")
write.csv(country_d_strong_agree,"/Users/gretacvega/Documents/GitHub/vcp/data/model_fit_safe_countries_strongly_agree.csv")


### create csv with all data
list.files()
fit_files = list.files("data/mmc2", pattern ="model_fit" )
f_list = list()
for (i in 1:length(fit_files)){
  d = read.csv(paste0("data/mmc2/",fit_files[i])) %>% 
    mutate(date = paste0(round(time), " - ", 
                         ifelse(round(12 * (time%%1)) == 0, 
                                1,
                                round(12 * (time%%1)))))
  f_list[[i]] = d
  names(f_list)[i] = fit_files[i]
}

names(f_list) = c("effective", "important", "safe")
df = ldply(f_list)
head(df)
#in arcgis create a concatenate variable with the question and the response
write.csv(df,"/Users/gretacvega/Documents/GitHub/vcp/data/model_fit_all_questions.csv")


# create wide tables for three questions
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
list.files()
fit_files = list.files("data/mmc2", pattern ="model_fit" )

for (i in 1:length(fit_files)){
  d = read.csv(paste0("data/mmc2/",fit_files[i]))
  question = strsplit(strsplit(fit_files[i],split = "_")[[1]][3], "-")[[1]][1]
  who_countries = d %>% 
    select(country.or.territory, who_region) %>% 
    distinct()
  mean_d = d %>% 
    #filter(response == "strongly agree") %>% 
    mutate(date = paste0(round(time), " - ", 
                         ifelse(round(12 * (time%%1)) == 0, 
                                1,
                                round(12 * (time%%1))))) %>% 
    select(country.or.territory, time, response, mean) %>% 
    dcast(country.or.territory ~  response + time, value.var = "mean") %>% 
    left_join(y= who_countries, by = "country.or.territory") %>% 
    mutate(question = question)
  fname = paste0("mean_",fit_files[i])
  write.csv(mean_d, fname)
}


## prepare data to have three lines per country, one with the mean and then the upper and lower limit. year	mean	lower	upper
d = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/mmc2/model_fit_safe-Table 1.csv")
head(d)
write.csv(d %>% 
  filter(country.or.territory == "Portugal" & response == "strongly agree"),
  "portugal_important_sa.csv")


d_sel = d %>% 
  filter(country.or.territory == "Portugal" ) %>% 
  select(time, response, mean, X95.HPDlow, X95.HPDhigh) 

d_sel_n = d_sel %>% 
  filter(response == "neither") %>% 
  mutate(mean_n = mean, low_n = X95.HPDlow, high_n = X95.HPDhigh) %>% 
  select(time, mean_n, low_n, high_n)
d_sel_sa = d_sel %>% 
  filter(response == "strongly agree") %>% 
  mutate(mean_sa = mean, low_sa = X95.HPDlow, high_sa = X95.HPDhigh) %>% 
  select(time, mean_sa, low_sa, high_sa)
d_sel_sd = d_sel %>% 
  filter(response == "strongly disagree") %>% 
  mutate(mean_sd = mean, low_sd = X95.HPDlow, high_sd = X95.HPDhigh) %>% 
  select(time, mean_sd, low_sd, high_sd)

d_sel_wide = d_sel_sa %>% 
  left_join(d_sel_n) %>% 
  left_join(d_sel_sd)
write.csv(d_sel_wide, "portugal_important_wide.csv", row.names = FALSE)


#clean country names to use with datawrapper: 
#Sudan, Andorra, Angola, Antigua and Barbuda, The Bahamas, Bahrain, Barbados, Belize, Bhutan, Brunei Darussalam, Cape Verde, Central African Republic, Congo, Cuba, Côte d'Ivoire, D. P. R. of Korea, Democratic Republic of Congo, Djibouti, Dominica, South Sudan, Equatorial Guinea, Eritrea, The Gambia, West Bank and Gaza, Greenland (Den.), Grenada, Guinea-Bissau, Guyana, Islamic Republic of Iran, Jamaica, Kiribati, Kyrgyz Republic, Lao People's Democratic Republic, Lesotho, Liechtenstein, Macau, SAR, Maldives, Marshall Islands, Federated States of Micronesia, Monaco, Nauru, Oman, Palau, United States of America, Qatar, Republic of Korea, Russian Federation, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines, Samoa, San Marino, São Tomé and Príncipe, Seychelles, Slovak Republic, Solomon Islands, Somalia, Suriname, Eswatini, Syrian Arab Republic, North Macedonia, Timor-Leste, Tonga, Trinidad and Tobago, Tuvalu, United Kingdom, Vanuatu, R. B. de Venezuela, Republic of Yemen, Hong Kong, SAR, Arab Republic of Egypt, Western Sahara

d = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/mmc2/model_fit_important-Table 1.csv")
# d_countries = sort(unique(d $country.or.territory))
# dw_available = sort(c("Sudan", "Andorra", "Angola", "Antigua and Barbuda", "The Bahamas", "Bahrain", "Barbados", "Belize", "Bhutan", "Brunei Darussalam", "Cape Verde", "Central African Republic", "Congo", "Cuba", "Côte d'Ivoire", "D. P. R. of Korea", "Democratic Republic of Congo", "Djibouti", "Dominica", "South Sudan", "Equatorial Guinea", "Eritrea", "The Gambia", "West Bank and Gaza", "Greenland (Den.)", "Grenada", "Guinea-Bissau", "Guyana", "Islamic Republic of Iran", "Jamaica", "Kiribati", "Kyrgyz Republic", "Lao People's Democratic Republic", "Lesotho", "Liechtenstein", "Macau", "SAR", "Maldives", "Marshall Islands", "Federated States of Micronesia", "Monaco", "Nauru", "Oman", "Palau", "United States of America", "Qatar", "Republic of Korea", "Russian Federation", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "São Tomé and Príncipe", "Seychelles", "Slovak Republic", "Solomon Islands", "Somalia", "Suriname", "Eswatini", "Syrian Arab Republic", "North Macedonia", "Timor-Leste", "Tonga", "Trinidad and Tobago", "Tuvalu", "United Kingdom", "Vanuatu", "R. B. de Venezuela", "Republic of Yemen", "Hong Kong", "SAR", "Arab Republic of Egypt", "Western Sahara"))
# d_intersect = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/country_codes_0.csv") %>% 
#   select(country.or.territory, NAME, FIPS_CNTRY, ISO_2DIGIT, ISO_3DIGIT,COUNTRYAFF)
# head(d_intersect)
# d_intersect %>% 
#   filter(country.or.territory!=NAME)
replacement = c("Democratic Republic of the Congo" = "Democratic Republic of Congo", 
"Egypt" =  "Arab Republic of Egypt",         
"Iran" = "Islamic Republic of Iran",           
"Ivory Coast" = "Côte d'Ivoire",
"Kyrgyzstan" = "Kyrgyz Republic",      
"Laos" = "Lao People's Democratic Republic",                        
"Palestine" = "West Bank and Gaza",                        
"Republic of Congo" = "Congo",                         
"Russia" = "Russian Federation",
"Slovakia"   = "Slovak Republic",                     
"South Korea" = "Republic of Korea",
"Swaziland"      = "Eswatini",                 
"Syria" = "Syrian Arab Republic",                          
"UK"  = "United Kingdom",                         
"USA" = "United States of America",
"Venezuela" = "R. B. de Venezuela",
"Yemen" = "Republic of Yemen",
"Gambia" = "The Gambia",
"Macedonia" = "North Macedonia")
d$country.or.territory = mapvalues(d$country.or.territory, from = names(replacement), to = replacement)
head(d)
d_map = d %>% 
  filter(response == "strongly agree" & time == 2020) %>% 
  select(country.or.territory,  mean)
dim(d_map)
write.csv(d_map, "world_important_stronglyagree_2020.csv", row.names = FALSE)
