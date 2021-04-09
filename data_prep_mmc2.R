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
