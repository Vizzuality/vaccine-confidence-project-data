# vaccine specific
library(plyr)
library(dplyr)
library(reshape2)
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
dir_list = c("2015_EOY","2016_Sahel","2018_EU","2019_EOY", "2019_WGM", "2020_AfricaCDC", "2020_EU", "2020_Janssen", "2020_UK")

data_path = paste0("data/VCP/", dir_list[9], "/Data/")
file_list = dir(data_path,patter = "ranking")
names(file_list) = c("no", "unsure_no", "unsure_yes", "yes")

res_list = list()
for (f in 1:length(file_list)){
  dd = read.csv(paste0(data_path,file_list[f]))
  d =  dd %>% 
    select(NUTS3_CODE, mean) 
  res_list[[f]] = d
  names(res_list)[f] = names(file_list)[f]
}
res_df = ldply(res_list, .id = "response") %>% 
  left_join(subset(dd, select = c("NUTS3_CODE", "NUTS3_REGION")))  %>% 
  dcast(NUTS3_CODE+ NUTS3_REGION ~ response, value.var = "mean")
  
write.csv(res_df, "outputs/uk_covid_2020.csv")

