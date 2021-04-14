#devtools::install_github("munichrocker/DatawRappr")
library(DatawRappr)
library(magick)
library(tidyr)
# connect to the API and authenticate
token = read.table(".env", sep = "=") %>% 
  filter(V1 == "DATAWRAPPER_TOKEN") %>% 
  select(V2)
datawrapper_auth(api_key = token[1,1])


d = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/mmc2/model_fit_important-Table 1.csv")
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
countries = unique(d$country.or.territory)


chart_id = "mgy8T" # this chart was created via the web and the visualise parameters are correct. 

good_graph_visualize = dw_retrieve_chart_metadata(chart_id)
#v_settings = good_graph_visualize$content$metadata$visualize
#rename series to Strongly Agree, Strongly Disagree and Neither.
for (i in 1:length(countries)){ #
  sel_country = countries[i]
  
  #prepare the data
  d_sel = d %>% 
    filter(country.or.territory == sel_country ) %>% 
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
  
  #update the data
  dw_data_to_chart(x = d_sel_wide, chart_id = chart_id)
  #change metadata (add country name to title)
  dw_edit_chart(chart_id, title = paste0("Importance in ", sel_country))
  #publish
  dw_publish_chart(chart_id = chart_id)
  #export
  p = dw_export_chart(chart_id, type = "png", unit = "px", width = 400, height = 240, scale = 1, plain = TRUE)
  image_write(p, path = paste0("charts_tooltip/",sel_country,"_importance.png"), format = "png")
}

# upload images to imgur, then get the name and the link from the uploaded images.
library(httr)

token_imgur = read.table(".env", sep = "=") %>% 
  filter(V1 == "IMGUR_TOKEN") %>% 
  select(V2)
bearer = paste("Bearer",token_imgur, sep = " ")
# there are several pages with data
getCountInJson <- httr::GET("https://api.imgur.com/3/account/gretacv/images/count",
                           accept_json(), 
                           add_headers('Authorization' = bearer))

res = list()
for (i in 0:as.integer(content(getCountInJson)$data/50)){
url = paste0('https://api.imgur.com/3/account/gretacv/images/', i)
getInfoInJson <- httr::GET(url, 
                           accept_json(), 
                           add_headers('Authorization' = bearer))


res[[i+1]] = ldply(lapply(content(getInfoInJson, as = "parsed")$data, unlist))


}
res_all = ldply(res) %>% 
  select(id, name) %>% 
  separate(name, 
           sep = "_", 
           into = c("country.or.territory", "question")) %>% 
  rename("imgur_id" = "id")

dim(res_all)
head(res_all)
# add the link information to the mean strongly agree df that is used for the map
d_map = d %>% 
  filter(response == "strongly agree" & time == 2020) %>% 
  select(country.or.territory,  mean) %>% 
  left_join(res_all) %>% 
  mutate(md_url = paste0("![](https://i.imgur.com/",imgur_id,".png)"))
write.csv(d_map, "world_important_stronglyagree_2020.csv", row.names = FALSE)

############
## testing
df = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/mmc2/model_fit_safe-Table 1.csv") %>% 
  filter(country.or.territory == "Portugal" & response == "strongly agree")

# create an empty chart
dw_create_chart(title = "portugal API sa",
                type = "d3-lines")


dw_data_to_chart(x = df, chart_id = "Vm15J")


dw_publish_chart(chart_id = "Vm15J")






new_graph_portugal = dw_retrieve_chart_metadata("Vm15J")
length(good_graph_portugal$content$metadata$visualize)
length(new_graph_portugal$content$metadata$visualize)

dw_edit_chart("Vm15J", visualize = good_graph_portugal$content$metadata$visualize)

good_graph_portugal$content$metadata$data
