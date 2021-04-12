#devtools::install_github("munichrocker/DatawRappr")
library(DatawRappr)
library(magick)
# connect to the API and authenticate
token = read.table(".env", sep = "=") %>% 
  filter(V1 == "DATAWRAPPER_TOKEN") %>% 
  select(V2)
datawrapper_auth(api_key = token[1,1])

# create an empty chart
dw_create_chart(title = "portugal API sa",
                type = "d3-lines")


#here we could have a loop, each loop is a country: the data is uploaded, it is formatted, it is published and exported as png.
# upload the data


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


chart_id = "mgy8T"
good_graph_visualize = dw_retrieve_chart_metadata(chart_id)
#v_settings = good_graph_visualize$content$metadata$visualize

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
  image_write(p, path = paste0(sel_country,"importance.png"), format = "png")
}




############
## testing
df = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/mmc2/model_fit_safe-Table 1.csv") %>% 
  filter(country.or.territory == "Portugal" & response == "strongly agree")
dw_data_to_chart(x = df, chart_id = "Vm15J")


dw_publish_chart(chart_id = "Vm15J")






new_graph_portugal = dw_retrieve_chart_metadata("Vm15J")
length(good_graph_portugal$content$metadata$visualize)
length(new_graph_portugal$content$metadata$visualize)

dw_edit_chart("Vm15J", visualize = good_graph_portugal$content$metadata$visualize)

good_graph_portugal$content$metadata$data
