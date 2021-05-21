# this script is for the datasets that only have spatio temporal information
# this script uses the world data and creates the three maps, one for importance, one for safety, one for efficiency. Inside the map, the tooltips have the temporal information. 
# it probably makes sense to create the process as functions to be as clean as possible.
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(DatawRappr)
library(magick)
#library(git2r)

# connect to the API and authenticate
token = read.table(".env", sep = "=") %>% 
  filter(V1 == "DATAWRAPPER_TOKEN") %>% 
  select(V2)
datawrapper_auth(api_key = token[1,1])


# Data preparation


# for each indicator there is one csv file
study = "mmc2"
fit_files = list.files(paste0("data/",study), pattern ="model_fit" )
names(fit_files) = c("eff", "imp", "saf")
titles = c("imp" = "Importance in ", "eff" = "Efficiency in ", "saf" = "Safety in ")

# select one indicator
sel_ind = "imp"
d = read.csv(paste0("data/mmc2/", fit_files[sel_ind]))

# initial preparation of data for map widget

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


d_map = d %>% 
  filter(response == "strongly agree" & time == 2020) %>% 
  select(country.or.territory,  mean)


# Create tooltop time series: using the datawrapper API to create images, save the images locally and push them to github. Get the url of the images and create the trend information to join it to d_map

countries = unique(d$country.or.territory)

chart_id = "mgy8T" # this chart was created via the web and the visualise parameters are correct. 

good_graph_visualize = dw_retrieve_chart_metadata(chart_id)

# create directory to save images
dir.create(paste0("output_tooltip/", study, "/", sel_ind), recursive = TRUE)
# producing the 150 images takes 15 minutes
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

  dw_edit_chart(chart_id, title = paste0(titles[sel_ind], sel_country))
  #publish
  dw_publish_chart(chart_id = chart_id)
  #export
  p = dw_export_chart(chart_id, type = "png", unit = "px", width = 190, height = 159, scale = 1, plain = TRUE) 
  p_crop = image_crop(p, "380x228+0+90")
  image_write(p_crop, path = paste0("output_tooltip/", study, "/", sel_ind,"/",sel_ind, "_", sel_country,".png"), format = "png")
}

dir_to_push = paste0("output_tooltip/", study, "/", sel_ind, "/*")
commit_m = paste("added time series charts for", study, sel_ind)
repo <- git2r::repository(".")
git2r::add(repo = repo, dir_to_push)
git2r::commit(repo, commit_m)
git2r::push(repo, credentials = cred) # i need to fix this, the pushing was done from iterm



