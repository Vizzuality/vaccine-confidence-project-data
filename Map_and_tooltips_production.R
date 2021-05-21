# this script is for the datasets that only have spatio temporal information
# this script uses the world data and creates the three maps, one for importance, one for safety, one for efficiency. Inside the map, the tooltips have the temporal information. 
# it probably makes sense to create the process as functions to be as clean as possible.
setwd("/Users/gretacvega/Documents/GitHub/vcp/")
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(DatawRappr)
library(magick)
#library(git2r)

# connect to the API and authenticate
token = read.table(".env", sep = "=") %>% 
  filter(V1 == "DATAWRAPPER_TOKEN") %>% 
  select(V2)
datawrapper_auth(api_key = token[1,1])


# Data preparation
study = "mmc2"
study_source_name  = "Figueiredo et al. 2020"
study_source_url = "https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)31558-0/fulltext"

# for each indicator there is one csv file

fit_files = list.files(paste0("data/",study), pattern ="model_fit" )
names(fit_files) = c("eff", "imp", "saf")
titles = c("imp" = "Importance in ", "eff" = "Efficiency in ", "saf" = "Safety in ")
map_titles = c("imp" = "Proportion of population who strongly agree vaccines are important in 2020", 
               "eff" = "Proportion of population who strongly agree vaccines are efficient in 2020", 
               "saf" = "Proportion of population who strongly agree vaccines are safe in 2020")

for (ind in c("eff", "imp", "saf")){

# select one indicator
sel_ind = ind
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



# Create tooltop time series: using the datawrapper API to create images, save the images locally and push them to github. Get the url of the images and create the trend information to create d_map

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
#git2r::push(repo, credentials = cred) # i need to fix this, the pushing was done from iterm
cat("do git push \n")
# Gathering the repository images url to add to the map table d_map

github_folder = paste0("https://raw.githubusercontent.com/Vizzuality/vaccine-confidence-project-data/main/", "output_tooltip/", study, "/", sel_ind, "/")
img_files = data.frame(fileName = dir(paste0("output_tooltip/", study, "/", sel_ind))) %>% 
  separate(fileName, 
           sep = "_", 
           into = c("country.or.territory", "question"),
           remove = FALSE) %>% 
  mutate(question = gsub('.{4}$', '', question),
         md_url = paste0("![](",github_folder,fileName,")"),
         url = paste0(github_folder,fileName))

d_map = d %>% 
  select(country.or.territory,  mean, time, response) %>% 
  filter(response == "strongly agree" & time %in% c(2015.833, 2020)) %>% 
  dcast(country.or.territory +  response ~ time, value.var = "mean") %>% 
  rename("mean" = "2020", "X2015.833" = "2015.833") %>% 
  mutate(since2015 = mean- X2015.833) %>% 
  select(country.or.territory,  since2015, mean) %>% 
  left_join(img_files)  

map_id = "0w5GJ"
new_map = dw_copy_chart(map_id) 
new_map_id = new_map$content$publicId

describe_list = new_map$content$metadata$describe
describe_list$`source-name` = study_source_name
describe_list$`source-url` = study_source_url

dw_data_to_chart(x = d_map, chart_id = new_map_id)
dw_edit_chart(new_map_id, 
              title = map_titles[sel_ind],
              describe = describe_list)
dw_publish_chart(chart_id = new_map_id)
}
