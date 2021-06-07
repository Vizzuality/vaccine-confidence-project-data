## script to identify the important columns to use in each dataset shared by Alex. 
library(foreign)
dd = dir(path = "data/VCP", recursive = TRUE)
data_files = dd[grep("/Data/", dd)]
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for (i in 1:length(data_files)){
  f = data_files[i]
  ext = substrRight(f,3)
  p = paste0("data/VCP/",f)
  if(ext =="sav"){
    d = read.spss(p, to.data.frame=TRUE)
  }
  if(ext == "csv"){
    d = read.csv(p)
  }
  print(f)
  #print(names(d))
  print(head(d))
  cat("#######\n")
}

head(d)
names(d)
d$Q13r3[1:10]
i = 1

# 2020_AfricaCDC/Data/africacdc.sav
dd = d %>% 
  select(Adm1, Adm2, Gender, Coded_Respondent_Age, B1r1,                    B1r2,                    B1r3, B1r4,    B1r5, Weight_IvoryCoast) %>% 
  filter(Adm1 =="6 Niger")
  
unique(d$Adm1)
unique(d$Adm2)
