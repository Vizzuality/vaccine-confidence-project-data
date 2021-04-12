#devtools::install_github("munichrocker/DatawRappr")
library(DatawRappr)
# connect to the API and authenticate
token = read.table(".env", sep = "=") %>% 
  filter(V1 == "DATAWRAPPER_TOKEN") %>% 
  select(V2)
datawrapper_auth(api_key = token[1,1])

# create an empty chart
dw_create_chart(title = "portugal API sa",
                type = "d3-lines")

# upload the data
df = read.csv("/Users/gretacvega/Documents/GitHub/vcp/data/mmc2/model_fit_safe-Table 1.csv") %>% 
  filter(country.or.territory == "Portugal" & response == "strongly agree")


dw_data_to_chart(x = df, chart_id = "Vm15J")





dw_publish_chart(chart_id = "Vm15J")

good_graph_portugal = dw_retrieve_chart_metadata("R0utq")
new_graph_portugal = dw_retrieve_chart_metadata("Vm15J")
length(good_graph_portugal$content$metadata$visualize)
length(new_graph_portugal$content$metadata$visualize)

dw_edit_chart("Vm15J", visualize = good_graph_portugal$content$metadata$visualize)


