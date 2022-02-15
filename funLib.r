####################################################
################# Package ##########################
####################################################

library(tidyverse)
library(ggplot2)
library(httr)
library(jsonlite)
library(leaflet)

####################################################
################ Constantes ########################
####################################################

url_base = "https://www.refugerestrooms.org/api"
url_byloc = "/v1/restrooms/by_location"
url_bydate = "/v1/restrooms/by_date"
url_bysearch= "/v1/restrooms/search"
url_all="/v1/restrooms"

urlLoc = paste0(url_base, url_byloc)
urlAll = paste0(url_base, url_all)

query_Tours <- list(lng="0.689797", lat="47.390185")
query_Londres <- list(lat="51.509865", lng="-0.118092", per_page="100")


latTours = 47.390185
longTours = 0.689797

totalPage = 488

###################################################
############## Fonctions ##########################
###################################################

req_to_df <- function(url, query){
  resp <- GET(url, query = query)
  jsonRespText <- content(resp,as="text")
  json_cont <- fromJSON(jsonRespText)
  return(json_cont)
}

alldataReq <- function(pagemax){
  dfA <- data.frame()
  for (i in 1:pagemax){
    query <- list(per_page=100, page=i)
    dfB <- req_to_df("https://www.refugerestrooms.org/api/v1/restrooms",
                     query = query)
    dfA <- rbind(dfA, dfB)}
  return(dfB)
}

df_transfo <- function(df){
  df %>% select(c(name, street, city, state, accessible, unisex, directions, comment))
} 

bigmap <- function(lng, lat){
  map = leaflet() %>% addTiles() %>% setView(lng, lat, zoom = 17)
  return(map)
}

pinMarker <- function(df){
  
}
