####################################################
################# Package ##########################
####################################################

library(tidyverse)
library(ggplot2)
library(httr)
library(jsonlite)
library(leaflet)
library(plotly)

####################################################
################ Constantes ########################
####################################################

worldcities <- read_csv("worldcities.csv")

url_base = "https://www.refugerestrooms.org/api"
url_byloc = "/v1/restrooms/by_location"
url_bydate = "/v1/restrooms/by_date"
url_bysearch= "/v1/restrooms/search"
url_all="/v1/restrooms"

urlLoc = paste0(url_base, url_byloc)
urlAll = paste0(url_base, url_all)

totalPage = 488


ville <- c("Tours", "London", "Berlin", "New-York")
lat <- c(47.390185, 51.509865, 52.529076, 40.764792)
lng <- c(0.689797, -0.118092, 13.388985, -73.985225)
tablo_coord <- data.frame(ville = ville, lat = lat, lng=lng)

load(file = "refuge_150222-final.RData")

###################################################
############## Fonctions ##########################
###################################################

################ Requetes to df ###################

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

################### Manip de df ###################

df_transfo <- function(df){
  df %>% select(c(name, street, city, state, accessible, unisex, directions, comment))
} 

################# Fonctions Map ###################

bigmap <- function(lng, lat){
  map = leaflet() %>% addTiles() %>% setView(lng, lat, zoom = 17)
  return(map)
}

coord_city <- function(obj){
  city_lat <- tablo_coord %>% filter(ville == obj) %>% pull(lat)
  city_lng <- tablo_coord %>% filter(ville == obj) %>% pull(lng)
  return(list(city_lng, city_lat))
}

################# Fonctions var_output ################

variable <- function(input){
  switch(
    input,
    "Tours" = "Tours",
    "London" = "London",
    "Berlin" = "Berlin",
    "New-York" = "New-York"
  )
}


################# Fonctions Output ################

ville_to_df <- function(ada, unisex, city){
  variable <- variable(city)
  coord <- coord_city(variable)
  query <- list(lng=coord[[1]], lat=coord[[2]], per_page=100, ada=ada, unisex=unisex)
  df_transfo <- req_to_df(urlLoc, query = query) %>% df_transfo()
  return(df_transfo)
}

ville_to_df2 <- function(coord, ada, unisex, city){
  query <- list(lng=coord[[1]], lat=coord[[2]], per_page=100, ada=ada, unisex=unisex)
  df_transfo <- req_to_df(urlLoc, query = query) %>% df_transfo()
  return(df_transfo)
}

display_map <- function(ada, unisex, city){
  variable <- variable(city)
  coord <- coord_city(variable)
  map = bigmap(coord[[1]],coord[[2]])
  query <- list(lng=coord[[1]], lat=coord[[2]], per_page=100, ada=ada, unisex=unisex)
  df <- req_to_df(urlLoc, query = query)
  mytext <- paste(
    "lng: ", df$long, "<br/>", 
    "lat: ", df$lat, "<br/>",
    "desc: ", df$directions, "<br/>",
    "comment: ", df$comment, "<br/>",
    "ada: ", df$accessible, sep="") %>%
    lapply(htmltools::HTML)
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion'
  )
  map %>%  addAwesomeMarkers(data = df, lng = df$long, lat = df$lat, label = mytext, icon = icons )
}

display_map2 <- function(ada, unisex, lat, long) {
  map = bigmap(lng=long,lat=lat)
  query <- list(lng=long, lat=lat, per_page=100, ada=ada, unisex=unisex)
  df <- req_to_df(urlLoc, query = query)
  mytext <- paste(
    "lng: ", df$long, "<br/>", 
    "lat: ", df$lat, "<br/>",
    "desc: ", df$directions, "<br/>",
    "comment: ", df$comment, "<br/>",
    "ada: ", df$accessible, sep="") %>%
    lapply(htmltools::HTML)
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion'
  )
  map %>%  addAwesomeMarkers(data = df, lng = df$long, lat = df$lat, label = mytext, icon = icons )}

######### brouillon #########

display_classement <- function() {res %>% group_by(country) %>% ggplot() +
  aes(x = fct_lump(fct_rev(fct_infreq(country)),10)) +
  geom_bar()+coord_flip()}

