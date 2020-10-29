###################
# CARGAR PAQUETES #
###################

library(tidyverse)
library(ggmap)
library(sf)
library(mapview)
library(leaflet)

datos_mesa <- read_csv("DatosPlebiscitoMesa.csv")

locales <- datos_mesa %>%
  filter(Region == "METROPOLITANA SANTIAGO") %>% 
  distinct(Comuna, Local) %>% 
  mutate(paragoogle = paste(Comuna, Local, sep = "+"),
         paragoogle = str_remove_all(paragoogle, '\\"'))

register_google(key = "PONER API DE GOOGLE", write = TRUE)

locales_direccion <- mutate_geocode(locales, location = paragoogle, output = "latlon")

datos_RM_datos_mesa <- datos_mesa %>% 
  mutate(paragoogle = paste(Comuna, Local, sep = "+"),
         paragoogle = str_remove_all(paragoogle, '\\"')) %>%
  filter(Region == "METROPOLITANA SANTIAGO") %>% 
  left_join(select(locales_direccion, paragoogle, lon, lat), by = "paragoogle") %>% 
  filter(!is.na(lon))

datos_RM_datos_mesa %>% 
  leaflet() %>% 
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(Comuna), label = ~as.character(Apruebo))

mapview(mapa_sf)


