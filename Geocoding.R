###################
# CARGAR PAQUETES #
###################
library(tidyverse)
library(ggmap)
library(sf)
library(mapview)

##########################################
# CARGAR PLANILLA DE RESULTADOS POR MESA #
##########################################
datos_mesa <- read_csv("DatosPlebiscitoMesa.csv")

# Filtrar mesas de la RM y generar string que se usará en google
locales <- datos_mesa %>%
  filter(Region == "METROPOLITANA SANTIAGO") %>% 
  distinct(Comuna, Local) %>% 
  mutate(paragoogle = paste(Comuna, Local, sep = "+"),
         paragoogle = str_remove_all(paragoogle, '\\"'))


#####################
# GEOREFERENCIACIÓN #
#####################
# Conexión a la API de google (acá deben generar una personal)
register_google(key = "PONER API DE GOOGLE", write = TRUE)

# Realizar consultas en google usando string generado
locales_direccion <- mutate_geocode(locales, location = paragoogle, output = "latlon")

########################
# PLANILLA CONSOLIDADA #
########################
datos_RM_mesa_coord <- datos_mesa %>% 
  mutate(paragoogle = paste(Comuna, Local, sep = "+"),
         paragoogle = str_remove_all(paragoogle, '\\"')) %>%
  filter(Region == "METROPOLITANA SANTIAGO") %>% 
  left_join(select(locales_direccion, paragoogle, lon, lat), by = "paragoogle") %>% 
  filter(!is.na(lon))

# Exportar planilla
datos_RM_mesa_coord %>% 
  write_excel_csv("DatosPlebiscitoMesaRMCoord.csv")

#####################
# MAPEAR RESULTADOS #
#####################
mapa_sf <- st_as_sf(datos_RM_mesa_coord, coords = c("lon", "lat"), crs = 4326)
mapview(mapa_sf)


