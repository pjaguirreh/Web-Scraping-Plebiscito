# Cargar paquetes
library(RSelenium)
library(rvest)
library(openxlsx)
library(tidyverse)

## ------------------------
## OBTENER LISTA DE COMUNAS
## ------------------------

comunas_web <- read_html("https://es.wikipedia.org/wiki/Anexo:Comunas_de_Chile")

comunas_sucio <- comunas_web %>% 
  html_nodes("tr :nth-child(2)") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(4:nrow(.)) 

com <- comunas_sucio %>% 
  slice(seq(1, nrow(.), by = 2)) %>% 
  dplyr::rename("comuna" = value)

reg <- comunas_sucio %>% 
  slice(seq(2, nrow(.), by = 2)) %>% 
  dplyr::rename("region" = value)

# Data frame con lista de comunas y regiones. La inicial se usará
# para interactuar con el menú desplegable de SERVEL
region_comuna <- bind_cols(reg, com) %>% 
  mutate(comuna = str_remove(comuna, "\\n")) %>% 
  arrange(comuna) %>% 
  mutate(inicial = str_sub(comuna, 1, 1))

## -------------------
## CONECTAR A SELENIUM
## -------------------

remDr <- rsDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "chrome"
)

# Ir a la web del SEVEL
url <- 'http://www.servelelecciones.cl/'
remDr$open()
remDr$navigate(url)

## ---------------------------------
## GENERAR DATA FRAME CON RESULTADOS
## ---------------------------------

datos_comuna <- tibble() # data frame que almacenará resultados
for (i in seq_along(region_comuna$inicial)){
  
  # inicial de la comuna
  c <- region_comuna$inicial[i]
  
  # insertar inicial en menú desplegable de web
  webElem$sendKeysToElement(list(c))
  
  # Nombre de la comuna 
  webElemComuna <- remDr$findElement(using = "css", value = "#divCabecera > h5 > span:nth-child(1)")
  comuna <- webElemComuna$getElementText()[[1]] %>% 
    str_remove("Votación por Comuna ")
  
  Sys.sleep(0.5)
  
  # Votos apruebo
  webElemApruebo <- remDr$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(2) > td:nth-child(3) > small > span")
  apruebo <- webElemApruebo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(0.5)
  
  # Votos rechazo
  webElemRechazo <- remDr$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(5) > td:nth-child(3) > small > span")
  rechazo <- webElemRechazo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(0.5)
  
  # Votos nulo
  webElemNulo <- remDr$findElement(using = "css", value = "#basic-table > table > tfoot > tr:nth-child(2) > th:nth-child(2) > strong")
  nulo <- webElemNulo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()  
  
  Sys.sleep(0.5)
  
  # Votos blanco
  webElemBlanco <- remDr$findElement(using = "css", value = "#basic-table > table > tfoot > tr:nth-child(3) > th:nth-child(2) > strong")
  blanco <- webElemBlanco$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()   
  
  # Ingresar valores en data frame
  datos_comuna[i,1] <- comuna
  datos_comuna[i,2] <- apruebo
  datos_comuna[i,3] <- rechazo
  datos_comuna[i,4] <- nulo
  datos_comuna[i,5] <- blanco
  
  # Reportar en cada iteración resultados
  datos_comuna %>% 
    slice(i) %>% print()
  
  Sys.sleep(0.5)
}

# Cambiar nombre de las columnas
names(datos_comuna) <- c("Comuna", "Apruebo", "Rechazo", "Nulo", "Blanco")

# Generar columnas nuevas y ordenar
datos_comuna_fin <- datos_comuna %>% 
  mutate(Validos = Apruebo+Rechazo,
         Total = Validos+Nulo+Blanco,
         Validos_per = Validos/Total,
         Apruebo_per = (Apruebo/Validos)*100,
         Rechazo_per = (Rechazo/Validos)*100,
         Nulo_per = (Nulo/Total)*100,
         Blanco_per = (Blanco/Total)*100) %>% 
  dplyr::arrange(Comuna) %>% 
  bind_cols(
region_comuna %>% 
  dplyr::arrange(comuna) %>% 
  dplyr::select(region) %>% 
  mutate(region = str_to_upper(region)) %>% 
  dplyr::rename("Region" = region)
) %>% 
  dplyr::select(Region, Comuna, Total, Validos, Apruebo, Rechazo, Nulo, Blanco, 
                Validos_per, Apruebo_per, Rechazo_per, Nulo_per, Blanco_per) %>% 
  dplyr::arrange(Region, Comuna) 

# --------
# EXPORTAR
# --------
datos_comuna_fin %>% 
  write_excel_csv("C:/Users/pjagu/OneDrive/Documentos/GitHub/Web-Scraping-Plebiscito/DatosPlebiscito.csv")


