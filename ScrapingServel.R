###################
# CARGAR PAQUETES #
###################

library(RSelenium)
library(rvest)
library(openxlsx)
library(tidyverse)

#######################
# CONECTAR A SELENIUM #
#######################

remDr <- rsDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browser = "firefox"
)

rd <- remDr$client

# Ir a la web del SERVEL
url <- 'http://www.servelelecciones.cl/'
#rd$open()
rd$navigate(url)

# Ir a la pag específica de la extracción
rd$findElement(using = "css", 
               value = ".menu_division > li:nth-child(3) > a:nth-child(1)"
)$clickElement()
# body > div.container.body.ng-scope > div:nth-child(1) > div.col-md-3.margen_cero.visible-desktop.visible-tablet > div > ul > li:nth-child(3) > a

#####################################
# Crear listas de regiones y comuna #
#####################################
# Función de ayuda
limpiar_lista <- function(x){
  x %>% 
    str_extract_all(">(.*?)<") %>% 
    data.frame() %>% as_tibble() %>% 
    rename(nombre = 1) %>% 
    filter(nombre != "><",
           !str_detect(nombre, "\\.\\.\\.")) %>% 
    mutate(nombre = str_remove(nombre, ">"),
           nombre = str_remove(nombre, "<")) %>% 
    mutate(inicial = str_sub(nombre, 1, 1)) %>% 
    arrange(inicial)
}

# Objeto que identifica la lista de regiones y comunas
webElemRegion <- rd$findElement(using = "css", 
                                value = "#selRegion")

# Extraer lista y limpiar
lista_regiones <- webElemRegion$getElementAttribute("outerHTML") %>% 
  limpiar_lista()

# Extraer lista de comunas para cada región
reg_com <- tibble()
for (r in seq_along(lista_regiones$inicial)){
  
  # inicial de la region
  ini_reg <- lista_regiones$inicial[r]
  
  Sys.sleep(0.5)
  
  # insertar inicial en menú desplegable de web
  webElemRegion <- rd$findElement(using = "css", 
                                  value = "#selRegion")
  webElemRegion$sendKeysToElement(list(ini_reg))
  
  Sys.sleep(0.5)
  
  # Generar lista de comunas
  webElemComuna <- rd$findElement(using = "css", 
                                  value = "#selComunas")
  lista_comunas <- webElemComuna$getElementAttribute("outerHTML") %>% 
    limpiar_lista()
  
  Sys.sleep(0.5)
  
  com_r <- lista_comunas %>% 
    mutate(reg = lista_regiones$nombre[r])
  
  Sys.sleep(0.5)
  
  reg_com <- bind_rows(reg_com, com_r)
}

reg_com <- reg_com %>% 
  arrange(nombre)

## ---------------------------------
## GENERAR DATA FRAME CON RESULTADOS
## ---------------------------------

rd$navigate(url)

# Ir a la pag específica de la extracción
rd$findElement(using = "css", 
               value = ".menu_division > li:nth-child(3) > a:nth-child(1)"
)$clickElement()

datos_comuna <- tibble() # data frame que almacenará resultados
for (i in seq_along(reg_com$inicial)){
  
  # inicial de la comuna
  c <- reg_com$inicial[i]
  
  # insertar inicial en menú desplegable de web
  webElemComuna <- rd$findElement(using = "css", 
                                  value = "#selComunas")
  webElemComuna$sendKeysToElement(list(c))
  
  # Votos apruebo
  webElemApruebo <- rd$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(2) > td:nth-child(3) > small > span")
  apruebo <- webElemApruebo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(0.5)
  
  # Votos rechazo
  webElemRechazo <- rd$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(5) > td:nth-child(3) > small > span")
  rechazo <- webElemRechazo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(0.5)
  
  # Votos nulo
  webElemNulo <- rd$findElement(using = "css", value = "#basic-table > table > tfoot > tr:nth-child(2) > th:nth-child(2) > strong")
  nulo <- webElemNulo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()  
  
  Sys.sleep(0.5)
  
  # Votos blanco
  webElemBlanco <- rd$findElement(using = "css", value = "#basic-table > table > tfoot > tr:nth-child(3) > th:nth-child(2) > strong")
  blanco <- webElemBlanco$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()   
  
  # Ingresar valores en data frame
  datos_comuna[i,1] <- reg_com$reg[i]
  datos_comuna[i,2] <- reg_com$nombre[i]
  datos_comuna[i,3] <- apruebo
  datos_comuna[i,4] <- rechazo
  datos_comuna[i,5] <- nulo
  datos_comuna[i,6] <- blanco
  
  # Reportar en cada iteración resultados
  datos_comuna %>% 
    slice(i) %>% print()
  
  cbind(nrow(dplyr::distinct(dplyr::select(datos_comuna, `...1`))), nrow(datos_comuna)) %>% print()
  
  Sys.sleep(0.5)
}

# Ajustar y exportar data frame
datos_comuna %>% 
  rename("Region" = 1,
         "Comuna" = 2,
         "Apruebo" = 3,
         "Rechazo" = 4,
         "Nulo" = 5,
         "Blanco" = 6
  ) %>%
  mutate(Region = str_remove(Region, "DE "),
         Region = str_remove(Region, "DEL "),
         Validos = Apruebo+Rechazo,
         Total = Validos+Nulo+Blanco,
         Validos_per = Validos/Total,
         Apruebo_per = (Apruebo/Validos)*100,
         Rechazo_per = (Rechazo/Validos)*100,
         Nulo_per = (Nulo/Total)*100,
         Blanco_per = (Blanco/Total)*100) %>% 
  select(Region, Comuna, Total, Validos, Apruebo, Rechazo, Nulo, Blanco, 
         Validos_per, Apruebo_per, Rechazo_per, Nulo_per, Blanco_per) %>% 
  dplyr::arrange(Region, Comuna) %>% 
  write_excel_csv("datos/DatosPlebiscito.csv")

rd$closeServer();rd$close();remDr$server$stop()

