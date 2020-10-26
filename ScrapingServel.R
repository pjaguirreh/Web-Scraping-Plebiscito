library(RSelenium)
library(tidyverse)
library(rvest)
library(openxlsx)

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

region_comuna <- bind_cols(reg, com) %>% 
  mutate(comuna = str_remove(comuna, "\\n")) %>% 
  arrange(comuna) %>% 
  mutate(inicial = str_sub(comuna, 1, 1))

remDr <- rsDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "chrome"
)
url <- 'http://www.servelelecciones.cl/'

remDr$open()
remDr$navigate(url)
webElem <- remDr$findElement(using = "id", value = "selComunas")
webElem$highlightElement()

nombre_comunas


webElemComuna <- remDr$findElement(using = "css", value = "#divCabecera > h5 > span:nth-child(1)")
#webElemComuna$highlightElement()

webElemApruebo <- remDr$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(2) > td:nth-child(3) > small > span")
#webElemApruebo$highlightElement()

webElemRechazo <- remDr$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(5) > td:nth-child(3) > small > span")
#webElemRechazo$highlightElement()

datos_comuna <- tibble()
for (i in seq_along(region_comuna$inicial)){
  
  c <- region_comuna$inicial[i]
  
  webElem$sendKeysToElement(list(c))
  
  webElemComuna <- remDr$findElement(using = "css", value = "#divCabecera > h5 > span:nth-child(1)")
  comuna <- webElemComuna$getElementText()[[1]] %>% 
    str_remove("Votación por Comuna ")
  
  Sys.sleep(1)
  webElemApruebo <- remDr$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(2) > td:nth-child(3) > small > span")
  apruebo <- webElemApruebo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(1)
  webElemRechazo <- remDr$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(5) > td:nth-child(3) > small > span")
  rechazo <- webElemRechazo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(1)
  
  datos_comuna[i,1] <- comuna
  datos_comuna[i,2] <- apruebo
  datos_comuna[i,3] <- rechazo
  
  Sys.sleep(1)
}

names(datos_comuna) <- c("Comuna", "Apruebo", "Rechazo")

datos_comuna_fin <- datos_comuna %>% 
  mutate(Total = Apruebo+Rechazo,
         Apruebo_per = (Apruebo/Total)*100,
         Rechazo_per = (Rechazo/Total)*100) %>% 
  dplyr::arrange(Comuna)

datos_comuna_fin %>% 
  bind_cols(
region_comuna %>% 
  dplyr::arrange(comuna) %>% 
  dplyr::select(region) %>% 
  mutate(region = str_to_upper(region)) %>% 
  dplyr::rename("Region" = region)
) %>% 
  dplyr::select(Region, everything()) %>% 
  dplyr::arrange(Region, Comuna) %>% write_csv("C:/Users/pjagu/OneDrive/Documentos/GitHub/Web Scraping Plebiscito/DatosPlebiscito.csv")
