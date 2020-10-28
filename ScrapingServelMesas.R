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
rd$open()
rd$navigate(url)

# Ir a la pag específica de la extracción
rd$findElement(using = "css", 
               value = "body > div.container.body.ng-scope > div:nth-child(1) > div.col-md-3.margen_cero.visible-desktop.visible-tablet > div > ul > li:nth-child(3) > a"
)$clickElement()

##########################
# Crear listas de comuna #
##########################

# Objeto que identifica la lista de comunas
webElemComuna <- rd$findElement(using = "css", 
               value = "#selComunas")

# Función de ayuda (se usará para lista de circunscripciones, locales, y mesas)
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

# Generar lista de comunas
lista_comunas <- webElemComuna$getElementAttribute("outerHTML") %>% 
  limpiar_lista()

regiones_comunas <- read_csv("reg_com.csv")

lista_comunas <- lista_comunas %>% 
  mutate(nombre2 = str_replace_all(nombre, "Ñ", "N")) %>% 
  left_join(regiones_comunas, by = c("nombre2" = "Comuna")) %>%
  distinct(Region, nombre)

lista_comunas <- lista_comunas %>% 
  filter(Region == "METROPOLITANA DE SANTIAGO") %>% 
  mutate(inicial = str_sub(nombre, 1, 1)) %>% 
  arrange(inicial)

## EXTRAER INFORMACIÓN
# Este loop itera comuna>circunscripción>local>mesa

resultados <- tibble()
contar_fila <- 1
#lista_comunas <- lista_comunas[-c(1:4),]
for (i in seq_along(lista_comunas$inicial)){ # ITERACIÓN DE COMUNAS
  
  # inicial de la comuna
  ini_com <- lista_comunas$inicial[i]
  
  Sys.sleep(0.5)
  
  # insertar inicial en menú desplegable de web
  webElemComuna <- rd$findElement(using = "css", 
                                  value = "#selComunas")
  webElemComuna$sendKeysToElement(list(ini_com))
  
  Sys.sleep(0.5)
  
  # crear lista de circunscripción
  webElemCircunscripcion <- rd$findElement(using = "css", 
                                           value = "#selCircunscripcionElectorales")
  lista_circunscripcion <- webElemCircunscripcion$getElementAttribute("outerHTML") %>% 
    limpiar_lista()
  
  Sys.sleep(0.5)
  
  for (j in seq_along(lista_circunscripcion$inicial)){ # ITERACIÓN DE CIRCUNSCRIPCIONES PARA CADA COMUNA
    
    # inicial de la circunscripción
    ini_cir <- lista_circunscripcion$inicial[j]
    
    # insertar inicial en menú desplegable de web
    webElemCircunscripcion$sendKeysToElement(list(ini_cir))
    
    Sys.sleep(0.5)
    
    # crear lista de local
    webElemLocal <- rd$findElement(using = "css", 
                                   value = "#selLocalesVotacion")
    lista_local <- webElemLocal$getElementAttribute("outerHTML") %>% 
      limpiar_lista()
    
    Sys.sleep(0.5)
  
    for (q in seq_along(lista_local$inicial)){ # ITERACIÓN DE LOCALES PARA CADA CIRCUNSCRIPCIÓN
      
      # inicial de la mesa
      ini_local <- lista_local$inicial[j]
      
      # insertar inicial en menú desplegable de web
      webElemLocal$sendKeysToElement(list(ini_local))
      
      Sys.sleep(0.5)
      
      # crear lista de mesas
      webElemMesa <- rd$findElement(using = "css", 
                                    value = "#selMesasReceptoras")
      lista_mesas <- webElemMesa$getElementAttribute("outerHTML") %>% 
        limpiar_lista()
      
      Sys.sleep(0.5)
      
      for (k in seq_along(lista_mesas$inicial)){ #>> ITERACIÓN DE MESAS PARA CADA LOCAL
        
        # inicial de la mesa
        ini_mesa <- lista_mesas$inicial[k]
        
        # insertar inicial en menú desplegable de web
        webElemMesa$sendKeysToElement(list(ini_mesa))
        
        Sys.sleep(0.5)
        
        ###################
        # Extraer valores #
        ###################
        
        # Votos apruebo
        webElemApruebo <- rd$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(2) > td:nth-child(3) > small > span")
        apruebo <- webElemApruebo$getElementText()[[1]] %>% 
          str_remove("\\.") %>% 
          as.numeric()
        
        # Votos rechazo
        webElemRechazo <- rd$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(5) > td:nth-child(3) > small > span")
        rechazo <- webElemRechazo$getElementText()[[1]] %>% 
          str_remove("\\.") %>% 
          as.numeric()
        
        # Votos nulo
        webElemNulo <- rd$findElement(using = "css", value = "#basic-table > table > tfoot > tr:nth-child(2) > th:nth-child(2) > strong")
        nulo <- webElemNulo$getElementText()[[1]] %>% 
          str_remove("\\.") %>% 
          as.numeric()
        
        # Votos blanco
        webElemBlanco <- rd$findElement(using = "css", value = "#basic-table > table > tfoot > tr:nth-child(3) > th:nth-child(2) > strong")
        blanco <- webElemBlanco$getElementText()[[1]] %>% 
          str_remove("\\.") %>% 
          as.numeric()
        
        ######################
        # Guardar resultados #
        ######################
        resultados[contar_fila, 1] <- lista_comunas$nombre[i]
        resultados[contar_fila, 2] <- lista_circunscripcion$nombre[j]
        resultados[contar_fila, 3] <- lista_local$nombre[q]
        resultados[contar_fila, 4] <- lista_mesas$nombre[k]
        resultados[contar_fila, 5] <- apruebo
        resultados[contar_fila, 6] <- rechazo
        resultados[contar_fila, 7] <- nulo
        resultados[contar_fila, 8] <- blanco
        
        # Para evaluar avance
        resultados %>% slice(contar_fila) %>% print()
        print(paste(lista_comunas$nombre[i], lista_circunscripcion$nombre[j], 
                    lista_local$nombre[q], 
                    nrow(lista_mesas), k), 
              sep = ";")
        
        # Contador de fila para almacenar de forma correcta
        contar_fila <- contar_fila + 1
      }
      
      Sys.sleep(1)
      
    }
    
    Sys.sleep(1)
    
  }
  
  Sys.sleep(3)
  
}

# SE CORTÓ EN COBUN
# SE CORTÓ EN COLLIPULLI
# SE CORTÓ EN COQUIMBO
# SE CORTÓ EN CUNCO

# Ajustar data frame
resultados %>% 
  rename("Comuna" = 1,
         "Circunscripción" = 2,
         "Local" = 3,
         "Mesa" = 4,
         "Apruebo" = 5,
         "Rechazo" = 6,
         "Nulo" = 7,
         "Blanco" = 8) %>% write_excel_csv("DatosPlebiscitoMesaRM.csv")





rd$closeServer();rd$close();remDr$server$stop()
