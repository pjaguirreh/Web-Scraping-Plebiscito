Web scraping - SERVEL - Plebiscito Constitucional 2020
======================================================
  
Este repositorio consiste en un ejercicio realizado luego del [plebiscito nacional de Chile](https://www.plebiscitonacional2020.cl/) para determinar si se "aprueba" o "rechaza" la redacción de una nueva constitución política.

Parte de los resultados de este ejercicio fueron explotados por:
  
  - [Información Transparente](http://informaciontransparente.com/plebiscito-transparente/)
  - [Juan Correa](https://twitter.com/Juanizio_C/status/1322177069741182976)

Los scripts presentes en este repositorio son:
  
  - `ScrapingServel.R`. Extracción de resultados nacionales a nivel de comuna
  - `ScrapingServelMesas.R`. Extracción de resultados nacionales a nivel de mesa de votación (incluye la información para agregar por los distintos niveles mayores que mesa)
  - `ScrapingServelTipoConvencion.R`. Este script no realiza ninguna extracción pero muestra un par de linea de código que permitirían editar los scripts anteriores para extraer resultados sobre el tipo de organo encargado de la redacción de la constitución.
  - `Geocoding.R`. Script que ocupa la [API de Google Maps](https://developers.google.com/maps/documentation/geocoding/overview) para asignar coordenadas a los locales de votación de la región metropolitana. **Es necesario tener una _key_ personal para utilizar la API**
  
También se incluye una carpeta `mapa interactivo` que genera y publica un mapa interactivo de las votaciones en la región metropolitana.
