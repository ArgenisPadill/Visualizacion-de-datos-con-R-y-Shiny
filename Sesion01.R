########################################################
# Taller: Visualización de datos con R y Shiny         #
# Argenis ALain Gustavo Padilla Cordoba                #
# Sesión: 01                                           #
# Fecha: 19/06/2023                                    #
# Tema 1: Manipulación de bases de datos               #
########################################################

# Instalación de paqueterías ----

# Instala el paquete 'readxl' y sus dependencias. 
# El paquete 'readxl' se utiliza para leer archivos de Excel (.xls y .xlsx) en R
install.packages("readxl", dependencies = T)

# Instala el paquete 'haven' y sus dependencias. 
# El paquete 'haven' se utiliza para leer y escribir diferentes formatos de datos, incluyendo Stata, SPSS y SAS
install.packages("haven", dependencies = T)

# Instala el paquete 'tidyverse' y sus dependencias. 
# 'tidyverse' es una colección de paquetes de R para ciencia de datos que incluye dplyr, ggplot2, tidyr, readr, purrr y otros
install.packages("tidyverse", dependencies = T)

# Instala el paquete 'foreign' y sus dependencias.
# 'foreign' es un paquete que proporciona funciones para leer y escribir datos de otros sistemas estadísticos como Minitab, S, SAS, SPSS, Stata, etc.
install.packages("foreign", dependencies = T)


# Librerías a utilizar ----

library(tidyverse)
library(haven)
library(foreign)
library(readxl)

# Importando base de datos -----

## Conociendo ruta de trabajo ----

getwd()

## Importando CSV ----

ENIGH <- read.csv("Bases/ENIGH/concentradohogar.csv",
                  encoding = "UTF-8")

## Importando dta ----

ENIGH <- read.dta("Bases/ENIGH/concentradohogar.dta")

## Importando sav -----

ENIGH <- read_sav("Bases/ENIGH/concentradohogar.sav")

## Importando DBF -----

ENIGH <- read.dbf("Bases/ENIGH/concentradohogar.dbf")




## Función especial para descargar ENIGH e importarla directo ------

# Funciones para descargar encuestas ----

## Función "ENIGH nueva serie encuestas del 2016 al 2020" ----

#https://inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_viviendas_dbf.zip

# Función para descargar y leer diferentes secciones de la base de datos del ENIGH (Encuesta Nacional de Ingresos y Gastos de los Hogares) de México 
enigh_inegi_NS <- function(año = NA, 
                           # Lista de secciones posibles para descargar de la base de datos
                           section = c("hogares", "poblacion", "gastohogar", "erogaciones", "gastotarjetas", "ingresos", "gastopersona", 
                                       "trabajos", "agro", "noagro", "concentradohogar", "agroproductos", "agroconsumo", "agrogasto", 
                                       "noagroimportes", "ingresos_JCF")){ 
  # URL base de donde se descargaran los datos
  url_enigh = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/"
  url_base  = paste0(url_enigh, año, "/microdatos/enigh", año, "_ns_",section,"_dbf.zip")
  
  # Comprueba si el directorio 'datos' existe. Si no, lo crea
  if (file.exists(paste0(getwd(), "/datos"))){
    zipdir = paste0(getwd(), "/datos")
  } else {
    dir.create(paste0(getwd(), "/datos"))
    zipdir = paste0(getwd(), "/datos")   
  }
  
  # Comprueba si el directorio 'zip' existe. Si no, lo crea
  if (file.exists(paste0(getwd(), "/zip"))){
    temp_enigh = paste0(getwd(), "/zip")
  } else {
    dir.create(paste0(getwd(), "/zip"))
    temp_enigh = paste0(getwd(), "/zip")   
  }
  
  # Descarga el archivo de datos comprimido y lo almacena en el directorio 'zip'
  file_name <- basename(temp_enigh)
  utils::download.file(url_base, paste(temp_enigh, file_name, sep="/"))
  
  # Descomprime el archivo descargado y lo almacena en el directorio 'datos'
  utils::unzip(paste(temp_enigh, file_name, sep="/"), exdir=zipdir)
  
  # Carga los datos correspondientes a la sección especificada como parámetro
  # Lee el archivo .dbf correspondiente a la sección especificada y lo devuelve
  if(section == "viviendas"){
    viviendas <-foreign::read.dbf(paste0(zipdir, "\\", section, ".dbf"))
    return(viviendas)
  }
  
  # Repite el proceso de lectura para cada sección posible
  # Aquí se está realizando un análisis de casos, donde para cada valor posible de 'section', se lee el archivo correspondiente
  # Otros casos siguientes ...
  
  # Ejemplo de uno de los casos:
  else if (section =="ingresos_JCF"){
    ingresos_JCF<-foreign::read.dbf(paste0(zipdir, "\\", section, ".dbf"))
    return(ingresos_JCF)                          
  }
}

# Llamada a la función para obtener la sección 'concentradohogar' del año 2020
concentradohogar <- enigh_inegi_NS(año = "2020", section ="concentradohogar")


# Crear columnas o variables ------

x <- 10

names(ENIGH)


### Para ver todas las columnas 
utils::View(concentradohogar)
utils::View(ENIGH)




# Cambia el nombre de la columna 'ï..folioviv' a 'folioviv' en el conjunto de datos ENIGH
# La función rename() del paquete dplyr se utiliza para cambiar los nombres de las columnas de un data frame
# El operador %>% es conocido como el operador de tubería y se utiliza para encadenar varias operaciones juntas
ENIGH <- ENIGH %>% 
  rename(folioviv = `ï..folioviv`)



## Crear variable: CVE_ENT ----
### Opción 1 usando "$" ----

# Crea una nueva columna en el conjunto de datos ENIGH llamada 'CVE_ENT' 
# Esta columna se llena con los dos primeros caracteres de la columna 'folioviv'
# La función substr() se utiliza para extraer partes de las cadenas de texto.
ENIGH$CVE_ENT <- substr(ENIGH$folioviv, 1,2)

# Muestra las primeras seis entradas de la nueva columna 'CVE_ENT' en el conjunto de datos ENIGH
# La función head() se utiliza para obtener las primeras n filas de un objeto de datos
head(ENIGH$CVE_ENT)

# Muestra la columna 'folioviv' del conjunto de datos ENIGH
ENIGH$folioviv

# Devuelve la clase de la columna 'folioviv' en el conjunto de datos ENIGH
# La función class() se utiliza para obtener la clase de un objeto en R.
class(ENIGH$folioviv)

# Proporciona una descripción compacta de la estructura del conjunto de datos ENIGH
# La función str() es una herramienta útil para inspeccionar los datos y entender su estructura
str(ENIGH)


### Opción 2 usando "mutate" y "pipe" de dplyr -----

ENIGH <- ENIGH %>% #ctrl+shift+m
  mutate(CVE_ENT2 = substr(folioviv, 1, 2))


## Recodificación -----

ENIGH$CVE_ENT2[ENIGH$CVE_ENT2=="01"] <- "Aguascalientes"

ENIGH <- ENIGH %>% 
  mutate(CVE_ENT2 = case_when(CVE_ENT2=="01"~"Aguascalientes",
                              CVE_ENT2=="02"~"Baja California",
                              CVE_ENT2=="03"~"Baja California Sur",
                              CVE_ENT2=="04"~"Campeche",
                              CVE_ENT2=="05"~"Coahuila de Zaragoza",
                              CVE_ENT2=="06"~"Colima",
                              CVE_ENT2=="07"~"Chiapas",
                              CVE_ENT2=="08"~"Chihuahua",
                              CVE_ENT2=="09"~"Ciudad de México",
                              CVE_ENT2=="10"~"Durango",
                              CVE_ENT2=="11"~"Guanajuato",
                              CVE_ENT2=="12"~"Guerrero",
                              CVE_ENT2=="13"~"Hidalgo",
                              CVE_ENT2=="14"~"Jalisco",
                              CVE_ENT2=="15"~"México",
                              CVE_ENT2=="16"~"Michiacán de Ocampo",
                              CVE_ENT2=="17"~"Morelos",
                              CVE_ENT2=="18"~"Nayarit",
                              CVE_ENT2=="19"~"Nuevo León",
                              CVE_ENT2=="20"~"Oaxaca",
                              CVE_ENT2=="21"~"Puebla",
                              CVE_ENT2=="22"~"Querétaro",
                              CVE_ENT2=="23"~"Quinata Roo",
                              CVE_ENT2=="24"~"San Luis Potosí",
                              CVE_ENT2=="25"~"Sinaloa",
                              CVE_ENT2=="26"~"Sonora",
                              CVE_ENT2=="27"~"Tabasco",
                              CVE_ENT2=="28"~"Tamaulipas",
                              CVE_ENT2=="29"~"Tlaxcala",
                              CVE_ENT2=="30"~"Veracruz de Ignacio de la Llave",
                              CVE_ENT2=="31"~"Yucatán",
                              CVE_ENT2=="32"~"Zacatecas"))

ENIGH$NOM_ENT <- ENIGH$CVE_ENT

CVE_ENT <- unique(ENIGH$CVE_ENT)
Ent <-unique(ENIGH$CVE_ENT2)

for (i in 1:32) {
  ENIGH$NOM_ENT[ENIGH$NOM_ENT==CVE_ENT[i]] <- Ent[i]
}

# Exportación de avances de la clase 1 (Solo base de datos)-----

write.csv(ENIGH, file = "Bases/ENIGH/ENIGH_SciData.csv",
          row.names = FALSE, fileEncoding = "UTF-8")







# Tarea 1 -----

# a) Sustraer de ubica_geo la clave de entidad (primeros dos digitos)
# b) Recodificar la variable "Educa_jefe" con base en el descriptor de archivos
