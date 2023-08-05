########################################################
# Visualización de datos con R y Shiny                 #
# Argenis Alain Gustavo Padilla Córdoba                #
# Tema 5: App Shiny del  Ingresos del Sistema del      # 
# Transporte Colectivo Metro de la  CDMX               #
########################################################

#Cargar librerias----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(dplyr)

#Cargar datos----

Ingresos_STC <- read.csv("Bases/STC/actu_ingreso.csv")


#Prepar Datos----

#str(Ingresos_STC)

#Cambio de tipo de caracter a fecha del campo Fecha----


Ingresos_STC$fecha <- as.Date(Ingresos_STC$fecha)

Ingresos_STC$ingreso <- as.numeric(Ingresos_STC$ingreso)


##crear totales----

Ingresos_totales <- Ingresos_STC %>% 
  group_by(fecha, linea) %>% 
  summarise(tipo_ingreso="Todas",
            ingreso= sum(ingreso, na.rm = T)) %>% 
  ungroup() %>% 
  select(fecha, tipo_ingreso, linea, ingreso)


# Crear base completa----
Ingresos_STC2 <- rbind(Ingresos_STC, Ingresos_totales)

##Creacion de datos de variable de interes ----





Ingresos_STC2 <- Ingresos_STC2 %>% 
  mutate(Año = format(fecha, "%Y"))

#Vectores de opciones ----

##Lineas de metro ----
lineas <- unique(Ingresos_STC2$linea)

## años----

años <- unique(Ingresos_STC2$Año)


Tipo_ingreso <- unique(Ingresos_STC2$tipo_ingreso)

# Esructura de ShinyApp----

##Interface de usuario----


 

ui <- fluidPage(
  theme = shinytheme('superhero'),
  #titulo de de la app incluyenndo logo
  headerPanel(div(img(src= "LogoMetro.png" ,
                      height="10%",
                      width= "10%"), "Ingreso del STC Metro del 2012 al 2023"),
              windowTitle = "Mi tablero"),
  
  
  #Apariencia de pestañas de la app
  tabsetPanel( type = "tabs",
               tabPanel('Gráficos: Ingresos por línea',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              inputId = "Linea",
                              label = "Seleccione Línea",
                              choices = lineas
                            )
                          ),
                          mainPanel(plotOutput('grafico1'))
                        )),
               tabPanel('Tablas: ingresos por linea',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "Linea2",
                            label =  "Seleccione Linea",
                            choices =  lineas
                          )
                        ),
                        mainPanel(tableOutput("Tabla1"))
                      )),
               
               tabPanel('Gráficos: Ingresos por tipo',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              inputId = "Tipo",
                              label = "Seleccione Tipo de ingreso",
                              choices = Tipo_ingreso
                            )
                          ),
                          mainPanel(plotOutput('grafico1'))
                        )),
               tabPanel('Tablas: ingresos por tipo',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              inputId = "Tipo",
                              label =  "seleccione tipo de ingreso",
                              choices =  Tipo_ingreso
                            )
                          ),
                          mainPanel(tableOutput("Tabla1"))
                        ))
               
               
               )
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)





