########################################################
# Visualización de datos con R y Shiny                 #
# Argenis Alain Gustavo Padilla Córdoba                #
# Tema 5: App Shiny del  Ingresos del Sistema del      # 
# Transporte Colectivo Metro de la  CDMX               #
########################################################

#---- Cargar librerias----

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(dplyr)

#---- Cargar datos----

Ingresos_STC <- read.csv("Bases/STC/actu_ingreso.csv")


#---- Prepar Datos----

##----  Con dplyr ----

Ingresos_STC <- Ingresos_STC %>%
  mutate(
    fecha = as.Date(fecha),
    ingreso = as.numeric(ingreso)
  )
##----  Sin usar dplyr----


Ingresos_STC$fecha <- as.Date(Ingresos_STC$fecha)

Ingresos_STC$ingreso <- as.numeric(Ingresos_STC$ingreso)



#---- Crear totales----

Ingresos_totales <- Ingresos_STC %>% 
  group_by(fecha, linea) %>% 
  summarise(
    tipo_ingreso = "Todas",
    ingreso = sum(ingreso, na.rm = TRUE)
  ) %>%   ungroup() %>% 
  select(fecha, tipo_ingreso, linea, ingreso)


#----  Crear base completa y agregar variable de interés ----

Ingresos_STC2 <- rbind(Ingresos_STC, Ingresos_totales)

##---- Creacion de datos de variable de interes ----

Ingresos_STC2 <- rbind(Ingresos_STC, Ingresos_totales) %>% 
  mutate(Año = format(fecha, "%Y"))







#----  Vectores de líneas de metro, años y tipo de ingreso ----

lineas <- unique(Ingresos_STC2$linea)
años <- unique(Ingresos_STC2$Año)
Tipo_ingreso <- unique(Ingresos_STC2$tipo_ingreso)

#---- Cargar fuentes externas si es necesario (por ejemplo, para visualizaciones) ----

extrafont::loadfonts(device = "win")





#---- Construcción de la interfaz de usuario de la aplicación Shiny ----



 

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
                        sidebarPanel (),
                        mainPanel( uiOutput ("Tabla1"),
                                   downloadButton("downloadDBLinea","Descarga BD"))
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
                          mainPanel(plotOutput('grafico2'))
                        )),
               tabPanel('Tablas: ingresos por tipo',
                        sidebarLayout(
                          sidebarPanel (),
                         
                          mainPanel(uiOutput("Tabla2"))
                        ))
               
               
               )
  
)

#---- Lógica del servidor: se encarga de procesar los datos y generar salidas (gráficos, tablas) ----


server <- function(input, output, session) {
  
  ##---- Define la base de datos en función de la línea seleccionada ----
  
  MyDataLinea <- reactive({
    Data <- Ingresos_STC2 %>% 
      filter(linea == input$Linea) %>% 
      group_by(Año) %>% 
      summarise(Total = sum(ingreso, na.rm = T), .groups = 'drop') %>% 
      ungroup()
  })
  
  #---- Define la base de datos en función del tipo de ingreso seleccionado ----
  
  
  MyDataIngreso <- reactive({
    Data <- Ingresos_STC2 %>% 
      filter(tipo_ingreso == input$Tipo) %>% 
      group_by(Año) %>% 
      summarise(Total = sum(ingreso, na.rm = T), .groups = 'drop') %>% 
      ungroup()
  })
  
  ##----Salida de gráfico ----
  ###--- Genera el gráfico de ingresos por línea----
  
  
  output$grafico1 <- renderPlot({
    ggplot(data= MyDataLinea(),
           aes (x=Año , y=Total))+
      geom_col (col="black", fill="steelblue")+
      geom_text(aes(x=Año ,
                    y= Total+100000000, 
                    label=scales::comma(Total)),
                size=3.5)+
      labs(title = paste("Ingresos Anuales por linea:", input$Linea),
           subtitle = "Del 2012 - 2023 en Pesos Mx",
           caption = "Elaboracion Propia con Datos abiertos de la CDMX",
           x="",
           y="")+
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            plot.background = element_rect(fill = "#e0eee0"), 
            legend.background = element_rect(fill ="#e0eee0"),
            panel.grid.major = element_line(color="grey"),
            panel.grid.minor = element_blank(),
            plot.title = ggtext::element_markdown(hjust=0.5,
                                                  size=18,
                                                  colour="black"),
            plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                     size=16,
                                                     colour="black"),
            plot.caption = element_text(hjust=0, size=14,
                                        colour="black"),
            text = element_text(family = "Times New Roman"),
            axis.text = element_text(size=14, colour="black"),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            
            axis.title = element_text(size=14, colour="black"),
            legend.text = element_text(size=14, colour="black"),
            legend.title = element_text(size=14, colour="black"))
    
    
  },
  height = 400 , width = 900)
  
  
  
  ### ---- Genera el gráfico de ingresos por tipo ----
  
  
  
  output$grafico2 <- renderPlot({
    ggplot(data= MyDataIngreso(),
           aes (x=Año , y=Total))+
      geom_col (col="black", fill="steelblue")+
      geom_text(aes(x=Año ,
                    y= Total+100000000, 
                    label=scales::comma(Total)),
                size=3.5)+
      labs(title = paste("Ingresos Anuales por tipo de ingreso:", input$Tipo),
           subtitle = "Del 2012 - 2023 en Pesos Mx",
           caption = "Elaboracion Propia con Datos abiertos de la CDMX",
           x="",
           y="")+
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            plot.background = element_rect(fill = "#e0eee0"), 
            legend.background = element_rect(fill ="#e0eee0"),
            panel.grid.major = element_line(color="grey"),
            panel.grid.minor = element_blank(),
            plot.title = ggtext::element_markdown(hjust=0.5,
                                                  size=18,
                                                  colour="black"),
            plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                     size=16,
                                                     colour="black"),
            plot.caption = element_text(hjust=0, size=14,
                                        colour="black"),
            text = element_text(family = "Times New Roman"),
            axis.text = element_text(size=14, colour="black"),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.title = element_text(size=14, colour="black"),
            legend.text = element_text(size=14, colour="black"),
            legend.title = element_text(size=14, colour="black"))
    
    
  },
  height = 400 , width = 900)
  
  
  ### ---- Genera la tabla de ingresos por línea -----
  
  output$Tabla1 <- renderUI({
    renderDataTable({MyDataLinea()})
  })
  
  output$downloadDBLinea <- downloadHandler(
    filename = function(){
      paste("DataLinea-", Sys.Date(), ".csv", sep = "")
      },
    content = function(file){
      write.csv(MyDataLinea(), file)
    }
  )
  
  
  ### ---- Genera la tabla de ingresos por tipo ----
  
  
  output$Tabla2 <- renderUI({
    renderDataTable(MyDataIngreso())
  })
    
    
   
  
  
}
#---- Ejecuta la aplicación Shiny----

shinyApp(ui, server)





