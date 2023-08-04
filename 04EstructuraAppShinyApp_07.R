########################################################
# Visualización de datos con R y Shiny                 #
# Argenis Alain Gustavo Padilla Córdoba                #
# Tema 4: App Shiny (principios)                       #
#                                                      #
########################################################

#Instalar paqueteria de shiny----
#install.packages("shiny",dependencies = TRUE)

#librerías a utilizar----

library(shiny)

#===============================Estructura de app de Shiny==========================


# Definiendo la Interfaz de Usuario (UI) de la aplicación
ui <- fluidPage(
  # Creando una salida para una tabla estática utilizando la función tableOutput
  tableOutput("static"),
  
  # Creando una salida para una tabla dinámica utilizando la función dataTableOutput
  dataTableOutput("dynamic")
)

# Definiendo la lógica del servidor para la aplicación
server <- function(input, output, session) {
  
  # Creando una tabla estática que mostrará las primeras 6 filas del conjunto de datos mtcars
  output$static <- renderTable(head(mtcars))
  
  # Creando una tabla dinámica que mostrará todo el conjunto de datos mtcars, con opciones para controlar la paginación
  # En este caso, se mostrarán 5 filas por página
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength=5))
}


shinyApp(ui, server)