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
library(shiny)

ui <- fluidPage(
  plotOutput("plot", width="400px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96)
}

shinyApp(ui, server)