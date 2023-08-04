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

ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code")
)
server <- function(input, output,session){
  output$text <- renderText({
    "¡Hola amigo!"
  })
  
  output$code <- renderPrint({
    summary(1:10)
  })
}


shinyApp(ui,server)
 