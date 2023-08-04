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

ui<- fluidPage(
  numericInput("num","Número uno", value = 0, min = 0, max = 100),
  sliderInput("num2", "Número dos", value = 50, min = 0, max = 100),
  sliderInput("rng", "Rango", value = c(10,20), min = 0, max = 100)
)
server <- function(input,output,session){
  
}
shinyApp(ui,server)