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
dateInput("dob","¿Cuál es tu fecha de nacimiento?"),
dateRangeInput("holiday","¿Cuando Quieres irte de  vacaciones?")
  
  
)
server <- function(input,output,session){
  
}
shinyApp(ui,server)