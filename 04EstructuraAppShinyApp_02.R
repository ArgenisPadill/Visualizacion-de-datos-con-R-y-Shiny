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
  textInput("name","Cual es tu nombre?"),
  passwordInput("password","Cual es tu contraseña?"),
  textAreaInput("story", "cuentame sobre ti", row = 3)
)
server <- function(input,output,session){
  
}
shinyApp(ui,server)
