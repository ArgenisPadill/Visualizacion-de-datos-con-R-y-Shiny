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

#Vector de opciones ----

animales <- c("Perro","Gato","Ratón","Pajaro","Otro")



ui<- fluidPage(
  selectInput("state","¿cual es tu estado Favorito", state.name, multiple = T),#Multiple = t o true es para seleccion multiple 
  radioButtons("animal","¿Cual es tu animal favorito?", animales), 
  checkboxInput("afirma","Si", value = T),
  checkboxInput("nega","No"),
  fileInput("upload",NULL)
  
  
)
server <- function(input,output,session){
  
}
shinyApp(ui,server)