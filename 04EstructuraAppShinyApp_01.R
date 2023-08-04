########################################################
# Visualización de datos con R y Shiny                 #
# Argenis Alain Gustavo Padilla Córdoba                #
# Tema 4: Estructura de App Shiny                      #
#                                                      #
########################################################

#Instalar paqueteria de shiny----
 #install.packages("shiny",dependencies = TRUE)

#librerías a utilizar----

library(shiny)

#==================Estructura de una app de Shiny============================

ui <- fluidPage(
selectInput("dataset", label = "Datos", choices = ls("package:datasets")),
verbatimTextOutput("sumary"),
tableOutput("table")
)
server <- function(input, output, session){
  
dataset <- reactive({
  get(input$dataset,"package:datasets")#la funcion se repite varias veces en el 
  #codigo por lo cual se ocupa reactive para declarar variable y llamarla posteriormente
})
  
    output$summary <- renderPrint({
    #dataset <- get(input$dataset,"package:datasets")
      
    sumary(dataset())
  })
    
    output$table <- renderTable({
    #dataset <- get(input$dataset,"package:datasets") 
      #la asignacion de package en el data set se repite por lo cual se cambia por  eliminaciond e uplicicdad de cod
      
    dataset()
  })
}

shinyApp(ui, server)

#Ui es la estructura 
#server es la  salida a travez de la estructura

