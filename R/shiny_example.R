#' #library(shiny)
#' #library(tidyverse)
#' source("R/helperFunctions_ef.R")
#' source("R/imageset_ef.R")
#' source("R/image_ef.R")
#' source("R/FeatureSpaceProjection.R")
#'  #  source("R/visualize_ef.R")
#'
#' # Wie man Plots einfügt, habe ich mit einem # auskommentiert, bei renderPlot({ steht die Plot Funktionen drinnen })
#' #"plotOutput ist aktuell der Titel
#' # mit input$g lässt sich auf die Eingegebenen Eigenvektoren zugreifen Bsp,
#'
#' ui <-fluidPage(
#'   titlePanel(title = "Shiny App"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       sliderInput("person", "Select the Person", min=1, max=40, value=1),
#'       #textInput("text1","Enter desired persons of the Olivetti Dataset"),
#'       #helpText("Example: 1"),
#'       br(),
#'       sliderInput("eig", "Select the number of eigenvectors", min=1, max=400, value=1),
#'
#'       submitButton("Run!"), # Reactivity is different
#'       p("Click on the Update button to update.")
#'     ), #Input, Title, Default
#'
#'     mainPanel(h2("Olivettiface"),plotOutput("plot",width="50%"),h2("Rekonstruktion"),plotOutput("plot2",width="50%"))
#'   ),
#'
#'
#' )
#'
#'
#'
#'
#' server <-function(input, output) {
#'   output$t <-renderText(input$g)
#'   output$plot <-renderPlot({
#'     td <- load_imageset_ef("olivetti_X.csv", c(64,64))
#'     imgShow_ef(td[[2]])
#'     ef <- get_eigenfaces(td, input$eig)
#'     #x <- reactive({
#'     # as.numeric(input$text1)
#'     #})
#'     #par(mfrow=c(1,2))
#'     #plot_examples(ef,input$eig, mode=ef)
#'     #FSP(td[[1]], ef, avg_face(normalize(td)))
#'     imgShow_ef(td[[input$person]])
#'   output$plot2 <-renderPlot({
#'     td <- load_imageset_ef("olivetti_X.csv", c(64,64))
#'     ef <- get_eigenfaces(td, input$eig)
#'     #x <- reactive({
#'      # as.numeric(input$text1)
#'     #})
#'     #par(mfrow=c(1,2))
#'    #plot_examples(ef,input$eig, mode=ef)
#'     FSP(td[[input$person]], ef, avg_face(normalize(td)))
#'   })
#'    #imgShow_ef(td[[2]])
#'
#'
#'   })
#' }
#' ?renderPlot
#' ?plotOutput
#' #' @export
#' shiny_App <- function(){
#'   shinyApp(ui, server)
#' }
#' #shinyApp(ui, server)
