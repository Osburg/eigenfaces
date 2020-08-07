library(shiny)

# Wie man Plots einfügt, habe ich mit einem # auskommentiert, bei renderPlot({ steht die Plot Funktionen drinnen })
#"plotOutput ist aktuell der Titel
# mit input$g lässt sich auf die Eingegebenen Eigenvektoren zugreifen Bsp,

ui <-fluidPage(
  sliderInput("g", "Eigenvektoren", min=0, max=400, value=50)#,
  #h2("plotOutput"),plotOutput("plot")

)



server <-function(input, output) {
  output$t <-renderText(input$g)#,
  #output$plot <-renderPlot({
  #})
}

shinyApp(ui, server)
