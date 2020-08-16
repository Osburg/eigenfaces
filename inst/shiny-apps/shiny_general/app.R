#library(shiny)

ef <- get_eigenfaces(td, 400, quick = FALSE) #auf FALSE aendern fuer finale Version

ui <- fluidPage(

  # App title ----
  titlePanel("Visualize Dataset"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of person ----
      sliderInput(inputId = "person",
                  label = "Number of Image:",
                  min = 1,
                  max = length(td),
                  value = 30),

      # Input: Slider for the nr of used ef to reconstruct ----
      sliderInput(inputId = "n_recon",
                  label = "Amount of Eigenvectors used for Reconstruction:",
                  min = 1,
                  max = 400,
                  value = 3),

      # Input: Slider for the eigenface number ----
      sliderInput(inputId = "n_eigen",
                  label = "Eigenvector Number:",
                  min = 1,
                  max = 400,
                  value = 1)

    ),


    # Main panel for displaying outputs ----
    mainPanel(

      # Output:
      h3("Original and Reconstructed Image"),
      fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c(200,200),
                           plotOutput(outputId = "face", width = 200, height =200),
                           plotOutput(outputId = "reconstruction", width = 200, height =200))),
      h3("Eigenvector"),
      plotOutput(outputId = "eigenface", width = 200, height = 200)

    )
  )
)


server <- function(input, output) {

  output$face <- renderPlot({
    par(mar = c(0,0,0,0))
    td[[input$person]]
  })

  output$eigenface <- renderPlot({
    par(mar = c(0,0,0,0))
    ef[[input$n_eigen]]
  })

  output$reconstruction <- renderPlot({
    par(mar = c(0,0,0,0))
    ef_part <- ef[1:input$n_recon]
    class(ef_part) <- "imageset_ef"
    FSP(td[[input$person]], ef_part, avg_face(normalize(td)))
  })

}

shinyApp(ui, server)
