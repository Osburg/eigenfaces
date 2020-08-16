#library(shiny)

olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
cat("Loading dataset...\n")
td <- load_imageset_ef(olivetti, c(64,64))
cat("Getting Eigenfaces...\n")
ef <- get_eigenfaces(td, 400, quick = FALSE) #auf FALSE aendern fuer finale Version

ui <- fluidPage(

  # App title ----
  titlePanel("Visualize Eigenfaces"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of person ----
      sliderInput(inputId = "person",
                  label = "Number of Person:",
                  min = 1,
                  max = 40,
                  value = 30),

      # Input: Slider for the perspective of person ----
      sliderInput(inputId = "perspective",
                  label = "Perspective of Person:",
                  min = 1,
                  max = 10,
                  value = 2),

      # Input: Slider for the nr of used ef to reconstruct ----
      sliderInput(inputId = "n_recon",
                  label = "Amount of Eigenfaces used for Reconstruction:",
                  min = 1,
                  max = length(ef),
                  value = 3),

      # Input: Slider for the eigenface number ----
      sliderInput(inputId = "n_eigen",
                  label = "Eigenface Number:",
                  min = 1,
                  max = length(ef),
                  value = 1)

    ),


    # Main panel for displaying outputs ----
    mainPanel(

      # Output:
      h3("Original and Reconstructed Face"),
      fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c(200,200),
                           plotOutput(outputId = "face", width = 200, height =200),
                           plotOutput(outputId = "reconstruction", width = 200, height =200))),
      h3("Eigenface"),
      plotOutput(outputId = "eigenface", width = 200, height = 200)

    )
  )
)


server <- function(input, output) {

  output$face <- renderPlot({
    par(mar = c(0,0,0,0))
    td[[input$person*10 + input$perspective]]
  })

  output$eigenface <- renderPlot({
    par(mar = c(0,0,0,0))
    ef[[input$n_eigen]]
  })

  output$reconstruction <- renderPlot({
    par(mar = c(0,0,0,0))
    FSP(td[[input$person*10 + input$perspective]],
        ef[1:input$n_recon], avg_face(normalize(td)))
  })

}

shinyApp(ui, server)
