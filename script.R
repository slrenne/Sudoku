library(shiny)
library(sudokuAlt)
library(gridExtra)
library(grid)

# Define UI
ui <- fluidPage(
  titlePanel("Printable Sudoku Generator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("size", "Puzzle Size:",
                  choices = list("4x4 (n = 2)" = 2,
                                 "9x9 (n = 3)" = 3,
                                 "16x16 (n = 4)" = 4)),
      selectInput("difficulty", "Difficulty:",
                  choices = c("Easy", "Medium", "Hard", "Expert")),
      actionButton("generate", "Generate Puzzle"),
      downloadButton("download_pdf", "Download as PDF")
    ),
    mainPanel(
      plotOutput("sudoku_plot", height = "600px")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Safe gaps by size and difficulty
  get_gaps <- function(n, difficulty) {
    size <- n^2
    total_cells <- size^2
    
    max_gaps <- switch(as.character(n),
                       "2" = 8,
                       "3" = 55,
                       "4" = 200)
    
    percent <- switch(difficulty,
                      "Easy" = 0.35,
                      "Medium" = 0.5,
                      "Hard" = 0.6,
                      "Expert" = 0.7)
    
    min(ceiling(total_cells * percent), max_gaps)
  }
  
  puzzle_data <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    n <- as.numeric(input$size)
    gaps <- get_gaps(n, input$difficulty)
    
    tryCatch({
      game <- makeGame(n = n, gaps = gaps)
      puzzle_data(game)
    }, error = function(e) {
      showNotification(paste("Error generating puzzle:", e$message), type = "error")
    })
  })
  
  output$sudoku_plot <- renderPlot({
    game <- puzzle_data()
    if (is.null(game)) return()
    
    size <- sqrt(length(game))
    mat <- matrix(game, nrow = size, byrow = TRUE)
    
    # Replace NAs with empty strings
    mat[is.na(mat)] <- ""
    
    grid.table(mat, rows = NULL, cols = NULL)
  })
  
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("sudoku-", input$size, "-", input$difficulty, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 7, height = 7)
      game <- puzzle_data()
      if (!is.null(game)) {
        size <- sqrt(length(game))
        mat <- matrix(game, nrow = size, byrow = TRUE)
        
        # Replace NAs with empty strings for PDF too
        mat[is.na(mat)] <- ""
        
        grid.table(mat, rows = NULL, cols = NULL)
      }
      dev.off()
    }
  )
  
}

shinyApp(ui, server)
