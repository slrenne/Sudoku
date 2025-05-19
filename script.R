library(shiny)
library(sudokuAlt)
library(gridExtra)
library(grid)

draw_sudoku <- function(mat, bold_every = sqrt(nrow(mat))) {
  size <- nrow(mat)
  
  grid.newpage()
  
  # Set up viewport
  pushViewport(viewport(layout = grid.layout(size, size)))
  
  for (i in 1:size) {
    for (j in 1:size) {
      val <- mat[i, j]
      
      # Draw cell background
      grid.rect(x = (j - 0.5)/size, y = 1 - (i - 0.5)/size,
                width = 1/size, height = 1/size,
                gp = gpar(fill = "white", col = "black", lwd = 0.5))
      
      # Draw number or emoji
      if (val != "") {
        grid.text(label = val,
                  x = (j - 0.5)/size,
                  y = 1 - (i - 0.5)/size,
                  gp = gpar(cex = 2))
      }
    }
  }
  
  # Draw bold borders
  for (k in 0:size) {
    lwd_h <- if (k %% bold_every == 0) 2 else 0.5
    lwd_v <- lwd_h
    
    # horizontal lines
    grid.lines(x = c(0, 1), y = rep(1 - k / size, 2),
               gp = gpar(lwd = lwd_h))
    
    # vertical lines
    grid.lines(y = c(0, 1), x = rep(k / size, 2),
               gp = gpar(lwd = lwd_v))
  }
  
  popViewport()
}





# UI
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
      checkboxInput("use_fruit", "Use Fruit Symbols (only for 4x4)", FALSE),
      actionButton("generate", "Generate Puzzle"),
      downloadButton("download_pdf", "Download as PDF")
    ),
    mainPanel(
      plotOutput("sudoku_plot", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  fruit_map <- c("1" = "🍐", "2" = "🍎", "3" = "🍋", "4" = "🍑")
  
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
  
  format_grid <- function(game, use_fruit) {
    size <- sqrt(length(game))
    mat <- matrix(game, nrow = size, byrow = TRUE)
    mat[is.na(mat)] <- ""
    
    if (use_fruit && size == 4) {
      mat <- apply(mat, c(1,2), function(x) {
        if (x == "") return("")
        fruit_map[as.character(x)]
      })
    }
    
    mat
  }
  
  output$sudoku_plot <- renderPlot({
    game <- puzzle_data()
    if (is.null(game)) return()
    
    mat <- format_grid(game, input$use_fruit)
    draw_sudoku(mat)
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("sudoku-", input$size, "-", input$difficulty, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 7, height = 7)
      game <- puzzle_data()
      if (!is.null(game)) {
        mat <- format_grid(game, input$use_fruit)
        draw_sudoku(mat)
      }
      dev.off()
    }
  )
  

}

shinyApp(ui, server)
