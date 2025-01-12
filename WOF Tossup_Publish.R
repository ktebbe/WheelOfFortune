library(shiny)
library(dplyr)
library(stringr)

# Load & clean dataset
clues_data <- read.csv("all_recent_puzzles.csv", stringsAsFactors = FALSE)
names(clues_data)[1] <- "Clue"

# filter to non-missing Toss-ups
clues_data <- clues_data %>%
  filter(str_detect(Round, "T")) %>%
  filter(Clue != "")

ui <- fluidPage(
  titlePanel("Wheel of Fortune Toss-Up Practice"),
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "Go"),
      actionButton("solve", "Solve"),
      actionButton("again", "Again"),
      br(), 
      br(), 
      print("This game gives you a random Toss-Up puzzle from Wheel of Fortune Seasons 30 - 42. One you press Go, a random letter will appear every second. Press Solve when you think you know it, and the answer will appear a few seconds later. Made by Kira Tebbe.")
    ),
    mainPanel(
      h3(textOutput("category")),
      h3(htmlOutput("clue_display")),
      h3(textOutput("real_clue", inline = TRUE), style = "color: blue;")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store the current state
  values <- reactiveValues(
    current_category = NULL,
    current_clue = NULL,
    displayed_clue = NULL,
    revealed_positions = NULL
  )
  
  # Flag to control the revealing process
  revealing_active <- reactiveVal(FALSE)
  
  # Reactive timer for 1-second intervals
  reveal_timer <- reactiveTimer(1000)  
  
  # Helper function: Format the displayed clue
  format_clue <- function(clue, revealed_positions) {
    clue_chars <- strsplit(clue, "")[[1]]
    formatted <- sapply(seq_along(clue_chars), function(i) {
      if (clue_chars[i] == " ") {
        HTML('&nbsp;&nbsp;')  # Add chunky spaces
      } else if (clue_chars[i] %in% c("'", "-", ",", "?", "!", "&")) {
        clue_chars[i]  
      } else if (revealed_positions[i]) {
        paste0(" ", clue_chars[i], " ")  
      } else {
        " _ " 
      }
    })
    paste(formatted, collapse = " ")
  }
  
  
  # Function to reset the game
  reset_game <- function() {
    random_row <- clues_data[sample(nrow(clues_data), 1), ]
    random_category <- ifelse(is.na(random_row$Category) || random_row$Category == "", 
                              "Uncategorized", random_row$Category)
    random_clue <- ifelse(is.na(random_row$Clue) || random_row$Clue == "" || 
                            nchar(random_row$Clue) == 0, "NO CLUE AVAILABLE", random_row$Clue)
    
    isolate({
      values$current_category <- random_category
      values$current_clue <- toupper(as.character(random_clue))
      values$revealed_positions <- rep(FALSE, nchar(values$current_clue))
      values$displayed_clue <- format_clue(values$current_clue, values$revealed_positions)
      revealing_active(FALSE)  # Stop any ongoing reveals
    })
    output$real_clue <- renderText("")  # Clear the real clue
  }
  
  # Initialize the game on app start
  observe({
    if (is.null(values$current_category)) {
      reset_game()
    }
  })
  
  # Display the current category and clue
  output$category <- renderText({
    req(values$current_category)
    paste("Category:", values$current_category)
  })
  
  output$clue_display <- renderUI({
    req(values$displayed_clue)
    HTML(values$displayed_clue)
  })
  
  # Single observer for revealing letters
  observe({
    reveal_timer()  # Fires every 1 second
    if (revealing_active()) {
      isolate({
        # Skip the first tick after activating
        if (isTRUE(values$skip_first_tick)) {
          values$skip_first_tick <- FALSE  # Reset the flag
          return()  # Exit this tick without revealing a letter
        }
        
        unrevealed_indices <- which(!values$revealed_positions & 
                                      strsplit(values$current_clue, "")[[1]] != " ")
        if (length(unrevealed_indices) > 0) {
          random_index <- sample(unrevealed_indices, 1)
          values$revealed_positions[random_index] <- TRUE
          values$displayed_clue <- format_clue(values$current_clue, values$revealed_positions)
        } else {
          revealing_active(FALSE)  # Stop when all letters are revealed
        }
      })
    }
  })
  
  # Start revealing letters when "Go" is pressed
  observeEvent(input$go, {
    if (!revealing_active()) {  # Prevent multiple concurrent reveals
      revealing_active(TRUE)
      values$skip_first_tick <- TRUE  # Flag to skip the immediate timer tick
    }
  })
  
  # Stop revealing and reset game when "Again" is pressed
  observeEvent(input$again, {
    revealing_active(FALSE)  # Stop revealing process
    reset_game()
    values$skip_first_tick <- FALSE  # Ensure flag is reset
  })
  
  # Reveal the full clue when "Solve" is pressed
  observeEvent(input$solve, {
    revealing_active(FALSE)  # Stop revealing process
    output$real_clue <- renderText({
      Sys.sleep(3)
      paste("Clue:", values$current_clue)
    })
  })
}

shinyApp(ui = ui, server = server)
