library(shiny)

# Load dataset
clues_data <- read.csv("all_recent_puzzles.csv", as.is=T)
clues_data <- clues_data[,1:2]
names(clues_data)[1] <- "Clue"

# Define the UI
ui <- fluidPage(
  titlePanel("Wheel of Fortune Puzzle Game"),
  sidebarLayout(
    sidebarPanel(
      textInput("letter_guess", "Enter a Letter:", ""),
      actionButton("guess", "Guess"),
      actionButton("reveal", "Reveal Answer"),
      br(),
      br(),
      textOutput("wrong"),
      textOutput("feedback"),
      br(), 
      print("This game gives you a random puzzle from Wheel of Fortune Seasons 30 - 42. Correct letter guesses will appear in the puzzle. Incorrect guesses will show in the 'Used Letters' section. Made by Kira Tebbe.")
    ),
    mainPanel(
      h3(textOutput("category_display")),
      h3(htmlOutput("clue_display")),
      h3(textOutput("revealed_answer"), style = "color: blue;")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive values to store the current state
  values <- reactiveValues(
    current_category = NULL,
    current_clue = NULL,
    displayed_clue = NULL,
    revealed_positions = NULL,
    feedback_message = "",
    wrong_letters = "Used Letters: ",
    feedback_timer = NULL,
    revealed_answer = NULL
  )
  
  # Helper function: Format the clue with blanks and punctuation preserved
  format_clue <- function(clue, revealed_positions) {
    clue_chars <- strsplit(clue, "")[[1]]
    formatted <- sapply(seq_along(clue_chars), function(i) {
      if (clue_chars[i] == " ") {
        HTML('&nbsp;&nbsp;')  # Add chunky spaces
      } else if (clue_chars[i] %in% c("'", "-", ",", "!", "?", "&")) {
        clue_chars[i]  # Keep punctuation visible
      } else if (revealed_positions[i]) {
        paste0(" ", clue_chars[i], " ")  # Reveal the letter
      } else {
        " _ "  # Show blanks for unrevealed letters
      }
    })
    paste(formatted, collapse = "")  # Combine everything into a single string
  }
  
  # Function to reset the game with a new puzzle
  reset_game <- function() {
    random_row <- clues_data[sample(nrow(clues_data), 1), ]
    random_category <- ifelse(is.na(random_row$Category) || random_row$Category == "", "Uncategorized", random_row$Category)
    random_clue <- ifelse(is.na(random_row$Clue) || random_row$Clue == "" || nchar(random_row$Clue) == 0, "NO CLUE AVAILABLE", random_row$Clue)
    
    isolate({
      values$current_category <- random_category
      values$current_clue <- toupper(as.character(random_clue))
      values$revealed_positions <- rep(FALSE, nchar(values$current_clue))
      values$displayed_clue <- format_clue(values$current_clue, values$revealed_positions)
      values$feedback_message <- ""
      values$wrong_letters <- "Used letters: "
      values$revealed_answer <- NULL
    })
  }
  
  # Initialize the game
  reset_game()
  
  # Display the category
  output$category_display <- renderText({
    req(values$current_category)
    paste("Category:", values$current_category)
  })
  
  # Display the clue as HTML to preserve spaces
  output$clue_display <- renderUI({
    req(values$displayed_clue)
    HTML(values$displayed_clue)
  })
  
  # Display feedback message
  output$feedback <- renderText({
    values$feedback_message
  })
  
  # Display wrong letters
  output$wrong <- renderText({
    values$wrong_letters
  })
  
  # Display the revealed answer
  output$revealed_answer <- renderText({
    req(values$revealed_answer)
    values$revealed_answer
  })
  
  # Handle letter guesses
  observeEvent(input$guess, {
    guess <- toupper(trimws(input$letter_guess))  # Get the guessed letter
    if (nchar(guess) != 1 || !grepl("[A-Z]", guess)) {
      values$feedback_message <- "Please enter a single letter."
      return()
    }
    
    # Check if the letter exists in the clue
    clue_chars <- strsplit(values$current_clue, "")[[1]]
    if (guess %in% clue_chars) {
      indices <- which(clue_chars == guess)
      isolate({
        values$revealed_positions[indices] <- TRUE
        values$displayed_clue <- format_clue(values$current_clue, values$revealed_positions)
        values$feedback_message <- ""  # Clear feedback if the letter is correct
      })
    } else {
      values$feedback_message <- "Not in Puzzle."  # Show feedback for incorrect guesses
      values$wrong_letters <- paste(values$wrong_letters, guess, ", ")
      
      # Clear the message after 3 seconds
      if (!is.null(values$feedback_timer)) {
        values$feedback_timer <- NULL
      }
      values$feedback_timer <- reactiveTimer(3000, session)
      observe({
        isolate({
          values$feedback_message <- ""
          values$feedback_timer <- NULL
        })
      })
    }
    
    # Clear the input field
    updateTextInput(session, "letter_guess", value = "")
  })
  
  # Reveal the correct answer
  observeEvent(input$reveal, {
    values$revealed_answer <- paste("The Correct Answer is:", values$current_clue)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
