library(shiny)
library(tidyverse)

# filter to only semi-popular words
clues_data <- read.csv("all_recent_puzzles.csv", stringsAsFactors = FALSE)  

all_words <- clues_data %>% # keeping the category with the word
  separate_rows(Puzzle, sep = " ") %>%
  mutate(Puzzle = trimws(Puzzle)) %>%
  filter(Puzzle != "")

words_to_keep <- all_words %>% 
  count(Puzzle, sort = TRUE) %>%
  filter(n >= 20) %>% # in at least 20 puzzles
  filter(nchar(Puzzle) >= 3) # at least 3 letters long
  
clues_data <- all_words %>%
  filter(Puzzle %in% words_to_keep$Puzzle) 

ui <- fluidPage(
  titlePanel("Wheel of Fortune Common Word Practice"),
  sidebarLayout(
    sidebarPanel(
      actionButton("reveal", "Reveal"),
      actionButton("new_word", "New Word"),
      br(), 
      br(), 
      print("This game gives you a word that is at least 3 letters long and has appeared at least 20 Wheel of Fortune puzzles since Seasons 30. Take a second to think about what word(s) could fit the pattern, and then press Reveal to see the selected word. Made by Kira Tebbe.")
    ),
    mainPanel(
      h3(textOutput("category_display")),
      h3(textOutput("word_display")),
      h3(textOutput("revealed_word"), style = "color: blue;")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive values to store the current state
  values <- reactiveValues(
    current_category = NULL,
    current_clue = NULL,
    chosen_word = NULL,
    displayed_word = NULL,
    word_revealed = FALSE
  )
  
  # Helper function: Mask letters in the word with _
  mask_word <- function(word, reveal_percentage) {
    word_chars <- strsplit(word, "")[[1]]
    total_letters <- length(word_chars)
    reveal_count <- ceiling(total_letters * reveal_percentage)
    unique_letters <- unique(word_chars[word_chars != ""])  # Ignore spaces and punctuation
    revealed_letters <- sample(unique_letters, min(reveal_count, length(unique_letters)))
    
    masked <- sapply(word_chars, function(char) {
      if (char %in% c("'", "-", ",", "!", "?", "&") || char %in% revealed_letters) {
        char  # Reveal WOF punctuation and selected letters
      } else {
        "_"
      }
    })
    paste(masked, collapse = " ")
  }
  
  # Select a random clue and word to display
  reset_game <- function() {
    random_row <- clues_data[sample(nrow(clues_data), 1), ]
    random_category <- ifelse(is.na(random_row$Category) || random_row$Category == "", "Uncategorized", random_row$Category)
    random_clue <- ifelse(is.na(random_row$Puzzle) || random_row$Puzzle == "" || nchar(random_row$Puzzle) == 0, "NO CLUE AVAILABLE", random_row$Puzzle)
    
    clue_words <- unlist(strsplit(random_clue, " "))
    chosen_word <- sample(clue_words, 1)
    reveal_percentage <- runif(1, 0.1, 0.5)
    displayed_word <- mask_word(chosen_word, reveal_percentage)
    
    isolate({
      values$current_category <- random_category
      values$current_clue <- random_clue
      values$chosen_word <- chosen_word
      values$displayed_word <- displayed_word
      values$word_revealed <- FALSE  # Reset reveal status
    })
  }
  
  # Initialize the game
  reset_game()
  
  # Display the category
  output$category_display <- renderText({
    req(values$current_category)
    paste("Category:", values$current_category)
  })
  
  # Display the masked word
  output$word_display <- renderText({
    req(values$displayed_word)
    values$displayed_word
  })
  
  # Reveal the actual word
  output$revealed_word <- renderText({
    if (values$word_revealed) {
      paste("Revealed Word:", values$chosen_word)
    } else {
      ""
    }
  })
  
  # Reveal the hidden word
  observeEvent(input$reveal, {
    values$word_revealed <- TRUE
  })
  
  # Reset the game when the "New Word" button is pressed
  observeEvent(input$new_word, {
    reset_game()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
