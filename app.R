library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(tidyverse)

#create the UI
ui <- fluidPage(
  useShinyjs(), # initialize package that allows the action button to start
                # already pressed
  titlePanel("Blackjack Game"), #title
  br(),
  p("This app displays the results of repeated games of a simplified version of 
    blackjack. In this version of blackjack, all players start with two cards and
    take turns deciding whether to hit (take another card), or stand (not take 
    another card). A player loses if their total card count exceeds 21. The game 
    ends when either all cards have been dealt, both players chose to stand in 
    the same round, or a player's total exceeds 21. The user inputs the number 
    of games they would like to simulate, as well as the stratgey for each player."),
  br(),
  sidebarLayout( #choose sidebar layout
    sidebarPanel( #whats here goes in the sidebar
      
      #allows user input of number of games
      numericInput("num_games", "Number of Games", 10, 1,10000),
      p("Player 1 will hit if the sum of their cards is less than the selected 
        number."),
      
      #allows user input of player 1's strategy
      sliderInput("p1_strategy", "Player 1 Strategy", 1,21,17),
      p("Player 2 will hit if the sum of their cards is less than the selected 
        number."),
      
      #allows user input of player 2's strategy
      sliderInput("p2_strategy", "Player 2 Strategy", 1,21,17),
      
      
      #action button to load the plot based on the user's input
      actionButton("playbutton", "Play")
      
    ),
    mainPanel( #whats here goes in the main panel
      plotOutput("winningplot"), # histogram of results
      tableOutput("winningtable") #table of results
    )
  )
)

#here is what the server runs
server <- function(input, output) {
  
  # Creates a df that holds the results of each game and
  # binds event to the play button
  playGame <- reactive({
    if (input$num_games == 0) {
      return(NULL)
    }
    game_table <- as.data.frame(matrix(nrow = input$num_games))
      for (i in 1:input$num_games) {
        game_table[i,] <- sim_blackjack(input$p1_strategy, input$p2_strategy)
      }
    game_table
    }) %>%
    bindEvent(input$playbutton)
  
  # Creates a histogram of who won each game
  output$winningplot <- renderPlot({
      if (is.null(playGame())) {
        return(NULL)
      }
      ggplot(playGame(), aes(x=V1)) +
        geom_histogram(stat = "count", fill = "skyblue") +
        labs(title = "Number of Blackjack Games Won for Each Player",
             x = "Result",
             y = "Count") +
        theme_classic() +
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              plot.title = element_text(size = 14))
  })
  
  # Auto clicks the play button upon first loading of site
  start_pressed <- observe({
    shinyjs :: click("playbutton")
    start_pressed$destroy() #destroys object so it only auto clicks once
  })
  
  # Table that displays frequencies for each player
  table <- reactive({
    if (is.null(playGame())) {
      return(NULL)
    }
    game <- playGame() %>%
      group_by(V1) %>%
      summarize(n = n())
      
    if (!("Draw" %in% unique(game$V1))) {
      new_row <- list("Draw", 0)
      game <- rbind(game, new_row)
    }
    else {
    }
    if (!("Player 1 wins" %in% unique(game$V1))) {
      new_row <- list("Player 1 wins", 0)
      game <- rbind(game, new_row)
    }
    else {
    }      
    if (!("Player 2 wins" %in% unique(game$V1))) {
       new_row <- list("Player 2 wins", 0)
      game <- rbind(game, new_row)
    }      
    else {
    }
    game <- game %>%
      mutate(Frequency = n/sum(n)) %>%
      select(!n) %>%
      pivot_wider(names_from = V1, values_from = Frequency) %>%
      select(Draw, "Player 1 wins", "Player 2 wins") #makes sure column names are in the same order
    game
      
  })
    
  #table output
  output$winningtable <- renderTable({
    if (is.null(table())) {
      return(NULL)
    }
    table()
  })
}

shinyApp(ui = ui, server = server)

