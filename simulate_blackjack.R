suppressPackageStartupMessages(library(tidyverse))

my_play <- function(hands, my_ind, my_strategy) {
  #' Returns a player's blackhack play based on a strategy
  #' @param hands numeric vector containing the values of the cards in your hand
  #' @param my_ind integer that indicates which hand is yours
  #' @param my_strategy integer, if the sum of your cards is less than my_strategy, the player hits
  #' @return string, 'H' for hit or 'S' for stand
  if (((lapply(hands, sum)[my_ind] < my_strategy))) {
    #if the sum of my hand is less than my_strategy
    
    play = "H"
  }
  
  else {
    play = "S"
  }
  return(play)
  
}

their_play <- function(hands, their_ind, their_strategy) {
  #' Returns a player's blackhack play based on a strategy
  #' @param hands numeric vector containing the values of the cards in their hand
  #' @param my_ind integer that indicates which hand is theirs
  #' @param my_strategy integer, if the sum of their cards is less than their_strategy, the player hits
  #' @return string, 'H' for hit or 'S' for stand
  if ((lapply(hands, sum)[their_ind] < their_strategy)) {
    #if the sum of their hand is less than their_strategy
    
    play = "H"
  }
  
  else {
    play = "S"
  }
  return(play)
  
}

shuffle <- function(x) {
  deck <- rep(seq(1,13,1), 4)
  shuffled_deck <- sample(deck, 52, replace=FALSE)
  shuffled_deck
}

sim_blackjack <- function(my_strategy, their_strategy) {
  #' Simulates the result of a simplified blackjack game given that each player follows a certain strategy
  #' @param my_strategy integer, if the sum of my cards is less than my_strategy, I choose to hit
  #' @param their_strategy integer, if the sum of their cards is less than their_strategy, the player hits
  #' @return string indicating which player won the game
  
  #shuffling the cards
  shuffled_deck <- shuffle()
  
  #dealing each player's starting hand
  me <- shuffled_deck[1]
  shuffled_deck <- shuffled_deck[-1]
  
  them <- shuffled_deck[1]
  shuffled_deck <- shuffled_deck[-1]
  
  me <- append(me,shuffled_deck[1])
  shuffled_deck <- shuffled_deck[-1]
  
  them <- append(them,shuffled_deck[1])
  shuffled_deck <- shuffled_deck[-1]
  
  
  #Creating a list that contains vectors of the cards in each player's hand
  hands <- list(me,them)
  
  #while the sum of both player's hands is less than 21 and at least one player "hit" on their turn and there are cards left in the deck
  while((lapply(hands, sum)[2] < 21) & ((lapply(hands, sum)[1] < 21)) & (my_play(hands, 1, my_strategy) == "H" | their_play(hands, 2, their_strategy) == "H") & length(shuffled_deck) != 0){
    
    me_current <- my_play(hands,1, my_strategy) #vector to hold my current play                
    them_current <-their_play(hands,2, their_strategy) #vector to hold their current play
    
    if(me_current == "H"){ #if I chose to hit
      me <- append(me, shuffled_deck[1]) #add a card to my hand
      shuffled_deck <- shuffled_deck[-1] #remove that card from the deck
      hands <- list(me, them) #add my new card to the list
    }
    else{
      
    }
    if(them_current == "H"){ #if they chose to hit
      them <- append(them, shuffled_deck[1]) #add a card to their hand
      shuffled_deck <- shuffled_deck[-1] #remove that card from the deck
      hands <- list(me, them) #add their new card to the list
    }
    else{
    }
  }
  my_total <- (as.integer(lapply(hands, sum)[1])) #Creating a vector with my total score
  their_total <- (as.integer(lapply(hands, sum)[2])) #Creating a vector with their total score
  
  if ((my_total > 21 & their_total > 21 )| (my_total == their_total)) { #if our totals are both >21 or we have the same total
    return("Draw")
  }
  else if (my_total <= 21 & ((my_total > their_total) | their_total > 21)) { 
    #if my total is less than 21 and either my total is greater than theirs or they have over 21
    return("Player 1 wins")
  }
  else {
    return("Player 2 wins")
  }
  
}

#sim_blackjack(17,17)

