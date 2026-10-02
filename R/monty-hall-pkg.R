#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#' Select a door
#'
#' @description
#' Randomly selects one of the three doors.
#'
#' @details
#' This is the contestants first choice before one of the
#' goat doors is opened.
#'
#' @return Returns a number between 1 and 3.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a goat door
#'
#' @description
#' Opens one of the doors that has a goat.
#'
#' @details
#' The host cannot open the contestants door or the door
#' with the car. If the contestant picked the car one of
#' the two goat doors is randomly selected.
#'
#' @param game The game with two goats and one car.
#' @param a.pick The contestants first door choice.
#'
#' @return Returns the number of the door that was opened.
#'
#' @examples
#' game <- c("car","goat","goat")
#' open_goat_door(game, 2)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)

  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }

  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }

  return( opened.door ) # number between 1 and 3
}



#' @title
#' Stay or switch doors
#'
#' @description
#' Decides the final door based on if the contestant stays
#' or switches.
#'
#' @details
#' If stay is TRUE the contestant keeps their first door.
#' If stay is FALSE they switch to the other unopened door.
#'
#' @param stay TRUE to stay and FALSE to switch.
#' @param opened.door The door opened by the host.
#' @param a.pick The contestants first door choice.
#'
#' @return Returns the final door number.
#'
#' @examples
#' change_door(stay=T, opened.door=2, a.pick=1)
#' change_door(stay=F, opened.door=2, a.pick=1)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }

  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine the winner
#'
#' @description
#' Checks if the final door has the car or a goat.
#'
#' @details
#' If the final door has the car the result is WIN.
#' If the final door has a goat the result is LOSE.
#'
#' @param final.pick The contestants final door choice.
#' @param game The game with two goats and one car.
#'
#' @return Returns WIN or LOSE.
#'
#' @examples
#' game <- c("goat","car","goat")
#' determine_winner(2, game)
#' determine_winner(1, game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }

  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}



#' @title
#' Play one game
#'
#' @description
#' Plays one full Monty Hall game.
#'
#' @details
#' The function creates a game, picks a door, opens a goat
#' door and checks the results for staying and switching.
#'
#' @return Returns a data frame with the strategy and outcome.
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)

  game.results <- data.frame(
    strategy,
    outcome,
    stringsAsFactors=F
  )

  return( game.results )
}



#' @title
#' Play multiple games
#'
#' @description
#' Plays the Monty Hall game multiple times.
#'
#' @details
#' Repeats the game n times and saves the results for the
#' stay and switch strategies.
#'
#' @param n Number of games to play. Default is 100.
#'
#' @return Returns a data frame with all of the game results.
#'
#' @examples
#' play_n_games(10)
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )

  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}
