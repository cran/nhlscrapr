\name{nhlscrapr-package}
\alias{nhlscrapr-package}
\alias{nhlscrapr}
\docType{package}
\title{
  nhlscrapr: Retrieve and process play-by-play game data from the NHL
  Real Time Scoring System
}
\description{
  This package contains routines for extracting play-by-play game data
  for regular-season and playoff NHL games, particularly for analyses
  that depend on which players are on the ice. 
}
\details{
  This package processes data files of the type expanded summary (ES)
  and play-by-play (PL) for all possible games between 2002 and 2013,
  and visual shift charts (SCH and SCV) for games between 2002 and
  2007. The purpose of this routine is to produce tables for players,
  games and on-ice events specifically to look at the impact of a player's 
  contribution to how events unfold. The examples below illustrate how 
  these tools can be used to produce these tables. 

  The command process.games() will download the appropriate game files
  and, for each game selected produce a table of events with all players on the ice
  and relevant information for each particular event, such as shot distance and type, 
  goals and assists, and the zone in which the event took place (relative to the home 
  team). 

  The command augment.game() takes a single game file and makes it more useful for 
  the purpose of statistical modelling. It replaces player names with a unique player 
  ID number, separates goaltenders from skaters, and obtains the event interval time. 
  This routine is called for each game in the table by merge.to.mega.file() as it combines 
  all games into one single R object, saving this object to disk along with the game table 
  and unique player roster.

}
\author{
A.C. Thomas <act@acthomas.ca>
}
\keyword{ package }

\examples{

\dontrun{


  #What are all games that can/should be downloaded?
  #valid=FALSE implies previously screened problems.
  #Just a subset for testing.
  game.table <- full.game.database()[201:203,]

  #Get the details for these 20 games.
  print(game.table)

  #Takes HTML files and (possibly) GIF images and produces event and player tables for each game.
  process.games (game.table)

  #Give me a single game record.
  sample.game <- retrieve.game (season="20022003", gcode="20201")

  #Take all the game table roster lists and produce both a unique player
  #list and an improved game table.
  roster.main <- construct.rosters (game.table)

  #Replace names with unique ID numbers.
  sample.plus <- augment.game (sample.game, roster.main$roster)

  #Augment all games and put them all in one big database.
  assemble.mega.file (roster.main, game.table, output.file="mynhlscrapes.RData")


  #################################################################
  # Extras:

  # This runs the whole thing.
  nhlscrapr.everything()


  #Process games in parallel!
  library(doMC)
  registerDoMC(4)

  res <- foreach (kk=1:dim(game.table)[1]) \%dopar\%
    if (game.table$valid[kk]) \{
      message (paste(kk, game.table[kk,1], game.table$gcode[kk]))
      item <- process.single.game(
                game.table[kk,1],
                game.table$gcode[kk],
                save.to.file=TRUE)
    \}
}

}
