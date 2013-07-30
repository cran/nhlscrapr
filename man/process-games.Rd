\name{process.games}
\alias{process.games}
\alias{process.single.game}
\alias{retrieve.game}
\alias{construct.rosters}
\alias{augment.game}
\alias{assemble.mega.file}
\alias{nhlscrapr.everything}

\title{Given downloaded NHL games, produce event tables with players on ice.}
\description{
  Produces game tables from the NHL source files.
}
\usage{
  process.single.game (season="20122013",
                       gcode="20001",
                       rdata.folder="nhlr-data",
                       override.download=FALSE,
                       save.to.file=TRUE)
  process.games       (games=full.game.database(),
                       rdata.folder="nhlr-data",
                       override.download=FALSE)
  retrieve.game       (season="20122013",
                       gcode="20001",
                       rdata.folder="nhlr-data")
  construct.rosters   (games=full.game.database(),
                       rdata.folder="nhlr-data",
                       roster.master=NULL,
                       positions=NULL)
  augment.game        (game.info, player.list)
  assemble.mega.file  (player.list,
                       games=full.game.database(),
                       rdata.folder="nhlr-data",
                       output.file="nhlscrapr-record.RData")
  nhlscrapr.everything ()

}
\arguments{
  \item{season}{A character string for the two years specifying an NHL
    season.}
  \item{gcode}{The five-digit ID number for a particular NHL game.}
  \item{rdata.folder}{The location within the current directory to
  which to save the downloaded files. Will be created if it does not
  exist.}
  \item{games}{A game database, such as the one produced by
    full.game.database().}
  \item{override.download}{Re-download the game files whether or not
    they are currently locally available.}
  \item{save.to.file}{Save the game object to file.}
  \item{game.info}{A list of objects pertaining to a single game, the output of process.single.game() .}
  \item{player.list}{A table with player names and unique IDs.}
  \item{output.file}{The name of a file into which the game event list
    will be saved.}
  \item{roster.master}{A roster table produced by construct.rosters()}
  \item{positions}{The positions assumed by players during each game,
    produced by construct.rosters().}
}
\details{
  This group of functions takes the downloaded HTML, image and JSON
  files and produces an event table for each game, particularly focusing
  on the players on the ice during each event. 
}
\value{
  process.single.game() produces an object called game.info that
  contains several pieces of information, primarily the data.frame
  playbyplay, with each column representing an event. Columns are as
  follows:
  event: The event number as recorded in the game.
  period, seconds: The period of the game and the elapsed time in seconds.
  a1,...,a6: The numbers and names of the away team players on the ice.
  h1,...,h6: The numbers and names of the home team players on the ice.
  ev.team: The team that registered the event in question.
  ev.player.1, ev.player.2, ev.player.3: The players involved in the event.
  distance: The distance in feet from the relevant goal for a shot on goal, missed shot or goal.
  type: Further information on the event, either shot type or penalty type.
  homezone: The zone in which the event took place, from the
    perspective of the home team.
  xcoord, ycoord: If available, the (x,y) location of a shot on
    goal or hit.

  process.games() runs this routine and saves all game files to disk.

  retrieve.game() retrieves the processed file from disk if it exists, and
  if not, it runs process.single.game() first.

  construct.rosters() takes the roster table from each game and assembles a unique player identifier list, as well as fills in the information for each game in the game table.

  augment.game() replaces the names in the event record with unique identifier numbers.

  assemble.mega.file() takes all games in the record, performs augment.game() and saves the result.

  nhlscrapr.everything() does everything that's needed to construct the entire record, in case the user doesn't want to do much.
}
\author{A.C. Thomas <act@acthomas.ca>}
\examples{

  #Select a part of the history.
  game.table <- full.game.database()[201:203,]

  #Process one game.
  g1 <- process.single.game (game.table$season[1], game.table$gcode[1])

  #Process all games.
  process.games (game.table)

  roster.block <- construct.rosters (game.table)

  g1.aug <- augment.game (g1, roster.block$roster.master)

}

