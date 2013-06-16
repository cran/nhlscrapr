\name{nhl20022013}
\docType{data}
\alias{roster}
\alias{roster.unique}
\alias{games}
\title{Pre-processed game table and player list}
\description{
While these data sets can be assembled using the function construct.roster() within nhlscrapr, these data objects are provided as a convenience to the user.
}
\usage{nhl20022013}
\format{
roster: a 4851-by-6 data frame with all player identifiers present in game data. Useful for replacing players with unique ID numbers in game records.

roster.unique: a 2476-by-6 data frame, derived from roster, with unique player IDs only. Useful for collecting individual statistics on players.

games: a game table, as called from full.game.database(), with cells filled in with game information.
}
\keyword{datasets}
