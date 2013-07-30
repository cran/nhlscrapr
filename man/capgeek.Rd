\name{capgeek}
\alias{download.capgeek.files}

\title{Download and process salary information from capgeek.com}
\description{
  Produces salary information from capgeek.com.
}
\usage{
download.capgeek.files (max.player.num=2552,
                        files=1:max.player.num,
                        rdata.folder="nhlr-data",
                        just.process=FALSE,
                        verbose=TRUE,
                        retrieve.tables=TRUE)

}
\arguments{
  \item{max.player.num}{The highest capgeek.com database player number.}
  \item{files}{Which capgeek.com database entries should be retrieved.}
  \item{rdata.folder}{The location within the current directory to
  which to save the downloaded files. Will be created if it does not
  exist.}
  \item{just.process}{If TRUE, don't download the files if they are
    present in the rdata.folder.}
  \item{verbose}{Display additional information.}
  \item{retrieve.tables}{Brings}
}
\value{
  download.capgeek.files() will not only download and save files to
  disk, but will process these results into tables.
}
\author{A.C. Thomas <act@acthomas.ca>}
