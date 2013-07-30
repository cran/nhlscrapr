 
# Functions for loading data from the HTML files.

full.game.database <- function () {

  game.roster <- NULL
  seasons <- c("20022003", "20032004", "20052006", "20062007",
               "20072008", "20082009", "20092010", "20102011",
               "20112012", "20122013")
  games <- c(rep(1230,9), 720)

  #Noted difficulties with existing data in regular season.
  bad.game.list <- list(c(1:127, 134,135,  #Not in system.   0203
                          #419, 483,
                          582, 598, 872),  #bad images.           
                        c(10, 251, 453, 456, 482, 802),     #0304
                        c(18, 140,   #Visitor GIF makes segfault.   0506
                          298,  #wrong ES file -- 398 instead of 298.
                          458,  #bogus ES file.
                          974),  #0506
                        c(1024),  #missing line of players for a goal.    #0607

                        c(), c(259, 409, 1077),     #0708 0809
                        c(81, 827, 836, 857, 863, 874), c(429),   #0910 1011
                        c(259), c()) #1112 1213
  bad.playoff <- matrix(c("20032004", "0134",
                          "20052006", "0233"), nrow=2)  #ucase TR TD
  
  # Playoff brackets.
  game.rec <- array(NA, c(7*15, 3)); count <- 0
  for (kk in 1:4) for (ll in 1:2^(4-kk)) for (mm in 1:7) {
    count <- count+1
    game.rec[count,] <- c(kk,ll,mm)
  }
  gnum <- paste0("0", game.rec[,1], game.rec[,2], game.rec[,3])

  #Game roster. Game session; Game number;
  #To add: away team; home team. score. Date.
  for (ss in 1:length(seasons)) {
    gn1 <- as.character(1:games[ss]); while(any(nchar(gn1)<4)) gn1[nchar(gn1)<4] <- paste0("0", gn1[nchar(gn1)<4])
    df1 <- data.frame(season=seasons[ss],
                      session=c(rep("Regular", games[ss]), rep("Playoffs", length(gnum))),
                      gamenumber=c(gn1, gnum),
                      awayteam="",
                      hometeam="",
                      awayscore="",
                      homescore="",
                      date="",
                      valid=c(!(1:games[ss] %in% bad.game.list[[ss]]), rep(TRUE, length(gnum))),
                      stringsAsFactors=FALSE)

    game.roster <- rbind(game.roster, df1)   
  }
  game.roster[,1] <- as.character(game.roster[,1])
  game.roster[,2] <- as.character(game.roster[,2])

  for (kk in 1:dim(bad.playoff)[2]) game.roster$valid[game.roster$season==bad.playoff[1,kk] &
                                                      game.roster$session=="Playoffs" &
                                                      game.roster$gamenumber==bad.playoff[2,kk]] <- FALSE

  game.roster$gcode <- paste0(2+1*(game.roster$session=="Playoffs"), game.roster$gamenumber)
  
  return(game.roster)
  
}

download.single.game <- function (season="20122013", gcode="20001", rdata.folder="nhlr-data", verbose=TRUE) {
  #season="20122013"; gcode="20018"; rdata.folder="nhlr-data"; verbose=TRUE
  if (!(season %in% c("20022003", "20032004", "20052006", "20062007",
                      "20072008", "20082009", "20092010", "20102011",
                      "20112012", "20122013"))) stop(paste("Invalid season: ",season))

  if (verbose) message(paste("Downloading files for game", season, gcode))
  
  error.free <- TRUE
  game.rec <- list()

  infile <- paste("http://www.nhl.com/scores/htmlreports/",season,"/ES0",gcode,".HTM",sep="")
  game.rec$es <- try(unlist(strsplit(gsub("\\t", "", gsub("\\r", "", getURL(infile))), "\n")), TRUE)
  if (class(game.rec$es) == "try-error") game.rec$es <- NULL  #error.free <- FALSE
      
  infile <- paste("http://www.nhl.com/scores/htmlreports/",season,"/PL0",gcode,".HTM",sep="")
  game.rec$pl <- try(unlist(strsplit(gsub("\\t", "", gsub("\\r", "", getURL(infile))), "\n")), TRUE)
  if (class(game.rec$pl) == "try-error") game.rec$pl <- NULL  #error.free <- FALSE

  #see if x-y is there.
  infile <- paste0("http://live.nhl.com/GameData/",season,"/",substr(season,1,4),"0",gcode,"/PlayByPlay.json")
  file2 <- try(getURL(infile), TRUE)
  if (class(file2) != "try-error") {
    game.rec$xy <- try(fromJSON(file2))
    if (class(game.rec$xy) == "try-error") game.rec$xy <- NULL
  } else game.rec$xy <- NULL
  
  if (season %in% c("20022003", "20032004", "20052006", "20062007")) {
    
    infile <- paste("http://www.nhl.com/scores/htmlreports/",season,"/SCH",gcode,".gif",sep="")
    outfile <- paste0(rdata.folder,"/",season,"H",gcode,".gif")
    g1 <- try(download.file(infile, outfile, mode="wb"), TRUE)
    if (class(g1) == "try-error") {
      game.rec$imh <- NULL  #error.free <- FALSE
    } else {game.rec$imh <- read.gif(outfile)$image; file.remove(outfile)}
      
    infile <- paste("http://www.nhl.com/scores/htmlreports/",season,"/SCV",gcode,".gif",sep="")
    outfile <- paste0(rdata.folder,"/",season,"V",gcode,".gif")
    g1 <- try(download.file(infile, outfile, mode="wb"), TRUE)
    if (class(g1) == "try-error") {
      game.rec$imv <- NULL  #error.free <- FALSE
    } else {game.rec$imv <- read.gif(outfile)$image; file.remove(outfile)}
    
  }
  
  #if (!error.free) game.rec <- NULL
  suppressWarnings(dir.create(rdata.folder))
  save (game.rec, file=paste0(rdata.folder, "/", season, "-", gcode, ".RData"))

  return (error.free)
  
}


download.games <- function (games=full.game.database(), rdata.folder="nhlr-data") {
  #games=full.game.database(); games = games[games$session=="Playoffs" & games[,1] == "20122013",]
  
  for (kk in 1:dim(games)[1]) if (games$valid[kk])
    games$valid[kk] <- download.single.game(games$season[kk],
                                            paste0(2+1*(games$session[kk]=="Playoffs"),
                                                   games$gamenumber[kk]))
  return(games)
  
}




process.single.game <- function (season="20122013", gcode="20001",
                                 rdata.folder="nhlr-data",
                                 override.download=FALSE,
                                 save.to.file=TRUE) {
  #season="20122013"; gcode="20712"; rdata.folder="nhlr-data"; override.download=FALSE; save.to.file=TRUE


  if (!file.exists(paste0(rdata.folder, "/", season, "-", gcode, ".RData")) | override.download) {

    #For Matt Taddy, 7-10-13.
    #message("process.single.game -- Current directory:",getwd()); message(season, gcode, rdata.folder)

    dl.time <- download.single.game(season, gcode, rdata.folder)
  }
  game.rec <- NULL   #loaded in next line.
  load (paste0(rdata.folder, "/", season, "-", gcode, ".RData"))
  if (season %in% c("20022003", "20032004", "20052006", "20062007") & (is.null(game.rec$imh) | is.null(game.rec$imv))) {
    message("Re-downloading single game files due to incompleteness in graphics files.")
    dl.time <- download.single.game(season, gcode, rdata.folder)
    load (paste0(rdata.folder, "/", season, "-", gcode, ".RData"))
  }

  #game.rec
  if (!is.null(game.rec)) {
    if (length(game.rec$es)>10 & length(game.rec$pl>10)) {
      if (season %in% c("20022003", "20032004", "20052006", "20062007")) {
        suppressWarnings(game.info <- integrate.old.pieces (game.rec$imh, game.rec$imv,
                                          game.rec$es, game.rec$pl, gcode))
      } else {
        suppressWarnings(game.info <- integrate.new.pieces (game.rec$es, game.rec$pl))
      }
    } else game.info <- list(playbyplay=data.frame(), teams=c("",""),
                            date=rep("",4), players=data.frame())
  } else game.info <- list(playbyplay=data.frame(), teams=c("",""),
                            date=rep("",4), players=data.frame())

  if (length(game.info$playbyplay) > 0) {
  #xy?
    game.info$playbyplay$xcoord <- NA; game.info$playbyplay$ycoord <- NA
    if (!is.null(game.rec$xy)) game.info <- match.xy(game.info, game.rec$xy)
 
    playbyplay <- game.info$playbyplay

  #Changes need scrubbing
    playbyplay$distance[playbyplay$etype=="CHANGE"] <- NA
    playbyplay$type[playbyplay$etype=="CHANGE"] <- ""
    playbyplay$homezone[playbyplay$etype=="CHANGE"] <- "Neu"


  #teams.
    playbyplay$awayteam <- game.info$teams[1]
    playbyplay$hometeam <- game.info$teams[2]
    dateinfo <- strptime(paste(game.info$date, collapse=" "), "%a %b %d %Y")
    ydays <- cumsum(c(0, 365, 366,
                      365, 365, 365, 366,
                      365, 365, 365, 366,
                      365, 365, 365, 366))   #through 2016.
    refdate <- dateinfo$yday+ydays[as.numeric(game.info$date[4])-2001]
    if (length(refdate) == 0) refdate <- 0
    playbyplay <- cbind(season, gcode, refdate, playbyplay)

  
  #goals and game score.
    playbyplay$away.score <- playbyplay$home.score <- 0
    home.goals <- which(playbyplay$etype=="GOAL" & playbyplay$ev.team==game.info$teams[2])
    if (length(home.goals)>0) for (gg in 1:length(home.goals)) if (home.goals[gg] < dim(playbyplay)[1])
      playbyplay$home.score[(home.goals[gg]+1):dim(playbyplay)[1]] <-
        playbyplay$home.score[(home.goals[gg]+1):dim(playbyplay)[1]] + 1
    
    away.goals <- which(playbyplay$etype=="GOAL" & playbyplay$ev.team==game.info$teams[1])
    if (length(away.goals)>0) for (gg in 1:length(away.goals)) if (away.goals[gg] < dim(playbyplay)[1])
      playbyplay$away.score[(away.goals[gg]+1):dim(playbyplay)[1]] <-
        playbyplay$away.score[(away.goals[gg]+1):dim(playbyplay)[1]] + 1

  #event length.
    playbyplay$event.length <- playbyplay$seconds
    playbyplay$event.length[2:dim(playbyplay)[1]] <-
      playbyplay$event.length[2:dim(playbyplay)[1]] - playbyplay$event.length[1:(dim(playbyplay)[1]-1)]

  #identify goaltenders, remove from player lists.
    player.list <- game.info$players
    player.list <- rbind(player.list, "")
    playbyplay$away.G <- ""; for (kk in paste0("a",1:6)) {
      column <- playbyplay[,kk]; column[is.na(column)] <- ""
      pl.match <- match(column, player.list$numfirstlast)
      picks <- player.list$pos[pl.match]; picks[is.na(picks)] <- ""
      if (sum(picks=="G")>0) {
        playbyplay$away.G[picks=="G"] <-
          player.list$numfirstlast[pl.match][picks=="G"]
        playbyplay[picks=="G", kk] <- ""
      }
    }
    playbyplay$home.G <- ""; for (kk in paste0("h",1:6)) {
      column <- playbyplay[,kk]; column[is.na(column)] <- ""
      pl.match <- match(column, player.list$numfirstlast)
      picks <- player.list$pos[pl.match]; picks[is.na(picks)] <- ""
      if (sum(picks=="G")>0) {
        playbyplay$home.G[picks=="G"] <-
          player.list$numfirstlast[pl.match][picks=="G"]
        playbyplay[picks=="G", kk] <- ""
      }
    }

  #Done.
    game.info$playbyplay <- playbyplay
  }
    
  if (save.to.file) save (game.info, file=paste0(rdata.folder, "/", season, "-", gcode, "-processed.RData"))
  return(game.info)
  
}

process.games <- function (games=full.game.database(),
                           rdata.folder="nhlr-data",
                           override.download=FALSE) {
  #games=full.game.database(); games = games[5341:5456,]
  
  
  item <- list()
  for (kk in which(games$valid)) {
    message (paste(kk, games[kk,1], paste0(2+1*(games$session[kk]=="Playoffs"), games$gamenumber[kk])))
    item <- #c(item,
              process.single.game(
                games$season[kk],
                games$gcode[kk],
                rdata.folder=rdata.folder,
                override.download=override.download,
                save.to.file=TRUE)
             # )
  }

  #save(item, file="all-archived.RData")
  return(TRUE)
  
}






retrieve.game <- function (season="20122013", gcode="20001",
                       rdata.folder="nhlr-data") {
  #season="20122013"; gcode="20001"; rdata.folder="nhlr-data"
  
  if (!file.exists(paste0(rdata.folder, "/", season, "-", gcode, "-processed.RData"))) {
    #For Matt Taddy, 7-10-13.
    #message("retrieve.game -- Current directory:",getwd()); message(season, gcode, rdata.folder)

    game.info <- process.single.game (season, gcode, rdata.folder, save.to.file=TRUE)
  } else load (paste0(rdata.folder, "/", season, "-", gcode, "-processed.RData"))
  return(game.info)

}




patch.for.shottypes <- function(grand.data) {

  grand.data$home.skaters <- apply(1*(grand.data[,c(paste0("h",1:6), "home.G")] > 1), 1, sum)
  grand.data$away.skaters <- apply(1*(grand.data[,c(paste0("a",1:6), "away.G")] > 1), 1, sum)

  etypes <- c("GOAL","SHOT","MISS","BLOCK")
  shot.rows <- which(grand.data$etype %in% etypes)

  grand.data$type[shot.rows][grand.data$type[shot.rows] == "Tip-in"] <- "Tip-In"
  grand.data$type[shot.rows][grand.data$type[shot.rows] == "Wrap-around"] <- "Wrap"  
  shotstyles <- c("Backhand", "Tip-In", "Wrist", "Snap", "Slap", "Wrap", "Deflected")
  grand.data$type[shot.rows][!(grand.data$type[shot.rows] %in% shotstyles)] <- "Unspecified"
  grand.data$distance <- as.numeric(grand.data$distance)
  rownames(grand.data) <- 1:nrow(grand.data)
  #save(grand.data, file="grand-10plus.RData")

  return(grand.data)
}

augment.game <- function (game.info, player.list) {
  #game.info=sample.game; player.list=roster; season=""; gcode=""

  playbyplay <- game.info$playbyplay; teams <- game.info$teams
  if (length(playbyplay) == 0) stop ("Play-by-play table does not exist.")
  if (length(player.list) == 0) stop ("Player roster does not exist.")
  
  
  #replace players with ID numbers.
  for (cc in c(paste0("a",1:6), paste0("h",1:6), "away.G", "home.G", "ev.player.1", "ev.player.2", "ev.player.3")) {
    replacement <- player.list$player.id[match(playbyplay[,cc], player.list$numfirstlast)]
    if (is.null(replacement)) replacement <- rep(NA, dim(playbyplay)[1])
    playbyplay[,cc] <- replacement
    playbyplay[is.na(playbyplay[,cc]),cc] <- 1
  }

  playbyplay <- patch.for.shottypes(playbyplay)
  return(playbyplay)
}


fix.names.manually <- function (master.list) {
  
  #one name, two players.
  master.list$first[which(master.list$last=="PICARD" & master.list$first=="ALEXANDRE" & master.list$pos == "D")] <- "ALEXANDRE R."
  
  #manual fixes.
  master.list$last[which(master.list$last=="ANDERSSON" & master.list$first=="CRAIG")] <- "ANDERSON"
  master.list$first[which(master.list$last=="ANTROPOV" & master.list$first=="NIKOLAI")] <- "NIK"
  master.list$first[which(master.list$last=="AULD" & master.list$first=="ALEXANDER")] <- "ALEX"
  master.list$first[which(master.list$last=="AXELSSON" & master.list$first=="PER JOHAN")] <- "P.J."
  master.list$first[which(master.list$first=="P. J. ")] <- "P.J."
  master.list$first[which(master.list$last=="BAILEY" & master.list$first=="JOSHUA")] <- "JOSH"
  master.list$first[which(master.list$last=="BARCH" & master.list$first=="KRYSTOFER")] <- "KRYS"
  master.list$first[which(master.list$last=="BARKER" & master.list$first=="CAMERON")] <- "CAM"
  master.list$first[which(master.list$last=="BERGFORS" & master.list$first=="NICKLAS")] <- "NICLAS"
  master.list$first[which(master.list$last=="BLACKBURN" & master.list$first=="DANIEL")] <- "DAN"
  master.list$first[which(master.list$last=="BLAKE" & master.list$first=="ROBERT")] <- "ROB"
  master.list$first[which(master.list$last=="BLUNDEN" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="BOURQUE" & master.list$first=="CHRISTOPHER")] <- "CHRIS"
  master.list$first[which(master.list$last=="BOYNTON" & master.list$first=="NICHOLAS")] <- "NICK"
  master.list$first[which(master.list$last=="BRIERE" & master.list$first=="DANNY")] <- "DANIEL"
  master.list$first[which(master.list$last=="BRYZGALOV" & master.list$first=="ILJA")] <- "ILYA"
  master.list$first[which(master.list$last=="BURROWS" & master.list$first=="ALEXANDRE")] <- "ALEX"
  master.list$first[which(master.list$last=="CAMMALLERI" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="CARCILLO" & master.list$first=="DANIEL")] <- "DAN"
  master.list$first[which(master.list$last=="CARLE" & master.list$first=="MATTHEW")] <- "MATT"
  master.list$first[which(master.list$last=="CLEARY" & master.list$first=="DAN")] <- "DANIEL"
  master.list$first[which(master.list$last=="CLEARY" & master.list$first=="DANNY")] <- "DANIEL"
  master.list$first[which(master.list$last=="CORVO" & master.list$first=="JOSEPH")] <- "JOE"
  master.list$first[which(master.list$last=="CRABB" & master.list$first=="JOSEPH")] <- "JOEY"
  master.list$first[which(master.list$last=="CROMBEEN" & master.list$first=="BJ")] <- "B.J."
  master.list$first[which(master.list$last=="CROMBEEN" & master.list$first=="BRANDON")] <- "B.J."
  master.list$first[which(master.list$last=="DADONOV" & master.list$first=="EVGENII")] <- "EVGENY"
  
  master.list$first[which(master.list$last=="DOWD" & master.list$first=="JAMES")] <- "JIM"
  master.list$first[which(master.list$last=="DOWELL" & master.list$first=="JACOB")] <- "JAKE"
  master.list$first[which(master.list$last=="DRAZENOVIC" & master.list$first=="NICHOLAS")] <- "NICK"

  master.list$first[which(master.list$last=="DUMONT" & master.list$first=="J P")] <- "JEAN-PIERRE"
  master.list$first[which(master.list$last=="DUMONT" & master.list$first=="J-P")] <- "JEAN-PIERRE"
  master.list$first[which(master.list$last=="EARL" & master.list$first=="ROBBIE")] <- "ROBERT"
  master.list$first[which(master.list$last=="FERNANDEZ" & master.list$first=="EMMANUEL")] <- "MANNY"
  master.list$first[which(master.list$last=="FROLOV" & master.list$first=="ALEXANDER")] <- "ALEX"
  master.list$first[which(master.list$first=="TJ")] <- "T.J."
  master.list$first[which(master.list$last=="GAUTHIER" & master.list$first=="DENIS JR.")] <- "DENIS"
  master.list$first[which(master.list$last=="GIGUERE" & master.list$first=="J")] <- "JEAN-SEBASTIEN"
  master.list$first[which(master.list$last=="GIRARDI" & master.list$first=="DAN")] <- "DANIEL"
  master.list$first[which(master.list$last=="GREENE" & master.list$first=="ANDY")] <- "ANDREW"
  master.list$first[which(master.list$last=="GREER" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="GROSSMAN" & master.list$first=="NIKLAS")] <- "NICKLAS"
  master.list$last[which(master.list$last=="GROSSMAN" & master.list$first=="NICKLAS")] <- "GROSSMANN"
  master.list$first[which(master.list$last=="GUENIN" & master.list$first=="NATE")] <- "NATHAN"
  master.list$first[which(master.list$last=="HALKO" & master.list$first=="STEVE")] <- "STEVENN"
  master.list$first[which(master.list$last=="HIGGINS" & master.list$first=="CHRISTOPHER")] <- "CHRIS"
  
  master.list$last[which(master.list$last=="HILLEN III" & master.list$first=="JOHN")] <- "HILLEN"
  master.list$first[which(master.list$last=="HILLEN" & master.list$first=="JOHN")] <- "JACK"
  master.list$first[which(master.list$last=="HOLIK" & master.list$first=="ROBERT")] <- "BOBBY"
  master.list$first[which(master.list$last=="HOWARD" & master.list$first=="JAMES")] <- "JIMMY"
  
  master.list$first[which(master.list$last=="IRWIN" & master.list$first=="MATTHEW")] <- "MATT"
  master.list$first[which(master.list$last=="JACKMAN" & master.list$first=="RICHARD")] <- "RIC"
  master.list$first[which(master.list$last=="JACQUES" & master.list$first=="J-F")] <- "JEAN-FRANCOIS"
  master.list$first[which(master.list$last=="JOHANSSON" & master.list$first=="MATTIAS")] <- "MATHIAS"
  master.list$first[which(master.list$last=="KALINSKI" & master.list$first=="JONATHON")] <- "JON"
  
  master.list$last[which(master.list$last=="KASTSITSYN")] <- "KOSTITSYN"
  master.list$first[which(master.list$last=="KOSTITSYN" & master.list$first=="SIARHEI")] <- "SERGEI"

  master.list$first[which(master.list$last=="KILLORN" & master.list$first=="ALEXANDER")] <- "ALEX"
  master.list$first[which(master.list$last=="KING" & master.list$first=="DWAYNE")] <- "D.J."
  
  master.list$first[which(master.list$first=="DJ")] <- "D.J."
  master.list$first[which(master.list$last=="KNUBLE" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="KOLANOS" & master.list$first=="KRYSTOFER")] <- "KRYS"
  master.list$first[which(master.list$last=="KOMISAREK" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="KONDRATIEV" & master.list$first=="MAX")] <- "MAXIM"
  master.list$first[which(master.list$last=="KOVALEV" & master.list$first=="ALEXEI")] <- "ALEX"
  master.list$first[which(master.list$last=="KRONVALL")] <- "KRONWALL"
  master.list$first[which(master.list$last=="LEGACE" & master.list$first=="EMMANUEL")] <- "MANNY"
  master.list$first[which(master.list$last=="LETANG" & master.list$first=="KRISTOPHER")] <- "KRIS"

  master.list$first[which(master.list$last=="MACIAS" & master.list$first=="RAYMOND")] <- "RAY"

  master.list$first[which(master.list$last=="MACLEAN" & master.list$first=="DONALD")] <- "DON"
  master.list$last[which(master.list$last=="MAGNAN-GRENIER")] <- "MAGNAN"
  master.list$first[which(master.list$last=="MAYOROV" & master.list$first=="MAXIM")] <- "MAKSIM"
  master.list$first[which(master.list$last=="MCCOLLUM" & master.list$first=="TOM")] <- "THOMAS"
  master.list$first[which(master.list$last=="MCGILLIS" & master.list$first=="DAN")] <- "DANIEL"
  master.list$last[which(master.list$last=="MEYER IV")] <- "MEYER"
  master.list$first[which(master.list$last=="MEYER" & master.list$first=="FREDDY")] <- "FREDERICK"
  master.list$first[which(master.list$last=="MILLER" & master.list$first=="ANDREW")] <- "DREW"

  master.list$first[which(master.list$last=="MILLS" & master.list$first=="BRADLEY")] <- "BRAD"
  
  master.list$first[which(master.list$last=="MODANO" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="MODIN" & master.list$first=="FREDDY")] <- "FREDRIK"
  master.list$first[which(master.list$last=="NEIL" & master.list$first=="CHRISTOPHER")] <- "CHRIS"

  master.list$first[which(master.list$last=="ODUYA" & master.list$first=="DAVID JOHNNY")] <- "JOHNNY"
  master.list$first[which(master.list$last=="ODUYA" & master.list$first=="JOHN")] <- "JOHNNY"
  master.list$last[which(master.list$last=="ORTMYER" & master.list$first=="JED")] <- "ORTMEYER"
  master.list$first[which(master.list$last=="OVECHKIN" & master.list$first=="ALEXANDER")] <- "ALEX"
  
  master.list$first[which(master.list$last=="PARENTEAU" & master.list$first=="PIERRE")] <- "P.A."
  master.list$first[which(master.list$last=="PARENTEAU" & master.list$first=="PA")] <- "P.A."
  master.list$first[which(master.list$last=="PELLEY" & master.list$first=="RODNEY")] <- "ROD"
  master.list$first[which(master.list$last=="PEVERLEY" & master.list$first=="JOHN")] <- "RICH"

  master.list$first[which(master.list$last=="POULIOT" & master.list$first=="MARC")] <- "MARC-ANTOINE"

  master.list$first[which(master.list$last=="PROSPAL" & master.list$first=="VINNY")] <- "VACLAV"
  master.list$first[which(master.list$last=="PURCELL" & master.list$first=="EDWARD")] <- "TEDDY"

  master.list$last[which(master.list$last=="PUSHKAREV" & master.list$first=="KONSTANTIN")] <- "PUSHKARYOV"
  master.list$first[which(master.list$last=="REINPRECHT" & master.list$first=="STEVE")] <- "STEVEN"
  master.list$first[which(master.list$last=="RISSMILLER" & master.list$first=="PAT")] <- "PATRICK"
  master.list$first[which(master.list$last=="RUPP" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="SANTORELLI" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="SCUDERI" & master.list$first=="ROBERT")] <- "ROB"

  master.list$first[which(master.list$last=="SESTITO" & master.list$first=="TOMMY")] <- "TOM"
  master.list$last[which(master.list$last=="SHISKANOV" & master.list$first=="TIMOFEI")] <- "SHISHKANOV"
  master.list$first[which(master.list$last=="SILLINGER" & master.list$first=="MICHAEL")] <- "MIKE"
  
  master.list$first[which(master.list$last=="SIM" & master.list$first=="JON")] <- "JONATHAN"
  master.list$first[which(master.list$last=="SIMON" & master.list$first=="BEN")] <- "BENJAMIN"
  master.list$first[which(master.list$last=="STAJAN" & master.list$first=="MATTHEW")] <- "MATT"
  
  master.list$first[which(master.list$last=="STEEN" & master.list$first=="ALEXANDER")] <- "ALEX"
  master.list$last[which(master.list$last=="ST LOUIS" & master.list$first=="MARTIN")] <- "ST. LOUIS"
  master.list$first[which(master.list$last=="STORTINI" & master.list$first=="ZACHERY")] <- "ZACK"
  master.list$last[which(master.list$last=="ST PIERRE" & master.list$first=="MARTIN")] <- "ST. PIERRE"
  master.list$last[which(master.list$last=="STREBAK" & master.list$first=="MARTIN")] <- "STRBAK"
  master.list$first[which(master.list$first=="PK")] <- "P.K."

  master.list$first[which(master.list$last=="TAYLOR" & master.list$first=="TIMOTHY")] <- "TIM"
  master.list$first[which(master.list$last=="THOMAS" & master.list$first=="TIMOTHY JR.")] <- "TIM"
  master.list$first[which(master.list$last=="THOMAS" & master.list$first=="WILLIAM")] <- "BILL"


  
  master.list$first[which(master.list$first=="RJ")] <- "R.J."
  master.list$first[which(master.list$last=="VALICEVIC" & master.list$first=="ROBERT")] <- "ROB"
  master.list$first[which(master.list$last=="VALIQUETTE" & master.list$first=="STEVE")] <- "STEPHEN"
  master.list$first[which(master.list$last=="VANDERMEER" & master.list$first=="JAMES")] <- "JIM"
  master.list$first[which(master.list$last=="VARLAMOV" & master.list$first=="SIMEON")] <- "SEMYON"
  master.list$last[which(master.list$last=="VANDE VELDE" & master.list$first=="CHRIS")] <- "VANDEVELDE"

  master.list$first[which(master.list$last=="WOZNIEWSKI" & master.list$first=="ANDREW")] <- "ANDY"
  master.list$first[which(master.list$last=="WYMAN" & master.list$first=="JT")] <- "JAMES"

  master.list$first[which(master.list$last=="YORK" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="ZHERDEV" & master.list$first=="NIKOLAY")] <- "NIKOLAI"
  master.list$first[which(master.list$last=="ZOLNIERCZYK" & master.list$first=="HARRISON")] <- "HARRY"
  
  master.list <- master.list[order(master.list[,2],
                                   master.list[,3],
                                   master.list[,1]),]
  return(master.list)
}

  
make.unique.player.list <- function (master.list) {

  #add Blanky.
  #master.list <- rbind("", master.list)
  #master.list <- fix.names.manually (master.list)

  master.list$index <- 1:dim(master.list)[1]
  for (kk in 2:dim(master.list)[1])
    if (all(master.list[kk,2:3] == master.list[kk-1,2:3]))
      master.list[kk,5] <- master.list[kk-1,5]
  player.id.proto <- unique(master.list[,5])
  master.list$player.id <- match(master.list[,5], player.id.proto)
  return(master.list)
}



construct.rosters <- function (games=full.game.database(),
                               rdata.folder="nhlr-data",

                               roster.master=NULL,
                               positions=NULL) {
  
  #games=full.game.database(); rdata.folder="nhlr-data"; roster.master=NULL; roster.unique=NULL; positions=NULL

  message ("Begin: Loading rosters from game files.")
  roster.collection <- list()
  miscount <- 0
  for (kk in 1:dim(games)[1])  #which(games$season=="20122013" & games$session == "Playoffs"))  #
    if (games$valid[kk]) {
      tryme <- try({
        game.info <- retrieve.game(games$season[kk], games$gcode[kk], rdata.folder)
        if (length(game.info$players)>0)
          roster.collection[[kk]] <- fix.names.manually(game.info$players[,c(2,5,6,8)])
          
        games$awayteam[kk] <- game.info$teams[1]
        games$hometeam[kk] <- game.info$teams[2]
        games$date[kk] <- paste(game.info$date, collapse=" ")
        games$awayscore[kk] <- length(which(game.info$playbyplay$etype=="GOAL" &
                                            game.info$playbyplay$ev.team==game.info$teams[1]))
        games$homescore[kk] <- length(which(game.info$playbyplay$etype=="GOAL" &
                                            game.info$playbyplay$ev.team==game.info$teams[2]))

        if (length(game.info$playbyplay) == 0) games$valid[kk] <- FALSE
      }, TRUE)
      if (class(tryme) == "try-error") {games$valid[kk] <- FALSE; miscount <- miscount+1}
      if (kk %% 100 == 0) message(paste("Roster loading: game",kk,"of",dim(games)[1],"with",miscount,"skips."))
    }
  message ("End: Loading rosters from game files.")

  blanky <- data.frame (pos="", last="", first="", numfirstlast="",
                        firstlast="", index=1,
                        player.id=1, stringsAsFactors=FALSE)
  if (is.null(roster.master))
    roster.master <- blanky
  
  for (kk in 1:length(roster.collection)) if (games$valid[kk]) {
    if (kk %% 500 == 0) message(paste("Roster merger: game",kk,"of",dim(games)[1]))
    for (pp in 1:dim(roster.collection[[kk]])[1]) {
      m1 <- match(roster.collection[[kk]]$numfirstlast[pp],
                  roster.master$numfirstlast)
      if (is.na(m1)) { #no exact match?
        rec1 <- blanky
        rec1$pos <- roster.collection[[kk]]$pos[pp];
        rec1$first <- roster.collection[[kk]]$first[pp];
        rec1$last <- roster.collection[[kk]]$last[pp];
        rec1$numfirstlast <- roster.collection[[kk]]$numfirstlast[pp];
        rec1$firstlast <- paste(rec1$first, rec1$last)
        rec1$index <- dim(roster.master)[1] + 1
        
        m2 <- match(rec1$firstlast,
                    roster.master$firstlast)
        if (is.na(m2)) rec1$player.id <- max(roster.master$player.id)+1 else rec1$player.id <- roster.master$player.id[m2]

        roster.master <- rbind(roster.master, rec1)
      }
    }
  }
  

  #positions table and unique player list.
  message("Finding most likely position for each player.")
    
  if (is.null(positions)) {
    zeroes <- rep(0, dim(roster.master)[1])
    positions <- data.frame(pC=zeroes, pL=zeroes, pR=zeroes, pD=zeroes, pG=zeroes)
  } else while (dim(positions)[1] < dim(roster.master)[1]) positions <- rbind(positions, 0)
  
  for (kk in 1:length(roster.collection)) if (games$valid[kk]) {
    r1.match <- match(roster.collection[[kk]]$numfirstlast,
                      roster.master$numfirstlast)
    positions$pC[r1.match[roster.collection[[kk]]$pos=="C"]] <-
      positions$pC[r1.match[roster.collection[[kk]]$pos=="C"]] + 1
    positions$pL[r1.match[roster.collection[[kk]]$pos=="L"]] <-
      positions$pL[r1.match[roster.collection[[kk]]$pos=="L"]] + 1
    positions$pR[r1.match[roster.collection[[kk]]$pos=="R"]] <-
      positions$pR[r1.match[roster.collection[[kk]]$pos=="R"]] + 1
    positions$pD[r1.match[roster.collection[[kk]]$pos=="D"]] <-
      positions$pD[r1.match[roster.collection[[kk]]$pos=="D"]] + 1
    positions$pG[r1.match[roster.collection[[kk]]$pos=="G"]] <-
      positions$pG[r1.match[roster.collection[[kk]]$pos=="G"]] + 1
  }

  unique.entries <- match(1:max(roster.master$player.id), roster.master$player.id)
  roster.unique <- roster.master[unique.entries,]

  roster.unique$pos <- sapply(1:max(roster.master$player.id), function(kk) {
    pcount <- apply(rbind(positions[roster.master$player.id==kk,]), 2, sum)
    c("C","L","R","D","G")[min(which(pcount==max(pcount)))]
  })
  
  #write.csv(roster.master, "master.list.csv", quote=FALSE); write.csv(games, "games.csv", quote=FALSE)
  return(list(roster.master=roster.master,
              roster.unique=roster.unique,
              games=games,
              positions=positions))
  
}




assemble.mega.file <- function (player.list,
                                games=full.game.database(),
                                rdata.folder="nhlr-data",
                                output.file="nhlscrapr-record.RData") {

  all.games <- list()
  for (kk in 1:dim(games)[1])  #which(games$season=="20122013" & games$session == "Playoffs"))  #
    if (games$valid[kk]) {
      tryme <- try({
        game.info <-
          retrieve.game(games$season[kk],
                    games$gcode[kk],
                    rdata.folder)
        if (length(game.info$playbyplay)>0) 
          all.games[[kk]] <- augment.game(game.info, player.list)
      }, TRUE)
      if (kk %% 100 == 0) message(paste("Event assembly: game",kk,"of",dim(games)[1]))
    }

  save(all.games, player.list, games, file=output.file)
  return(TRUE)

}



fold.frames <- function(frame.list) {
  #frame.list = all.games

  repeat {
    hold.list <- list()
    for (kk in 1:floor(length(frame.list)/2))
      hold.list[[kk]] <- tryCatch(
                           rbind(frame.list[[2*kk-1]], frame.list[[2*kk]]),
                           warning = function(war) message(paste(kk, war)),
                           error = function(err) message(paste(kk, err)),
                           finally = {})
    if (length(frame.list) %% 2 == 1) hold.list[[kk]] <- rbind(hold.list[[kk]], frame.list[[2*kk+1]])
    frame.list <- hold.list
    rm(hold.list)
    print(length(frame.list))
    if (length(frame.list) == 1) break
  }
  
  return(frame.list[[1]])
}



nhlscrapr.everything <- function () {

  #What are all games that can/should be downloaded? valid=FALSE implies previously screened problems.
  #Just a subset for testing.
  games <- full.game.database()

  #Takes HTML files and (possibly) GIF images and produces event and player tables for each game.
  process.games (games)

  #Take all the game table roster lists and produce both a unique player
  #list and an improved game table.
  roster.main <- construct.rosters (games)

  roster.master <- roster.main$roster.master
  roster.unique <- roster.main$roster.unique
  games <- roster.main$games
  
  save(roster.master, roster.unique, games, file="nhl20022013.RData")
 
  #Do it all at once for a big database.
  assemble.mega.file (roster.master, games, output.file="mynhlscrapes.RData")

  #library(nhlscrapr); load("mynhlscrapes.RData"); grand.data <- fold.frames(all.games); save(grand.data, file="grand10.RData")
  
}





