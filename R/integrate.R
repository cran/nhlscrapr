

#display.html <- function(season, gcode, folder="nhlr-data") {load(paste0(folder,"/",season,"-",gcode,".RData")); write.table(game.rec$es, "es1.html", row.names=FALSE, col.names=FALSE, quote=FALSE); write.table(game.rec$pl, "pl1.html", row.names=FALSE, col.names=FALSE, quote=FALSE); system(paste("google-chrome es1.html"), wait=FALSE); system(paste("google-chrome pl1.html"), wait=FALSE)}

# Combine all the data frames into one big collection.

.onAttach <- function (...) {
  packageStartupMessage("nhlscrapr v 1.8")
}

fold.frames <- function(frame.list) {
  #frame.list = new.pbp.2

    if (length(frame.list) > 1) repeat {
        if (length(frame.list) == 1) break
        hold.list <- list()
        for (kk in 1:floor(length(frame.list)/2))
            hold.list[[kk]] <- tryCatch(
                rbind(frame.list[[2*kk-1]], frame.list[[2*kk]]),
                warning = function(war) message(paste(kk, war)),
                error = function(err) message(paste(kk, err)),
                finally = {})
        if (length(frame.list) %% 2 == 1)
            if (length(hold.list) > 0)
                hold.list[[length(hold.list)]] <- rbind(hold.list[[length(hold.list)]],
                                                        frame.list[[2*kk+1]]) else hold.list <- frame.list[2*kk+1]
        
        frame.list <- hold.list
        rm(hold.list)
        message ("Folding data frames. Total: ",length(frame.list))
        if (length(frame.list) == 1) break
    }
    
    return(frame.list[[1]])
}



# Produce the database of games. Add on extra seasons if desired.
full.game.database <- function (extra.seasons=0) {

  game.roster <- NULL
  seasons <- c("20022003", "20032004", "20052006", "20062007",
               "20072008", "20082009", "20092010", "20102011",
               "20112012", "20122013", "20132014", "20142015")
  if (extra.seasons > 0)
    seasons <- c(seasons, paste(2013+1:extra.seasons,
                                2014+1:extra.seasons, sep=""))
  games <- c(rep(1230,9), 720, 1230, 1230, rep(1230,extra.seasons))

  #Noted difficulties with existing data in regular season.
  bad.game.list <- list(c(1:127, 134,135,  #Not in system.   0203
                          #419, 483,
                          582, 598, 872),  #bad images.           
                        c(10, 251, 453, 456, 482, 802,   1205),     #0304
                        c(18, 140,   #Visitor GIF makes segfault.   0506
                          127,
                          234,
                          298,  #wrong ES file -- 398 instead of 298.
                          458,  #bogus ES file.
                          974),  #0506
                        c(1024),  #missing line of players for a goal.    #0607

                        c(1178), c(259, 409, 1077),     #0708 0809
                        c(81, 827, 836, 857, 863, 874, 885), c(124, 429),   #0910 1011
                        c(259), c(), c(), c()) #1112 1213 1314
  if (extra.seasons > 0) bad.game.list[[length(bad.game.list)+1]] <- c()
    
  # Playoff brackets.
  playoff.series <- c("11","12","13","14","15","16","17","18",
                      "21","22","23","24","31","32","41")
  gnum <- paste0("0", c(t(outer(playoff.series, 1:7, paste0))))

  #Game roster. Game session; Game number;
  #To add: away team; home team. score. Date.
  for (ss in 1:length(seasons)) {
    gn1 <- as.character(1:games[ss]); while(any(nchar(gn1)<4)) gn1[nchar(gn1)<4] <- paste0("0", gn1[nchar(gn1)<4])
    df1 <- data.frame(season=seasons[ss],
                      session=c(rep("Regular", games[ss]), rep("Playoffs", length(gnum))),
                      gamenumber=c(gn1, gnum),
                      gcode="",
                      status=1,
                      valid=c(!(1:games[ss] %in% bad.game.list[[ss]]), rep(TRUE, length(gnum))),
                      
                      awayteam="", hometeam="", awayscore="", homescore="",
                      date="",

                      game.start="", game.end="", periods=0,
                      
                      stringsAsFactors=FALSE)

    game.roster <- rbind(game.roster, df1)
  }
  game.roster[,1] <- as.character(game.roster[,1])
  game.roster[,2] <- as.character(game.roster[,2])
  game.roster$gcode <- paste0(2+1*(game.roster$session=="Playoffs"), game.roster$gamenumber)

  game.roster$status[!game.roster$valid] <- 0
  game.roster <- game.roster[,colnames(game.roster) != "valid"]

  #Knock out unplayed playoff games. Here's the data from the last 11 seasons.
  playoff.series.lengths <- 
    c(5,5,6,7,6,4,7,7, 6,5,6,7, 7,4, 7,
      5,7,5,7,6,5,7,5, 4,6,6,6, 7,6, 7, #2004
      5,6,4,6,6,5,7,5, 5,5,6,4, 7,5, 7, #2006
      5,6,4,5,6,5,7,5, 6,5,6,5, 5,6, 5,
      7,4,7,5,6,7,6,6, 5,5,4,6, 5,6, 6, #2008
      4,7,7,6,6,4,4,6, 7,7,7,6, 4,5, 7,
      7,5,6,6,6,6,6,7, 7,7,5,6, 5,4, 6,
      5,7,7,7,7,6,4,6, 4,4,6,7, 7,5, 7, #2011
      7,7,7,6,5,5,6,5, 7,5,4,5, 6,5, 6,
      6,5,7,7,5,7,4,6, 5,5,7,7, 4,5, 6, #2013
      5,4,6,7,7,6,6,7, 7,7,6,7, 6,7, 5, #2014
      rep(7,15),
      rep(7, 15*(extra.seasons)))  #,matrix( nrow=15)
  sequence.seven <- function(nn) c(rep(1, nn), rep(0, 7-nn))
  playoff.status <- c(sapply(playoff.series.lengths, sequence.seven))
  game.roster$status[game.roster$session=="Playoffs"] <- playoff.status
  
  bad.playoff <- matrix(c("20032004", "30134",
                          "20052006", "30233"), nrow=2)  #ucase TR TD
  for (kk in 1:dim(bad.playoff)[2]) 
    game.roster$status[game.roster$season == bad.playoff[1,kk] &
                       game.roster$gcode == bad.playoff[2,kk]] <- 0

  ## Add game data for 2014-2015. uses "date201415.RData".
  gamecols <- match(paste0("20142015",as.character(nhlscrapr::date201415$gcode)), paste0(game.roster$season, game.roster$gcode))
  game.roster$awayteam[gamecols] <- as.character(nhlscrapr::date201415$awayteam)
  game.roster$hometeam[gamecols] <- as.character(nhlscrapr::date201415$hometeam)
  unplayed <- which(game.roster$game.start[gamecols] == "")
  game.roster$game.start[gamecols[unplayed]] <- paste(as.character(nhlscrapr::date201415$StartET[unplayed]),"ET")
  game.roster$date[gamecols[unplayed]] <- as.character(nhlscrapr::date201415$GameDate[unplayed])

  return(game.roster)
  
}


current.games <- function (rdata.folder="nhlr-data") {

  records <- list.files (rdata.folder)
  #unprocessed.games.files <- records[grep("[0-9]+\\-[0-9]+\\.RData", records)]
  processed.games.files <- records[grep("processed", records)]
  processed.games <- data.frame(season=substr(processed.games.files, 1, 8),
                                gcode=substr(processed.games.files, 10, 14))
  return(processed.games)

}



download.single.game <- function (season="20122013", gcode="20001", rdata.folder="nhlr-data", verbose=TRUE, wait=20) {
  #season="20122013"; gcode="20018"; rdata.folder="nhlr-data"; verbose=TRUE
  valid.seasons <- paste(2002:2020, 2003:2021, sep="")
  if (!(season %in% valid.seasons)) stop(paste("Invalid season: ",season))

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
    game.rec$xy <- try(fromJSON(file2), TRUE)
    if (class(game.rec$xy) == "try-error") {warning("Could not recover x-y coordinates."); game.rec$xy <- NULL}
  } else {warning("Could not download x-y coordinate file."); game.rec$xy <- NULL}
  
  if (season %in% c("20022003", "20032004", "20052006", "20062007")) {
    
    infile <- paste("http://www.nhl.com/scores/htmlreports/",season,"/SCH",gcode,".gif",sep="")
    dir.create(rdata.folder, showWarnings = FALSE)
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

  message ("Pausing: ", wait)
  Sys.sleep(wait)
  
  #if (!error.free) game.rec <- NULL
  suppressWarnings(dir.create(rdata.folder))
  if (length(game.rec$es) > 10 & length(game.rec$pl) > 10) save (game.rec, file=paste0(rdata.folder, "/", season, "-", gcode, ".RData")) else error.free <- FALSE

  return (error.free)
  
}


download.games <- function (games=full.game.database(), rdata.folder="nhlr-data", ...) {
  #games=full.game.database(); games = games[games$session=="Playoffs" & games[,1] == "20122013",]

  success <- rep(FALSE, nrow(games))
  for (kk in 1:nrow(games)) if (games$status[kk] > 0) 
    success[kk] <- download.single.game(games$season[kk],
                                        paste0(2+1*(games$session[kk]=="Playoffs"),
                                               games$gamenumber[kk]), ...)
  return(success)
  
}





process.single.game <- function (season="20122013", gcode="20001",
                                 rdata.folder="nhlr-data",
                                 override.download=FALSE,
                                 save.to.file=TRUE, ...) {
  #season="20132014"; gcode="20274"; rdata.folder="nhlr-data"; override.download=FALSE; save.to.file=TRUE


  if (!file.exists(paste0(rdata.folder, "/", season, "-", gcode, ".RData")) | override.download) {

    #For Matt Taddy, 7-10-13.
    #message("process.single.game -- Current directory:",getwd()); message(season, gcode, rdata.folder)

    dl.time <- download.single.game(season, gcode, rdata.folder, ...)
  }

  if (file.exists(paste0(rdata.folder, "/", season, "-", gcode, ".RData"))) {
    game.rec <- NULL   #loaded in next line.
    load (paste0(rdata.folder, "/", season, "-", gcode, ".RData"))
    if (season %in% c("20022003", "20032004", "20052006", "20062007") & (is.null(game.rec$imh) | is.null(game.rec$imv))) {
      message("Re-downloading single game files due to incompleteness in graphics files.")
      dl.time <- download.single.game(season, gcode, rdata.folder, ...)
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
    game.info$score <- c(homescore=0, awayscore=0)
    game.info$status <- 1
    
    if (length(game.info$playbyplay) > 0) {

      game.info$status <- 2 + 1*(length(grep("; ([Ee]nd|[Ff]in)", game.rec$es))>0 |
                                 length(grep("Final", game.rec$es))>0 |
                                 length(grep("End of Period 4", game.rec$es))>0)
      
      #xy?
      game.info$playbyplay$xcoord <- NA; game.info$playbyplay$ycoord <- NA
      if (!is.null(game.rec$xy)) game.info <- match.xy(game.info, game.rec$xy)
      
      playbyplay <- game.info$playbyplay

      #CHANGEs need scrubbing.
      playbyplay$distance[playbyplay$etype=="CHANGE"] <- NA
      playbyplay$type[playbyplay$etype=="CHANGE"] <- ""
      playbyplay$homezone[playbyplay$etype=="CHANGE"] <- "Neu"
      playbyplay$ev.player.1[playbyplay$etype=="CHANGE"] <- ""
      playbyplay$ev.player.2[playbyplay$etype=="CHANGE"] <- ""
      playbyplay$ev.player.3[playbyplay$etype=="CHANGE"] <- ""
      playbyplay$xcoord[playbyplay$etype=="CHANGE"] <- NA
      playbyplay$ycoord[playbyplay$etype=="CHANGE"] <- NA
      

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

      game.info$score <- c(homescore=sum(playbyplay$period[home.goals] <= 4),
                           awayscore=sum(playbyplay$period[away.goals] <= 4))
      if (game.info$score[1] == game.info$score[2]) {
          game.info$score[1] <- game.info$score[1] + 1*(length(home.goals)>length(away.goals))
          game.info$score[2] <- game.info$score[2] + 1*(length(home.goals)<length(away.goals))
      }

      
      #event length.
      ## First, fix period 4/5 glitch.

      
      if (substr(gcode,1,1) == 2) {
          playbyplay$seconds[playbyplay$seconds >= 3900] <- 3900

          #playbyplay$event.length[playbyplay$seconds >= 3900] <- 0
          #playbyplay$event.length[playbyplay$seconds == 3900 & playbyplay$etype == "CHANGE"] <- 0
          #playbyplay$seconds[playbyplay$seconds >= 4800] <- playbyplay$seconds[playbyplay$seconds >= 4800] - 900

      }
      
      playbyplay$event.length <- playbyplay$seconds
      playbyplay$event.length[2:dim(playbyplay)[1]] <-
        playbyplay$event.length[2:dim(playbyplay)[1]] - playbyplay$event.length[1:(dim(playbyplay)[1]-1)]
      
      
      #identify goaltenders, remove from player lists.
      player.list <- game.info$players
      player.list <- rbind(player.list, "")
      playbyplay$away.G <- ""; for (kk in paste0("a",1:6)) {
        playbyplay[is.na(playbyplay[,kk]),kk] <- ""
        column <- playbyplay[,kk];
        pl.match <- match(column, player.list$numfirstlast)
        picks <- player.list$pos[pl.match]; picks[is.na(picks)] <- ""
        if (sum(picks=="G")>0) {
          playbyplay$away.G[picks=="G"] <-
            player.list$numfirstlast[pl.match][picks=="G"]
          playbyplay[picks=="G", kk] <- ""
        }
      }
      playbyplay$home.G <- ""; for (kk in paste0("h",1:6)) {
        playbyplay[is.na(playbyplay[,kk]),kk] <- ""
        column <- playbyplay[,kk]; 
        pl.match <- match(column, player.list$numfirstlast)
        picks <- player.list$pos[pl.match]; picks[is.na(picks)] <- ""
        if (sum(picks=="G")>0) {
          playbyplay$home.G[picks=="G"] <-
            player.list$numfirstlast[pl.match][picks=="G"]
          playbyplay[picks=="G", kk] <- ""
        }
      }
      
      playbyplay$home.skaters <- apply(1*(playbyplay[,c(paste0("h",1:6), "home.G")] != ""), 1, sum)
      playbyplay$away.skaters <- apply(1*(playbyplay[,c(paste0("a",1:6), "away.G")] != ""), 1, sum)
      
      etypes <- c("GOAL","SHOT","MISS","BLOCK")
      shot.rows <- which(playbyplay$etype %in% etypes)
      
      playbyplay$type[shot.rows][playbyplay$type[shot.rows] == "Tip-in"] <- "Tip-In"
      playbyplay$type[shot.rows][playbyplay$type[shot.rows] == "Wrap-around"] <- "Wrap"  
      shotstyles <- c("Backhand", "Tip-In", "Wrist", "Snap", "Slap", "Wrap", "Deflected")
      playbyplay$type[shot.rows][!(playbyplay$type[shot.rows] %in% shotstyles)] <- "Unspecified"
      playbyplay$distance <- as.numeric(playbyplay$distance)
      rownames(playbyplay) <- 1:nrow(playbyplay)
      
      game.info$playbyplay <- playbyplay
      
    }
    
    if (save.to.file) save (game.info, file=paste0(rdata.folder, "/", season, "-", gcode, "-processed.RData"))
  } else game.info <- NULL
  
  return(game.info)
  
}


process.games <- function (games=full.game.database(),
                           rdata.folder="nhlr-data",
                           override.download=FALSE) {
  #games=full.game.database(); games = games[5341:5456,]

  bogus.count <- 0
  #item <- list()
  for (kk in which(games$status > 0)) {
    
    message (paste(kk, games[kk,1], paste0(2+1*(games$session[kk]=="Playoffs"), games$gamenumber[kk])))
    item <- process.single.game(
      games$season[kk],
      games$gcode[kk],
      rdata.folder=rdata.folder,
      override.download=override.download,
      save.to.file=TRUE)

    if (item$status == 1) bogus.count <- bogus.count+1 else bogus.count <- 0
    if (bogus.count >= 10) break
    
  }

  #save(item, file="all-archived.RData")
  return(bogus.count)
  
}

retrieve.game <- function (season="20122013",
                           gcode="20001",
                           rdata.folder="nhlr-data",
                           force=TRUE, ...) {
  #season="20122013"; gcode="20001"; rdata.folder="nhlr-data"
  
  if (!file.exists(paste0(rdata.folder, "/", season, "-", gcode, "-processed.RData"))) {
    if (force) game.info <- process.single.game (season, gcode, rdata.folder, save.to.file=TRUE, ...) else game.info <- NULL
  } else load (paste0(rdata.folder, "/", season, "-", gcode, "-processed.RData"))
  return(game.info)

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

  #playbyplay <- patch.for.shottypes(playbyplay)
  return(playbyplay)
}

make.unique.roster <- function(roster.master) {
  unique.entries <- match(1:max(roster.master$player.id), roster.master$player.id)
  roster.unique <- roster.master[unique.entries,]
  roster.unique$pos <- sapply(1:max(roster.master$player.id), function(kk) {
    pcount <- apply(rbind(roster.master[roster.master$player.id==kk,
                                        c("pC","pL","pR","pD","pG")]), 2, sum)
    c("C","L","R","D","G")[min(which(pcount==max(pcount)))]
  })
  roster.unique
}

construct.rosters.from.list <- function (roster.collection,  #raw list
                                         roster.master=NULL) {
  #roster.collection=new.roster;  roster.master=NULL
  
  blanky <- data.frame (pos="", last="", first="", numfirstlast="", firstlast="",
                        index=1, player.id=1,
                        pC=0, pL=0, pR=0, pD=0, pG=0,
                        stringsAsFactors=FALSE)
  if (is.null(roster.master)) roster.master <- blanky
  
  for (kk in 1:length(roster.collection)) if (!is.null(roster.collection[[kk]])) if (nrow(roster.collection[[kk]])>0) { # ) {    #
    
    if (kk %% 500 == 0) message(paste("Roster merger: game",kk,"of",length(roster.collection)))
    
    this.roster <- fix.names.manually(roster.collection[[kk]][,c("number","pos","last","first","numfirstlast")])

    match1 <- match(this.roster$numfirstlast,
                    roster.master$numfirstlast)
    if (any(is.na(match1))) {
      rows <- which(is.na(match1))
      newrecs <- data.frame (pos=this.roster$pos[rows],
                             last=this.roster$last[rows],
                             first=this.roster$first[rows],
                             numfirstlast=this.roster$numfirstlast[rows],
                             firstlast="",  index=nrow(roster.master) + 1:length(rows),
                             player.id=NA,
                             pC=0, pL=0, pR=0, pD=0, pG=0,
                             stringsAsFactors=FALSE)
      
      newrecs$firstlast <- paste(newrecs$first, newrecs$last)

      m2 <- match(newrecs$firstlast, roster.master$firstlast)
      if (any(!is.na(m2))) newrecs$player.id[!is.na(m2)] <- roster.master$player.id[m2[!is.na(m2)]]
      if (any(is.na(m2))) newrecs$player.id[is.na(m2)] <- max(roster.master$player.id) + 1:sum(is.na(m2))

      roster.master <- rbind(roster.master, newrecs)
      
      #zeroes <- rep(0, length(rows))
      #positions <- rbind(positions, data.frame(pC=zeroes, pL=zeroes, pR=zeroes, pD=zeroes, pG=zeroes))
    }
    
    r1.match <- match(this.roster$numfirstlast,
                      roster.master$numfirstlast)
    roster.master$pC[r1.match[this.roster$pos=="C"]] <-
      roster.master$pC[r1.match[this.roster$pos=="C"]] + 1
    roster.master$pL[r1.match[this.roster$pos=="L"]] <-
      roster.master$pL[r1.match[this.roster$pos=="L"]] + 1
    roster.master$pR[r1.match[this.roster$pos=="R"]] <-
      roster.master$pR[r1.match[this.roster$pos=="R"]] + 1
    roster.master$pD[r1.match[this.roster$pos=="D"]] <-
      roster.master$pD[r1.match[this.roster$pos=="D"]] + 1
    roster.master$pG[r1.match[this.roster$pos=="G"]] <-
      roster.master$pG[r1.match[this.roster$pos=="G"]] + 1
      
  }
  
  return(roster.master)

}


#objects:
#  games - data frame of games played.
#  grand.data - all game records.
#  roster.master, roster.unique, 
#  distance.adjust, scoring.models, shot.tables

#

## setwd("/home/acthomas/Documents/nhlr"); source("nhlscrapr/R/convert-gif.R"); source("nhlscrapr/R/convert-html.R"); source("nhlscrapr/R/convert-json.R"); source("nhlscrapr/R/GIF.R"); source("nhlscrapr/R/integrate.R"); source("nhlscrapr/R/manual-name-fixes.R"); source("nhlscrapr/R/operations.R"); source("nhlscrapr/R/subzone-adjustments.R"); load ("nhlscrapr/data/date201415.RData"); load ("nhlscrapr/data/quadsarray.RData")


compile.all.games <- function (rdata.folder="nhlr-data",
                               output.folder="source-data",
                               new.game.table=NULL,
                               seasons=NULL,

                               verbose=FALSE,
                               
                               override.days.back=NULL,
                               date.check=FALSE,
                               
                               ...) {

    #rdata.folder="nhlr-data"; output.folder="source-data"; new.game.table=NULL; seasons=NULL; verbose=FALSE; override.days.back=NULL; date.check=FALSE
    
    suppressWarnings(dir.create(output.folder))

    if (file.exists(paste0(output.folder,"/nhlscrapr-core.RData"))) {
        message ("Loading game and player data.")
        load(paste0(output.folder,"/nhlscrapr-core.RData"))
    } else {
        message ("Creating game table and player data.")
        
        games <- full.game.database() #games <- subset(new.game.table, season %in% c("20022003", "20032004", "20052006"));   games <- games[13841:13962,]
        grand.data <- NULL
        roster.master <- NULL
        distance.adjust <- scoring.models <- shot.tables <- NULL

        ## Pre-cleaning.

        ## Correct missing dates.
        blanks <- which(is.na(games$date))
        for (kk in blanks[blanks>1 & blanks<nrow(games)]) if (!is.na(games$date[kk-1]) && !is.na(games$date[kk+1]) && games$date[kk-1] == games$date[kk+1]) games$date[kk] <- games$date[kk-1]
        
    }

    ## If dates got replaced by their integer counterparts.
    repl <- grep("^[0-9]+$", games$date)
    games$date[repl] <- as.character(as.Date("1970-01-01") + as.numeric(games$date[repl]))

    if (!is.null(new.game.table)) {
        games <- new.game.table
    }
    
    if (!is.null(seasons)) {
        message ("Overriding existing game table to create one with specified seasons.")
        eligible.seasons <- c("20022003", "20032004", "20052006",
                              "20062007", "20072008", "20082009",
                              "20092010", "20102011", "20112012",
                              "20122013", "20132014", "20142015")
        if (!all(seasons %in% eligible.seasons)) stop ("Specified seasons must be within ", paste(eligible.seasons, collapse=", "))
        games <- full.game.database()
        games <- games[games$season %in% seasons,]
    }

    today.now <- format(as.POSIXct(Sys.time(), tz="America/Los_Angeles"), tz="America/Los_Angeles", usetz=TRUE)
    today <- as.Date(today.now)
    override.dates <- as.character(today - override.days.back)
    
    override.rows <- which(games$date %in% override.dates)
    games$status[which(games$date %in% override.dates)] <- 2
    
    ## Go season by season in the update process.
    for (this.season in unique(games$season)) {
            
    ##2. try and download new games -- 1s and 2s.
        new.pbp <- new.roster <- list()
        cons.failures <- 0

        replace.rows <- which(games$season == this.season & games$status %in% c(1,2))
        sub.games <- games[replace.rows,]
    
        if (length(replace.rows) == 0) {
            message (this.season,": no games need updating.")
            next
        }
        
        for (kk in 1:nrow(sub.games)[1]) {
            if (kk %% 500 == 0) message(paste0("Event assembly: ",this.season," game",kk))

            if (verbose) message ("Trying game ", sub.games$season[kk],sub.games$gcode[kk], " ", sub.games$date[kk], " because it's ", today.now)
            if (grepl("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}", sub.games$date[kk]) | !date.check) {
                if (sub.games$date[kk] > today.now) {
                    if (verbose) message ("Skipping Game ", sub.games$season[kk],sub.games$gcode[kk], " ", sub.games$date[kk], " because it's ", today.now)
                    next
                }} else next
            
            tryme <- try({
      
                game.info <- retrieve.game(sub.games$season[kk], sub.games$gcode[kk],
                                           rdata.folder, force=FALSE)

                doit <- FALSE
                if (is.null(game.info)) doit <- TRUE else if (game.info$status %in% 1:2) doit <- TRUE
                if (doit) game.info <-     ## re-download it.
                    process.single.game(sub.games$season[kk], sub.games$gcode[kk],
                                        rdata.folder=rdata.folder, override.download=TRUE, ...) 
      
                sub.games$status[kk] <- game.info$status
                sub.games$awayteam[kk] <- game.info$teams[1]
                sub.games$hometeam[kk] <- game.info$teams[2]
                sub.games$awayscore[kk] <- game.info$score[2]
                sub.games$homescore[kk] <- game.info$score[1]
                
                sub.games$date[kk] <- as.character(as.Date(paste(game.info$date, collapse=" "), format="%A %B %d %Y"))
                
                sub.games$game.start[kk] <- game.info$game.times[1]
                sub.games$game.end[kk] <- game.info$game.times[2]
                sub.games$periods[kk] <- max(game.info$playbyplay$period)
                
                new.pbp[[kk]] <- game.info
                new.roster[[kk]] <- game.info$players
                
            }, TRUE)
            if (class(tryme) == "try-error") cons.failures <- cons.failures + 1 else cons.failures <- 0
            if (cons.failures >= 20) {
                message ("20 consecutive failed attempts; stopping file retrieval.")
                break
            }
        }
        games[replace.rows,] <- sub.games

        if (length(new.roster) > 0) {

            ## update rosters.
            message(this.season," -- updating rosters on each game file.")
            roster.master <- construct.rosters.from.list (new.roster, roster.master)
            new.pbp.2 <- lapply(new.pbp, function (game.info) {
                out <- try(augment.game(game.info, roster.master), TRUE)
                if (class(out) == "try-error") out <- NULL
                return(out)
            })

            secondary.data <- fold.frames(new.pbp.2)
            secondary.data$adjusted.distance <- NA
            secondary.data$shot.prob.distance <- NA
            secondary.data$prob.goal.if.ongoal <- NA
            
            message ("Adding event location sections.")
            coords <- secondary.data[,c("xcoord","ycoord")]
            flip <- which(coords[,1] < 0)
            coords[flip,1] <- -coords[flip,1]; coords[flip,2] <- -coords[flip,2];
            secondary.data$loc.section <- pick.section (coords)
            
            #secondary.data$loc.section <- NA
            secondary.data$new.loc.section <- secondary.data$loc.section
            secondary.data$newxc <- secondary.data$xcoord
            secondary.data$newyc <- secondary.data$ycoord

        } else secondary.data <- NULL
    
        if (!is.null(secondary.data)) {

            if (file.exists(paste0(output.folder,"/nhlscrapr-",this.season,".RData"))) {
                load (paste0(output.folder,"/nhlscrapr-",this.season,".RData"))
            } else grand.data <- secondary.data[secondary.data$season=="0000",]

            grand.data <- rbind(grand.data[!(grand.data$gcode %in% unique(secondary.data$gcode)),],
                                secondary.data)
            grand.data$gcode <- as.character(grand.data$gcode)

            save (grand.data, file=paste0(output.folder,"/nhlscrapr-",this.season,".RData"))
            
        }

        
    }
    
    message("Saving to output file")
    roster.unique <- manual.patches(roster.master[match(1:max(roster.master$player.id), roster.master$player.id),])
    save(roster.master, roster.unique, games, file=paste0(output.folder,"/nhlscrapr-core.RData"))

    #print(quadsarray)
    ## Here would go the adjusted location/imputed position part.
    ## if (!skip.steps) grand.data <- create.subzone.adjustments (grand.data)
  
    return(TRUE)

}

