
# Combine all the data frames into one big collection.

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



# Produce the database of games. Add on extra seasons if desired.
full.game.database <- function (extra.seasons=0) {

  game.roster <- NULL
  seasons <- c("20022003", "20032004", "20052006", "20062007",
               "20072008", "20082009", "20092010", "20102011",
               "20112012", "20122013", "20132014")
  if (extra.seasons > 0)
    seasons <- c(seasons, paste(2013+1:extra.seasons,
                                2014+1:extra.seasons, sep=""))
  games <- c(rep(1230,9), 720, 1230, rep(1230,extra.seasons))

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
                        c(259), c(), c()) #1112 1213 1314
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

      rep(7, 15*(1+extra.seasons)))  #,matrix( nrow=15)
  sequence.seven <- function(nn) c(rep(1, nn), rep(0, 7-nn))
  playoff.status <- c(sapply(playoff.series.lengths, sequence.seven))
  game.roster$status[game.roster$session=="Playoffs"] <- playoff.status
  
  bad.playoff <- matrix(c("20032004", "30134",
                          "20052006", "30233"), nrow=2)  #ucase TR TD
  for (kk in 1:dim(bad.playoff)[2]) 
    game.roster$status[game.roster$season == bad.playoff[1,kk] &
                       game.roster$gcode == bad.playoff[2,kk]] <- 0
 
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



download.single.game <- function (season="20122013", gcode="20001", rdata.folder="nhlr-data", verbose=TRUE) {
  #season="20122013"; gcode="20018"; rdata.folder="nhlr-data"; verbose=TRUE
  valid.seasons <- paste(2002:2020, 2003:2021, sep="")
  if (!(season %in% c("20022003", "20032004", "20052006", "20062007",
                      "20072008", "20082009", "20092010", "20102011",
                      "20112012", "20122013", "20132014"))) stop(paste("Invalid season: ",season))

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
  if (length(game.rec$es) > 10 & length(game.rec$pl) > 10) save (game.rec, file=paste0(rdata.folder, "/", season, "-", gcode, ".RData")) else error.free <- FALSE

  return (error.free)
  
}


download.games <- function (games=full.game.database(), rdata.folder="nhlr-data") {
  #games=full.game.database(); games = games[games$session=="Playoffs" & games[,1] == "20122013",]

  success <- rep(FALSE, nrow(games))
  for (kk in 1:nrow(games)) if (games$status[kk] > 0) 
    success[kk] <- download.single.game(games$season[kk],
                                        paste0(2+1*(games$session[kk]=="Playoffs"),
                                               games$gamenumber[kk]))
  return(success)
  
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

  if (file.exists(paste0(rdata.folder, "/", season, "-", gcode, ".RData"))) {
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
    game.info$score <- c(homescore=0, awayscore=0)
    game.info$status <- 1
    
    if (length(game.info$playbyplay) > 0) {

      game.info$status <- 2 + 1*(length(grep("; [Ee]nd", game.rec$es))>0 |
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

      game.info$score <- c(homescore=length(home.goals),
                           awayscore=length(away.goals))

      
      #event length.
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
                           force=TRUE) {
  #season="20122013"; gcode="20001"; rdata.folder="nhlr-data"
  
  if (!file.exists(paste0(rdata.folder, "/", season, "-", gcode, "-processed.RData"))) {
    if (force) game.info <- process.single.game (season, gcode, rdata.folder, save.to.file=TRUE) else game.info <- NULL
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
    this.roster <- fix.names.manually(roster.collection[[kk]][,c(2,5,6,8)])

    match1 <- match(roster.collection[[kk]]$numfirstlast,
                    roster.master$numfirstlast)
    if (any(is.na(match1))) {
      rows <- which(is.na(match1))
      newrecs <- data.frame (pos=roster.collection[[kk]]$pos[rows],
                             last=roster.collection[[kk]]$last[rows],
                             first=roster.collection[[kk]]$first[rows],
                             numfirstlast=roster.collection[[kk]]$numfirstlast[rows],
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
    
    r1.match <- match(roster.collection[[kk]]$numfirstlast,
                      roster.master$numfirstlast)
    roster.master$pC[r1.match[roster.collection[[kk]]$pos=="C"]] <-
      roster.master$pC[r1.match[roster.collection[[kk]]$pos=="C"]] + 1
    roster.master$pL[r1.match[roster.collection[[kk]]$pos=="L"]] <-
      roster.master$pL[r1.match[roster.collection[[kk]]$pos=="L"]] + 1
    roster.master$pR[r1.match[roster.collection[[kk]]$pos=="R"]] <-
      roster.master$pR[r1.match[roster.collection[[kk]]$pos=="R"]] + 1
    roster.master$pD[r1.match[roster.collection[[kk]]$pos=="D"]] <-
      roster.master$pD[r1.match[roster.collection[[kk]]$pos=="D"]] + 1
    roster.master$pG[r1.match[roster.collection[[kk]]$pos=="G"]] <-
      roster.master$pG[r1.match[roster.collection[[kk]]$pos=="G"]] + 1
      
  }
  
  return(roster.master)

}


#objects:
#  games - data frame of games played.
#  grand.data - all game records.
#  roster.master, roster.unique, 
#  distance.adjust, scoring.models, shot.tables

compile.all.games <- function (mega.file="nhlscrapr-probs.RData",
                               output.file=mega.file,
                               rdata.folder="nhlr-data",
                               new.game.table=NULL) {
  #mega.file="nhlscrapr-pros.RData"; output.file="nhls-2.RData"; rdata.folder="nhlr-testdata"
  #mega.file="nhlscrapr-probs-2.RData"; output.file="nhls-2.RData"; rdata.folder="nhlr-data"
  #mega.file="nhlscrapr-2014.RData"; output.file=mega.file; rdata.folder="nhlr-data"; new.game.table=full.game.database()

  #First cut:
  #library(nhlscrapr); mega.file="nhls0.RData"; output.file=mega.file; rdata.folder="nhlr-data"; new.game.table=full.game.database()[12840 + 1:600,]
  
  #Second cut:
  #library(nhlscrapr); mega.file="nhls0.RData"; output.file=mega.file; rdata.folder="nhlr-data"; new.game.table=NULL
  
  if (file.exists(mega.file)) {
    load(mega.file)
  } else {
    games <- full.game.database() #games <- subset(new.game.table, season %in% c("20022003", "20032004", "20052006"));   games <- games[13841:13962,]
    grand.data <- NULL
    roster.master <- NULL
    distance.adjust <- scoring.models <- shot.tables <- NULL
  }
  if (!is.null(new.game.table)) {
    games <- new.game.table
  }

  #Second cut:
  #games <- rbind(games, full.game.database()[12840 + 601:900,])

  #subd = grand.data
  if (!is.null(grand.data)) {
    #remove games in progress from grand.data -- code 1 or 2.
    
    ss.gcode <- paste(grand.data$season, grand.data$gcode)
    table.ss.gcode <- paste(games$season, games$gcode)
    gd.status <- games$status[match(ss.gcode, table.ss.gcode)]
    grand.data <- grand.data[!(gd.status %in% 1:2),]

    #what do we have left? Should be all threes.
    ss.gcode <- paste(grand.data$season, grand.data$gcode)
    gr.da.games <- unique(ss.gcode)
    
    replace.rows <- which(is.na(match(table.ss.gcode, gr.da.games)) & games$status %in% c(1,2))
    
  } else {
    replace.rows <- which(games$status > 0)   #all valid ones.
  }
  sub.games <- games[replace.rows,]

  if (length(replace.rows) == 0) {
    message ("No games need updating!")
    return(FALSE)
  }
  
  #2. try and download new games -- 1s and 2s.
  new.pbp <- new.roster <- list()
  cons.failures <- 0
  for (kk in 1:nrow(sub.games)[1]) {
    if (kk %% 500 == 0) message(paste("Event assembly: game",kk,"of",dim(sub.games)[1]))
    tryme <- try({
      
      game.info <-
        retrieve.game(sub.games$season[kk],
                      sub.games$gcode[kk],
                      rdata.folder, force=FALSE)

      doit <- FALSE
      if (is.null(game.info)) doit <- TRUE else if (game.info$status %in% 1:2) doit <- TRUE
      if (doit)
                                        #re-download it.
        game.info <-
          process.single.game(sub.games$season[kk],
                              sub.games$gcode[kk],
                              rdata.folder=rdata.folder,
                              override.download=TRUE) 
      
      sub.games$status[kk] <- game.info$status
      
      sub.games$awayteam[kk] <- game.info$teams[1]
      sub.games$hometeam[kk] <- game.info$teams[2]
      sub.games$awayscore[kk] <- game.info$score[2]
      sub.games$homescore[kk] <- game.info$score[1]
      
      sub.games$date[kk] <- paste(game.info$date, collapse=" ")
      
      new.pbp[[kk]] <- game.info
      new.roster[[kk]] <- game.info$players
      
    }, TRUE)
    if (class(tryme) == "try-error") cons.failures <- cons.failures + 1 else cons.failures <- 0
    if (cons.failures >= 10) {
      message ("10 consecutive failed attempts; stopping file retrieval.")
      break
    }
  }

  #update rosters.
  message("Updating rosters on each game file.")
  roster.master <- construct.rosters.from.list (new.roster, roster.master)
  new.pbp.2 <- lapply(new.pbp, function (game.info) {
    out <- try(augment.game(game.info, roster.master), TRUE)
    if (class(out) == "try-error") out <- NULL
    return(out)
  })

  #new ones: fold together.   source("nhlscrapr/R/operations.R")
  secondary.data <- fold.frames(new.pbp.2)

  sd1 <- create.adjusted.distance(secondary.data, distance.adjust)
  secondary.data <- sd1$grand.data
  distance.adjust <- sd1$distance.adjust
  rm(sd1)

  if (is.null(scoring.models)) {
    sd1 <- make.shot.probs.simple (secondary.data)
    secondary.data <- sd1$grand.data
    scoring.models <- sd1$scoring.models
    shot.tables <- sd1$shot.tables
    rm(sd1)
  } else {
    secondary.data <-
      fit.shot.probs.simple (secondary.data, scoring.models, shot.tables)
  }

  grand.data <- rbind(grand.data, secondary.data)
  
  games[replace.rows,] <- sub.games
  
  message("Saving to ",output.file)
  save(grand.data, roster.master, games,   #roster.unique, 
       distance.adjust, scoring.models, shot.tables,
       file=output.file)
  
  return(TRUE)

}

