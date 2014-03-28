
# Things you can do with the big grand.data object and unique roster.

#Block the seasons into 4. This will make it easier to add new games as time goes along.
season.block <- function (season) {
  year <- as.numeric(substr(season,1,4))
  1 + 1*(year >= 2005) + 1*(year >= 2008) + 1*(year >= 2011)
}

make.adjusted.distance.homeaway.table <- function (shots.in) { #,

  #shots.in = shot.data
  shots.in$seasonblock <- season.block(shots.in$season)
  seasonblocks <- 1:max(shots.in$seasonblock)
  shots.split <- lapply(seasonblocks, function (bb) shots.in[shots.in$seasonblock==bb,  c("type", "ev.team", "hometeam", "awayteam", "distance")])
  
  split.table <- expand.grid (seasonblock=seasonblocks, type=unique(shots.in$type),
                              teams=unique(shots.in$hometeam), stringsAsFactors = FALSE)

  split.table$dilation <- sapply (1:nrow(split.table), function(rr) {
    prop <- split.table[rr,]
    home.mean <- mean(shots.split[[prop[[1]]]]$distance[shots.split[[prop[[1]]]]$type == prop[[2]] &
                                                        shots.split[[prop[[1]]]]$ev.team == prop[[3]] &
                                                        shots.split[[prop[[1]]]]$hometeam == prop[[3]]], na.rm=TRUE)
    away.mean <- mean(shots.split[[prop[[1]]]]$distance[shots.split[[prop[[1]]]]$type == prop[[2]] &
                                                        shots.split[[prop[[1]]]]$ev.team == prop[[3]] &
                                                        shots.split[[prop[[1]]]]$awayteam == prop[[3]]], na.rm=TRUE)
    away.mean/home.mean
  })
  split.table$dilation[is.na(split.table$dilation)] <- 1
  return (split.table)
}


update.adjusted.distance <- function (shots.in,
                                      split.table,
                                      shrinkage.factor=1) {
  
  #shots.in=shot.data; split.table=distance.adjust; shrinkage.factor=1
  shots.in$seasonblock <- season.block(shots.in$season)
  seasonblocks <- 1:max(shots.in$seasonblock)
  shots.in$adjusted.distance <- shots.in$distance
  
  shots.split <- lapply(seasonblocks,
                        function (bb) shots.in[shots.in$seasonblock==bb,
                                               c("type", "ev.team", "hometeam", "awayteam",
                                                 "distance", "adjusted.distance")])

  for (rr in 1:nrow(split.table)) {
    prop <- split.table[rr,]
    rows <- which (shots.split[[prop[[1]]]]$type == prop[[2]] & shots.split[[prop[[1]]]]$hometeam == prop[[3]])
    shots.split[[prop[[1]]]]$adjusted.distance[rows] <- shots.split[[prop[[1]]]]$adjusted.distance[rows]*
      (shrinkage.factor*prop[[4]] + (1-shrinkage.factor))  #away.mean/home.mean
  }

  for (bb in seasonblocks) 
    shots.in$adjusted.distance[shots.in$seasonblock==bb] <- shots.split[[bb]]$adjusted.distance
  shots.in$adjusted.distance[shots.in$adjusted.distance > 200] <- 200
  
  return(shots.in$adjusted.distance)

}


create.adjusted.distance <- function (sub.data, distance.adjust=NULL) {   #, split.table
  #sub.data=secondary.data; distance.adjust=NULL
  message ("Correcting for Distance Anomalies.")
  sub.data$adjusted.distance <- NA
  
  etypes <- c("GOAL","SHOT","MISS")
  shot.rows <- which (sub.data$etype %in% etypes)
  shot.data <- sub.data[shot.rows,
                        c("season", "type", "ev.team", "hometeam", "awayteam", "distance")]
#  shottypes <- unique(shot.data$type)
  if (is.null(distance.adjust)) distance.adjust <- make.adjusted.distance.homeaway.table (shot.data)

  dist.one <- update.adjusted.distance (shot.data, distance.adjust)
  sub.data$adjusted.distance[shot.rows] <- dist.one

  return(list(grand.data=sub.data,
              distance.adjust=distance.adjust))

}






expit <- function(x) exp(x)/(1+exp(x))

prob.score.simple.objects <- function (shots.in) {
  #shots.in = shot.data[rows,]
  #distance=shots.in$adjusted.distance
  
  shots.in <- shots.in[shots.in$etype %in% c("SHOT","GOAL","MISS"),]
  df1 <- data.frame (cut.distance = cut (shots.in$adjusted.distance, c(seq(0,100,by=5), Inf), include.lowest = TRUE),
                     its.a.sog = 1*(shots.in$etype %in% c("SHOT","GOAL")),
                     its.a.goal = 1*(shots.in$etype == "GOAL"))
  df2 <- df1[df1$its.a.sog == 1,]
    
  sog.obj <- suppressWarnings(bigglm (its.a.sog ~ cut.distance, family=binomial(link=logit), data=df1))
  glm.obj <- suppressWarnings(bigglm (its.a.goal ~ cut.distance, family=binomial(link=logit), data=df2))

  shot.if.ongoal <- expit(predict(sog.obj, df1))
  df1$cut.distance[!(df1$cut.distance %in% unique(df2$cut.distance))] <- NA
  goal.if.ongoal <- expit(predict(glm.obj, df1))
 
  return(list(sog.obj=sog.obj, glm.obj=glm.obj,
              shot.if.ongoal=shot.if.ongoal,
              goal.if.ongoal=goal.if.ongoal))
  
}


shot.sub.rows <- function (shot.data, seasonblock, shottype, condition) {

  shot.data$seasonblock <- season.block(shot.data$season)
  
  rows <- NULL
  if (condition == "homeESSH")
    rows <- which(shot.data$home.sk <= shot.data$away.sk &
                  shot.data$ev.team == shot.data$hometeam &
                  shot.data$seasonblock == seasonblock &
                  shot.data$type == shottype &
                  shot.data$away.G > 1 & shot.data$home.G > 1)
  if (condition == "homePP")
    rows <- which(shot.data$home.sk > shot.data$away.sk &
                  shot.data$ev.team == shot.data$hometeam &
                  shot.data$seasonblock == seasonblock &
                  shot.data$type == shottype &
                  shot.data$away.G > 1 & shot.data$home.G > 1)
  if (condition == "homeEN")  #home team pulled their goalie. There are no "SHOT"s 
    rows <- which(shot.data$ev.team == shot.data$hometeam &
                  shot.data$seasonblock == seasonblock &
                  shot.data$type == shottype &
                  shot.data$away.G > 1 & shot.data$home.G == 1)
  if (condition == "homeOnEN")  #home team pulled their goalie. There are no "SHOT"s 
    rows <- which(shot.data$ev.team == shot.data$hometeam &
                  shot.data$seasonblock == seasonblock &
                  shot.data$type == shottype &
                  shot.data$away.G == 1 & shot.data$home.G > 1)
  
  if (condition == "awayESSH")
    rows <- which(shot.data$home.sk >= shot.data$away.sk &
                  shot.data$ev.team == shot.data$awayteam &
                  shot.data$seasonblock == seasonblock &
                  shot.data$type == shottype &
                  shot.data$away.G > 1 & shot.data$home.G > 1)
  if (condition == "awayPP")
    rows <- which(shot.data$home.sk < shot.data$away.sk &
                  shot.data$ev.team == shot.data$awayteam &
                  shot.data$seasonblock == seasonblock &
                  shot.data$type == shottype &
                  shot.data$away.G > 1 & shot.data$home.G > 1)
  if (condition == "awayEN")
    rows <- which(shot.data$ev.team == shot.data$awayteam &
                  shot.data$seasonblock == seasonblock &
                  shot.data$type == shottype &
                  shot.data$away.G == 1 & shot.data$home.G > 1)
  if (condition == "awayOnEN")
    rows <- which(shot.data$ev.team == shot.data$awayteam &
                  shot.data$seasonblock == seasonblock &
                  shot.data$type == shottype &
                  shot.data$away.G > 1 & shot.data$home.G == 1)
  return(rows)
}



make.shot.probs.simple <- function (grand.data) {
  #load("nhlscrapr-probs.RData")
  
  if (!("shot.prob.distance" %in% colnames(grand.data))) grand.data$shot.prob.distance <- NA
  if (!("prob.goal.if.ongoal" %in% colnames(grand.data))) grand.data$prob.goal.if.ongoal <- NA
  
  etypes <- c("GOAL","SHOT","MISS")
  shot.rows <- which (grand.data$etype %in% etypes)
  shot.data <- grand.data[shot.rows,]
  shottypes <- unique(shot.data$type)
  
  
  #Get all the unique types.
  shot.tables <- cbind(expand.grid (season.block=1:4,
                                    shottypes=shottypes,
                                    condition=c("homeESSH", "homePP", "homeEN", "homeOnEN",
                                      "awayESSH", "awayPP", "awayEN", "awayOnEN"),
                                    stringsAsFactors = FALSE),
                       shotcount=0)

  out.objects <- list()
  for (rr in 1:nrow(shot.tables)) {

    message (paste(shot.tables[rr,], collapse=" "))
    rows <- shot.sub.rows(shot.data, shot.tables[rr,1], shot.tables[rr,2], shot.tables[rr,3])
    shot.tables[rr,4] <- length(rows)
    
    result.1 <- try(prob.score.simple.objects (shot.data[rows,]), TRUE)
    if (class(result.1) != "try-error") {
      #prob.ongoal <- result.1[[1]]$fitted.values
      #print(c(length(result.1[[3]]), length(result.1[[4]])))
      shot.data$prob.goal.if.ongoal[rows] <- result.1[[4]]
      shot.data$shot.prob.distance[rows] <- result.1[[3]] * result.1[[4]]

      result.1[[3]] <- result.1[[4]] <- NULL
      out.objects[[rr]] <- result.1
    } else {
      #print(table(shot.data[rows,]$etype))
      if (length(rows)>0) {
        shot.data$prob.goal.if.ongoal[rows] <- 0
        shot.data$shot.prob.distance[rows] <- 0
      }
      out.objects[[rr]] <- NULL
    }
      
  }

  
  grand.data$shot.prob.distance[shot.rows] <- shot.data$shot.prob.distance
  grand.data$prob.goal.if.ongoal[shot.rows] <- shot.data$prob.goal.if.ongoal
  
  return(list(grand.data=grand.data, scoring.models=out.objects, shot.tables=shot.tables))
  
}



fit.shot.probs.simple <- function (grand.data,
                                   scoring.models,
                                   shot.tables) {
  
  #load("nhlscrapr-probs.RData"); grand.data
  
  if (!("shot.prob.distance" %in% colnames(grand.data))) grand.data$shot.prob.distance <- NA
  if (!("prob.goal.if.ongoal" %in% colnames(grand.data))) grand.data$prob.goal.if.ongoal <- NA
  
  etypes <- c("GOAL","SHOT","MISS")
  shot.rows <- which (grand.data$etype %in% etypes)
  shot.data <- grand.data[shot.rows,]
  shot.data$cut.distance <- cut (shot.data$adjusted.distance, c(seq(0,100,by=5), Inf), include.lowest = TRUE)
  shot.data$its.a.sog <- 1*(shot.data$etype %in% c("SHOT","GOAL"))
  shot.data$its.a.goal <- 1*(shot.data$etype == "GOAL")

  
  for (rr in 1:nrow(shot.tables)) {

    message (paste(shot.tables[rr,], collapse=" "))
    rows <- shot.sub.rows(shot.data, shot.tables[rr,1], shot.tables[rr,2], shot.tables[rr,3])

    if (rr <= length(scoring.models)) if (!is.null(scoring.models[[rr]])) {

      sog.1 <- expit(predict(scoring.models[[rr]][[1]], shot.data[rows,]))
      gol.1 <- expit(predict(scoring.models[[rr]][[2]], shot.data[rows,]))
      
      shot.data$prob.goal.if.ongoal[rows] <- gol.1
      shot.data$shot.prob.distance[rows] <- gol.1 * sog.1
      
    } else {
      shot.data$prob.goal.if.ongoal[rows] <- 0
      shot.data$shot.prob.distance[rows] <- 0
    }
    
  }

  
  grand.data$shot.prob.distance[shot.rows] <- shot.data$shot.prob.distance
  grand.data$prob.goal.if.ongoal[shot.rows] <- shot.data$prob.goal.if.ongoal
  
  return(grand.data=grand.data)
  
}





game.team.breakdown <- function (single.game.data) {
  #single.game.data=grand.data[grand.data$season=="20132014" & grand.data$gcode=="20001",]
  
  esub.c <- single.game.data[abs(single.game.data$home.score - single.game.data$away.score) <= 1,]
  esub.f <- single.game.data[abs(single.game.data$home.score - single.game.data$away.score) > 1,]

  retval <- c(home.eg.c=sum(esub.c$shot.prob.distance[esub.c$ev.team==esub.c$hometeam], na.rm=TRUE),
              away.eg.c=sum(esub.c$shot.prob.distance[esub.c$ev.team==esub.c$awayteam], na.rm=TRUE),
              home.eg.f=sum(esub.f$shot.prob.distance[esub.f$ev.team==esub.f$hometeam], na.rm=TRUE),
              away.eg.f=sum(esub.f$shot.prob.distance[esub.f$ev.team==esub.f$awayteam], na.rm=TRUE),

              home.go.c=sum(esub.c$etype[esub.c$ev.team==esub.c$hometeam]=="GOAL", na.rm=TRUE),
              away.go.c=sum(esub.c$etype[esub.c$ev.team==esub.c$awayteam]=="GOAL", na.rm=TRUE),
              home.go.f=sum(esub.f$etype[esub.f$ev.team==esub.f$hometeam]=="GOAL", na.rm=TRUE),
              away.go.f=sum(esub.f$etype[esub.f$ev.team==esub.f$awayteam]=="GOAL", na.rm=TRUE),
              
              home.sh.c=sum(esub.c$etype[esub.c$ev.team==esub.c$hometeam]=="SHOT", na.rm=TRUE),
              away.sh.c=sum(esub.c$etype[esub.c$ev.team==esub.c$awayteam]=="SHOT", na.rm=TRUE),
              home.sh.f=sum(esub.f$etype[esub.f$ev.team==esub.f$hometeam]=="SHOT", na.rm=TRUE),
              away.sh.f=sum(esub.f$etype[esub.f$ev.team==esub.f$awayteam]=="SHOT", na.rm=TRUE),
              
              home.mi.c=sum(esub.c$etype[esub.c$ev.team==esub.c$hometeam]=="MISS", na.rm=TRUE),
              away.mi.c=sum(esub.c$etype[esub.c$ev.team==esub.c$awayteam]=="MISS", na.rm=TRUE),
              home.mi.f=sum(esub.f$etype[esub.f$ev.team==esub.f$hometeam]=="MISS", na.rm=TRUE),
              away.mi.f=sum(esub.f$etype[esub.f$ev.team==esub.f$awayteam]=="MISS", na.rm=TRUE),
              
              home.bl.c=sum(esub.c$etype[esub.c$ev.team==esub.c$hometeam]=="BLOCK", na.rm=TRUE),
              away.bl.c=sum(esub.c$etype[esub.c$ev.team==esub.c$awayteam]=="BLOCK", na.rm=TRUE),
              home.bl.f=sum(esub.f$etype[esub.f$ev.team==esub.f$hometeam]=="BLOCK", na.rm=TRUE),
              away.bl.f=sum(esub.f$etype[esub.f$ev.team==esub.f$awayteam]=="BLOCK", na.rm=TRUE))

  retval <- c(retval,
              sum(retval[seq(5,19,by=2)]), sum(retval[seq(6,20,by=2)]), #corsifor <- 
              sum(retval[seq(5,15,by=2)]), sum(retval[seq(6,16,by=2)]), #fenwickfor
              sum(retval[seq(5,13,by=4)]), sum(retval[seq(6,14,by=4)]), #fenwickclose
              sum(retval[c(1,3)]), sum(retval[c(2,4)])) #weighted
  names(retval)[21:28] <- c("home.corsifor","away.corsifor","home.fenwickfor","away.fenwickfor","home.fenwickclose","away.fenwickclose","home.expectedgoals","away.expectedgoals")
  
  return(retval)
}

augment.game.stats <- function(grand.data, games) {

  #add: expected goals, corsi, fenwick, fenwick close, fenwick far
  #games$home.eg.c <- games$away.eg.c <- games$home.eg.f <- games$away.eg.f <-
  #  games$home.go.c <- games$away.go.c <- games$home.go.f <- games$away.go.f <-
  #    games$home.sh.c <- games$away.sh.c <- games$home.sh.f <- games$away.sh.f <-
  #      games$home.mi.c <- games$away.mi.c <- games$home.mi.f <- games$away.mi.f <-
  #        games$home.bl.c <- games$away.bl.c <- games$home.bl.f <- games$away.bl.f <-
  #          NA

  oldgamecols <- 10
  nameholder <- game.team.breakdown (grand.data[1,])
  if (dim(games)[2] == oldgamecols) {
    statpack <- array(NA, c(nrow(games), length(nameholder)))
    colnames(statpack) <- names(nameholder)
    games <- cbind(games, statpack)
  }

  hometeams <- unique(games$hometeam)
  for (tt in hometeams[hometeams != ""]) {
    message ("Augmenting team ",tt)
    rowchoice <- which(games$hometeam == tt)
    game.sub <- games[rowchoice,]
    event.sub <- grand.data[grand.data$hometeam == tt &
                            grand.data$etype %in% c("GOAL","MISS","SHOT","BLOCK"),]
    
    for (gg in 1:length(rowchoice)) {
      #message(":",tt," ",gg,":")
      single.game.data <- event.sub[event.sub$season == game.sub$season[gg] &
                                    event.sub$gcode == game.sub$gcode[gg] ,]
      entire.game <- min(which(games$season == game.sub$season[gg] & games$gcode == game.sub$gcode[gg]))
      games[entire.game, oldgamecols + 1:28] <- game.team.breakdown (single.game.data)
      #statpack[entire.game,] <- game.team.breakdown (single.game.data)
    }
  }

  #games <- cbind(games, statpack)
  #save(games, file="games-plus.RData")
  return(games)
}

team.table <- function (games.aug) {
  #games.aug <- games[games$season == "20132014" & !is.na(games$away.expectedgoals),]
  hometeam.for <- games.aug[,seq(11, 37, by=2)]
  hometeam.against <- games.aug[,1+seq(11, 37, by=2)]
  restable <- t(sapply (unique(c(games.aug$awayteam, games.aug$hometeam)), function(tt) {
    teamfor <- apply(hometeam.for[games.aug$hometeam==tt,], 2, sum) + apply(hometeam.against[games.aug$awayteam==tt,], 2, sum)
    teamag <- apply(hometeam.against[games.aug$hometeam==tt,], 2, sum) + apply(hometeam.for[games.aug$awayteam==tt,], 2, sum)
    c(teamfor, teamag)
  }))
  restable.df <- data.frame(restable)
  #colnames(restable) <- unique(c(games.aug$awayteam, games.aug$hometeam))
  thisplot <- function () {
    par(mfrow=c(1,3))
    plot(restable.df$home.expectedgoals, restable.df$home.go.c+restable.df$home.go.f, ty="n"); text(restable.df$home.expectedgoals, restable.df$home.go.c+restable.df$home.go.f, rownames(restable.df)); abline(a=0,b=1,col=2)
    plot(restable.df$away.expectedgoals, restable.df$away.go.c+restable.df$away.go.f, ty="n"); text(restable.df$away.expectedgoals, restable.df$away.go.c+restable.df$away.go.f, rownames(restable.df)); abline(a=0,b=1,col=2)
    plot(restable.df$home.expectedgoals - restable.df$away.expectedgoals,
         restable.df$home.go.c+restable.df$home.go.f - restable.df$away.go.c-restable.df$away.go.f - (restable.df$home.expectedgoals - restable.df$away.expectedgoals),
         ty="n");
    text(restable.df$home.expectedgoals - restable.df$away.expectedgoals,
         restable.df$home.go.c+restable.df$home.go.f - restable.df$away.go.c-restable.df$away.go.f - (restable.df$home.expectedgoals - restable.df$away.expectedgoals),
         rownames(restable.df));
         abline(a=0,b=0,col=2)
  }
}
#games[games$season == "20132014" & !is.na(games$away.expectedgoals),]


player.summary <- function (grand.data, roster.unique) {
  #grand.data=recent.data
  
  events <- c("PENL", "SHOT", "GOAL", "MISS", "BLOCK")
  #player in event (1,2,3); player on ice for; player on ice against. 5 deep.

  columns <- 3+c(5:16, 18:20, 28:29)
  involved.players <- NULL;
  for (cc in columns) involved.players <- unique(c(involved.players, grand.data[,cc]))
  involved.players <- sort(involved.players)
  output <- array(0, c(length(involved.players), length(events), 5))

  #Probably a faster way to do this, but OK for now.
  for (ee in events) {
    message(paste("Matching", ee))
    little.data <- grand.data[grand.data$etype==ee,]
    if (dim(little.data)[1]>0) {
      for (cc in c("ev.player.1", "ev.player.2", "ev.player.3")) {
        evs <- table(little.data[,cc])
        rws <- match(as.numeric(names(evs)), involved.players)
        output[rws, which(ee==events), cc-20] <-
          output[rws, which(ee==events), cc-20] + evs
      }

      #away team
      for (cc in c("a1","a2","a3","a4","a5","a6", "away.G")) {
        evs <- table(little.data[little.data$ev.team==little.data$hometeam, cc])
        rws <- match(as.numeric(names(evs)), involved.players)
        output[rws, which(ee==events), 5] <-
          output[rws, which(ee==events), 5] + evs
        evs <- table(little.data[little.data$ev.team==little.data$awayteam, cc])
        rws <- match(as.numeric(names(evs)), involved.players)
        output[rws, which(ee==events), 4] <-
          output[rws, which(ee==events), 4] + evs
      }

      #home team
      for (cc in c("h1","h2","h3","h4","h5","h6", "home.G")) {
        evs <- table(little.data[little.data$ev.team==little.data$hometeam, cc])
        rws <- match(as.numeric(names(evs)), involved.players)
        output[rws, which(ee==events), 4] <-
          output[rws, which(ee==events), 4] + evs
        evs <- table(little.data[little.data$ev.team==little.data$awayteam, cc])
        rws <- match(as.numeric(names(evs)), involved.players)
        output[rws, which(ee==events), 5] <-
          output[rws, which(ee==events), 5] + evs
      }
    }
  }
  output <- output[involved.players > 0,,]
  involved.players <- involved.players[involved.players > 0]
  rownames(output) <- roster.unique$firstlast[involved.players]
  colnames(output) <- events
  return(output)
}




#net probability of goal scoring.
NP.score <- function(grand.data, seconds=20) {
  #load("nhlscrapes-short.RData")
  if (length(seconds)>1) stop ("seconds must have one entry.")
  gt.home <- gt.away <- rep(3600, dim(grand.data)[1])
  lastgoal.home <- lastgoal.away <- 100000
  for (kk in (dim(grand.data)[1]-1):1) {
    if (grand.data$seconds[kk+1] < grand.data$seconds[kk]) {lastgoal.home <- lastgoal.away <- 100000}
    if (grand.data$etype[kk]=="GOAL") {
      if (grand.data$ev.team[kk] == grand.data$hometeam[kk]) {
        lastgoal.home <- grand.data$seconds[kk]
      } else {
        lastgoal.away <- grand.data$seconds[kk]
      }
    }
    gt.home[kk] <- lastgoal.home - grand.data$seconds[kk]
    gt.away[kk] <- lastgoal.away - grand.data$seconds[kk]
  }
  home.event <- grand.data$ev.team==grand.data$hometeam

  counts.table <- NULL
  for (ev in unique(grand.data$etype))
    for (ht in unique(home.event))
      for (zn in unique(grand.data$homezone)) {
        events <- which(grand.data$etype==ev & home.event==ht & grand.data$zone==zn)
        sub.home <- gt.home[events]
        sub.away <- gt.away[events]
        times <- c(sum(sub.away <= seconds), sum(sub.home <= seconds))
          
        counts.table <- rbind(counts.table,
                              c(ev, ht, zn, length(events),
                                times))
                                        #}
      }
  colnames(counts.table) <- c("etype", "home", "h.zone", "N.total", "G.away", "G.home")
  counts.table <- as.data.frame(counts.table, stringsAsFactors=FALSE)
  for (cc in 4:6) counts.table[,cc] <- as.numeric(counts.table[,cc])
  counts.table$NP <- (counts.table$G.home - counts.table$G.away)/counts.table$N.total

  return(counts.table)

}

