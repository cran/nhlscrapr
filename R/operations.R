
# Things you can do with the big grand.data object and unique roster.

player.summary <- function (grand.data, roster.unique) {

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
      for (cc in 3+18:20) {
        evs <- table(little.data[,cc])
        rws <- match(as.numeric(names(evs)), involved.players)
        output[rws, which(ee==events), cc-20] <-
          output[rws, which(ee==events), cc-20] + evs
      }

      #away team
      for (cc in 3+c(5:10, 28)) {
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
      for (cc in 3+c(11:16, 29)) {
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
  rownames(output) <- roster.unique$numfirstlast[involved.players]
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
      for (zn in unique(grand.data$zone)) {
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

