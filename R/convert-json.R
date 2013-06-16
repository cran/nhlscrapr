
match.xy <- function (pl.table, json.object) {
  #json.object=game.rec$xy
  
  #just get the relevant stuff.
  if (is.null(json.object$data)) ob1 <- json.object$game$plays$play else ob1 <- json.object$data$game$plays$play
   
  xy.event.table <- t(sapply(ob1, function(pp) with(pp, c(ycoord, xcoord, period, time, type))))

  if (length(xy.event.table)>0) {
    s1 <- 60*as.numeric(substr(xy.event.table[,4],1,2))+as.numeric(substr(xy.event.table[,4],4,5))
    if (s1[2]-s1[1]<0 & s1[3]-s1[2]<0) s1 <- 1200-s1
  
    xy.frame <- data.frame(xcoord=as.numeric(xy.event.table[,2]),
                           ycoord=as.numeric(xy.event.table[,1]),
                           seconds=1200*(as.numeric(xy.event.table[,3])-1)+s1,
                           etype=toupper(xy.event.table[,5]))
    
    rows <- match(xy.frame$seconds, pl.table$game.record$seconds)
    pl.table$game.record$xcoord[rows[!is.na(rows)]] <- xy.frame$xcoord[!is.na(rows)]
    pl.table$game.record$ycoord[rows[!is.na(rows)]] <- xy.frame$ycoord[!is.na(rows)]
  }
  
  return(pl.table)

}