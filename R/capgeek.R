
# Scraping capgeek.com.

process.capgeek.html <- function (capgeek.html) {
  #capgeek.html=capgeek.file

  # Get name.
  nameline <- capgeek.html[grep ("NHL Salary Contract History", capgeek.html)]
  name <- toupper (gsub ("(.*?) *NHL Salary Contract History", "\\1", nameline))
  if (nchar(name)>0) {

    contractlines <- grep("EXPIRY STATUS:", capgeek.html)
    if (length(contractlines)>0) {
    
      birthdayline <- capgeek.html[grep("Birthdate:", capgeek.html)]
      birthdaystring <- gsub(".*</strong>([A-Za-z]+ [0-9]+, [0-9]+)<strong>.*", "\\1", birthdayline)
      
      yearlines <- grep("^<td>[0-9]{4}\\-[0-9]{2}", capgeek.html)
      
      outtable <- t(sapply(yearlines, function (yy) {
        year1 <- gsub("<td>([0-9]{4}).*", "\\1", capgeek.html[yy])
        ahlsalary <- gsub("<.*>(.*)</td>", "\\1", capgeek.html[yy+1])
        nhlsalary <- gsub("<.*>(.*)</td>", "\\1", capgeek.html[yy+2])
        pbonus <- gsub("<.*>(.*)</td>", "\\1", capgeek.html[yy+3])
        sbonus <- gsub("<.*>(.*)</td>", "\\1", capgeek.html[yy+4])
        caphit <- gsub("<.*><strong>(.*)</strong></td>", "\\1", capgeek.html[yy+5])
        data.frame(year1=year1, ahlsalary=ahlsalary, nhlsalary,
                   pbonus=pbonus, sbonus=sbonus, caphit=caphit, stringsAsFactors=FALSE)
      }))
      
      expirystatus <- gsub(".*</strong>(.*)", "\\1", capgeek.html[contractlines])
      expiryvector <- rep(expirystatus[1], dim(outtable)[1])
      if (length(expirystatus) > 1) for (ee in 2:length(expirystatus)) expiryvector[yearlines > contractlines[ee]] <- expirystatus[ee]
      
      outtable <- cbind(outtable, expiryvector)

    } else outtable <- NULL    
  } else outtable <- NULL
    
  if (length(outtable)>0) {
    outtable <- cbind(name, birthdaystring, outtable)
    for (cc in 4:8) {
      outtable[,cc] <- gsub("[$,]", "", outtable[,cc])
      outtable[,cc] <- gsub(".*\u0094", "0", outtable[,cc])
    }
  }
  outtable <- outtable[which(nchar(outtable[,5])>0), ]
  return(outtable)
}

download.capgeek.files <- function (max.player.num=2552,
                                    files=1:max.player.num,
                                    rdata.folder="nhlr-data",
                                    just.process=FALSE,
                                    verbose=TRUE,
                                    retrieve.tables=TRUE
                                    #,output.file="capgeek-raw.RData"
                                    ) {
  download.single <- function (pnum) {
    infile <- paste0("http://www.capgeek.com/player/", pnum)
    capgeek.file <- try(unlist(strsplit(gsub("\\t", "", gsub("\\r", "", getURL(infile))), "\n")), TRUE)
    if (class(capgeek.file) == "try-error") capgeek.file <- NULL  #error.free <- FALSE
    save (capgeek.file, file=paste0(rdata.folder,"/",pnum,"-capgeek.RData"))
    return(capgeek.file)
  }
  output <- list()
  
  for (pnum in files) {
    if (verbose) message("Processing ",pnum)

    if (!just.process) {
      capgeek.file <- download.single (pnum)
    } else if (!file.exists(paste0(rdata.folder, "/",pnum,"-capgeek.RData")))
      capgeek.file <- download.single (pnum) else load (paste0(rdata.folder, "/",pnum,"-capgeek.RData"))

    # Make the table
    capgeek.table <- process.capgeek.html (capgeek.file)
    save (capgeek.table, file=paste0(rdata.folder, "/",pnum,"-capgeek-table.RData"))
    if (retrieve.tables) output[[pnum]] <- capgeek.table
  }

  return(output)
}

