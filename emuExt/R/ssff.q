## utilities for reading ssff files in R

getHeader <- function(signal)attr(signal, "header")
  

ssffHeader <- function(filename)
  {
    ## parse the header - not sure about all of the details yet
    ## this is a limited reader - doesn't deal with anything except
    ## raw data. lpc coefficients are out.
    machineTypes <- c("SPARC", "IBM-PC")  ## don't know what the other options are
    machineEndian <- c("big", "little")
    names(machineEndian) <- machineTypes

    con <- file(filename, open="rb")
    head <- NULL
    inHeader <- TRUE
    while (inHeader) {
      ll <- readLines(con, n=1)
      inHeader <- length(grep("-------", ll)) == 0
      head <- c(head, ll)
    }

    ## now create a list object from head
    ## Endian, RecordFreq, StartTime, DataType, Columns, HeaderLength

    ## Key words are "Machine", "Record_Freq", "Column", "Start_Time"
    p1 <- grep("^Machine", head)
    m <- gsub("Machine *", "", head[p1])
    
    p1 <- grep("^Start_Time", head)
    st <- as.numeric(gsub("Start_Time *", "", head[p1]))

    p1 <- grep("^Record_Freq", head)
    rf <- as.numeric(gsub("Record_Freq *", "", head[p1]))

    ## column definitions - there could be lots of them
    p1 <- grep("^Column", head)
    h1 <- gsub("^Column *", "", head[p1])
    h2 <- unlist(strsplit(h1, " "))
    dim(h2) <- c(3, length(h1))

    colcount <- as.numeric(h2[3,])
    nn <- as.vector(sapply(colcount, function(x)1:x))
    colnames <- rep(h2[1,], colcount)
    colnames <- paste(colnames, nn, sep="")
    coltype <- rep(h2[2, ], colcount)
    header <- list(connection=con, endian=machineEndian[m],
                   StartTime=st, RecordFreq=rf, colnames=colnames,
                   coltypes=coltype, orighead=head, filename=filename)
    header
  }

loadSSFF <- function(filename)
  {
    h <- ssffHeader(filename)

    uu <- unique(h$coltypes)
    
    if (length(uu)==1) {
      ## data all the same type
      what <- switch(uu, "FLOAT"=single(),
                     "DOUBLE"=double(),
                     "SHORT"=integer(),
                     "LONG"=integer())
      size <- switch(uu, "FLOAT"=4,
                     "DOUBLE"=8,
                     "SHORT"=2,
                     "LONG"=8)
      bytes <- file.info(filename)$size
      count <- bytes/size
      data <- readBin(h$connection, what=what, n=count, size=size,
                      signed=FALSE, endian=h$endian)
      #close(h$connection)
      dim(data) <- c(length(h$coltypes), length(data)/length(h$coltypes))
      attr(data, "header") <- h
      return(data)

    } else {
      stop("Multiple data types - haven't written this bit\n")
    }
    
  }

writeSSFF <- function(filename, signal)
  {
    con <- file(filename, open="wb")
    machineTypes <- c("SPARC", "IBM-PC")  ## don't know what the other options are
    machineEndian <- c("big", "little")
    names(machineTypes) <- machineEndian

    hd <- getHeader(signal)
    thisType <- switch(storage.mode(signal), "integer"="INTEGER", "double"="FLOAT")
    thisSize <- switch(storage.mode(signal), "integer"=4, "double"=4)
    ## build the header
    hh <- "SSFF   ---- (c) SHLRC"
    hh <- c(hh, paste("Machine", machineTypes[.Platform$endian]))
    hh <- c(hh, "Start_Time 0.0000")
    hh <- c(hh, paste("Record_Freq", as.numeric(hd$RecordFreq)))
    hh <- c(hh, paste("Column artic", thisType, nrow(signal)))
    hh <- c(hh, "-----------------")
    writeLines(text=hh, con=con)
    writeBin(as.vector(signal), con, size=thisSize)
    close(con)
  }

loadSamp <- function(name, path=NULL,
                     labs=c("top_lip", "lower_lip", "chin", "nose_bridge",
                       "tongue_tip", "tongue_body", "empty", "empty",
                       "teeth_reference", "teeth_ref_50mm"))
  {
    if (is.null(path)) {
      path <- dirname(name)
    }

    name <- basename(name)
    nn <- gsub("\\.s?art[tb]", "", name)
    tname <- paste(path, "/", nn, ".sartt", sep="")
    bname <- paste(path, "/", nn, ".sartb", sep="")

    top <- loadSSFF(tname)
    bot <- loadSSFF(bname)
    res <- rbind(top, bot)
    labs <- rep(labs, rep(2,length(labs)))
    lbs <- paste(labs, c("_X", "_Y"), sep="")
    dimnames(res) <- list(lbs, NULL)
    attr(res, "header") <- getHeader(top)
    res
  }

smoothOutliersOLD <- function(signal)
  {
    ## remove the extreme values  - not necessarily very memory efficient way
    ## of doing it

    ## first find the extreme values
    extrm <- signal>=65000
    cnt <- sum(extrm)
    if (cnt==0) {
      return(signal)
    } else {
      cat("Replacing ", cnt, "extreme values\n")
    }

    for (i in 1:nrow(signal)) {
      ## do a runlength encoding to figure out the size of the
      ## median filter
      ee <- extrm[i,]
      rr <- rle(ee)
      if (any(rr$values)) {
        kern <- max(rr$lengths[rr$values])
        ## make kern odd and big enough
        kern <- 2*kern + 7
        ## now median smooth the original
        smth <- runmed(signal[i,], k=kern, algorithm="Turlach")
        signal[i, ee] <- smth[ee]
        ## job may not be complete with funny combinations - just warn for now
        if (any(signal[i,] > 30000)) {
          warning("Extreme values still present")
        }
      }
    }

    signal
  }
smoothOutliers <- function(signal)
  {
    ## remove the extreme values  - not necessarily very memory efficient way
    ## of doing it

    ## first find the extreme values
    extrm <- signal>=15000
    cnt <- sum(extrm)
    if (cnt==0) {
      return(signal)
    } else {
      cat("Replacing ", cnt, "extreme values\n")
    }

    for (i in 1:nrow(signal)) {
      ## do a runlength encoding to figure out the size of the
      ## median filter
      ee <- extrm[i,]

      if (ee[1] | ee[length(ee)]) {
        warning("Start or end of signal is bad")
      }
      
      rr <- rle(ee)
      if (any(rr$values)) {
        #print(rr)
        where <- 1:length(rr$values)
        gaps <- where[rr$values]
        pos <- cumsum(rr$lengths)
        for (j in gaps){
          first <- pos[j-1]
          last <- pos[j] + 1
          firstVal <- signal[i, first]
          lastVal <- signal[i, last]
          #print(c(firstVal, lastVal))
          replace <- as.integer(seq(from=firstVal, to=lastVal, length=rr$length[j]))
          #print(replace)
          signal[i, (first+1):(last-1)] <- replace
          #print(c(first, last))
        }

#        if (any(signal[i,] > 30000)) {
#          warning("Extreme values still present")
#        }
      }
    }

    signal
  }
estCorrection <- function(signal, labs=c("teeth_reference", "teeth_ref_50mm"))
  {
    labs <- rep(labs, rep(2,length(labs)))
    lbs <- paste(labs, c("_X", "_Y"), sep="")
    signal2 <- signal[lbs,]
    m1A <- apply(signal2, MARGIN=1, mean)
    r1 <- m1A[1:2]
    r2 <- m1A[3:4]

    v <- r2 - r1
    theta <- -atan2(v[2], v[1])
    rot <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
    list(rotation=rot, shift=r1)
  }

relCorrect <- function(signal, labs=c('nose_bridge'))
  {
    ## substract the position of the nose bridge at each instant
    labs <- rep(labs, rep(2,length(labs)))
    lbs <- paste(labs, c("_X", "_Y"), sep="")

    uu <- signal[lbs,]
    ## do this with a loop:

    ii <- seq(from=1, to=nrow(signal), by=2)

    for (I in ii) {
      dd <- c(I,I+1)
      signal[dd,] <- signal[dd,]-uu
    }
    signal
  }

doCuts <- function(signal, cutfile=NULL)
  {
    h <- getHeader(signal)
    if (is.null(cutfile)) {
      ff <- h$filename
      dname <- dirname(ff)
      bname <- basename(ff)
      bname <- gsub("\\..*", "", bname)
      cutfile <- paste(dname, "/", bname, ".cutlist.as", sep="")
    } 
    cuts <- read.table(cutfile)
    colnames(cuts) <- c("start", "end", "wav", "name")
    freq <- h$RecordFreq
    cuts[,c("start", "end")] <- cuts[,c("start", "end")] * freq
    #browser()

    window <- 0.1  ## seconds
    smoothSig <- function(sig)
      {
        ll <- length(sig)
        duration <- ll/freq
        span <- window/duration
        dd <- data.frame(x=1:ll, y=sig)
        mod <- loess(y~x, dd, span=span)
        res <- predict(mod, dd)
        #browser()
        res
      }
    
    for (i in 1:nrow(cuts)) {
      yy <- cuts[i,"start"]:cuts[i,"end"]
      ss <- signal[,yy]
      attr(ss, "header") <- getHeader(signal)
      ss1 <- t(apply(ss, MARGIN=1, smoothSig))
      #browser()
      attr(ss1, "header") <- getHeader(signal)
      ##  write the output here
      nm <- paste(cuts[i, "name"], ".ema", sep="")
      writeSSFF(nm, ss1)
    }
  }

applyCorrection <- function(signal, correct)
  {
    ## correct for rotation
    s <- seq(from=1, to=nrow(signal), by=2)
    rot <- correct$rotation
    shft <- correct$shift
    res <- signal
    for (i in 1:length(s)) {
      v <- s[i]:(s[i]+1)
      res[v,] <- (rot %*% (signal[v,] - shft))+shft
##      res[v,] <- rot %*% signal[v,]
    }
    res
  }

showPoints <- function(signal)
  {
    m1A <- apply(signal, MARGIN=1, mean)

    dim(m1A) <- c(2, length(m1A)/2)
    
    mall <- t(m1A)
    plot(mall, type='n')
    PP<<-mall
    cc <- c(rep('black', 8), rep('red',2))
    text(mall, paste(1:nrow(mall)),col=cc)
    lines(mall[9:10,])
  }

chkDist <- function(pts)
  {
    x <- pts[,1]
    y <- pts[,2]

    X <- outer(x, x, "-")
    Y <- outer(y, y, "-")

    sqrt(X^2 + Y^2)
  }

applyScale <- function(signal, scale=1.46766)
  {
    ## scale was computed using the DOP, which is known to have a 50mm separation
    ## between sensors.
  }

summaryPlot <- function(signal, MED=TRUE)
  {
    # a plot of all sensors
    # intended for looking at the DOP signals
    signames <- rownames(signal)
    rem <- grep('empty', signames)
    signal <- signal[-rem,]
    signames <- rownames(signal)
    xs <- grep('X', signames)
    ys <- grep('Y', signames)
    #xr <- range(as.vector(signal[xs,]))
    #yr <- range(as.vector(signal[ys,]))
    #plot(t(signal[c(1,2),]), type='n', xlim=xr, ylim=yr)
    cc <- 1
    if (MED) {
      res <- NULL
      for (i in xs) {
        locs <- apply(signal[c(i,i+1),], MARGIN=1, median)
        #print(signames[i])
        print(locs)
        res <- rbind(res, locs)
      }
      xr <- range(res[,1])
      yr <- range(res[,2])
      xr <- c(xr[1] - 500, xr[2]+500)
      yr <- c(yr[1] - 500, yr[2]+500)
      plot(res, type='n', xlim=xr, ylim=yr)
      for (i in 1:nrow(res)) {
        text(res[i,1], res[i,2], labels=signames[2*i], col=cc)
        cc <- cc+1
      }
    } else {
      for (i in xs) {
        lines(t(signal[c(i,i+1),]), col=cc)
        cc <- cc+1
      }

    }
  }

summaryPlot2 <- function(signal, channels=c("tongue_tip", "tongue_body", "lower_lip"))
  {
    xnames <- paste(channels, "_X", sep="")
    ynames <- paste(channels, "_Y", sep="")

    xr <- range(as.vector(signal[xnames,]))
    yr <- range(as.vector(signal[ynames,]))

    plot(NA, xlim=xr, ylim=yr, xlab='X', ylab='Y')
    for (i in 1:length(xnames)) {
      points(t(signal[c(xnames[i], ynames[i]),]), col=i)
    }
    
  }

function()
  {
    q1 <- loadSamp(path="EMA", "mtlssentdop01.artb")
    q1 <- smoothOutliers(q1)
    q2 <- relCorrect(q1)
    dd <- estCorrection(q2)

    l1 <- loadSamp(path="EMA", "mtlssent01.artt")
    l1 <- smoothOutliers(l1)
    l2 <- relCorrect(l1)
    l2 <- applyCorrection(l2, dd)

    doCuts(l2)
  }

function()
  {
    q1 <- loadSamp(path="EMA", "mtmtsentdop01.artb")
    q1 <- smoothOutliers(q1)
    q2 <- relCorrect(q1)
    dd <- estCorrection(q2)


  }
function()
  {
    q1 <- loadSamp(path="EMA", "mtrbsentdop01.artb")
    q1 <- smoothOutliers(q1)
    q2 <- relCorrect(q1)
    dd <- estCorrection(q2)

    l1 <- loadSamp(path="EMA", "mtrbsent01.artt")
    l1 <- smoothOutliers(l1)


    doCuts(l2)
  }

## Aboriginal data - Sab
function()
  {
    ## load DOP
    q1 <- loadSamp(name="mtsabdop01.artb",
                   path="H:/Arrernte_Database/Cutlists",
                   labs=c("top_lip", "lower_lip", "chin",
                     "nose_bridge",  "tongue_tip", "empty",
                     "tongue_body",
                     "teeth_reference", "empty", 
                     "teeth_ref_50mm"))
    
    q1 <- smoothOutliers(q1)
    q2 <- relCorrect(q1)
    dd <- estCorrection(q2)
    
  }

# JT
jt1 <- function()
  {
    channels <- c("top_lip", "lower_lip", "chin",
                     "nose_bridge",  "tongue_tip", "tongue_body",
                     "empty","empty", "teeth_reference",
                     "teeth_ref_50mm")
    ## load DOP 01
    d1 <- loadSamp(name="mtarrjtdop01.artb",
                   path="H:/Arrernte_Database/Cutlists",
                   labs=channels)
    
    d1 <- smoothOutliers(d1)
    d2 <- relCorrect(d1)
    dd <- estCorrection(d2)

    l1 <- loadSamp(name="mtarrjt01.artt", path="H:/Arrernte_Database/Cutlists",
                   labs=channels)
    
    l1 <- smoothOutliers(l1)
    l2 <- relCorrect(l1)
    l2 <- applyCorrection(l2, dd)

    doCuts(l2, cutfile="H:/Arrernte_Database/Cutlists/mtarrjt01bsd.txt")
  }


