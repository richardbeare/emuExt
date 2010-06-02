"dplot.norm"<- function(dataset, labs = NULL, average = F,
                        main = "", xlab = "time (ms)", ylab = "",
                        xlim = NULL, ylim = NULL, cex = 0.5,
                        linetype = F, colour = T,
                        legn = "tl", axes = T, n = 20, printmatrix=T)
{
  ovec <- NULL
  if(is.null(ylim))
    ylim <- range(dataset$data)
  if(is.null(xlim))
    xlim <- c(0, 1)
  if(is.null(labs)) {
    legn <- ""
    labs <- rep(1, nrow(dataset$index))
  }

  col.lty <- mu.colour(labs, colour, linetype)
  colour <- col.lty$colour
  linetype <- col.lty$linetype

  mat.na <- linear(dataset, n)
  mat.na$ftime <- dataset$ftime
  class(mat.na) <- "trackdata"

  xvec <- seq(0, 1, length = n)
  lval <- nrow(dataset$index)

  if(!average) {
    for(j in 1:lval) {
      plot(xvec, mat.na[j]$data, xlim = xlim, ylim = ylim, 
           xlab = "", ylab = "", axes = F, type = "l", col
           = colour[j], lty = linetype[j])
      par(new = T)
    }
    ovec$data <- mat.na
    ovec$time <- xvec
    ovec$labs <- labs
  }
  else {
    if(!is.null(labs)) {
      outmat <- NULL
      outlabs <- NULL
      for(j in unique(labs)) {
        temp <- labs == j
        vals <- mat.na[temp]$data
        vals <- matrix(vals, ncol = n, byrow = T)
        ## take the mean only if there are more than one
        if( is.matrix(vals) ) {
          mvals <- apply(vals, 2, mean)
        } else {
          mvals <- vals
        }
        outmat <- rbind(outmat, mvals)
        outlabs <- c(outlabs, j)
      }
    } else {
      outmat <- apply(matrix(mat.na, ncol = 20, byrow = T), 2,
                      mean)
      outmat <- rbind(outmat)
      outlabs <- 1
    }
    col.code <- match(col.lty$legend$lab, unique(labs))
    colour <- col.lty$legend$col
    linetype <- col.lty$legend$lty
    for(j in 1:nrow(outmat)) {
      plot(xvec, outmat[j,  ], xlim = xlim, ylim = ylim, xlab
           = "", ylab = "", axes = F, type = "l", col = 
           colour[col.code[j]], lty = linetype[col.code[j]
                                  ])
      par(new = T)
    }
    ovec$data <- outmat
    ovec$time <- xvec
    ovec$labs <- outlabs
  }
  if(axes) {
    axis(side = 1, cex = cex)
    axis(side = 2, cex = cex)
    box()
  }
  
  title(main = main, xlab = xlab, ylab = ylab, cex = cex)

  if(legn != "") {
    legn <- mu.legend(legn, xlim, ylim)
    legend(legn$x, legn$y, col.lty$legend$lab, col = col.lty$legend$
           col, lty = col.lty$legend$lty, cex = cex)
  }
  invisible(ovec)
}

###################################################################################

"xyplot"<-
function(x, y, colour = T, linetype = F, xlim = NULL, ylim = NULL, main = "", 
	xlab = "displacement (mm)", ylab = "displacement (mm)", timelims = NULL,
	legn = "br", type = "l", marktime = NULL, marklab = NULL, defaultcolour
	 = 1, pch = "b", arrow = NULL, rel = F, size = 0.2, timestamp=T)
{
# arrow a list with components
# $time: time in ms at which the arrowhead should be drawn
# optionally $size: a vector in inches of the size of
# the arrowhead
	labs <- x$labs
	xdata <- x$data[[1]]
	ydata <- y$data[[1]]
	if(!is.null(timelims)) {
		temp <- (x$time >= timelims[1]) & (x$time <= timelims[2])
		xdata <- xdata[, temp]
		ydata <- ydata[, temp]
		x$time <- x$time[temp]
		if(!is.matrix(xdata)) {
			xdata <- rbind(xdata)
			ydata <- rbind(ydata)
		}
	}
	if(is.null(xlim))
		xlim <- range(xdata, na.rm = T)
	if(is.null(ylim))
		ylim <- range(ydata, na.rm = T)
	col.lty <- mu.colour(labs, colour, linetype, defaultcolour = 
		defaultcolour)
	mark.lty <- mu.colour(labs, colour, timestamp, defaultcolour = 
		defaultcolour)

	for(j in 1:nrow(xdata)) {
		plot(xdata[j,  ], ydata[j,  ], xlim = xlim, ylim = ylim, xlab
			 = "", ylab = "", col = col.lty$colour[j], lty = 
			col.lty$linetype[j], type = type, pch = pch)
            if (timestamp)
              points(xdata[j,  ], ydata[j,  ], col = mark.lty$colour[j], pch = mark.lty$linetype[j])

		if(is.list(arrow)) {
			temp <- closest(x$time, arrow$time)
			if(!any(arrow$size))
				arrow$size <- 0.2
			arrows(xdata[j, temp - 1], ydata[j, temp - 1], xdata[j, 
				temp], ydata[j, temp])
		}
		if(!is.null(marktime)) {
			for(i in 1:length(marktime)) {
				if(is.null(marklab))
				  marklab <- rep("X", length(marktime))
				temp <- closest(x$time, marktime[i])
				text(xdata[j, temp], ydata[j, temp], marklab[i],
				  col = colour[j])
			}
		}
		par(new = T)
	}
		
	title(main = main, xlab = xlab, ylab = ylab)
	if(legn != F) {
		legn <- mu.legend(legn, xlim, ylim)
            if (timestamp) PCH<- mark.lty$linetype 
            else PCH <- NULL
		legend(legn$x, legn$y, legend = as.character(col.lty$legend$lab
			), col = col.lty$legend$col, lty = col.lty$legend$lty, pch = PCH)
	}
}
###################################################################################
## new plotting routines -- currently they have stupid names. These "replace"
## the dplot routines that seem to have become unreliable

## support routines
## convert to an object in which all tracks are the same length by sampling
## the originals -- this uses emu's "linear" function
normTracks <- function(tracks, samples=20, normlength=100)
  {
    if (class(tracks) != "trackdata") {
      stop("Requires a trackdata object\n")
    }

    if (any(apply(is.na(tracks$data), MARGIN=2, all))) {
      stop("A column of only NA's in data - conversion to normalised length won't work\n")
    }
    res <- linear(tracks, n=samples)
    ## convert to a pretend trackdata object

    ftime <- cbind(rep(0, nrow(tracks$index)),
                   rep(normlength, nrow(tracks$index)))
    res$ftime <- ftime
    res$trackname <- "normalised"
    class(res) <- class(tracks)
    res
  }

## produces a track object with as many tracks as labels. Each track is an
## average of all tracks with the same label in the original.
## If the data is not normalised it will do it.
aveTracks <- function(tracks, labels=stop("Labels must be present for averaging"),

                  samples=20, normlength=100)
  {
    if (class(tracks) != "trackdata") {
      stop("Requires a trackdata object\n")
    }

    IdxCount <- tracks$index[,2] - tracks$index[,1] + 1
    if (length(unique(IdxCount)) != 1) {
      cat("Data needs normalising - doing it\n")
      newdata <- normTracks(tracks, samples=samples, normlength=normlength)
    } else {
      cat("Data already normalised\n")
      samples <- IdxCount[1]
      newdata <- tracks
    }
    ULabs <- unique(labels)
    NCOL <- ncol(newdata$data)

    if(is.null(NCOL)) NCOL <- 1

    NewDat <- NULL
    for (i in 1:length(ULabs)) {
      these <- (labels == ULabs[i])
      sub <- newdata[these,]
      newres <- matrix(NA, nrow=samples, ncol=NCOL)
      for (j in 1:NCOL) {
        dat <- sub[,j]$data
        dim(dat) <- c(samples, length(dat)/samples)
        newres[,j] <- apply(dat, MARGIN=1, mean)
      }
      NewDat <- rbind(NewDat, newres)
    }
    NewIdx <- ((0:(length(ULabs)-1)) * samples) + 1
    NewIdx <- cbind(NewIdx, NewIdx + samples -1)
    NewFtime <- newdata$ftime[1:length(ULabs), ]
    newtrack <- list(data=NewDat, index=NewIdx, ftime=NewFtime, trackname="averaged")
    class(newtrack) <- class(tracks)
    return(list(track=newtrack, labels=ULabs))
  }

## plot tracks against time
plotTracks <- function(tracks, labels=NULL, xlab="time (ms)",
                       ylab="", main="", legn="tl")
  {
    ## plot the unaveraged versions
    if (class(tracks) != "trackdata") {
      stop("Requires a trackdata object\n")
    }


    NewFtime <- tracks$ftime - tracks$ftime[,1]
    IdxCount <- tracks$index[,2] - tracks$index[,1] + 1
    StartI <- tracks$index[,1]
    EndI <- tracks$index[,2]

    xrange <- range(NewFtime)
    tt <- !is.na(tracks$data)
    yrange <- range(tracks$data[tt])
    NROWS <- nrow(tracks$index)
    if (is.null(labels)) {
      ThisColor <- rep(1, NROWS)
    } else {
      ULabs <- unique(labels)
      ThisColor <- match(labels, ULabs)
    }
    xIdx <- function(idx){
        seq(from=NewFtime[idx, 1], to=NewFtime[idx, 2], length=IdxCount[idx])
      }
    ## set up the axes etc
    plot(xIdx(1), tracks[1,1]$data, xlim=xrange, ylim=yrange, xlab=xlab,
         ylab=ylab, main=main, type='n')

    ## now plot the data
    plotSingle <- function(r, c) {
      dd <- tracks$data[StartI[r]:EndI[r], c]
      lines(xIdx(r), dd, col=ThisColor[r])
    }
    for (i in 1:ncol(tracks$data)) {
      lapply(1:NROWS, plotSingle, i)
    }
    ## legend
    LegPos <- mu.legend(legn, xrange, yrange)
    if (!is.null(labels)) {
      legend(LegPos$x, LegPos$y, ULabs, col=1:length(ULabs), lty=1)
    }
  }

plotAveTracks <- function(tracks, labels=stop("Labels must be present for averaging"),
                  xlab="time (ms)", ylab="", main="", legn="tl",
                  samples=20, normlength=100)
  {
    atracks <- aveTracks(tracks, labels=labels, samples=samples,
                         normlength=normlength)
    newtracks <- atracks$track
    ULabs <- atracks$labels
    plotTracks(newtracks, labels=ULabs, xlab=xlab, ylab=ylab, main=main, legn=legn)
    invisible()
  }

createLabColors <- function(labels)
  {
    ## creates a vector of labels suitable for plotXYTrack and plotXYAveTrack
    distinct.colors <- colors()[c(552, 257, 26, 68, 450, 91, 81, 31, 36,
                                490, 656, 642, 572, 639, 587, 96, 614,
                                476, 506, 429, 594, 117, 12,
                                563, 633, 461, 645, 84, 640, 556)]
    if (length(labels) > length(distinct.colors)) {
      stop("Not enough colours")
    }
    result <- distinct.colors[1:length(labels)]
    names(result) <- labels
    return(result)
  }

plotXYTrack <- function (tracksx, tracksy, labels = NULL, xlab = "", ylab = "", 
    main = "", legn = "tl", velMarkers = TRUE, markStart = TRUE, 
    palaisX = NULL, palaisY = NULL, doColor=TRUE, labColours=NULL) 
{
    if ((class(tracksx) != "trackdata") | (class(tracksy) != 
        "trackdata")) {
        stop("Requires two trackdata objects\n")
    }
    if (ncol(tracksx$data) != ncol(tracksy$data)) {
        stop("Number of columns in tracksx and tracksy must be the same\n")
    }
    if (ncol(tracksx$data) != ncol(tracksy$data)) {
        stop("Sample lengths must be identical\n")
    }
    NewFtime <- tracksx$ftime - tracksy$ftime[, 1]
    IdxCount <- tracksx$index[, 2] - tracksx$index[, 1] + 1
    StartI <- tracksx$index[, 1]
    EndI <- tracksx$index[, 2]
    tt <- !is.na(tracksx$data)
    if (is.null(palaisX)) {
        xrange <- range(tracksx$data[tt])
    }
    else {
        xrange <- range(c(tracksx$data[tt], palaisX))
    }
    tt <- !is.na(tracksy$data)
    if (is.null(palaisY)) {
        yrange <- range(tracksy$data[tt])
    }
    else {
        yrange <- range(c(tracksy$data[tt], palaisY))
    }
    NROWS <- nrow(tracksx$index)
    if (doColor) {
      if (is.null(labels)) {
        ThisColor <- rep(1, NROWS)
      } else {
        ULabs <- unique(labels)
        if (is.null(labColours)) {
          ## use some default colours. These aren't going to match labels between plots  
          ThisColor <- match(labels, ULabs)
        } else {
          ## use a user defined set of labels. This should be a vector of colour indexes
          ## with names corresponding to labels. Use createLabColors(labels)
          ThisColor <- labColours[labels]
        }
      }
    } else {
      if (!is.null(labels))
        ULabs <- unique(labels)
      ThisColor <- rep(1, NROWS)
    }

    if (is.null(labels)) {
      plotChar <- rep(1, NROWS)
    } else {
      if (is.null(labColours)) {
         plotChar <- 1:length(ULabs)
      } else {
         dd <- 1:length(labColours)
         names(dd) <- names(labColours)
         plotChar <- dd[labels]
      }
    }
    
    
    plot(tracksx[1, 1]$data, tracksy[1, 1]$data, xlim = xrange, 
        ylim = yrange, xlab = xlab, ylab = ylab, main = main, 
        type = "n")
    if (!is.null(palaisX) & !is.null(palaisY)) {
        lines(palaisX, palaisY)
    }
    if (velMarkers) {
        plotSingle <- function(r, c) {
            ddx <- tracksx$data[StartI[r]:EndI[r], c]
            ddy <- tracksy$data[StartI[r]:EndI[r], c]
            lines(ddx, ddy, col = ThisColor[r])
            points(ddx, ddy, col = ThisColor[r], pch = plotChar[r])
            if (markStart) {
                text(ddx[1], ddy[1], labels = "s", col = ThisColor[r], 
                  pos = 2)
            }
        }
    }
    else {
        plotSingle <- function(r, c) {
            ddx <- tracksx$data[StartI[r]:EndI[r], c]
            ddy <- tracksy$data[StartI[r]:EndI[r], c]
            lines(ddx, ddy, col = ThisColor[r])
            if (markStart) {
                text(ddx[1], ddy[1], labels = "s", col = ThisColor[r], 
                  pos = 2)
            }
        }
    }
    for (i in 1:ncol(tracksx$data)) {
        lapply(1:NROWS, plotSingle, i)
    }
    LegPos <- mu.legend(legn, xrange, yrange)
    if (!is.null(labels)) {
        if (velMarkers) {
            legend(LegPos$x, LegPos$y, ULabs, col = ThisColor, 
                pch = plotChar, lty = 1)
        }
        else {
            legend(LegPos$x, LegPos$y, ULabs, col = ThisColor, 
                lty = 1)
        }
    }
}

plotXYAveTrack <- function (tracksx, tracksy, labels = stop("Labels must be present for averaging"), 
    xlab = "", ylab = "", main = "", legn = "tl", velMarkers = TRUE, 
    markStart = TRUE, samples = 20, normlength = 100, palaisX = NULL, 
    palaisY = NULL, doColor=TRUE, labColours=NULL) 
{
    aveX <- aveTracks(tracksx, labels = labels, samples = samples, 
        normlength = normlength)
    aveY <- aveTracks(tracksy, labels = labels, samples = samples, 
        normlength = normlength)
    if (!is.null(palaisX) & !is.null(palaisY)) {
        palaisX <- lowess(palaisX, f = 1/10)$y
        palaisY <- lowess(palaisY, f = 1/10)$y
    }
    plotXYTrack(aveX$track, aveY$track, aveX$labels, xlab = xlab, 
        ylab = ylab, main = main, velMarkers = velMarkers, markStart = markStart, 
        legn = legn, palaisX = palaisX, palaisY = palaisY, doColor=doColor, labColours=labColours)
}


euclidDist <- function(xdata, ydata, toplip, bottomlip, 
                       LabelList=NULL, timeMarks=list(NULL), pause=TRUE)
{
  freq <- diff(as.vector(xdata[1,1]$ftime))/1000
  freq <- (diff(as.vector(xdata[1,1]$index)) + 1)/freq

  markPoints <- list()
  for (uu in 1:length(timeMarks)) {
    markPoints[[uu]] <- (timeMarks[[uu]] - xdata$ftime[,1]) * freq/1000
  }

  if (pause) {
    thisPar <- par(ask=TRUE)
  }

  result <- NULL
  for (j in 1:nrow(xdata$ftime)) {
     xt <- xdata[j, toplip]$data
     yt <- ydata[j, toplip]$data
     xb <- xdata[j, bottomlip]$data
     yb <- ydata[j, bottomlip]$data
     sampTime <- xdata[j, ]$ftime
     timesPos <- seq(sampTime[1], sampTime[2], length = nrow(xt))
     edist <- sqrt((xt - xb)^2 + (yt - yb)^2)
     mpos <- which.min(edist)
     val <- edist[mpos]
     # return data
     result <- rbind(result, c(val, timesPos[mpos]))
     maintext <- paste("Token ", j, ":" )
     if (!is.null(LabelList)) {
       for (i in LabelList) {
          maintext <- paste(maintext, i[j])
       }
     }

     plot(timesPos, edist, main=maintext)
     points(timesPos[mpos], val, pch=2, col="blue")
     for (ll in 1:length(markPoints)) {
       pos <- markPoints[[ll]][j,]
       points(timesPos[pos], edist[pos], pch="O", col="red", cex=1.5)
     }

  }
  if (pause) {
    par(thisPar)
  }
  result
}
