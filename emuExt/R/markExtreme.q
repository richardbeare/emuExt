markMaxMin <- function(xdata, ydata, timeMarks=list(NULL), labels=list(NULL),
                       extremes=NULL)
{
   tokenCount <- nrow(xdata$index)

   if (any(xdata$index != ydata$index)) 
       stop("Signals don't match\n")

   lablengths <- sapply(labels, length)
   if (length(unique(lablengths)) != 1) {
      stop("Label lengths aren't equal")
   }

   freq <- diff(as.vector(xdata[1,1]$ftime))/1000
   freq <- (diff(as.vector(xdata[1,1]$index)) + 1)/freq

   markPoints <- list()
   for (uu in 1:length(timeMarks)) {
     markPoints[[uu]] <- (timeMarks[[uu]] - xdata$ftime[,1]) * freq/1000
   }

   doPlot <- function(i, posX, posY, maxPos=NULL, minPos=NULL)
     {
     sensors <- 1:ncol(posX)
     rrx <- range(posX)
     rry <- range(posY)
     heading <- paste(sapply(labels, function(X){X[i]}), collapse=" ")
     heading <- paste(i, ":", heading)
     plot(posX[, 1], posY[, 1], ylim = rry, xlim = rrx, 
          type = "n", main = heading)

     for (k in sensors) {
       lines(posX[, k], posY[, k], col = k)
       points(posX[, k], posY[, k], col = k)
       points(posX[1, k], posY[1, k], pch="+", col = "red", cex=1.5)
       for (ll in 1:length(markPoints)) {
         pos <- markPoints[[ll]][i,]
         points(posX[pos, k], posY[pos,k], pch="O", col="red", cex=1.5)
       }
       if (!is.null(maxPos)) {
         points(posX[maxPos, k], posY[maxPos, k], pch=24, cex=1.5, col='red', bg='yellow')
       }
       if (!is.null(minPos)) {
         points(posX[minPos, k], posY[minPos, k], pch=25, cex=1.5, col='red', bg='yellow')
       }

     }
     }

   maxCount <- sum(extremes==1)
   minCount <- sum(extremes==0)
   msg1 <- paste("Mark ", maxCount, " maxima by clicking with the left button\n", sep="")
   msg2 <- paste("Mark ", minCount, " minima by clicking with the left button\n", sep="")
   maxresult <- minresult <- list()
   for (token in 1:tokenCount) {
      posX <- xdata[token, ]$data
      posY <- ydata[token, ]$data
      sampTime <- xdata[token, ]$ftime
      timesPos <- seq(sampTime[1], sampTime[2], length = nrow(posX))
      timesVel <- timesPos[-1]

      modifying <- TRUE
      while (modifying) {
        doPlot(token, posX, posY)
        if (maxCount > 0) {
	     cat(msg1)
           flush.console()      
           maxPos <- identify(posX[, 1], posY[, 1], n = maxCount)
           doPlot(token, posX, posY, maxPos=maxPos)
        }
        if (minCount > 0) {
	     cat(msg2)
           flush.console()      
           minPos <- identify(posX[, 1], posY[, 1], n = minCount)
           doPlot(token, posX, posY, maxPos=maxPos, minPos=minPos)
        }
        cat("Is this OK([y]/n/q)\n")
        modifying <- quitting <- FALSE
        ans <- scan(quiet=TRUE, what="", n=1)
        if (length(ans) > 0) {
          if (ans == "n" | ans == "N")
             modifying <- TRUE
          else if (ans == "q" | ans == "Q")
             quitting <-TRUE
        }
      }
      # now make the measurements
      vel <- sqrt(diff(posX)^2 + diff(posY)^2)
      if (maxCount > 0) {
        velMaxs <- vel[maxPos]
        aa <- cbind(FTimes = timesVel[maxPos], MaxVel = velMaxs, 
                    MaxPosX = posX[maxPos,], 
                    MaxPosY = posY[maxPos,])
        rownames(aa) <- NULL
        maxresult[[token]] <- aa
      }
      if (minCount > 0) {
        velMins <- vel[minPos]
        bb <- cbind(FTimes = timesVel[minPos], MinVel = velMins, 
                    MinPosX = posX[minPos,], 
                    MinPosY = posY[minPos,])
        rownames(bb) <- NULL
        minresult[[token]] <- bb
      }
      if (quitting) break
   }
   list(max=maxresult, min=minresult)
}
