epgmax <- function(trackdata, gap=2)
{
  ## find the maximum value, when it started and proportion of the time


  rr <- nrow(trackdata)
  maxVals <- Start <- End <- Middle <- Prop <- Dur <- PropStart <- rep(NA, rr)
  
  for (j in 1:rr) {
    data <- trackdata[j,]$data
    maxVal <- max(data)
    pk <- as.vector(data >= (maxVal - gap))
    #browser()
    # put FALSE at each end
    rpk <- rle(pk)
    # find the maximum plateau width
    TT <- rpk$values
    longest <- which.max(rpk$lengths * as.numeric(rpk$values))
    mxPkLen <- rpk$lengths[longest]
    # start of the run
    cc <- cumsum(rpk$lengths)
    if (longest > 1) {
    	start <- cc[longest - 1] + 1
    } else {
      start <- 1
    }
    last <- start + mxPkLen - 1
    maxVals[j] <- maxVal
    Start[j] <- start
    End[j] <- last
    Dur[j] <- mxPkLen 
    Middle[j] <- (start + last)/2
    Prop[j] <- mxPkLen/length(pk)
    PropStart[j] <- Middle[j]/length(pk)
  }
  rbind(MaxVal=maxVals, Start=Start, End=End, Duration=Dur, Middle=Middle, PropOfTotal=Prop, PropPeakPos=PropStart)
}