

plot.spectra <- function(data, labels, window, bt=1, tp=NA, average=FALSE, erb=FALSE, doColor=TRUE, doLty=TRUE, labLty=NULL, labColors=NULL, xlab="Frequency(Hz)", ylab="Magnitude (dB)", ... )
{

  ## figure out the sampling frequency
  details <- trackdata.details(data, window, bt, tp)
  ftime <- details$ftime

  sampfreq <- details$sampfreq
  
  window.samples <- details$window.samples
  # bt and tp are in Hz, so we need to figure out fstart and fend
  fstart <- details$fstart
  fend <- details$fend
  f <- details$f

  ham <- hamming(window.samples)


  ## support functions
  doFt <- function(r, dat)
  {
     ss <- dat[r,]$data
     if (length(ss) < window.samples) stop("Window is too long")
     sound <- ss[1:length(ham)] * ham
     FT <- fft(sound)
     FTlin <- Mod(FT[fstart:fend])

  }


  un <- unique(labels)

  signals <- list()

  for (I in 1:length(un)) {
    keep <- labels == un[I]
    dd <- data[keep,]
    signals[[I]] <- sapply(1:nrow(dd), doFt, dd)
  }

  if (average) {

     avesignals <- NULL

     for (I in 1:length(signals)) {
        ss <- apply(signals[[I]], MARGIN=1, mean)
        avesignals <- cbind(avesignals, ss)
     }

     if (is.null(labColors)) {
       labColors <- 1:length(un)
     }
     if (!doColor) {
       labColors <- rep(1, length(un))
     }

     if (is.null(labLty)) {
       labLty <- 1:length(un)
     }
     if (!doLty) {
       labLty <- rep(1, length(un))
     }
     
     matplot(f, 20*log10(avesignals), type='l', lty=labLty, col=labColors, xlab=xlab, ylab=ylab, ...)
     legend(x="topright", legend=un, lty=labLty, col=labColors)
     
  } else {
	combsignals <- NULL
        colors <- NULL
        lty <- NULL
	for (I in 1:length(signals)) {
           combsignals <- cbind(combsignals, signals[[I]])
           lty <- c(lty, rep(I, ncol(signals[[I]])))
           colors <- c(colors, rep(I, ncol(signals[[I]])))
	}

        
     matplot(f, 20*log10(combsignals), type='l', lty=lty, col=colors, xlab=xlab, ylab=ylab, ...)
  }

}
