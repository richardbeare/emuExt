## if this.lab is provided the function will do the subsetting for you
## plots spectra in successive windows, averaged across all tokens
## the number of time steps is that of the minimum token length

plot.running.spectra <- function(data, labs, this.lab=NULL, window=12, frameshift=5, bt=1000, tp=5000)
  {
    if (!is.null(this.lab)) {
      kp <- labs == this.lab
      data <- data[kp,]
      labs <- labs[kp]
    }

    if (length(unique(labs)) != 1) {
      stop("multiple labels provided")
    }

    ## figure out the sampling frequency and other details
    details <- trackdata.details(data, window, bt, tp)
    ftime <- details$ftime
    
    sampfreq <- details$sampfreq
    
    window.samples <- details$window.samples
    ## bt and tp are in Hz, so we need to figure out fstart and fend
    fstart <- details$fstart
    fend <- details$fend
    f <- details$f
    
    ham <- hamming(window.samples)
    
    res <- NULL

    ## support functions
    doFt <- function(r, dat)
      {
#        browser()
        ss <- dat[r,]$data
        ## number of iterations

        half.windows <- floor(length(ss)/(window.samples/2))
        ft.count <- half.windows - 1

        res <- NULL
        first <- 1
        for (i in 1:ft.count) {
          last <- floor(first+window.samples)
          sound <- ss[first:last] * ham
          FT <- fft(sound)
          FTlin <- Mod(FT[fstart:fend])
          first <- first + floor(window.samples/2)

          res <- cbind(res, FTlin)
        }
        return(res)
      }

    signals <- lapply(1:nrow(data), doFt, data)

    win.count <- sapply(signals, ncol)
    maxcols <- max(win.count)
    maxrows <- nrow(signals[[1]])

    ## turn it into a big array object
    default.array <- array(data=NA, dim=c(maxrows, maxcols, length(signals)))

    for (I in 1:length(signals)) {
      ss <- signals[[I]]
      default.slice <- array(data=NA, dim=c(maxrows, maxcols))
      default.slice[1:length(ss)] <- ss
      default.array[,,I] <- default.slice
    }
    cc <- apply(default.array, MARGIN=c(1,2), mean, na.rm=T)
    #require(lattice)
    cloud(20*log10(cc), xlab="freq", ylab="window number", zlab="Mag(dB)",
              scales=list(x=f, y=1:maxcols, arrows=F), type='l')
    return(cc)
    
  }
