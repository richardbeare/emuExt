trackdata.details <- function(data, window, bt, tp=NA)
  {
    # figure out the sampling frequency and other details
    # of a trackdata object
    ftime <- data[1,]$ftime
    sampfreq <- (nrow(data[1,]$data) - 1)/((ftime[2] - ftime[1])/1000)
    
    if (is.na(tp)) {
      tp <- sampfreq/2
    }
    window.samples <- window/1000*sampfreq
    ## bt and tp are in Hz, so we need to figure out fstart and fend
    fstart <- window.samples * bt/sampfreq
    fend <- window.samples * tp/sampfreq  
    
    f <- ((1:(window.samples )) * (sampfreq/window.samples))[fstart:fend]
    return(list(sampfreq=sampfreq, window.samples=window.samples,
                fstart=fstart, fend=fend, f=f))
 }
