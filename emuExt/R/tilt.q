## Program written by Richard Beare for Marija Tabain,
## August 2002.
## This program follows for the most part the spectral tilt analysis
## described in Smits et al. (1996) JASA.
## It analyzes the rate-of-change in spectral tilt across an utterance. 
## ERB-rate is used for frequency based on Moore & Glasberg's 1990 formula,
## and amplitude values are converted to dB.
## The input is an utterance list, and FFTs are calculated
## across each token,
## based on the specified window length and frame-shift.
## "bt" and "tp" define the low and high points of the
## bandpass filter respectively.
## The output is $primary for the raw tilt values from each window,
## and $secondary for the output of the secondary regression.
## The "lsweight=T" parameter weights the primary regression
## with the linear amplitude. This emphasizes the spectral peaks. 
## "lsweight=F" uses an unweighted regression.
## "plotduring" switches a display of primary regressions on and off.
## window and frameshift in milliseconds
## Note that there is no zero-padding. 

spectral.tilt <- function(dataset, lsweight=F, plotduring=T,
                          window=12, frameShift=5, bt=1000,
                          tp=5000, firstOnly=F, do.tilt=T, do.moments=T, use.erb=F)
  {
    sp.sub <- function(I, DS)
      {
        cat(I, "\n")
        res<-list()
        res.moment <- list()
        i<-1
        ftm <- DS$ftime[I,]
        idx <- DS$index[I,]
        data <- DS$data[idx[1]:idx[2],,drop=F]
        ## sampling frequency
        rng <- as.numeric(ftm[2] - ftm[1])
        freq<-1000*nrow(data)/rng

        ## points in windows
        winlen <- freq*window/1000
        shft <- freq*frameShift/1000
        ham <- hamming(winlen)
        sd <- 1:winlen
        start <- 0
        ## frequency start and end points
        fstart <- winlen*bt/freq
        fend <- winlen*tp/freq
        f<-((1:(winlen))*(freq/winlen))[fstart:fend]
        erb <- 21.4 * log10(f*0.00437 + 1)

        if (use.erb) {
          fff <- erb
        } else {
          fff <- f
        }
        fsqr <- fff^2
        ## only do the first window in each token
        ## naturally the secondary regression
        ## will be empty
        if (firstOnly) {
          ED <- 1
        } else {
          ED <- nrow(data)
        }
        while (start < ED) {
          if ((start + sd[winlen]) < nrow(data)) {
            D <- data[sd+start]*ham
            FT <- fft(D)
            FTlin<-Mod(FT[fstart:fend])
            FTdb<-20*log10(FTlin)

            if (do.tilt) {
              if (lsweight) {
                regr <- lsfit(erb, FTdb, wt=FTlin)
              } else {
                regr <- lsfit(erb, FTdb)
              }
              res[[i]] <- regr$coefficients[2]
            }
            
            if (do.moments) {
              FTdbn <- FTdb - min(FTdb)
              prodvals <- FTdbn * fff
              s1 <- sum(FTdbn)
              s2 <- sum(prodvals)
              first.moment <- s2/s1

              sqrprodvals <- fsqr * FTdbn
              second.moment <- sqrt(sum(sqrprodvals)/s1 - first.moment^2)
              res.moment[[i]] <- c(first.moment, second.moment)
##              plot(f, FTdbn, type='l')
            }
            if (plotduring) {
              plot(erb, FTdb, type='l')
              abline(regr$coefficients[1],
                     regr$coefficients[2],col='green')
            }
          }
          start <- start + shft
          i<-i+1
        }

        if (do.tilt) {
          res <- unlist(res)
        }
        if (do.moments) {
          res.moment <- unlist(res.moment)
          dim(res.moment) <- c(2, length(res.moment)/2)
        }
        list(res.tilt = res, res.moment=res.moment)
      }

    LSfit<-function(i, sec)
      {
        a<-sec[[i]]
        if (length(a) == 1)
          return(NA)
        rs<-lsfit(1:length(a), a)
        rs$coefficients[2]
      }
    
    indx <- 1:nrow(dataset$index)
    primary <- lapply(indx, sp.sub, dataset)
    ## now extract the data to individual lists

    res.tilt <- lapply(primary, function(x)x$res.tilt)
    if (do.moments)
      res.moment <- lapply(primary, function(x)x$res.moment)
    else
      res.moment <- list()

    if (do.tilt)
      secondary <- lapply(indx, LSfit, res.tilt)
    else
      secondary <- list()
    
    list(primary=res.tilt, secondary=unlist(secondary),
         moments=res.moment)
  }

spect.utter<-function(utt.list, lsweight=F, plotduring=TRUE,
                      window=12, frameShift=5, bt=1000,
                      tp=5000, firstOnly=F, do.tilt=T, do.moments=T,
                      use.erb=F)
  {
    samdat<-emu.track(utt.list, "samples")
    o<-spectral.tilt(dataset=samdat, lsweight=lsweight,
                     plotduring=plotduring,
                     window=window, frameShift=frameShift,
                     bt=bt, tp=tp, firstOnly=firstOnly,
                     do.tilt=do.tilt, do.moments=do.moments, use.erb=use.erb)
    PP <- unlist(o$moments)
    dim(PP) <- c(2,length(PP)/2)
    
    list(primary=o$primary, secondary=o$secondary,
         first.moments=PP[1,], second.moment=PP[2,])
  }
