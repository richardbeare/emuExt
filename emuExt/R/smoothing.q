#############################################################################
#                                                                           #
#   copyright            : (C) 2000 SHLRC, Macquarie University             #
#   email                : Steve.Cassidy@mq.edu.au			    #
#   url			 : http://www.shlrc.mq.edu.au/emu		    #
#									    #
#   This program is free software; you can redistribute it and/or modify    #
#   it under the terms of the GNU General Public License as published by    #
#   the Free Software Foundation; either version 2 of the License, or       #
#   (at your option) any later version.                                     #
#									    #
#############################################################################

## Written by Richard Beare for Marija Tabain,
## August 2002.
## "dataset" is EMU track data. 
## This is a smoothing function, written to
## smooth EMA data more effectively than dsmooth() does. 
## lowess()is based on a regression, and data are smoothed over a fraction 
## of the data length (e.g. 1/2) rather than over a fixed width (e.g. 50 ms). 
## The default value for the window width of lowess() is 2/3 
## (i.e. 2/3 of the length of the data sample). 
## However, MT found that this was too large a window for EMA data, 
## and has made the default 1/3. Experimenting with the window width 
## is recommended in order to find the best value for the data being
## examined (can be used to apply to any EMU track data). 

lowess.sub <- function(data, ftime, window)
  {
    a <- data
    for (i in 1:dim(data)[2]) {
      b<-data[,i]
      a[,i]<-lowess(1:length(b), b, f=window)$y
    }
    list(data=a, ftime=ftime)
  }

dlowess <- function(dataset, window=1/3)
  {
    dapply(dataset, lowess.sub, window)
  }

dloess <- function(dataset, window=1/3)
  {
    loess.sub <- function(data, ftime, window)
      {
        a <- data
        for (i in 1:dim(data)[2]) {
          bb <- data.frame(x=1:nrow(a), y=as.vector(data[,i]))
          sm <- loess(y~x, bb, span = window)
          a[, i] <- predict(sm, bb)
        }
        list(data = a, ftime = ftime)

      }

    dapply(dataset, loess.sub, window)
  }

## this converts the data produced by a track query to numeric if required
m.emu.track <- function(...)
  {
    ff <- emu.track(...)
    if (storage.mode(ff)=="character") {
      dd <- dim(ff)
      ff <- as.numeric(ff)
      dim(ff) <- dd
    } else {
      if (storage.mode(ff$data)=="character") {
        dd <- dim(ff$data)
        ff$data <- as.numeric(ff$data)
        dim(ff$data) <- dd
      }
    }
    ff
  }

