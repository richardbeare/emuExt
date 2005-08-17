findMaxMin <- function(xdata, ydata, doplot=TRUE)
 {
    tokenCount <- nrow(xdata$index)
    res <- NULL

    if (any(xdata$index != ydata$index))
       stop("Signals don't match\n")

    for (i in 1:tokenCount) {
       posX <- xdata[i,]$data
       posY <- ydata[i,]$data
       sensors <- 1:ncol(posX)
       vel <- sqrt(diff(posX)^2 + diff(posY)^2)
       sampTime <- xdata[i,]$ftime
       timesPos <- seq(sampTime[1], sampTime[2], length=nrow(posX))
       timesVel <- timesPos[-1]
       velMax <- apply(vel, MARGIN=2, which.max)
       velMin <- apply(vel, MARGIN=2, which.min)
       positions <- c(velMax, velMin)
       ftimes <- timesVel[c(velMax, velMin)]
       velMax1 <- cbind(velMax, sensors)
       velMin1 <- cbind(velMin, sensors)
       posMax1 <- cbind(velMax, sensors)
       posMin1 <- cbind(velMin, sensors)

       velocity <-  c(vel[velMax1], vel[velMin1])
       xposition <- c(posX[posMax1], posX[posMin1])
       yposition <- c(posY[posMax1], posY[posMin1])
       aa <- cbind(ftimes=ftimes, positions=positions, velocity=velocity, 
                   xposition=xposition, yposition=yposition)
       if (doplot) {
          rrx <- range(posX)
          rry <- range(posY)
          plot(posX[,1], posY[,1], ylim=rry, xlim=rrx, type='n', main=paste("Token number", i))
                    
          for (k in sensors) {
            lines(posX[,k], posY[,k], col=k)
            points(posX[,k], posY[,k], col=k) 
            text(posX[1,k], posY[1,k], labels="X", col='blue', offset=1)                                   
            points(posX[velMax[k],k], posY[velMax[k], k], col='red', cex=3)
            points(posX[velMin[k],k], posY[velMin[k], k], pch=2, col='red', cex=3)
          }
          cat("Press m to modify, enter to continue\n")
          ans <- scan(quiet=TRUE,what="", n=1)
          if (length(ans) > 0)
            if (ans == "M" | ans == "m") {
              cat("Modifying - left click on the max and min velocity points\n")
              flush.console()
              positions <- identify(posX[,1], posY[,1], n=2)
              ftimes  <- timesVel[positions]
              velMax1 <- cbind(velMax, sensors)
              velMin1 <- cbind(velMin, sensors)
              posMax1 <- cbind(velMax, sensors)
              posMin1 <- cbind(velMin, sensors)

              velocity  <-  c(vel[velMax1], vel[velMin1])
              xposition <- c(posX[posMax1], posX[posMin1])
              yposition <- c(posY[posMax1], posY[posMin1])
              aa  <- cbind(ftimes=ftimes, positions=positions, velocity=velocity, 
                           xposition=xposition, yposition=yposition)
              
            }
       }
       res <- rbind(res, aa)
    }
    rownames(res) <- rep(c("max", "min"), nrow(res)/2)
    nn <- 0:(tokenCount - 1) * 2 + 1
    rmx <- res[nn,]
    rmn <- res[nn+1,]
    list(max=rmx, min=rmn)
 }

findMaxMinOld <- function(signals, doplot=TRUE)
 {
    tokenCount <- nrow(signals$index)
    res <- list()
  
    for (i in 1:tokenCount) {
       this <- signals[i,]
       pos <- this$data
       sensors <- 1:ncol(pos)
       vel <- abs(diff(pos))
       sampTime <- this$ftime
       timesPos <- seq(sampTime[1], sampTime[2], length=nrow(pos))
       timesVel <- timesPos[-1]
       posMax <- apply(pos, MARGIN=2, which.max)
       posMin <- apply(pos, MARGIN=2, which.min)
       velMax <- apply(vel, MARGIN=2, which.max)
       velMin <- apply(vel, MARGIN=2, which.min)
       positions <- c(posMax, posMin, velMax, velMin)
       ftimes <- c(timesPos[c(posMax, posMin)], timesVel[c(velMax, velMin)])
       posMax1 <- cbind(posMax, sensors)
       posMin1 <- cbind(posMin, sensors)
       velMax1 <- cbind(velMax, sensors)
       velMin1 <- cbind(velMin, sensors)

       values <- c(pos[posMax1], pos[posMin1], vel[velMax1], vel[velMin1])
       res[[i]] <- cbind(ftimes=ftimes, positions=positions, values=values)
       if (doplot) {
          rr <- range(pos)
          plot(pos[,1], ylim=rr, type='n')
          for (k in sensors) {
            lines(pos[,k], col=k)
            points(posMax[k], pos[posMax[k], k])
            points(posMin[k], pos[posMin[k], k], pch=2)
            points(velMax[k], pos[velMax[k], k], col='red')
            points(velMin[k], pos[velMin[k], k], pch=2, col='red')
          }
          cat("Press enter to continue\n")
          scan(quiet=TRUE)
       }
    }
    res
 }

     
