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
