eplot <- function (data, labs, chars, formant = F, scaling = "linear",
                   prob = 0.95, nsdev = NULL, dopoints = F, doellipse
= T, centroid = F,
                   main = "", xlab = "", ylab = "", axes = T, xlim,
ylim, colour = T,
                   linetype = F, boundary = F, ...)
{
  ocall <- match.call()
  if (is.null(nsdev))
    nsdev <- sqrt(qchisq(prob, 2))
  bplot <- function(data, labs, N = 10, xlim, ylim, metric = "bayes") {
    tdat <- train(data, labs)
    points <- cbind(sort(rep(1:N/N, N)), rep(1:N/N, N))
    points[, 1] <- points[, 1] * (xlim[2] - xlim[1]) + xlim[1]
    points[, 2] <- points[, 2] * (ylim[2] - ylim[1]) + ylim[1]
    blabs <- classify(points, tdat, metric = metric)
    ulabs <- sort(unique(labs))
    k <- 1
    colours <- mu.colour(ulabs, T, F)$colour
    for (j in ulabs) {
      temp <- muclass(blabs, j)
      text(points[temp, ], blabs[temp], col = colours[k])
      k <- k + 1
    }
  }
  if (missing(labs))
    labs <- rep(".", nrow(data))
  if (!doellipse & !dopoints)
    centroid <- T
  if (nrow(data) != length(labs))
    stop("Data and labels don't match")
  if (ncol(data) != 2)
    stop("Eplot needs 2 dimensional data")
  if (!missing(chars))
    if (length(labs) != length(chars))
      stop("Length of chars must match that of labs")
  if (scaling == "mel")
    data <- mel(data)
  if (scaling == "bark")
    data <- bark(data)
  if (formant) {
    data <- cbind(-data[, 2], -data[, 1])
    if (!missing(xlim))
      xlim <- -rev(xlim)
    if (!missing(ylim))
      ylim <- -rev(ylim)
  }
  col.lty <- mu.colour(labs, colour, linetype)

  lty <- col.lty$linetype
  uniqlabels <- sort(unique(labs))
  emat <- nums <- cen <- k <- l <- NULL
  for (j in uniqlabels) {
    temp <- labs == j
    mat <- data[temp, , drop = F]
    if (nrow(mat) > 2) {
      evals <- eigen(var(mat))
      m1 <- mean(mat[, 1])
      m2 <- mean(mat[, 2])
      e <- ellipse(m1, m2, sqrt(evals$values[1]) * nsdev,
                   sqrt(evals$values[2]) * nsdev, aperm(evals$vectors,
                                                        c(2, 1)))
    }
    else {
      cat("Too few data points for label ", j, " will plot a point or a line\n")
      m1 <- mean(mat[, 1])
      m2 <- mean(mat[, 2])
      e <- mat
    }
    nums <- c(nums, nrow(e))
    emat <- rbind(emat, e)
    k <- c(k, col.lty$legend$col[match(j, col.lty$legend$lab)])
    l <- c(l, col.lty$legend$lty[match(j, col.lty$legend$lab)])
    if (centroid)
      cen <- rbind(cen, cbind(m1, m2))
  }
  if (doellipse) {
    if (missing(xlim))
      xlim <- range(c(emat[, 1], data[, 1]))
    if (missing(ylim))
      ylim <- range(c(emat[, 2], data[, 2]))
  }
  else {
    if (missing(xlim))
      xlim <- range(data[, 1])
    if (missing(ylim))
      ylim <- range(data[, 2])
  }
  rightlim <- cumsum(nums)
  leftlim <- cumsum(nums) - (nums - 1)
  rowmarker <- cbind(leftlim, rightlim)
  for (j in 1:nrow(rowmarker)) {
    lowerlim <- rowmarker[j, 1]
    upperlim <- rowmarker[j, 2]
    if (doellipse) {
      plot(emat[lowerlim:upperlim, ], type = "l", axes = F,
           xlim = xlim, ylim = ylim, col = k[j], xlab = "",
           ylab = "", lty = as.numeric(l[j]))
    }
    else {
      plot(emat[lowerlim:upperlim, ], type = "n", axes = F,
           xlim = xlim, ylim = ylim, col = k[j], xlab = "",
           ylab = "", lty = as.numeric(l[j]))
    }
    if (dopoints) {
      centroid <- F
      singlelab <- uniqlabels[j]
      temp <- labs == singlelab
      if (!missing(chars))
        text(data[temp, 1], data[temp, 2], chars[temp],
             col = k[j])
      else text(data[temp, 1], data[temp, 2], labs[temp],
                col = k[j])
    }
    if (centroid) {
      singlelab <- uniqlabels[j]
      text(cen[j, 1], cen[j, 2], singlelab, col = k[j])
       }
    if (j < nrow(rowmarker))
      par(new = T)
  }
  par(col = 1)
  box()
  if (axes) {
    if (formant) {
      if (missing(xlab))
        xlab <- "F2"
      if (missing(ylab))
        ylab <- "F1"
      xaxp <- par("xaxp")
      yaxp <- par("yaxp")
      xat <- seq(xaxp[1], xaxp[2], length.out = xaxp[3] +
                 1)
      yat <- seq(yaxp[1], yaxp[2], length.out = yaxp[3] +
                 1)
      axis(1, at = xat, labels = -xat)
      axis(2, at = yat, labels = -yat, srt = 90)
    }
    else {
      axis(1)
      axis(2)
    }
   }
  title(main = main, xlab = xlab, ylab = ylab)
  if (boundary) {
    if (is.null(ocall$nboundary))
      ocall$nboundary <- 50
    if (is.null(ocall$metric))
      ocall$metric <- "bayes"
    bplot(data, labs, N = ocall$nboundary, xlim = xlim, ylim = ylim,
          metric = ocall$metric)
  }
}

mu.colour <- function (labs, col, linetype, defaultcolour = 1)
{
   len <- length(labs)
   ulen <- length(unique(labs))
   if (length(col) == 2 * ulen) {
       colours <- 1:len
       n <- 1
       while (n <= ulen) {
           lab <- col[2 * n - 1]
           colour <- col[2 * n]
           colours[labs == lab] <- as.numeric(colour)
           n <- n + 1
       }
       col <- colours
   }
   else if (is.numeric(col)) {
       if (length(col) == 1)
           col <- rep(col, len)
       else {
           if (length(col) != length(labs))
               stop("length of colour vector differs from that of labels")
       }
   }
   else if (!is.logical(col)) {
       col <- 1:length(unique(col))
       if (length(col) != length(labs))
           stop("length of colour vector differs from that of labels")
   }
   else {
       if (col) {
           colours <- 1:ulen
           col <- labs
           ulabels <- sort(unique(labs))
           for (i in 1:len) {
               col[i] <- colours[ulabels == labs[i]]
           }
       }
       else {
           col <- rep(defaultcolour, len)
       }
   }
   if (!is.logical(linetype)) {
       if (length(linetype) == 1)
           linetype <- rep(linetype, len)
       else linetype <- 1:length(unique(linetype))
       if (length(linetype) != length(labs))
           stop("length of linetype vector differs from that of labels")
   }
   else {
       if (linetype) {
           lty <- 1:ulen
           linetype <- labs
           ulabels <- sort(unique(labs))
           for (i in 1:len) {
               linetype[i] <- lty[ulabels == labs[i]]
           }
       }
       else {
           linetype <- rep(1, len)
       }
   }
   foo <- NULL
   foo$colour <- col
   foo$linetype <- linetype
   p1 <- paste(labs, col, linetype)
   p1.temp <- duplicated(p1)
   foo$legend$lab <- labs[!p1.temp]
   foo$legend$col <- foo$colour[!p1.temp]
   foo$legend$lty <- foo$linetype[!p1.temp]
   return(foo)
}

