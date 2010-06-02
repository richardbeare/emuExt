function (tracksx, tracksy, labels = NULL, xlab = "", ylab = "", 
    main = "", legn = "tl", velMarkers = TRUE, markStart = TRUE, 
    palaisX = NULL, palaisY = NULL) 
{
    if ((class(tracksx) != "trackdata") | (class(tracksy) != 
        "trackdata")) {
        stop("Requires two trackdata objects\n")
    }
    if (ncol(tracksx$data) != ncol(tracksy$data)) {
        stop("Number of columns in tracksx and tracksy must be the same\n")
    }
    if (ncol(tracksx$data) != ncol(tracksy$data)) {
        stop("Sample lengths must be identical\n")
    }
    NewFtime <- tracksx$ftime - tracksy$ftime[, 1]
    IdxCount <- tracksx$index[, 2] - tracksx$index[, 1] + 1
    StartI <- tracksx$index[, 1]
    EndI <- tracksx$index[, 2]
    tt <- !is.na(tracksx$data)
    if (is.null(palaisX)) {
        xrange <- range(tracksx$data[tt])
    }
    else {
        xrange <- range(c(tracksx$data[tt], palaisX))
    }
    tt <- !is.na(tracksy$data)
    if (is.null(palaisY)) {
        yrange <- range(tracksy$data[tt])
    }
    else {
        yrange <- range(c(tracksy$data[tt], palaisY))
    }
    NROWS <- nrow(tracksx$index)
    if (is.null(labels)) {
        ThisColor <- rep(1, NROWS)
    }
    else {
        ULabs <- unique(labels)
        ThisColor <- match(labels, ULabs)
    }
    plot(tracksx[1, 1]$data, tracksy[1, 1]$data, xlim = xrange, 
        ylim = yrange, xlab = xlab, ylab = ylab, main = main, 
        type = "n")
    if (!is.null(palaisX) & !is.null(palaisY)) {
        lines(palaisX, palaisY)
    }
    if (velMarkers) {
        plotSingle <- function(r, c) {
            ddx <- tracksx$data[StartI[r]:EndI[r], c]
            ddy <- tracksy$data[StartI[r]:EndI[r], c]
            lines(ddx, ddy, col = ThisColor[r])
            points(ddx, ddy, col = ThisColor[r], pch = ThisColor[r])
            if (markStart) {
                text(ddx[1], ddy[1], labels = "s", col = ThisColor[r], 
                  pos = 2)
            }
        }
    }
    else {
        plotSingle <- function(r, c) {
            ddx <- tracksx$data[StartI[r]:EndI[r], c]
            ddy <- tracksy$data[StartI[r]:EndI[r], c]
            lines(ddx, ddy, col = ThisColor[r])
            if (markStart) {
                text(ddx[1], ddy[1], labels = "s", col = ThisColor[r], 
                  pos = 2)
            }
        }
    }
    for (i in 1:ncol(tracksx$data)) {
        lapply(1:NROWS, plotSingle, i)
    }
    LegPos <- mu.legend(legn, xrange, yrange)
    if (!is.null(labels)) {
        if (velMarkers) {
            legend(LegPos$x, LegPos$y, ULabs, col = 1:length(ULabs), 
                pch = 1:length(ULabs), lty = 1)
        }
        else {
            legend(LegPos$x, LegPos$y, ULabs, col = 1:length(ULabs), 
                lty = 1)
        }
    }
}
