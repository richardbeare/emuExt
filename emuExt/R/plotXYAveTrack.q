function (tracksx, tracksy, labels = stop("Labels must be present for averaging"), 
    xlab = "", ylab = "", main = "", legn = "tl", velMarkers = TRUE, 
    markStart = TRUE, markEnd = TRUE, markMid = TRUE, samples = 20, normlength = 100, palaisX = NULL, 
    palaisY = NULL) 
{
    aveX <- aveTracks(tracksx, labels = labels, samples = samples, 
        normlength = normlength)
    aveY <- aveTracks(tracksy, labels = labels, samples = samples, 
        normlength = normlength)
    if (!is.null(palaisX) & !is.null(palaisY)) {
        palaisX <- lowess(palaisX, f = 1/10)$y
        palaisY <- lowess(palaisY, f = 1/10)$y
    }
    plotXYTrack(aveX$track, aveY$track, aveX$labels, xlab = xlab, 
                ylab = ylab, main = main, velMarkers = velMarkers, markStart = markStart,
                markEnd = markEnd, markMid=markMid,
                legn = legn, palaisX = palaisX, palaisY = palaisY)
}
