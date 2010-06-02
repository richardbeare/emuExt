


epgaiRB <- function (epgdata, rows=8) 
{
    if (!inherits(epgdata, "EPG")) 
        p <- palate(epgdata)
    else p <- epgdata
    if (length(dim(p)) == 2) {
        p <- array(p, c(8, 8, 1))
        class(p) <- "EPG"
    }
    weights <- 9^(0:(rows-1))
## 0.75 is ratio of 6/8 - contacts in front row/contacts in other rows
    weights[rows] <- round(weights[rows]*0.75) 
    print(weights)
    N <- dim(p)[3]
    o <- epgsum(p, 1, rows = rows:1)
    w <- matrix(weights, nrow = N, ncol = rows, byrow = T)
    divisor <- matrix(c(rep(8, rows-1), 6), nrow = N, ncol = rows, byrow = T)
    num <- log(apply(w * o/divisor, 1, sum) + 1)
    den <- log(sum(weights) + 1)
    result <- cbind(num/den)
    if (is.trackdata(epgdata)) {
        epgdata$data <- result
        epgdata$trackname <- "anteriority"
    }
    else epgdata <- result
    epgdata
}