## this probably isn't the most efficient way
## of attacking this problem, but it will probably do the job
f2prime.drv <- function(F)
  {
    f0 <- F[1]
    f2 <- F[2]
    f3 <- F[3]
    f4 <- F[4]

    f3mf2 <- f3 - f2
    f4mf3 <- f4 - f3
    f4mf2 <- f4 - f2

    find.weights <- function()
      {
        if (f3mf2 > 3.5) {
          return(list(w2=1, w3=0, w4=0))
        }

        if (f3mf2 > 2.5) {
          if (f4mf3 < 2) {
            if (f0 < 3) {
              return(list(w2=1, w3=0.6, w4=0.6))
            } else {
              return(list(w2=1, w3=0.5, w4=0.5))
            }
          } else {
            if (f0 < 3) {
              return(list(w2=1, w3=0.25*(3.5-f3mf2), w4=0))
            } else {
              return(list(w2=1, w3=0.1*(1-f3mf2), w4=0))
            }
          }
        } else {
          if (f4mf2 > 3.5) {
            if (f0 < 3) {
              return(list(w2=1, w3=0.5, w4=0))
            } else {
              return(list(w2=1, w3=0.1, w4=0))
            }
          } else {
            if (f4mf3 > f3mf2) {
              if (f0 < 3) {
                return(list(w2=1, w3=0.5, w4=0))
              } else {
                return(list(w2=1, w3=0.4, w4=0))
              }
            } else {
              if (f0 < 3) {
                return(list(w2=0, w3=1.5, w4=1))
              } else {
                return(list(w2=0, w3=1, w4=0.5))
              }
            }
          }
        }        
      }

    wts <- find.weights()
    
    (wts$w2*f2 + wts$w3*f3 + wts$w4*f4)/(wts$w2 + wts$w3 + wts$w4)
  }

## this function will accept any combination of vectors
## and matrices, provided there are 4 columns in total.
## The order of the columns is assumed to be f0, f2, f3, f4.
f2prime <- function(f0, f2=NULL, f3=NULL, f4=NULL)
  {
    all <- cbind(f0, f2, f3, f4)

    if (ncol(all) != 4) {
      stop("4 input columns needed\n")
    }
    res <- apply(all, MARGIN=1, f2prime.drv)
    res
  }
