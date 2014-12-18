# posthoc test

marijas.posthoc <- function(labs, measure, levene=FALSE)
  {
    mm <- as.factor(labs)
    nlv <- nlevels(mm)
    levs <- levels(mm)

    # break into groups
    gg <- tapply(measure, labs, function(X){X})
    if (levene) {
      adiff <- lapply(gg, function(X){abs(X - mean(X))})
    } else {
      adiff <- gg
    }
    for (m in 1:(nlv-1)) {
      for (n in (m+1):nlv) {

        teetest <- t.test(adiff[[m]], adiff[[n]], paired=F)
        cat(paste("Labels: ", levs[m], levs[n]))
        print(teetest)
      }
    }
  }

# fixed levene.test from package "car" - mean used instead of median

marijas.levene.test <- function (y, group, ...) 
{
    if (!is.numeric(y)) 
        stop(deparse(substitute(y)), " is not a numeric variable")
    if (!is.factor(group)) {
        warning(deparse(substitute(group)), " coerced to factor.")
        group <- as.factor(group)
    }
    meds <- tapply(y, group, mean, na.rm = TRUE)
    resp <- abs(y - meds[group])
    table <- anova(lm(resp ~ group))[, c(1, 4, 5)]
    rownames(table)[2] <- " "
    attr(table, "heading") <- "Levene's Test for Homogeneity of Variance"
    table
}

# marijas.eta takes the result of lm() as input

marijas.eta <- function(anv){
return(anv$"Sum Sq"[1]/sum(anv$"Sum Sq"))
}


