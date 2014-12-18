tokenMeans <- function(df, tokenColName, measureColNames)
{
# call is something like : tokenMeans(df, "utts", c("ai", "ci"))

  all.tokens <- df[, tokenColName]

  # make everything lower case, replace the trailing _digit and digit combinations.
  # removing all digits then trailing underscores should do
  t1 <- toupper(gsub("_*[[:digit:]]+","" ,all.tokens))
  
  jj <- table(t1)

  rr <- NULL
  for (I in measureColNames) {
    rr <- cbind(rr, tapply(df[, I], t1, mean))

  }
  rr <- cbind(rr, jj)
  colnames(rr) <- c(measureColNames, "repeats")
  return(rr)
}