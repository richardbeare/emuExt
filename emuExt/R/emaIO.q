readEmu <- function(filename)
  {
    ## function to read files in the format used by Emu.
    ## not completely sure about the header, but this is a first
    ## guess

    ## deal with the header - header is terminated with a line of "----"
    EndHeader <- "-----------------"
    header <- NULL
    inHeader <- TRUE
    con <- file(filename, open="rb")
    while (inHeader) {
      ll <- readLines(con=con, n=1)
      if (length(grep(pattern="------", ll))) {
        inHeader <- FALSE
      } else {
        header <- c(header, ll)
      }
    }
    ## process the header - we are looking for Machine, Start_Time, Record_Freq
    ## and Column tags

    filtText <- function(txt, tag)
      {
        where <- grep(pattern=tag, txt)
        tt <- txt[where]
        tt <- gsub(pattern=tag, replacement="", tt)
        tt <- gsub(pattern="^ ", replacement="", tt)
        tt
      }

    res <- list()
    res$Machine <- filtText(header, "Machine")
    res$StartTime <- as.numeric(filtText(header, "Start_Time"))
    res$RecordFreq <- as.numeric(filtText(header, "Record_Freq"))

    Columns <- filtText(header, "Column")
    ## column details are recorded as triplets : name, type, number
    Columns <- strsplit(Columns, split="[[:space:]]")
    colnames <- sapply(Columns, function(X)X[1])
    coltypes <- sapply(Columns, function(X)X[2])
    colcount <- sapply(Columns, function(X)as.numeric(X[3]))
    res$ColNames <- colnames
    res$ColTypes <- coltypes
    res$ColCount <- colcount

    datatype <- unique(coltypes)
    if (length(datatype) != 1) {
      stop("Reading multiple types not supported")
    }

    datasize <- switch(datatype,
                       FLOAT=4,
                       other=0)
    sizeest <- file.info(filename)$size
    sizeest <- sizeest/datasize
    data <- readBin(con, what="numeric", n=sizeest, size=datasize)
    close(con)
    ## now massage the data into the correct form
    totalCols <- sum(res$ColCount)
    totalRows <- length(data)/totalCols
    dim(data) <- c(totalCols, totalRows)
    data <- t(data)
    attr(data, "fileheader") <- res
    data
  }
