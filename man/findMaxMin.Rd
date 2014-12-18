\name{findMaxMin}
\alias{findMaxMin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automated and interactive selection of max/min velocities in XY plane}
\description{
  This function iterates through an array of tokens, displaying each one
  in turn. The automatically detected max and min velocity points are
  displayed and the user is given the opportunity to change them.
}
\usage{
findMaxMin(xdata, ydata, doplot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdata}{The X position data (emu object)}
  \item{ydata}{The Y position data (emu object)}
  \item{doplot}{Should the plots be displayed}
}

\details{
When the \code{doPlot} option is TRUE the plot is displayed and the user
is given the opportunity to correct the automatic detection.

The procedure is as follows:

The user is prompted with the following message:

"Press m to modify, enter to continue"

If "m" is pressed then \code{identify} is used to extract positions.
The user should first click on the position of the maximum and then on
the position of the minimum.

The colour/symbols used are as follows:

Blue X's mark the token start.

Red o's mark the predicted position of the velocity maximum.

Red triangles the predicted position of the velocity minimum.
}

\value{
  A list with 2 components
  \item{max}{An array containing time, index, velocity and x and y
    positions of the maxima}
  \item{min}{An array containing time, index, velocity and x and y
    positions of the minima}
}

\author{Richard Beare}


\seealso{\code{\link{identify}}}
\keyword{ misc }% at least one, from doc/KEYWORDS

