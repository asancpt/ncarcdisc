% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Round.R
\name{Round}
\alias{Round}
\title{Round Half Away from Zero}
\usage{
Round(x, n = 0)
}
\arguments{
\item{x}{numeric to be rounded}

\item{n}{indicating decimal digits}
}
\value{
ordinarily rounded value
}
\description{
This is an ordinary rounding function, so called round half away from zero
}
\details{
The function \code{round} in R base rounds to the even number, i.e.
\code{round(0.5)} is 0 not 1. If you want rounding 0.5 be 1, you can use
this \code{Round} function. This function is for the consistency with other
software like MS-Excel, SAS.
}
\examples{
(x = 1:10 - 0.5)
Round(x)
round(x) # compare with the above
}
\references{
See wikipedia subject "Rounding"
}
\keyword{round}
\keyword{rounding}

