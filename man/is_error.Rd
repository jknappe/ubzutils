% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{is_error}
\alias{is_error}
\title{Check if an expression returns an error}
\usage{
is_error(expr, tell = FALSE, force = FALSE)
}
\arguments{
\item{expr}{Expression to be tested for returning an error}

\item{tell}{Logical: Should the error message be printed via \code{\link{message}}? DEFAULT: FALSE}

\item{force}{Logical: Should an error be returned if the expression is not an error? DEFAULT: FALSE}
}
\value{
TRUE/FALSE
}
\description{
Does a given expression return an error?
}
\details{
Useful for tests where you want to make sure your function throws an error.
This function was imported from the berryFunctions package.
}
\examples{
is_error(  log(3)              )
is_error(  log("a")            )
is_error(  log(3),   tell=TRUE )
is_error(  log("a"), tell=TRUE )
stopifnot( is_error( log("a")  )  ) # or shorter:
is_error(  log("a"), force=TRUE)
# is_error(  log(3),   force=TRUE)
stopifnot(is_error(  is_error(log(3), force=TRUE)  ))

}
\seealso{
\code{\link{stop}}, \code{\link{try}}, \code{\link{inherits}}

Other helper functions: 
\code{\link{key_valid}()}
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, May 2016
}
\concept{helper functions}
