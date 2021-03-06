% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/carport_utilities.R
\name{cp_load_loadcells}
\alias{cp_load_loadcells}
\title{Loads carport load cell data into R environment}
\usage{
cp_load_loadcells(load_from)
}
\arguments{
\item{load_from}{Path to the local folder where the .rds file is stored.}
}
\value{
tibble with data loaded from the .rds file in the specified folder.
}
\description{
loads carport load cell data from local .rds file into R environment.
}
\details{
This function loads carport load cell data from the uncompressed .rds file
produced by `cp_tidy_loadcells()` into the R environment. Please see the
documentation of other function in the \emph{carport utility functions}
group for preprocessing the data.
}
\seealso{
Other carport utility functions: 
\code{\link{cp_pull_loadcells}()},
\code{\link{cp_read_events}()},
\code{\link{cp_tidy_loadcells}()}
}
\concept{carport utility functions}
