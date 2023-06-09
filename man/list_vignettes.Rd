% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vignette.R
\name{list_vignettes}
\alias{list_vignettes}
\title{list_vignettes}
\usage{
list_vignettes(package)
}
\arguments{
\item{package}{The package you wish to check the vignettes for (string).}
}
\value{
A character vector of available vignettes
}
\description{
This function will list vignettes available for a package in your notebook/console.
}
\examples{

# Load the library to ensure example data sets are available
library(gcptools)

# calling function
list_vignettes("readr")
}
