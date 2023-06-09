% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/automated_disclosure_control.R
\name{check_number_of_charts}
\alias{check_number_of_charts}
\title{Check Number of Charts}
\usage{
check_number_of_charts(file, pattern, config)
}
\arguments{
\item{file}{An .html file to extract number of charts from.}

\item{pattern}{A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.}

\item{config}{A list config file to check number of charts against. This file contains information about expected file metadata.}
}
\description{
Checking number of charts in a .html document match as expected.
}
