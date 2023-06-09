% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/automated_disclosure_control.R
\name{check_name_pattern}
\alias{check_name_pattern}
\title{Check Name Pattern}
\usage{
check_name_pattern(pattern, config)
}
\arguments{
\item{pattern}{A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.}

\item{config}{A list config file to check file names against. This file contains information about expected file metadata.}
}
\description{
Takes a file name pattern to determine if it exists within config file.
}
