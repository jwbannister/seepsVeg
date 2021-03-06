% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_functions.R
\name{load_dbfs}
\alias{load_dbfs}
\title{Read and concatenate multiple DBF files.}
\usage{
load_dbfs(files = NULL)
}
\description{
\code{load_dbfs} loads a multiple DBF (dBase) files and joins them on a
 common ID variable into a dataframe object.
}
\details{
@param file Vector of character strings. DBF files to be read,
   including path.
 @return Dataframe with three columns: point ID, x coordinate,
   and y coordinate
}

