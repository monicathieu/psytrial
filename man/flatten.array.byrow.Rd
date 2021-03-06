% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-replicate.R
\name{flatten.array.byrow}
\alias{flatten.array.byrow}
\title{internal
flattens an array of vectors row-wise
designed for use inside an \code{lapply} call vectorized
along columns of an array}
\usage{
flatten.array.byrow(x, a, colnames)
}
\arguments{
\item{x}{index of column to be flattened}

\item{a}{array being flattened}

\item{colnames}{names of columns of each output dataframe}
}
\value{
a list of dataframes, where each list element was a
column of the original array, and each column in each dataframe
was a vector in that column
}
\description{
internal
flattens an array of vectors row-wise
designed for use inside an \code{lapply} call vectorized
along columns of an array
}
