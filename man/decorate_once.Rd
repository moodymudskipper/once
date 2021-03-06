% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decorate_once.R
\name{decorate_once}
\alias{decorate_once}
\title{Apply a function operator to a function for one call only}
\usage{
decorate_once(f, decorator, ...)
}
\arguments{
\item{f}{a function}

\item{decorator}{a function operator}

\item{...}{additional argument passed to decorator}
}
\description{
Apply a function operator to a function for one call only
}
\examples{
decorate_once(is.logical, Negate)
is.logical(TRUE)
is.logical(TRUE)
}
