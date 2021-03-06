% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sink_once.R
\name{sink_once}
\alias{sink_once}
\title{Sink the Output of a Function Once}
\usage{
sink_once(f, file, append = FALSE, type = "output", split = FALSE)
}
\arguments{
\item{f}{a function}

\item{file}{a writable \link[base]{connection} or a character string naming the
    file to write to, or \code{NULL} to stop sink-ing.}

\item{append}{logical.  If \code{TRUE}, output will be appended to
    \code{file}; otherwise, it will overwrite the contents of
    \code{file}.}

\item{type}{either "output", "message", or a vector containing both, they can
be abbreviated}

\item{split}{logical: if \code{TRUE}, output will be sent to the new
    sink and to the current output stream, like the Unix program \code{tee}.}
}
\value{
\code{invisible()}
}
\description{
Works a lot like \code{base::sink()} except that it's applied on the output of a specific
function (only once). There is no need to call \code{sink()} a second time to end
the output diversion. The other difference is that one can feed
\code{type = c("output", "message")} rather than choose one of them or call \code{sink()}
for each one.
}
\examples{
greet <- function(x,y){
  message(x)
  print(y)
}

file <- tempfile()
sink_once(greet, file, type = c("o","m"))

# output is diverted
greet("Hi", "What's up?")

# let's read it back
readLines(file)

# further calls work normally
greet("Hi", "What's up?")
}
