% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/alphaThreshold-methods.R
\name{alphaThreshold}
\alias{alphaThreshold}
\alias{alphaThreshold<-}
\alias{alphaThreshold,FgseaList-method}
\alias{alphaThreshold<-,FgseaList,numeric-method}
\title{Alpha threshold}
\usage{
alphaThreshold(object, ...)
alphaThreshold(object, ...) <- value

\S4method{alphaThreshold}{FgseaList}(object)

\S4method{alphaThreshold}{FgseaList,numeric}(object) <- value
}
\arguments{
\item{object}{Object.}

\item{value}{Value to assign.}

\item{...}{Additional arguments.}
}
\value{
\code{numeric(1)}.
Alpha cutoff level.
}
\description{
Alpha threshold
}
\note{
Updated 2022-04-27.
}
\examples{
data(fgsea)

## FgseaList ====
object <- fgsea
alphaThreshold(object) <- 0.1
alphaThreshold(object)
}
