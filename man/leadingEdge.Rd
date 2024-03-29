% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/leadingEdge-methods.R
\name{leadingEdge}
\alias{leadingEdge}
\alias{leadingEdge,FgseaList-method}
\title{Leading edge genes}
\usage{
leadingEdge(object, ...)

\S4method{leadingEdge}{FgseaList}(object, contrast, collection, set)
}
\arguments{
\item{object}{Object.}

\item{contrast}{\code{character(1)}.
Contrast name.}

\item{collection}{\code{character(1)}.
Gene set collection name.
Typically refers to \code{h} (hallmark), \code{c1}-\code{c7} collections from MSigDb.
Can obtain using \code{collectionNames()} on \code{FgseaList} object.}

\item{set}{\code{character(1)}.
Gene set name, in a defined \code{collection}.
For example, \code{"HALLMARK_ADIPOGENESIS"}.}

\item{...}{Additional arguments.}
}
\value{
\code{character}.
Elements of gene set that correspond to leading edge.
}
\description{
Leading edge genes
}
\note{
Updated 2022-04-27.
}
\examples{
data(fgsea)

## FgseaList ====
object <- fgsea
contrast <- contrastNames(object)[[1L]]
collection <- collectionNames(object)[[1L]]
set <- geneSetNames(object = object, collection = collection)[[1L]]
leadingEdge(
    object = object,
    contrast = contrast,
    collection = collection,
    set = set
)
}
