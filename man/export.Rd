% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/export-methods.R
\name{export}
\alias{export}
\alias{export,FgseaList,character-method}
\title{Export data from R}
\usage{
export(object, con, ...)

\S4method{export}{FgseaList,character}(
  object,
  con,
  geneSetResults = FALSE,
  compress = FALSE,
  overwrite = TRUE,
  quiet = FALSE
)
}
\arguments{
\item{object}{Object.}

\item{con}{\code{character(1)}.
Directory path.}

\item{geneSetResults}{\code{logical(1)} or \code{character}.
Export per-gene set expression including log fold change values generated
from DESeq2. Can be slow when processing all MSigDB collections, so
disabled by default. Alternatively, can declare specific collections to
process (see \code{collectionNames} for supported values).}

\item{compress}{\code{logical(1)}.
Apply gzip compression to all files.}

\item{overwrite}{\code{logical(1)}.
Overwrite existing file on disk.}

\item{quiet}{\code{logical(1)}.
Perform command quietly, suppressing messages.}

\item{...}{Additional arguments.}
}
\value{
Invisible \code{character} or \code{list} of file paths.
}
\description{
Export data from R
}
\note{
Updated 2023-09-13.
}
\section{On-disk structure}{


Example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{file.path("object", "mutant_vs_control", "c1.csv")
}\if{html}{\out{</div>}}

S4 object is currently structured by:
\enumerate{
\item Gene set (c1-c8, h).
\item Contrast.
}

The object was structured in this manner to flow with the R Markdown
template. However, when writing to disk, I think it makes more sense to
organize by:
\enumerate{
\item Contrast
\item Gene set.
}
}

\examples{
data(fgsea)

## FgseaList ====
object <- fgsea
con <- AcidBase::tempdir2()
out <- export(
    object = object,
    con = con
)
print(out)
AcidBase::unlink2(con)
}
