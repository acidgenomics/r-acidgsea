#' @name defunct
#' @inherit basejump::defunct
NULL

#' @name deprecated
#' @inherit basejump::deprecated
NULL



# v0.0.5 =======================================================================
#' @rdname defunct
#' @export
plotEnrichments <- function(...) {
    .Defunct("plotEnrichment")
}

#' @rdname defunct
#' @export
plotGSEATables <- function(...) {
    .Defunct("plotGSEATable")
}

#' @rdname deprecated
#' @export
statsList <- function(...) {
    .Deprecated("rankedList")
    rankedList(...)
}
