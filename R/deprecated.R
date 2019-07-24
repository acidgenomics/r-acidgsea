## nocov start



#' @name defunct
#' @inherit basejump::defunct
#' @keywords internal
NULL

#' @name deprecated
#' @inherit basejump::deprecated
#' @keywords internal
NULL



## v0.0.5 ======================================================================
#' @rdname defunct
#' @export
plotEnrichments <- function(...) {
    .Defunct("plotEnrichedGeneSets")
}

#' @rdname defunct
#' @export
plotGSEATables <- function(...) {
    .Defunct("plotGSEATable")
}

#' @rdname deprecated
#' @export
statsList <- function(...) {
    .Deprecated("RankedList")
    RankedList(...)
}



## v0.1.7 ======================================================================
#' @rdname defunct
#' @export
plotEnrichment <- function(...) {
    .Defunct("plotEnrichedGeneSets")
}



## nocov end
