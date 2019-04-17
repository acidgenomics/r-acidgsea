#' @name defunct
#' @inherit basejump::defunct
#' @keywords internal
NULL

#' @name deprecated
#' @inherit basejump::deprecated
#' @keywords internal
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
    .Deprecated("RankedList")
    RankedList(...)
}
