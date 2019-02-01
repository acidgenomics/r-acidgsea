#' @name deprecated
#' @inherit basejump::deprecated
NULL



# v0.0.5 =======================================================================
#' @rdname deprecated
#' @export
statsList <- function(...) {
    .Deprecated("rankedList")
    rankedList(...)
}
