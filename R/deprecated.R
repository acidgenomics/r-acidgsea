## nocov start



#' @name defunct
#' @inherit basejump::defunct description return title
#' @keywords internal
NULL

#' @name deprecated
#' @inherit basejump::deprecated description return title
#' @keywords internal
NULL



## v0.2.0 =====================================================================
#' @rdname deprecated
#' @export
rgpfgsea <- function(...) {
    .Deprecated("FGSEAList")
    FGSEAList(...)
}



## nocov end
