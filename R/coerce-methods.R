#' @name coerce
#' @aliases as
#' @inherit methods::as
#' @importFrom methods coerce
#' @exportMethod coerce
#'
#' @return Object of new class.
#'
#' @seealso
#' - [methods::coerce()].
#' - [methods::canCoerce()].
#' - [methods::as()].
NULL



#' @rdname coerce
#' @name coerce,list,FGSEAList-method
setAs(
    from = "list",
    to = "FGSEAList",
    def = function(from) {
        new(Class = "FGSEAList", SimpleList(from))
    }
)
