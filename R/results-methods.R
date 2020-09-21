#' @name results
#' @inherit acidgenerics::results
#' @note Updated 2020-09-21.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' results(fgsea)
NULL



#' @rdname results
#' @name results
#' @importFrom acidgenerics results
#' @usage results(object, ...)
#' @export
NULL



`results,FGSEAList` <-  # nolint
    function(object) {
        ## FIXME
        validObject(object)
    }
