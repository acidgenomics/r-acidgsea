#' @name updateObject
#' @author Michael Steinbaugh
#' @inherit BiocGenerics::updateObject
#' @note Updated 2019-11-07.
#'
#' @inheritParams acidroxygen::params
#'
#' @return Modified object.
#'
#' @examples
#' data(gsea)
#' updateObject(gsea)
NULL



#' @rdname updateObject
#' @name updateObject
#' @importFrom BiocGenerics updateObject
#' @usage updateObject(object, ...)
#' @export
NULL



`updateObject,FGSEAList` <-  # nolint
    function(object) {
        object
    }



#' @rdname updateObject
#' @export
setMethod(
    f = "updateObject",
    signature = signature("FGSEAList"),
    definition = `updateObject,FGSEAList`
)
