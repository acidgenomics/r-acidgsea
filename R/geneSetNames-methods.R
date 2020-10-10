#' @name geneSetNames
#' @inherit AcidGenerics::geneSetNames
#' @note Updated 2020-10-01.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#' geneSetNames(object = fgsea, collection = "h")
NULL



#' @rdname geneSetNames
#' @name geneSetNames
#' @importFrom AcidGenerics geneSetNames
#' @usage geneSetNames(object, ...)
#' @export
NULL



`geneSetNames,FGSEAList` <-  # nolint
    function(object, collection) {
        validObject(object)
        assert(
            isString(collection),
            isSubset(collection, collectionNames(object))
        )
        out <- object[[collection]][[1L]][["pathway"]]
        assert(isCharacter(out))
        out
    }



#' @rdname geneSetNames
#' @export
setMethod(
    f = "geneSetNames",
    signature = signature("FGSEAList"),
    definition = `geneSetNames,FGSEAList`
)
