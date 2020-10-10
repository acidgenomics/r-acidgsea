#' @name geneSet
#' @inherit AcidGenerics::geneSet
#' @note Updated 2020-09-21.
#' @inheritParams params
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#' geneSet(
#'     object = fgsea,
#'     collection = "h",
#'     set = "HALLMARK_P53_PATHWAY"
#' )
NULL



#' @rdname geneSet
#' @name geneSet
#' @importFrom AcidGenerics geneSet
#' @usage geneSet(object, ...)
#' @export
NULL



## Updated 2020-09-21.
`geneSet,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        set
    ) {
        validObject(object)
        assert(
            isString(collection),
            isSubset(collection, collectionNames(object)),
            isString(set)
        )
        x <- metadata(object)[["collections"]][[collection]][[set]]
        assert(isCharacter(x))
        x
    }



#' @rdname geneSet
#' @export
setMethod(
    f = "geneSet",
    signature = signature("FGSEAList"),
    definition = `geneSet,FGSEAList`
)
