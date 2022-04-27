#' @name geneSet
#' @inherit AcidGenerics::geneSet
#' @note Updated 2022-04-27.
#' @inheritParams params
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' collection <- collectionNames(object)[[1L]]
#' set <- geneSetNames(object = object, collection = collection)[[1L]]
#' x <- geneSet(
#'     object = object,
#'     collection = collection,
#'     set = set
#' )
#' head(x)
NULL



## Updated 2020-09-21.
`geneSet,FGSEAList` <- # nolint
    function(object,
             collection,
             set) {
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
    signature = signature(object = "FGSEAList"),
    definition = `geneSet,FGSEAList`
)
