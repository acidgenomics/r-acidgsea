#' @name geneSetNames
#' @inherit AcidGenerics::geneSetNames
#' @note Updated 2022-04-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' collection <- collectionNames(object)[[1L]]
#' head(geneSetNames(object = object, collection = collection))
NULL



## Updated 2022-04-27.
`geneSetNames,FgseaList` <- # nolint
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
    signature = signature(object = "FgseaList"),
    definition = `geneSetNames,FgseaList`
)
