#' Gene set names
#'
#' @name geneSetNames
#' @note Updated 2020-09-23.
#'
#' @inheritParams params
#'
#' @return `character`.
#'
#' @examples
#' data(fgsea)
#' geneSetNames(object = fgsea, collection = "h")
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
