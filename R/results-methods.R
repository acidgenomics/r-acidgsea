#' @name results
#' @inherit acidgenerics::results
#' @note Updated 2020-09-21.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' results(
#'     object = fgsea,
#'     collection = "h",
#'     contrast = "condition_B_vs_A"
#' )
NULL



#' @rdname results
#' @name results
#' @importFrom acidgenerics results
#' @usage results(object, ...)
#' @export
NULL



`results,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        contrast
    ) {
        validObject(object)
        assert(
            isString(collection),
            isSubset(collection, collectionNames(object)),
            isString(contrast),
            isSubset(contrast, contrastNames(object))
        )
        data <- object[[collection]][[contrast]]
        assert(is(data, "data.table"))
        data <- as(data, "DataFrame")
        data <- camelCase(data)
        data
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("FGSEAList"),
    definition = `results,FGSEAList`
)
