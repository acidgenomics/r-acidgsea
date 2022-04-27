#' @name leadingEdge
#' @inherit AcidGenerics::leadingEdge
#' @note Updated 2022-04-27.
#' @inheritParams params
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' contrast <- contrastNames(object)[[1L]]
#' collection <- collectionNames(object)[[1L]]
#' set <- geneSetNames(object = object, collection = collection)[[1L]]
#' leadingEdge(
#'     object = object,
#'     contrast = contrast,
#'     collection = collection,
#'     set = set
#' )
NULL



## Updated 2020-09-21.
`leadingEdge,FGSEAList` <- # nolint
    function(object,
             contrast,
             collection,
             set) {
        validObject(object)
        assert(
            isString(contrast),
            isSubset(contrast, contrastNames(object)),
            isString(collection),
            isSubset(collection, collectionNames(object)),
            isString(set)
        )
        data <- object[[collection]][[contrast]]
        assert(is(data, "data.table"))
        ## Coerce to DataFrame, to use standard subsetting syntax.
        data <- as(data, "DataFrame")
        keep <- match(set, table = data[["pathway"]])
        if (!isInt(keep)) {
            alertWarning(sprintf("Failed to match '%s' set.", set))
            return(NULL)
        }
        genes <- unlist(
            x = unname(data[keep, "leadingEdge"]),
            recursive = FALSE
        )
        assert(isCharacter(genes))
        genes
    }



#' @rdname leadingEdge
#' @export
setMethod(
    f = "leadingEdge",
    signature = signature(object = "FGSEAList"),
    definition = `leadingEdge,FGSEAList`
)
