## FIXME Rethink this, unit test?



#' @name leadingEdge
#' @inherit AcidGenerics::leadingEdge
#' @note Updated 2021-02-17.
#' @inheritParams params
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#' leadingEdge(
#'     object = fgsea,
#'     contrast = "condition_B_vs_A",
#'     collection = "h",
#'     set = "HALLMARK_P53_PATHWAY"
#' )
NULL


## FIXME Allow the user to pass in positional contrast or collection.
## FIXME Inform the user in the results call what we matched...

## Updated 2020-09-21.
`leadingEdge,FGSEAList` <-  # nolint
    function(
        object,
        contrast,
        collection,
        set
    ) {
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
        ## FIXME Need to check that this is a defined list column.
        genes <- unlist(unname(data[keep, "leadingEdge"]))
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
