#' @name leadingEdge
#' @inherit acidgenerics::leadingEdge
#' @note Updated 2020-09-21.
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



#' @rdname leadingEdge
#' @name leadingEdge
#' @importFrom acidgenerics leadingEdge
#' @usage leadingEdge(object, ...)
#' @export
NULL



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
            isString(collection),
            isString(set)
        )
        data <- object[[collection]][[contrast]]
        assert(is(data, "data.table"))
        ## Coerce to DataFrame, to use standard subsetting syntax.
        data <- as(data, "DataFrame")
        keep <- match(set, table = data[["pathway"]])
        if (!isInt(keep)) {
            stop(sprintf("Failed to match '%s' set.", set))
        }
        genes <- unlist(unname(data[keep, "leadingEdge"]))
        assert(isCharacter(genes))
        genes
    }



#' @rdname leadingEdge
#' @export
setMethod(
    f = "leadingEdge",
    signature = signature("FGSEAList"),
    definition = `leadingEdge,FGSEAList`
)
