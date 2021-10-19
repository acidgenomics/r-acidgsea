## FIXME Allow user to look up by position.



#' @name results
#' @inherit AcidGenerics::results
#' @note Updated 2021-10-19.
#' @inheritParams params
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' results(
#'     object = fgsea,
#'     contrast = "condition_B_vs_A",
#'     collection = "h"
#' )
NULL



## Updated 2021-10-19.
`results,FGSEAList` <-  # nolint
    function(
        object,
        contrast,
        collection
    ) {
        validObject(object)
        assert(
            isScalar(contrast),
            isScalar(collection)
        )
        data <- object[[collection]][[contrast]]
        assert(
            is(data, "data.table"),
            isSubset("leadingEdge", colnames(data)),
            msg = sprintf(
                "Failed to extract results for {.cls %s}.",
                "FGSEAList"
            )
        )
        data <- as(data, "DataFrame")
        data <- camelCase(data, strict = TRUE)
        ## Coerce "leadingEdge" list column to string.
        data[["leadingEdge"]] <-
            unlist(lapply(X = data[["leadingEdge"]], FUN = toString))
        assert(allAreAtomic(data))
        data
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature(object = "FGSEAList"),
    definition = `results,FGSEAList`
)
