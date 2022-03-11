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
.resultsForAllContrasts <- function(
    object,
    collection
) {
    assert(
        is(object, "FGSEAList"),
        isString(collection)
    )
    df <- do.call(
        what = rbind,
        args = lapply(
            X = contrastNames(object),
            FUN = function(contrast) {
                df <- results(
                    object = object,
                    contrast = contrast,
                    collection = collection
                )
                df[["contrast"]] <- contrast
                df
            }
        )
    )
    assert(
        is(df, "DataFrame"),
        isSubset(
            x = c("nes", "padj", "pathway"),
            y = colnames(df)
        )
    )
    metadata(df)[["collection"]] <- collection
    df
}



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
        colnames(data) <- camelCase(colnames(data), strict = TRUE)
        ## Coerce the "leadingEdge" list column to a character string.
        data[["leadingEdge"]] <- unlist(
            x = lapply(
                X = data[["leadingEdge"]],
                FUN = toString
            ),
            recursive = FALSE
        )
        data
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature(object = "FGSEAList"),
    definition = `results,FGSEAList`
)
