#' @name results
#' @inherit AcidGenerics::results
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
#' results(
#'     object = fgsea,
#'     contrast = contrast,
#'     collection = collection
#' )
NULL



## Updated 2021-10-19.
.resultsForAllContrasts <-
    function(object,
             collection) {
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
            is(df, "DFrame"),
            isSubset(
                x = c("nes", "padj", "pathway"),
                y = colnames(df)
            )
        )
        metadata(df)[["collection"]] <- collection
        df
    }



## Updated 2021-10-19.
`results,FGSEAList` <- # nolint
    function(object,
             contrast,
             collection) {
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
        data <- as(data, "DFrame")
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
