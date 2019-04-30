#' @name plotEnrichedUpset
#' @inherit bioverbs::plotEnrichedUpset
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(gsea)
#' plotEnrichedUpset(gsea, collection = "h")
NULL



#' @rdname plotEnrichedUpset
#' @name plotEnrichedUpset
#' @importFrom bioverbs plotEnrichedUpset
#' @usage plotEnrichedUpset(object, ...)
#' @export
NULL



plotEnrichedUpset.FGSEAList <-  # nolint
    function(object, collection) {
        validObject(object)
        listInput <- enrichedGeneSets(object = object, collection = collection)

        # Require at least 2 vectors.
        # Otherwise, UpSetR will return array of at least two dimensions error.
        if (sum(bapply(X = listInput, FUN = hasLength)) < 2L) {
            message("Less than 2 enriched sets returned. Skipping plot.")
            return(invisible())
        }

        # Suppressing message about single contrast not having up/down overlap:
        # geom_path: Each group consists of only one observation.
        suppressMessages(
            upset(data = fromList(listInput))
        )
    }



#' @rdname plotEnrichedUpset
#' @export
setMethod(
    f = "plotEnrichedUpset",
    signature = signature("FGSEAList"),
    definition = plotEnrichedUpset.FGSEAList
)
