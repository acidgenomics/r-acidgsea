#' @name plotEnrichedUpset
#' @inherit acidgenerics::plotEnrichedUpset
#' @note Updated 2020-03-18.
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
#' @importFrom acidgenerics plotEnrichedUpset
#' @usage plotEnrichedUpset(object, ...)
#' @export
NULL



## Updated 2020-03-18.
`plotEnrichedUpset,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        alpha = NULL,
        nesThreshold = NULL,
        direction = c("both", "up", "down")
    ) {
        validObject(object)
        direction <- match.arg(direction)
        listInput <- enrichedGeneSets(
            object = object,
            collection = collection,
            alpha = alpha,
            nesThreshold = nesThreshold,
            direction = direction
        )
        ## Require at least 2 vectors.
        ## Otherwise, UpSetR will return array of at least two dimensions error.
        if (sum(bapply(X = listInput, FUN = hasLength)) < 2L) {
            ## nocov start
            cli_alert_warning(
                "Less than 2 enriched sets returned. Skipping plot."
            )
            return(invisible())
            ## nocov end
        }
        ## Suppressing message about single contrast not having up/down overlap:
        ## geom_path: Each group consists of only one observation.
        suppressMessages(
            plotUpset(object = fromList(listInput))
        )
    }



#' @rdname plotEnrichedUpset
#' @export
setMethod(
    f = "plotEnrichedUpset",
    signature = signature("FGSEAList"),
    definition = `plotEnrichedUpset,FGSEAList`
)
