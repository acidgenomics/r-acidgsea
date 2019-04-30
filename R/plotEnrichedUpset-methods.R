#' UpSet plot of directional enriched pathway intersections across contrasts
#'
#' @name plotEnrichedUpset
#' @inheritParams params
#'
#' @return Graphical output.
#'
#' @examples
#' data(gsea)
#' plotEnrichedUpset(gsea, collection = "h")
NULL



plotEnrichedUpset.FGSEAList <-  # nolint
    function(
        object,
        collection,
        alpha = 0.05
    ) {
        listInput <- enriched(
            object = object,
            collection = collection,
            alpha = alpha
        )
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
