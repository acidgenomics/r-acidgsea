#' UpSet plot of directional enriched pathway intersections across contrasts
#'
#' @name plotEnrichedUpset
#' @inheritParams params
#'
#' @return Graphical output.
#'
#' @examples
#' plotEnrichedUpset(gsea)
NULL



plotEnrichedUpset.FGSEAList <-  # nolint
    function(object, alpha = 0.05) {
        listInput <- enriched(object, alpha = alpha)
        # Suppressing message about single contrast not having up/down DEG overlap:
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
