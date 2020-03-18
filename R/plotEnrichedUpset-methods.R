#' @name plotEnrichedUpset
#' @inherit acidgenerics::plotEnrichedUpset
#' @note Updated 2020-03-18.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(gsea)
#' plotEnrichedUpset(gsea, collection = "h", alpha = 0.9)
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
        args <- list(
            object = object,
            collection = collection,
            alpha = alpha,
            nesThreshold = nesThreshold
        )
        ## Upregulated gene sets.
        if (isSubset(direction, c("both", "up"))) {
            suppressMessages(
                up <- do.call(
                    what = enrichedGeneSets,
                    args = c(args, direction = "up")
                )
            )
            names(up) <- makeNames(paste(names(up), "up"))
        } else {
            up <- NULL
        }
        ## Downregulated gene sets.
        if (isSubset(direction, c("both", "down"))) {
            suppressMessages(
                down <- do.call(
                    what = enrichedGeneSets,
                    args = c(args, direction = "down")
                )
            )
            names(down) <- makeNames(paste(names(down), "down"))
        } else {
            down <- NULL
        }
        ## Combine directional output and hand off to UpSetR package.
        listInput <- c(up, down)
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
