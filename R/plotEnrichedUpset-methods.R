#' @name plotEnrichedUpset
#' @inherit AcidGenerics::plotEnrichedUpset
#' @note Updated 2020-08-05.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' alphaThreshold(fgsea) <- 0.9
#' plotEnrichedUpset(fgsea, collection = "h")
NULL



#' @rdname plotEnrichedUpset
#' @name plotEnrichedUpset
#' @importFrom AcidGenerics plotEnrichedUpset
#' @usage plotEnrichedUpset(object, ...)
#' @export
NULL



## Updated 2020-08-05.
`plotEnrichedUpset,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        direction = c("both", "up", "down")
    ) {
        validObject(object)
        direction <- match.arg(direction)
        args <- list(
            object = object,
            collection = collection
        )
        ## Upregulated gene sets.
        if (isSubset(direction, c("both", "up"))) {
            suppressMessages({
                up <- do.call(
                    what = enrichedGeneSets,
                    args = c(args, direction = "up")
                )
            })
            names(up) <- makeNames(paste(names(up), "up"))
        } else {
            up <- NULL
        }
        ## Downregulated gene sets.
        if (isSubset(direction, c("both", "down"))) {
            suppressMessages({
                down <- do.call(
                    what = enrichedGeneSets,
                    args = c(args, direction = "down")
                )
            })
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
            return(invisible(NULL))
            ## nocov end
        }
        ## Suppressing message about single contrast not having up/down overlap:
        ## geom_path: Each group consists of only one observation.
        suppressMessages({
            plotUpset(listInput)
        })
    }



#' @rdname plotEnrichedUpset
#' @export
setMethod(
    f = "plotEnrichedUpset",
    signature = signature("FGSEAList"),
    definition = `plotEnrichedUpset,FGSEAList`
)
