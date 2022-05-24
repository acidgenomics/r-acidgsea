#' @name plotEnrichedGeneSets
#' @inherit AcidGenerics::plotEnrichedGeneSets
#' @note Updated 2022-04-27.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return `ggplot`.
#'
#' @seealso [plotGeneSet()].
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' alphaThreshold(object) <- 0.9
#' collection <- collectionNames(object)[[1L]]
#' plotEnrichedGeneSets(
#'     object = object,
#'     collection = collection,
#'     n = 1L
#' )
NULL



## Modified 2020-09-21.
`plotEnrichedGeneSets,FGSEAList` <- # nolint
    function(object,
             collection,
             direction = c("both", "up", "down"),
             n = 10L,
             headerLevel = 3L) {
        validObject(object)
        assert(
            isString(collection),
            isSubset(collection, collectionNames(object)),
            isInt(n),
            isHeaderLevel(headerLevel)
        )
        alphaThreshold <- alphaThreshold(object)
        nesThreshold <- nesThreshold(object)
        direction <- match.arg(direction)
        data <- object[[collection]]
        invisible(Map(
            contrast = names(data),
            data = data,
            MoreArgs = list(
                "alphaThreshold" = alphaThreshold,
                "nesThreshold" = nesThreshold,
                "direction" = direction,
                "n" = n
            ),
            f = function(contrast,
                           data,
                           alphaThreshold,
                           nesThreshold,
                           direction,
                           n) {
                markdownHeader(
                    text = contrast,
                    level = headerLevel,
                    tabset = TRUE,
                    asis = TRUE
                )
                sets <- .headtail(
                    object = data,
                    alphaThreshold = alphaThreshold,
                    nesThreshold = nesThreshold,
                    direction = direction,
                    n = n,
                    idCol = "pathway",
                    alphaCol = "padj",
                    nesCol = "NES"
                )
                if (!hasLength(sets)) {
                    return(invisible(NULL)) # nocov
                }
                ## Using an `mapply()` call here so we can pass the pathway
                ## names in easily into the `markdownHeader()` call.
                Map(
                    set = sets,
                    MoreArgs = list(
                        "collection" = collection,
                        "contrast" = contrast,
                        "headerLevel" = headerLevel + 1L
                    ),
                    f = function(set,
                                   collection,
                                   contrast,
                                   headerLevel) {
                        markdownHeader(set, level = headerLevel, asis = TRUE)
                        p <- plotGeneSet(
                            object,
                            collection = collection,
                            contrast = contrast,
                            set = set
                        )
                        tryCatch(
                            expr = print(p),
                            error = function(e) invisible(NULL)
                        )
                    }
                )
            }
        ))
    }



#' @rdname plotEnrichedGeneSets
#' @export
setMethod(
    f = "plotEnrichedGeneSets",
    signature = signature(object = "FGSEAList"),
    definition = `plotEnrichedGeneSets,FGSEAList`
)
