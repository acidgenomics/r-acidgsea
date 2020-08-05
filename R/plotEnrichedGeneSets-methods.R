#' @name plotEnrichedGeneSets
#' @inherit acidgenerics::plotEnrichedGeneSets
#' @note Updated 2020-08-05.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return `ggplot`.
#'
#' @seealso [plotGeneSet()].
#'
#' @examples
#' ## This requires MSigDB to be installed at `${HOME}`.
#' if (isTRUE(dir.exists(file.path("~", "msigdb")))) {
#'     data(fgsea)
#'     plotEnrichedGeneSets(fgsea, collection = "h", alpha = 0.9, n = 1L)
#' }
NULL



#' @rdname plotEnrichedGeneSets
#' @name plotEnrichedGeneSets
#' @importFrom acidgenerics plotEnrichedGeneSets
#' @usage plotEnrichedGeneSets(object, ...)
#' @export
NULL



## Modified 2020-08-05.
`plotEnrichedGeneSets,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        alpha = NULL,
        nesThreshold = NULL,
        direction = c("both", "up", "down"),
        n = 10L,
        headerLevel = 3L
    ) {
        validObject(object)
        assert(
            isScalar(collection),
            isInt(n),
            isHeaderLevel(headerLevel)
        )
        alphaThreshold <- alphaThreshold(object)
        nesThreshold <- nesThreshold(object)
        direction <- match.arg(direction)
        data <- object[[collection]]
        invisible(mapply(
            contrast = names(data),
            data = data,
            MoreArgs = list(
                alphaThreshold = alphaThreshold,
                nesThreshold = nesThreshold,
                direction = direction,
                n = n
            ),
            FUN = function(
                contrast,
                data,
                alphaThreshold,
                nesThreshold,
                direction,
                n
            ) {
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
                    return()  # nocov
                }
                ## Using an `mapply()` call here so we can pass the pathway
                ## names in easily into the `markdownHeader()` call.
                mapply(
                    set = sets,
                    MoreArgs = list(
                        collection = collection,
                        contrast = contrast,
                        headerLevel = headerLevel + 1L
                    ),
                    FUN = function(
                        set,
                        collection,
                        contrast,
                        headerLevel
                    ) {
                        markdownHeader(set, level = headerLevel, asis = TRUE)
                        p <- plotGeneSet(
                            object,
                            collection = collection,
                            contrast = contrast,
                            set = set
                        )
                        tryCatch(
                            expr = print(p),
                            error = function(e) invisible()
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
    signature = signature("FGSEAList"),
    definition = `plotEnrichedGeneSets,FGSEAList`
)
