## FIXME Store gene set in object, for portability.



#' @name plotEnrichedGeneSets
#' @inherit acidgenerics::plotEnrichedGeneSets
#' @note Updated 2020-07-22.
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



## Modified 2020-07-22.
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
        if (is.null(alpha)) {
            alpha <- alphaThreshold(object)
        }
        if (is.null(nesThreshold)) {
            nesThreshold <- 0L
        }
        assert(
            isScalar(collection),
            isAlpha(alpha),
            isNumber(nesThreshold),
            isInt(n),
            isHeaderLevel(headerLevel)
        )
        direction <- match.arg(direction)
        data <- object[[collection]]
        invisible(mapply(
            contrast = names(data),
            data = data,
            MoreArgs = list(
                alpha = alpha,
                direction = direction,
                n = n,
                nesThreshold = nesThreshold
            ),
            FUN = function(
                contrast,
                data,
                alpha,
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
                    alpha = alpha,
                    nesThreshold = nesThreshold,
                    direction = direction,
                    n = n,
                    idCol = "pathway",
                    alphaCol = "padj",
                    nesCol = "NES"
                )
                if (!hasLength(sets)) {
                    return(invisible())  # nocov
                }
                ## Using an `mapply()` call here so we can pass the pathway
                ## names in easily into the `markdownHeader()` call.
                mapply(
                    set = set,
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
