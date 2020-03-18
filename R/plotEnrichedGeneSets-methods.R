#' @name plotEnrichedGeneSets
#' @inherit acidgenerics::plotEnrichedGeneSets
#' @note Updated 2020-03-18.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return `ggplot`.
#'
#' @seealso [fgsea::plotEnrichment()].
#'
#' @examples
#' data(gsea)
#' plotEnrichedGeneSets(gsea, collection = "h", alpha = 0.9, n = 1L)
NULL



#' @rdname plotEnrichedGeneSets
#' @name plotEnrichedGeneSets
#' @importFrom acidgenerics plotEnrichedGeneSets
#' @usage plotEnrichedGeneSets(object, ...)
#' @export
NULL



## Modified 2019-06-12.
`plotEnrichedGeneSets,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        alpha = NULL,
        nesThreshold = NULL,
        direction = c("both", "up", "down"),
        n = 10L,
        headerLevel = 3L,
        theme = acidplots::acid_theme_light()
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
        stats <- RankedList(object)
        gmtFile <- metadata(object)[["gmtFiles"]][[collection]]
        assert(
            identical(names(data), names(stats)),
            isAFile(gmtFile)
        )
        invisible(mapply(
            contrast = names(data),
            data = data,
            stats = stats,
            MoreArgs = list(
                alpha = alpha,
                direction = direction,
                gmtFile = gmtFile,
                n = n,
                nesThreshold = nesThreshold
            ),
            FUN = function(
                contrast,
                data,
                stats,
                gmtFile,
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
                ## Here we're getting the gene set vector for each pathway from
                ## the GMT file. Then we're matching against the significant
                ## pathways from our FGSEA analysis.
                pathways <- .headtail(
                    object = data,
                    alpha = alpha,
                    nesThreshold = nesThreshold,
                    direction = direction,
                    n = n,
                    idCol = "pathway",
                    alphaCol = "padj",
                    nesCol = "NES"
                )
                if (!hasLength(pathways)) {
                    return(invisible())  # nocov
                }
                pathways <- gmtPathways(gmt.file = gmtFile)[pathways]
                ## Using an `mapply()` call here so we can pass the pathway
                ## names in easily into the `markdownHeader()` call.
                mapply(
                    name = names(pathways),
                    pathway = pathways,
                    MoreArgs = list(
                        stats = stats,
                        headerLevel = headerLevel + 1L,
                        contrast = contrast
                    ),
                    FUN = function(
                        name,
                        pathway,
                        stats,
                        headerLevel,
                        contrast
                    ) {
                        markdownHeader(name, level = headerLevel, asis = TRUE)
                        ## Suppressing warnings here to for minimal example to
                        ## work. `min(bottoms)` and `max(tops)` failure can
                        ## occur, causing ggplot to fail. Safe to remove
                        ## `fgsea::` here once we remove now defunct
                        ## `plotEnrichment()` from export.
                        p <- suppressWarnings(
                            fgsea::plotEnrichment(
                                pathway = pathway,
                                stats = stats
                            )
                        )
                        p <- p + labs(title = name, subtitle = contrast)
                        ## fgsea sets a theme that is too hard to read.
                        if (isAll(theme, c("theme", "gg"))) {
                            p <- p + theme
                        }
                        ## Note that we need the `print()` call here for loops.
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
