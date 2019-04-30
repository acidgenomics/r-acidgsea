#' @name plotEnrichedGeneSets
#' @inherit bioverbs::plotEnrichedGeneSets
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
#' plotEnrichedGeneSets(gsea, collection = "h", n = 1L)
NULL



#' @rdname plotEnrichedGeneSets
#' @name plotEnrichedGeneSets
#' @importFrom bioverbs plotEnrichedGeneSets
#' @usage plotEnrichedGeneSets(object, ...)
#' @export
NULL



plotEnrichedGeneSets.FGSEAList <- function(
    object,
    collection,
    alpha = 0.05,
    n = 10L,
    headerLevel = 3L,
    theme = acid_theme_light()
) {
    validObject(object)
    assert(
        isString(collection),
        isSubset(collection, collectionNames(object)),
        isAlpha(alpha),
        isInt(n),
        isHeaderLevel(headerLevel)
    )
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
            gmtFile = gmtFile,
            n = n,
            alpha = alpha
        ),
        FUN = function(contrast, data, stats, gmtFile, n, alpha) {
            markdownHeader(
                text = contrast,
                level = headerLevel,
                tabset = TRUE,
                asis = TRUE
            )

            # Here we're getting the gene set vector for each pathway from the
            # GMT file. Then we're matching against the significant pathways
            # from our FGSEA analysis.
            pathways <- .headtail(data, alpha = alpha, n = n)
            if (!hasLength(pathways)) {
                return(invisible())
            }
            pathways <- gmtPathways(gmt.file = gmtFile)[pathways]

            # Using an `mapply()` call here so we can pass the pathway names
            # in easily into the `markdownHeader()` call.
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
                    # Suppressing warnings here to for minimal example to work.
                    # `min(bottoms)` and `max(tops)` failure can occur, causing
                    # ggplot to fail.
                    p <- suppressWarnings(
                        fgsea::plotEnrichment(pathway = pathway, stats = stats)
                    )
                    p <- p + labs(title = name, subtitle = contrast)
                    # fgsea sets a theme that is too hard to read.
                    if (isAll(theme, c("theme", "gg"))) {
                        p <- p + theme
                    }
                    # Note that we need the `print()` call here for looping.
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
    definition = plotEnrichedGeneSets.FGSEAList
)
