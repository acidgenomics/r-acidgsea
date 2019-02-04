#' Plot enrichment
#'
#' @name plotEnrichment
#' @inheritParams params
#' @return `ggplot`.
#'
#' @seealso [fgsea::plotEnrichment()].
NULL



plotEnrichment.FGSEAList <- function(
    object,
    geneSet,
    alpha = 0.05,
    n = 5L,
    headerLevel = 2L,
    theme = basejump::theme_paperwhite()
) {
    validObject(object)
    assert(
        isString(geneSet),
        isSubset(geneSet, names(object)),
        isAlpha(alpha),
        isInt(n),
        isHeaderLevel(headerLevel)
    )
    data <- object[[geneSet]]
    stats <- RankedList(object)
    gmtFile <- metadata(object)[["gmtFiles"]][[geneSet]]
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
                    p <- fgsea::plotEnrichment(pathway = pathway, stats = stats)
                    p <- p + labs(title = name, subtitle = contrast)
                    # fgsea sets a theme that is too hard to read.
                    if (isAll(theme, c("theme", "gg"))) {
                        p <- p + theme
                    }
                    # Note that we need the `print()` call here for looping.
                    print(p)
                }
            )
        }
    ))
}



#' @rdname plotEnrichment
#' @export
setMethod(
    f = "plotEnrichment",
    signature = signature("FGSEAList"),
    definition = plotEnrichment.FGSEAList
)
