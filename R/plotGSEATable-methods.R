#' Plot GSEA table
#'
#' Wrapper for [fgsea::plotGseaTable()] that enables easy plotting of multiple
#' pathways of interest in a single call.
#'
#' @name plotGSEATable
#' @inheritParams params
#' @return `ggplot`.
#'
#' @seealso [fgsea::plotGseaTable()].
NULL



plotGSEATable.FGSEAList <- function(
    object,
    geneSet,
    alpha = 0.05,
    n = 5L,
    headerLevel = 2L
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
    stats <- rankedList(object)
    gmtFile <- metadata(object)[["gmtFiles"]][[geneSet]]
    assert(
        identical(names(data), names(stats)),
        isAFile(gmtFile)
    )
    invisible(mapply(
        name = names(data),
        data = data,
        stats = stats,
        MoreArgs = list(
            gmtFile = gmtFile,
            alpha = alpha,
            n = n
        ),
        FUN = function(name, data, stats, gmtFile, alpha, n) {
            # Stash the unmodified FGSEA results table.
            # We need this for the `plotGseaTable()` call below.
            fgseaRes <- data

            markdownHeader(text = name, level = headerLevel, asis = TRUE)
            data <- .filterResults(data, alpha = alpha)

            # Here we're getting the gene set vector for each pathway from the
            # GMT file. Then we're matching against the significant pathways
            # from our FGSEA analysis.
            pathways <- unique(c(
                head(data[["pathway"]], n = n),
                tail(data[["pathway"]], n = n)
            ))
            # If nothing is significant, early return without plotting.
            if (length(pathways) == 0L) {
                message("No significant pathways.")
                return(NULL)
            }
            gmtPathways <- gmtPathways(gmt.file = gmtFile)
            assert(isSubset(pathways, names(gmtPathways)))
            pathways <- gmtPathways[pathways]
            assert(is.list(pathways))

            p <- fgsea::plotGseaTable(
                pathways = pathways,
                stats = stats,
                fgseaRes = fgseaRes,
                # GSEA-like parameter.
                # Adjusts displayed statistic values.
                # Values closer to 0 flatten plots.
                # 0.5 generally looks better than 1L.
                gseaParam = 0.5
            )
            # Consider making the theme user definable in a future update.
            p <- p + theme_paperwhite()
            p
        }
    ))
}



#' @rdname plotGSEATable
#' @export
setMethod(
    f = "plotGSEATable",
    signature = signature("FGSEAList"),
    definition = plotGSEATable.FGSEAList
)
