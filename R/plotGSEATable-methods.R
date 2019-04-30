#' @name plotGSEATable
#' @inherit bioverbs::plotGSEATable
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return `ggplot`.
#'
#' @seealso [fgsea::plotGseaTable()].
#'
#' @examples
#' data(gsea)
#' plotGSEATable(gsea, collection = "h")
NULL



#' @rdname plotGSEATable
#' @name plotGSEATable
#' @importFrom bioverbs plotGSEATable
#' @usage plotGSEATable(object, ...)
#' @export
NULL



plotGSEATable.FGSEAList <- function(
    object,
    collection,
    n = 10L,
    headerLevel = 3L
) {
    validObject(object)
    alpha <- alphaThreshold(object)
    assert(
        isScalar(collection),
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
            alpha = alpha,
            n = n
        ),
        FUN = function(contrast, data, stats, gmtFile, alpha, n) {
            # Stash the unmodified FGSEA results table.
            # We need this for the `plotGseaTable()` call below.
            fgseaRes <- data

            markdownHeader(text = contrast, level = headerLevel, asis = TRUE)

            # Here we're getting the gene set vector for each pathway from the
            # GMT file. Then we're matching against the significant pathways
            # from our FGSEA analysis.
            pathways <- .headtail(data, alpha = alpha, n = n)
            if (!hasLength(pathways)) {
                return(invisible())
            }
            pathways <- gmtPathways(gmt.file = gmtFile)[pathways]

            # This returns a gtable plot object, which is hard to customize.
            # Note that we can't set title or subtitle here.
            fgsea::plotGseaTable(
                pathways = pathways,
                stats = stats,
                fgseaRes = fgseaRes,
                # GSEA-like parameter.
                # Adjusts displayed statistic values.
                # Values closer to 0 flatten plots.
                # 0.5 generally looks better than 1L.
                gseaParam = 0.5
            )
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
