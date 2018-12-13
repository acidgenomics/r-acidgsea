#' Plot GSEA Table
#'
#' Wrapper for `fgsea::plotGseaTable`.
#'
#' @name plotGSEATable
#' @inheritParams params
#' @return `ggplot`.
NULL



#' @describeIn plotGSEATable
#'   Plot a table of the top up- and down-regulated processes in the results
#'   returned from `fgsea`.
#' @export
plotGSEATable <- function(
    results,
    stats,
    gmtFile,
    alpha,
    n = 5L
) {
    assert(
        is.data.frame(results),
        is.numeric(stats),
        isAFile(gmtFile),
        isInt(n)
    )

    results <- results %>%
        as_tibble() %>%
        filter(!!sym("padj") < !!alpha) %>%
        arrange(desc(!!sym("NES")))

    pathways <- unique(c(
        head(results[["pathway"]], n = n),
        tail(results[["pathway"]], n = n)
    ))
    if (length(pathways) == 0L) {
        return(NULL)
    }
    pathways <- gmtPathways(gmt.file = gmtFile)[pathways]

    fgsea::plotGseaTable(
        pathways = pathways,
        stats = stats,
        fgseaRes = results,
        gseaParam = 0.5  # 1 also works.
    ) +
        theme_paperwhite()
}



#' @describeIn plotGSEATable
#'   Parameterized. Loops across the result contrasts and adds Markdown header.
#' @export
plotGSEATables <- function(
    resultsList,
    statsList,
    gmtFile,
    alpha,
    n = 5L,
    headerLevel = 3L
) {
    assert(
        is.list(resultsList),
        is.list(statsList),
        identical(names(resultsList), names(statsList)),
        isAFile(gmtFile),
        containsAlpha(alpha),
        isInt(n),
        containsHeaderLevel(headerLevel)
    )
    invisible(mapply(
        name = names(resultsList),
        results = resultsList,
        stats = statsList,
        MoreArgs = list(gmtFile = gmtFile, n = n),
        FUN = function(name, results, stats, gmtFile, n) {
            markdownHeader(text = name, level = headerLevel, asis = TRUE)
            fgsea::plotGSEATable(
                results = results,
                stats = stats,
                gmtFile = gmtFile,
                alpha = alpha,
                n = n
            )
        }
    ))
}
