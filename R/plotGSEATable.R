#' Plot GSEA Table
#'
#' Wrapper for [fgsea::plotGseaTable()].
#'
#' @name plotGSEATable
#' @inheritParams params
#' @return `ggplot`.
NULL



#' @describeIn plotGSEATable Plot a table of the top up- and down-regulated
#'   processes in the results returned from [fgsea()].
#' @export
plotGSEATable <- function(
    results,
    stats,
    gmtFile,
    alpha,
    n = 5L
) {
    assert_is_data.frame(results)
    assert_is_numeric(stats)
    assert_is_a_string(gmtFile)
    assert_all_are_existing_files(gmtFile)
    assert_is_a_number(n)

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



#' @describeIn plotGSEATable Parameterized. Loops across the result contrasts
#'   and adds a Markdown header.
#' @export
plotGSEATables <- function(
    resultsList,
    statsList,
    gmtFile,
    alpha,
    n = 5L,
    headerLevel = 3L
) {
    assert_is_list(resultsList)
    assert_is_list(statsList)
    assert_are_identical(names(resultsList), names(statsList))
    assert_is_a_string(gmtFile)
    assert_all_are_existing_files(gmtFile)
    assertIsAlpha(alpha)
    assert_is_a_number(n)
    assertIsHeaderLevel(headerLevel)

    invisible(mapply(
        name = names(resultsList),
        results = resultsList,
        stats = statsList,
        MoreArgs = list(gmtFile = gmtFile, n = n),
        FUN = function(name, results, stats, gmtFile, n) {
            markdownHeader(text = name, level = headerLevel, asis = TRUE)
            plotGSEATable(
                results = results,
                stats = stats,
                gmtFile = gmtFile,
                alpha = alpha,
                n = n
            )
        }
    ))
}
