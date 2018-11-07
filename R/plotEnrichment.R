#' Plot Enrichment
#'
#' Wrapper for [fgsea::plotEnrichment()] that enables easy plotting of multiple
#' pathways of interest in a single call.
#'
#' @name plotEnrichment
#' @inheritParams params
#' @return `ggplot`.
NULL



#' @describeIn plotEnrichment Parameterized. Returns multiple `ggplot`s,
#'   with a Markdown header for each pathway.
#' @export
plotEnrichment <- function(
    pathways,
    stats,
    gmtFile,
    headerLevel = 3L
) {
    assert_is_character(pathways)
    assert_is_numeric(stats)
    assert_is_a_string(gmtFile)
    assert_all_are_existing_files(gmtFile)
    gmtPathways <- gmtPathways(gmt.file = gmtFile)
    invisible(lapply(
        X = pathways,
        FUN = function(pathway) {
            markdownHeader(pathway, level = headerLevel, asis = TRUE)
            p <- fgsea::plotEnrichment(
                pathway = gmtPathways[[pathway]],
                stats = stats
            ) +
                labs(title = pathway) +
                theme_paperwhite()
            print(p)
        }
    ))
}



#' @describeIn plotEnrichment Parameterized. Loops across the contrasts and
#'  returns an additional Markdown header, on top of the [plotEnrichment()]
#'  return per pathway.
#' @export
plotEnrichments <- function(
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
    assertIsHeaderLevel(headerLevel)

    invisible(mapply(
        name = names(resultsList),
        results = resultsList,
        stats = statsList,
        MoreArgs = list(gmtFile = gmtFile, n = n),
        FUN = function(name, results, stats, gmtFile, n) {
            markdownHeader(
                text = name,
                level = headerLevel,
                tabset = TRUE,
                asis = TRUE
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

            plotEnrichment(
                pathways = pathways,
                stats = stats,
                gmtFile = gmtFile,
                headerLevel = headerLevel + 1L
            )
        }
    ))
}
