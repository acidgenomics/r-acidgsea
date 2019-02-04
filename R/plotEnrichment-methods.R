#' Plot enrichment
#'
#' Wrapper for [fgsea::plotEnrichment()] that enables easy plotting of multiple
#' pathways of interest in a single call.
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
            data <- .filterResults(data, alpha = alpha)

            # Here we're getting the gene set vector for each pathway from the
            # GMT file. Then we're matching against the significant pathways
            # from our FGSEA analysis.
            pathways <- unique(c(
                head(data[["pathway"]], n = n),
                tail(data[["pathway"]], n = n)
            ))
            # If nothing is significant, early return without plotting.
            if (!hasLength(pathways)) {
                message("No significant pathways.")
                return(NULL)
            }
            gmtPathways <- gmtPathways(gmt.file = gmtFile)
            assert(isSubset(pathways, names(gmtPathways)))
            pathways <- gmtPathways[pathways]
            assert(is.list(pathways))

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
                    p <- fgsea::plotEnrichment(
                        pathway = pathway,
                        stats = stats
                    )
                    # Consider making these plot settings user definable.
                    p <- p +
                        labs(
                            title = name,
                            subtitle = contrast
                        ) +
                        theme_paperwhite()
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
