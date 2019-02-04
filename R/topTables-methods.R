#' @name topTables
#' @description Top tables of significantly enriched pathways.
#' @details
#' Supports looping across multiple DEG results, and adds a Markdown header for
#' each contrast.
#'
#' @inherit bioverbs::topTables
#' @inheritParams params
#'
#' @return Markdown output.
NULL



#' @importFrom bioverbs topTables
#' @aliases NULL
#' @export
bioverbs::topTables



topTables.FGSEAList <- function(
    object,
    geneSet,
    alpha = 0.05,
    n = 10L,
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
    invisible(mapply(
        header = names(data),
        data = data,
        FUN = function(header, data) {
            markdownHeader(text = header, level = headerLevel, asis = TRUE)
            data <- data %>%
                as_tibble() %>%
                # Drop the nested list columns (e.g. leadingEdge, nMoreExtreme).
                select_if(is.atomic) %>%
                # Filter and arrange by desired significance.
                filter(!!sym("padj") < !!alpha) %>%
                arrange(desc(!!sym("NES")), !!sym("padj")) %>%
                # Drop uninformative columns.
                # Note: dplyr `-UQS` approach doesn't work for SE drops.
                select(!!!syms(
                    setdiff(colnames(.), c("ES", "nMoreExtreme", "pval"))
                )) %>%
                # Now we can subset to the desired number of top pathways.
                head(n = n)
            print(kable(data, digits = 3L))
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    ))
}



#' @rdname topTables
#' @export
setMethod(
    f = "topTables",
    signature = signature("FGSEAList"),
    definition = topTables.FGSEAList
)
