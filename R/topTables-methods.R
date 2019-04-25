#' @name topTables
#' @inherit bioverbs::topTables
#'
#' @description Top tables of significantly enriched pathways.
#'
#' @details
#' Supports looping across multiple DEG results, and adds a Markdown header for
#' each contrast.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return Markdown output.
#'
#' @examples
#' data(gsea)
#' topTables(gsea, geneSet = "h")
NULL



#' @rdname topTables
#' @name topTables
#' @importFrom bioverbs topTables
#' @usage topTables(object, ...)
#' @export
NULL



topTables.FGSEAList <- function(
    object,
    geneSet,
    alpha = 0.05,
    n = 10L,
    headerLevel = 3L
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
        contrast = names(data),
        data = data,
        MoreArgs = list(
            alpha = alpha,
            n = n,
            headerLevel = headerLevel
        ),
        FUN = function(contrast, data, alpha, n, headerLevel) {
            markdownHeader(
                text = contrast,
                level = headerLevel,
                asis = TRUE
            )

            # Filter our results, and early return if nothing's significant.
            data <- .filterResults(data, alpha = alpha)
            if (!hasRows(data)) {
                return(invisible())
            }

            # Sanitize and minimize the results before printing.
            data <- data %>%
                # Drop the nested list columns (e.g. leadingEdge, nMoreExtreme).
                select_if(is.atomic) %>%
                # Drop additional uninformative columns.
                # Note: dplyr `-UQS` approach doesn't work for SE drops.
                select(!!!syms(
                    setdiff(colnames(.), c("ES", "nMoreExtreme", "pval"))
                ))

            # Generate our subset tibbles to print.
            up <- data %>%
                filter(!!sym("NES") > 0L) %>%
                arrange(!!sym("padj"), desc(!!sym("NES"))) %>%
                head(n = n)
            down <- data %>%
                filter(!!sym("NES") < 0L) %>%
                arrange(!!sym("padj"), !!sym("NES")) %>%
                head(n = n)

            markdownHeader(
                text = "Upregulated",
                level = headerLevel + 1L,
                asis = TRUE
            )
            if (hasRows(up)) {
                print(kable(up, digits = 3L))
            } else {
                message("No upregulated sets.")
            }

            markdownHeader(
                text = "Downregulated",
                level = headerLevel + 1L,
                asis = TRUE
            )
            if (hasRows(down)) {
                print(kable(down, digits = 3L))
            } else {
                message("No downregulated sets.")
            }
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
