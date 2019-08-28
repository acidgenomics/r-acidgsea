#' @name topTables
#' @inherit bioverbs::topTables
#' @note Updated 2019-08-28.
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
#' topTables(gsea, collection = "h")
NULL



#' @rdname topTables
#' @name topTables
#' @importFrom bioverbs topTables
#' @usage topTables(object, ...)
#' @export
NULL



## Updated 2019-08-28.
`topTables,FGSEAList` <-  # nolint
    function(
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
                ## Filter our results, and early return on no sig features.
                data <- .filterResults(data, alpha = alpha)
                if (!hasRows(data)) {
                    return(invisible())  # nocov
                }
                ## Sanitize and minimize the results before printing.
                ## Drop the nested list columns (e.g. leadingEdge).
                data <- selectIf(data, is.atomic)
                ## Drop additional uninformative columns.
                keep <- setdiff(colnames(data), c("ES", "nMoreExtreme", "pval"))
                data <- data[, keep, drop = FALSE]
                markdownHeader(
                    text = "Upregulated",
                    level = headerLevel + 1L,
                    asis = TRUE
                )
                up <- data[data[["NES"]] > 0L, , drop = FALSE]
                up <- up[order(up[["padj"]], -up[["NES"]]), , drop = FALSE]
                up <- head(up, n = n)
                if (hasRows(up)) {
                    print(kable(as.data.frame(up), digits = 3L))
                } else {
                    message("No upregulated sets.")  # nocov
                }
                markdownHeader(
                    text = "Downregulated",
                    level = headerLevel + 1L,
                    asis = TRUE
                )
                down <- data[data[["NES"]] < 0L, , drop = FALSE]
                down <- down[
                    order(down[["padj"]], down[["NES"]]), , drop = FALSE
                    ]
                down <- head(down, n = n)
                if (hasRows(down)) {
                    print(kable(as.data.frame(down), digits = 3L))
                } else {
                    message("No downregulated sets.")  # nocov
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
    definition = `topTables,FGSEAList`
)
