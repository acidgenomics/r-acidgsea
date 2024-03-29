#' @name markdownTables
#' @inherit AcidGenerics::markdownTables
#' @note Updated 2022-05-24.
#'
#' @details
#' Top tables of significantly enriched pathways.
#'
#' Supports looping across multiple DEG results, and adds a Markdown header for
#' each contrast.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' alphaThreshold(object) <- 0.9
#' collection <- collectionNames(object)[[1L]]
#' markdownTables(
#'     object = object,
#'     collection = collection
#' )
NULL



## Updated 2022-05-24.
`markdownTables,FgseaList` <- # nolint
    function(object,
             collection,
             n = 10L,
             headerLevel = 3L) {
        assert(
            requireNamespaces("knitr"),
            validObject(object),
            isString(collection),
            isInt(n),
            isHeaderLevel(headerLevel)
        )
        args <- list(
            object = object,
            collection = collection
        )
        ## Upregulated gene sets.
        suppressMessages({
            up <- do.call(
                what = enrichedGeneSets,
                args = c(args, direction = "up")
            )
        })
        ## Downregulated gene sets.
        suppressMessages({
            down <- do.call(
                what = enrichedGeneSets,
                args = c(args, direction = "down")
            )
        })
        assert(
            is.list(up),
            is.list(down),
            identical(names(up), names(down))
        )
        ## Loop across the contrasts.
        data <- object[[collection]]
        assert(identical(names(data), names(up)))
        invisible(Map(
            name = names(data), # contrast name
            data = data, # fgsta data.table output
            up = up, # upregulated pathways
            down = down, # downregulated pathways
            MoreArgs = list(
                "headerLevel" = headerLevel,
                "n" = n
            ),
            f = function(name,
                         data,
                         up,
                         down,
                         n,
                         headerLevel) {
                idCol <- "pathway"
                dropCols <- c("ES", "nMoreExtreme", "pval")
                markdownHeader(
                    text = name,
                    level = headerLevel,
                    asis = TRUE
                )
                data <- as(data, "DFrame")
                ## Sanitize and minimize the results before printing.
                ## Drop the nested list columns (e.g. leadingEdge).
                data <- selectIf(data, is.atomic)
                ## Drop additional uninformative columns.
                keep <- setdiff(colnames(data), dropCols)
                data <- data[, keep, drop = FALSE]
                ## Upregulated gene sets.
                markdownHeader(
                    text = "Upregulated",
                    level = headerLevel + 1L,
                    asis = TRUE
                )
                idx <- match(x = up, table = data[[idCol]])
                up <- data[idx, , drop = FALSE]
                up <- head(up, n = n)
                if (hasRows(up)) {
                    print(knitr::kable(as.data.frame(up), digits = 3L))
                } else {
                    alertInfo("No upregulated gene sets.") # nocov
                }
                ## Downregulated gene sets.
                markdownHeader(
                    text = "Downregulated",
                    level = headerLevel + 1L,
                    asis = TRUE
                )
                idx <- match(x = down, table = data[[idCol]])
                down <- data[idx, , drop = FALSE]
                down <- head(down, n = n)
                if (hasRows(down)) {
                    print(knitr::kable(as.data.frame(down), digits = 3L))
                } else {
                    alertInfo("No downregulated gene sets.") # nocov
                }
            }
        ))
    }



#' @rdname markdownTables
#' @export
setMethod(
    f = "markdownTables",
    signature = signature(object = "FgseaList"),
    definition = `markdownTables,FgseaList`
)
