#' @name topTables
#' @inherit acidgenerics::topTables
#' @note Updated 2020-03-18.
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
#' data(fgsea)
#' topTables(fgsea, collection = "h")
NULL



#' @rdname topTables
#' @name topTables
#' @importFrom acidgenerics topTables
#' @usage topTables(object, ...)
#' @export
NULL



## Updated 2020-03-18.
`topTables,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        alpha = NULL,
        nesThreshold = NULL,
        n = 10L,
        headerLevel = 3L
    ) {
        validObject(object)
        assert(
            isScalar(collection),
            isInt(n),
            isHeaderLevel(headerLevel)
        )
        args <- list(
            object = object,
            collection = collection,
            alpha = alpha,
            nesThreshold = nesThreshold
        )
        ## Upregulated gene sets.
        suppressMessages(
            up <- do.call(
                what = enrichedGeneSets,
                args = c(args, direction = "up")
            )
        )
        ## Downregulated gene sets.
        suppressMessages(
            down <- do.call(
                what = enrichedGeneSets,
                args = c(args, direction = "down")
            )
        )
        assert(
            is.list(up),
            is.list(down),
            identical(names(up), names(down))
        )
        ## Loop across the contrasts.
        data <- object[[collection]]
        assert(identical(names(data), names(up)))
        invisible(mapply(
            name = names(data),     # contrast name
            data = data,            # fgsta data.table output
            up = up,                # upregulated pathways
            down = down,            # downregulated pathways
            MoreArgs = list(
                headerLevel = headerLevel,
                n = n
            ),
            FUN = function(
                name,
                data,
                up,
                down,
                n,
                headerLevel
            ) {
                idCol <- "pathway"
                dropCols <- c("ES", "nMoreExtreme", "pval")
                markdownHeader(
                    text = name,
                    level = headerLevel,
                    asis = TRUE
                )
                data <- as(data, "DataFrame")
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
                    print(kable(as.data.frame(up), digits = 3L))
                } else {
                    cli_alert_info("No upregulated gene sets.")  # nocov
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
                    print(kable(as.data.frame(down), digits = 3L))
                } else {
                    cli_alert_info("No downregulated gene sets.")  # nocov
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
