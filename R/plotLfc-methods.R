#' @name plotLfc
#' @inherit AcidGenerics::plotLfc
#' @note Updated 2023-08-15.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param contrast `character` or `NULL`.
#' Contrast name.
#' If `NULL`, plot all contrasts.
#'
#' @param points `logical(1)`.
#' Show individual data points.
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' collection <- collectionNames(object)[[1L]]
#' set <- geneSetNames(object = object, collection = collection)[[1L]]
#' plotLfc(
#'     object = object,
#'     collection = collection,
#'     set = set
#' )
NULL



## Updated 2023-08-15.
`plotLfc,FgseaList` <- # nolint
    function(object,
             contrast = NULL,
             collection,
             set,
             geom = c("boxplot", "boxplot"),
             points = TRUE,
             labels = list(
                 "title" = "log2 fold change",
                 "subtitle" = NULL
             )) {
        validObject(object)
        if (is.null(contrast)) {
            contrast <- contrastNames(object)
        }
        assert(
            isCharacter(contrast),
            isSubset(contrast, contrastNames(object)),
            isFlag(points)
        )
        geom <- match.arg(geom)
        labels <- matchLabels(labels)
        suppressMessages({
            list <- Map(
                contrast = contrast,
                f = geneSetResults,
                MoreArgs = list(
                    "object" = object,
                    "collection" = collection,
                    "set" = set
                )
            )
        })
        lfc <- vapply(
            X = list,
            FUN = `[[`,
            "log2FoldChange",
            FUN.VALUE = numeric(length(list[[1L]][["log2FoldChange"]])),
            USE.NAMES = TRUE
        )
        assert(is.matrix(lfc))
        rownames(lfc) <- rownames(list[[1L]])
        data <- melt(lfc)
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[["colname"]],
                y = .data[["value"]]
            )
        )
        p <- p + switch(
            EXPR = geom,
            "boxplot" = geom_boxplot(
                mapping = aes(
                    color = .data[["colname"]]
                ),
                fill = NA,
                outlier.shape = NA,
                show.legend = FALSE
            ),
            "violin" = geom_violin(
                mapping = aes(
                    color = .data[["colname"]]
                ),
                fill = NA,
                show.legend = FALSE
            )
        )
        if (isTRUE(points)) {
            p <- p + geom_jitter(
                mapping = aes(color = .data[["colname"]]),
                show.legend = FALSE
            )
        }
        ## Labels.
        labels[["x"]] <- "contrast"
        labels[["y"]] <- "log2 fold change"
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + acid_scale_color_discrete()
        ## Return.
        p
    }



#' @rdname plotLfc
#' @export
setMethod(
    f = "plotLfc",
    signature = signature(object = "FgseaList"),
    definition = `plotLfc,FgseaList`
)
