#' @name plotLFC
#' @inherit AcidGenerics::plotLFC
#' @note Updated 2021-09-10.
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
#' ## FGSEAList ====
#' data(fgsea)
#' plotLFC(
#'     object = fgsea,
#'     collection = "h",
#'     set = "HALLMARK_P53_PATHWAY"
#' )
NULL



## Updated 2021-09-10.
`plotLFC,FGSEAList` <- # nolint
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
            list <- mapply(
                contrast = contrast,
                FUN = geneSetResults,
                MoreArgs = list(
                    "object" = object,
                    "collection" = collection,
                    "set" = set
                ),
                SIMPLIFY = FALSE,
                USE.NAMES = TRUE
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
        data <- as_tibble(melt(lfc))
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("colname"),
                y = !!sym("value")
            )
        )
        p <- p + switch(
            EXPR = geom,
            "boxplot" = geom_boxplot(
                mapping = aes(
                    color = !!sym("colname")
                ),
                fill = NA,
                outlier.shape = NA,
                show.legend = FALSE
            ),
            "violin" = geom_violin(
                mapping = aes(
                    color = !!sym("colname")
                ),
                fill = NA,
                show.legend = FALSE
            )
        )
        if (isTRUE(points)) {
            p <- p + geom_jitter(
                mapping = aes(color = !!sym("colname")),
                show.legend = FALSE
            )
        }
        ## Labels.
        labels[["x"]] <- "contrast"
        labels[["y"]] <- "log2 fold change"
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + autoDiscreteColorScale()
        ## Return.
        p
    }



#' @rdname plotLFC
#' @export
setMethod(
    f = "plotLFC",
    signature = signature(object = "FGSEAList"),
    definition = `plotLFC,FGSEAList`
)
