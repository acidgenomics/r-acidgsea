#' @name plotLFC
#' @inherit AcidGenerics::plotLFC
#' @note Updated 2020-10-01.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param contrast `character` or `NULL`.
#'   Contrast name.
#'   If `NULL`, plot all contrasts.
#' @param points `logical(1)`.
#'   Show individual data points.
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#' plotLFC(
#'     object = fgsea,
#'     collection = "h",
#'     set = "HALLMARK_P53_PATHWAY"
#' )
NULL



## Updated 2020-09-23.
`plotLFC,FGSEAList` <-  # nolint
    function(
        object,
        contrast = NULL,
        collection,
        set,
        geom = c("boxplot", "boxplot"),
        points = TRUE,
        color
    ) {
        validObject(object)
        if (is.null(contrast)) {
            contrast <- contrastNames(object)
        }
        assert(
            isCharacter(contrast),
            isSubset(contrast, contrastNames(object)),
            isGGScale(color, scale = "discrete", aes = "colour", nullOK = TRUE),
            isFlag(points)
        )
        geom <- match.arg(geom)
        suppressMessages({
            list <- mapply(
                contrast = contrast,
                FUN = geneSetResults,
                MoreArgs = list(
                    object = object,
                    collection = collection,
                    set = set
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
                color = "black",
                fill = NA,
                outlier.shape = NA
            ),
            "violin" = geom_violin(
                color = "black",
                fill = NA
            )
        )
        if (isTRUE(points)) {
            p <- p + geom_jitter(
                mapping = aes(color = !!sym("colname")),
                show.legend = FALSE
            )
        }
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        p <- p +
            labs(
                x = "contrast",
                y = "log2 fold change"
            )
        p
    }

formals(`plotLFC,FGSEAList`)[["color"]] <- formalsList[["color.discrete"]]



#' @rdname plotLFC
#' @export
setMethod(
    f = "plotLFC",
    signature = signature("FGSEAList"),
    definition = `plotLFC,FGSEAList`
)
