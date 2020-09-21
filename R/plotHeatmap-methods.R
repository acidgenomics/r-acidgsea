#' @name plotHeatmap
#' @inherit acidplots::plotHeatmap description return title
#' @note Updated 2020-09-21.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea, deseq)
#' plotHeatmap(
#'     object = fgsea,
#'     DESeqAnalysis = deseq,
#'     contrast = "condition_B_vs_A",
#'     collection = "h",
#'     set = "HALLMARK_P53_PATHWAY"
#' )
NULL



#' @rdname plotHeatmap
#' @name plotHeatmap
#' @importFrom acidgenerics plotHeatmap
#' @usage plotHeatmap(object, ...)
#' @export
NULL



## Updated 2020-09-21.
`plotHeatmap,FGSEAList` <-  # nolint
    function(
        object,
        DESeqAnalysis,  # nolint
        contrast,
        contrastSamples = TRUE,
        collection,
        set,
        leadingEdge = FALSE,
        ...
    ) {
        deseq <- DESeqAnalysis
        validObject(object)
        validObject(deseq)
        assert(
            is(deseq, "DESeqAnalysis"),
            identical(
                x = names(object[[1L]]),
                y = contrastNames(deseq)
            ),
            isString(contrast),
            isFlag(contrastSamples),
            isString(collection),
            isString(set),
            isFlag(leadingEdge)
        )
        ## Map the genes we want to plot to the DESeq data.
        if (isTRUE(leadingEdge)) {
            genes <- leadingEdge(
                object = object,
                contrast = contrast,
                collection = collection,
                set = set
            )
        } else {
            genes <- geneSet(
                object = object,
                collection = collection,
                set = set
            )
        }
        ## Plot the log counts from DESeqTransform object.
        dt <- as(deseq, "DESeqTransform")
        dt <- .matchGeneSet(object = dt, genes = genes)
        if (isTRUE(contrastSamples)) {
            colnames <- contrastSamples(deseq, i = contrast)
            dt <- dt[, colnames, drop = FALSE]
        }
        args <- list(
            object = as(dt, "RangedSummarizedExperiment"),
            title = paste0(set, "\n", collection, "  |  ", contrast)
        )
        args <- c(args, list(...))
        do.call(what = plotHeatmap, args = args)
    }



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("FGSEAList"),
    definition = `plotHeatmap,FGSEAList`
)
