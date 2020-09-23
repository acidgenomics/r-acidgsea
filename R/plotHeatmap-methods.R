#' @name plotHeatmap
#' @inherit acidplots::plotHeatmap description return title
#' @note Updated 2020-09-23.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#' plotHeatmap(
#'     object = fgsea,
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



## Updated 2020-09-23.
`plotHeatmap,FGSEAList` <-  # nolint
    function(
        object,
        contrast,
        contrastSamples = TRUE,
        collection,
        set,
        leadingEdge = FALSE,
        ...
    ) {
        validObject(object)
        assert(
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
        deseq <- `DESeqAnalysis,FGSEAList`(object)
        dt <- as(deseq, "DESeqTransform")
        dt <- .matchGeneSet(
            object = dt,
            set = set,
            genes = genes
        )
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
