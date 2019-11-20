#' @name plotHeatmap
#' @inherit acidplots::plotHeatmap description return title
#' @note Updated 2019-11-19.
#'
#' @inheritParams acidroxygen::params
#' @param leadingEdge `logical(1)`.
#'   Visualize only the leading edge genes returned by GSEA. If `FALSE`, plot
#'   all genes in the gene set, which requires the external gene set file that
#'   was originally used to run the analysis. This file is referenced internally
#'   inside the object at `metadata(object)[["gmtFiles"]]`.
#' @param ... Additional arguments.
NULL



#' @rdname plotHeatmap
#' @name plotHeatmap
#' @importFrom bioverbs plotHeatmap
#' @usage plotHeatmap(object, ...)
#' @export
NULL



## Updated 2019-11-19.
`plotHeatmap,FGSEAList` <-  # nolint
    function(
        object,
        DESeqAnalysis,  # nolint
        contrast,
        contrastSamples = TRUE,
        collection,
        set,
        leadingEdge = TRUE,
        ...
    ) {
        validObject(object)
        validObject(DESeqAnalysis)
        assert(
            is(DESeqAnalysis, "DESeqAnalysis"),
            identical(
                x = names(object[[1L]]),
                y = contrastNames(DESeqAnalysis)
            ),
            isScalar(contrast),
            isFlag(contrastSamples),
            isScalar(collection),
            isString(set),
            isFlag(leadingEdge)
        )
        ## Match collection to name, if necessary.
        if (!isString(collection)) {
            collection <- names(object)[[collection]]
        }
        ## Match contrast to name, if necessary.
        if (!isString(contrast)) {
            contrast <- names(object[[collection]])[[contrast]]
        }
        ## Map the genes we want to plot to the DESeq data.
        if (isTRUE(leadingEdge)) {
            genes <- .leadingEdge(
                object = object,
                contrast = contrast,
                collection = collection,
                set = set
            )
        } else {
            ## Locate the GMT file used to run GSEA.
            gmt <- import(file = metadata(object)[["gmtFiles"]][[collection]])
            genes <- gmt[[set]]
        }
        ## Plot the log counts from DESeqTransform object.
        dt <- as(DESeqAnalysis, "DESeqTransform")
        rownames <- mapGenesToRownames(object = dt, genes = genes)
        dt <- dt[rownames, , drop = FALSE]
        if (isTRUE(contrastSamples)) {
            colnames <- contrastSamples(DESeqAnalysis, i = contrast)
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
