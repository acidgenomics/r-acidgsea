#' @name plotHeatmap
#' @inherit acidplots::plotHeatmap description return title
#' @note Updated 2019-11-18.
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



## Updated 2019-11-18.
`plotHeatmap,FGSEAList` <-  # nolint
    function(
        object,
        DESeqAnalysis,
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












    }



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("FGSEAList"),
    definition = `plotHeatmap,FGSEAList`
)
