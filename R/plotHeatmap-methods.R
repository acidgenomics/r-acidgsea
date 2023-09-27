#' @name plotHeatmap
#' @inherit AcidPlots::plotHeatmap description return title
#' @note Updated 2021-03-16.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' contrast <- contrastNames(object)[[1L]]
#' collection <- collectionNames(object)[[1L]]
#' set <- geneSetNames(object = object, collection = collection)[[1L]]
#' plotHeatmap(
#'     object = object,
#'     contrast = contrast,
#'     collection = collection,
#'     set = set
#' )
NULL



## Updated 2021-03-16.
`plotHeatmap,FgseaList` <- # nolint
    function(object,
             contrast,
             contrastSamples = TRUE,
             collection,
             set,
             leadingEdge = FALSE,
             ...) {
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
            genes <- NULL
        }
        ## Plot the log counts from DESeqTransform object.
        genes <- geneSet(object, collection = collection, set = set)
        rownames <- .matchGenesToIds(object, set = set, genes = genes)
        deseq <- .getDESeqAnalysis(object)
        dt <- as(deseq, "DESeqTransform")
        dt <- dt[rownames, ]
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
    signature = signature(object = "FgseaList"),
    definition = `plotHeatmap,FgseaList`
)
