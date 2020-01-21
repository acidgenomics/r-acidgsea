#' @name plotCounts
#' @inherit acidplots::plotCounts description params return title
#' @note Updated 2020-01-20.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
NULL



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom acidgenerics plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



## Updated 2019-11-19.
`plotCounts,FGSEAList` <-  # nolint
    function(
        object,
        DESeqAnalysis,  # nolint
        contrast,
        collection,
        set,
        n = 12L,
        contrastSamples = TRUE,
        line = "mean",
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
            isScalar(collection),
            isString(set),
            isInt(n),
            isFlag(contrastSamples)
        )
        ## Match collection to name, if necessary.
        if (!isString(collection)) {
            collection <- names(object)[[collection]]
        }
        ## Match contrast to name, if necessary.
        if (!isString(contrast)) {
            contrast <- names(object[[collection]])[[contrast]]
        }
        genes <- .leadingEdge(
            object = object,
            contrast = contrast,
            collection = collection,
            set = set
        )
        genes <- head(genes, n = n)
        ## Plot the log counts from DESeqTransform object.
        dt <- as(DESeqAnalysis, "DESeqTransform")
        if (isTRUE(contrastSamples)) {
            colnames <- contrastSamples(DESeqAnalysis, i = contrast)
            dt <- dt[, colnames, drop = FALSE]
        }
        ## Using DESeqTransform method defined in DESeqAnalysis.
        args <- list(
            object = dt,
            genes = genes,
            line = line,
            labels = list(
                title = set,
                subtitle = paste(collection, contrast, sep = "  |  ")
            )
        )
        args <- c(args, list(...))
        do.call(what = plotHeatmap, args = args)
    }



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("FGSEAList"),
    definition = `plotCounts,FGSEAList`
)
