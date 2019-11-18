#' @name plotCounts
#' @inherit acidplots::plotCounts description return title
#' @note Updated 2019-11-18.
#'
#' @inheritParams acidroxygen::params
#'
#' @param DESeqAnalysis `DESeqAnalysis`.
#'   Corresponding DESeq2 data used to perform GSEA.
#' @param contrast `character(1)` or `integer(1)`.
#'   DESeqResults contrast.
#' @param contrastSamples `logical(1)`.
#'   Only visualize the samples defined in the contrast.
#' @param collection `character(1)` or `integer(1)`.
#'   Collection name or position, corresponding to values defined in
#'   [`names()`][base::names].
#'   For example, `"c1"` or `"h"` (for hallmark).
#' @param set `character(1)`.
#'   Gene set name, in a defined `collection`.
#'   For example, `"HALLMARK_ADIPOGENESIS"`.
#'
#' @param ... Additional arguments.
NULL



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom bioverbs plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



## Updated 2019-11-18.
`plotCounts,FGSEAList` <-  # nolint
    function(
        object,
        DESeqAnalysis,
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
        ## Plot the log counts from DESeqTransform object.
        dt <- as(DESeqAnalysis, "DESeqTransform")
        ## Match collection to name, if necessary.
        if (!isString(collection)) {
            collection <- names(object)[[collection]]
        }
        ## Match contrast to name, if necessary.
        if (!isString(contrast)) {
            contrast <- names(object[[collection]])[[contrast]]
        }
        ## Extract the gene symbols from the gene set.
        data <- object[[collection]][[contrast]]
        assert(is(data, "data.table"))
        ## Coerce to DataFrame, to use standard subsetting syntax.
        data <- as(data, "DataFrame")
        keep <- match(set, table = data[["pathway"]])
        if (!isInt(keep)) {
            ## FIXME Tell the user which collection.
            stop(sprintf("Failed to match '%s' set.", set))
        }
        genes <- unlist(unname(data[keep, "leadingEdge"]))
        genes <- head(genes, n = n)
        if (isTRUE(contrastSamples)) {
            colnames <- contrastSamples(DESeqAnalysis, i = contrast)
            dt <- dt[, colnames, drop = FALSE]
        }
        plotCounts(
            object = dt,
            genes = genes,
            line = line,
            labels = list(
                title = set,
                subtitle = paste(collection, contrast, sep = " | ")
            ),
            ...
        )
    }



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("FGSEAList"),
    definition = `plotCounts,FGSEAList`
)
