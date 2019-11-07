#' @name plotCounts
#' @inherit acidplots::plotCounts
#' @note Updated 2019-11-07.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#'
#' @param collection `character(1)` or `integer(1)`.
#'   Collection name or position, corresponding to values defined in
#'   [`names()`][base::names].
#'   For example, `"c1"` or `"h"` (for hallmark).
#' @param contrast `character(1)` or `integer(1)`.
#'   DESeqResults contrast.
#' @param set `character(1)`.
#'   Gene set name, in a defined `collection`.
#'   For example, `"HALLMARK_ADIPOGENESIS"`.
#' @param DESeqAnalysis `DESeqAnalysis`.
#'   Corresponding DESeq2 data used to perform GSEA.
#' @param samples `character` or `NULL`.
#'   Specific samples to plot.
#'   If `NULL` (default), include all samples.
#'
#' @param ... Additional arguments.
NULL



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom bioverbs plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



## Need to average the data here, to reduce the amount of information.
## Can average per sample or per gene.
## Not sure which approach is best yet.



`plotCounts,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        contrast,
        set,
        DESeqAnalysis,
        samples = NULL
    ) {
        validObject(object)
        validObject(DESeqAnalysis)
        assert(
            isScalar(collection),
            isScalar(contrast),
            isString(set),
            is(DESeqAnalysis, "DESeqAnalysis"),
            isCharacter(samples, nullOK = TRUE)
        )

        ## Extract the gene symbols from the gene set.
        data <- object[[collection]][[contrast]]
        assert(is(data, "data.table"))
        keep <- match(x = set, table = data[["pathway"]])
        assert(isInt(keep))
        genes <- unlist(unname(data[keep, "leadingEdge"]))

        ## Now we need to map the GSEA gene symbols back to the corresponding
        ## DESeqDataSet row names (gene identifiers).
        dds <- as(DESeqAnalysis, "DESeqDataSet")
        ## > map <- mapGenesToRownames(object = dds, genes = symbols)
        ## > dds <- dds[map, ]

        ## FIXME Currently this errors out for more than 20 genes...
        ## FIXME May need to add an average per sample here?
        ## Need to think of an aggregate approach here.
        plotCounts(
            object = dds,
            genes = head(genes, n = 2),
            style = "facet",
            interestingGroups = c(
                "vector",
                "dox",
                "compound"
            )
        ) +
            ## FIXME Need to define this utility in acidplots.
            ## Quick functions to hide x, y, or both axes.
            ggplot2::theme(
                axis.title.x = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )

        ## Extract the symbols from the gene set.
        ## Alternatively can parse:
        ## > metadata(object)[["gmtFiles"]]
    }
