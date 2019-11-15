## FIXME This isn't fully implemented yet



#' @name plotCounts
#' @inherit acidplots::plotCounts description return title
#' @note Updated 2019-11-15.
#'
#' @inheritParams acidroxygen::params
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

## FIXME For this plot, just plot the mean without the dots.
## Consider connecting the points by a line.
## Let's just write custom ggplot code here to handle it ourselves.



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
        dt <- as(DESeqAnalysis, "DESeqTransform")
        map <- mapGenesToRownames(object = dt, genes = genes)
        dt <- dt[map, , drop = FALSE]

        res <- DESeqAnalysis@results[[1]]
        res <- res[map, , drop = FALSE]

        ## Now we need to average the expression for all genes, and plot
        ## as an X-Y scatter with interesting groups on the X axis (e.g.
        ## compound treatment), with the distribution of replicates shown on the
        ## Y axis, which are the average expression of the normalized counts.

        ## FIXME Currently this errors out for more than 20 genes...
        ## FIXME May need to add an average per sample here?
        ## Need to think of an aggregate approach here.

        p <- plotCounts(
            object = dt,
            genes = head(genes, n = 12L),
            style = "wide",
            interestingGroups = c(
                "vector",
                "dox",
                "compound"
            ),
            line = "median"
        )

        pdf(
            file = "~/gsea-interferon-1.pdf",
            width = 10,
            height = 10
        )
        p
        dev.off()

        ## Strip the points from the plot, which are too busy.
        ## Find the crossbar layer (median line) and only use that here.
        p$layers <- p$layers[2]

        pdf(
            file = "~/gsea-interferon-2.pdf",
            width = 10,
            height = 10
        )
        p
        dev.off()



        ## Extract the symbols from the gene set.
        ## Alternatively can parse:
        ## > metadata(object)[["gmtFiles"]]
    }



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("FGSEAList"),
    definition = `plotCounts,FGSEAList`
)
