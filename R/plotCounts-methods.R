## FIXME What's up with this?
##
# plotCounts(
#     object = gsea,
#     DESeqAnalysis = deseq_analysis,
#     contrast = "day1_ts001637_0_1_vs_dmso",
#     collection = "h",
#     set = "HALLMARK_MYC_TARGETS_V1",
#     interestingGroups = "compound"
# )
# Contrast: group_day1_ts001637_0_1_vs_day1_dmso_na
# Factor column: group.
# Numerator samples: x30194_016, x30194_017, x30194_018.
# Denominator samples: x30194_019, x30194_020, x30194_021.
# Scaling matrix per row (z-score).
# 19872 rows don't have enough variance: ENSG00000000005, ENSG00000000971, ENSG00000001626, ENSG00000002079, ENSG00000002745, ENSG00000002933, ENSG00000003137, ENSG00000003987, ENSG00000004799, ENSG00000004809, ENSG00000004948, ENSG00000.....
# Performing hierarchical clustering.
# Using 'stats::hclust(method = "ward.D2")'.
# Arranging rows using 'hclust()'.
# Arranging columns using 'hclust()'.
# Error in .pheatmapArgs(args) : Assert failure.
# [1] isSubset(names(args), formalArgs(pheatmap)) is not TRUE.
# Cause: 'names(args)' has elements not in 'formalArgs(pheatmap)': genes, line, labels
# Calls: plotCounts ... <Anonymous> -> <Anonymous> -> .local -> .pheatmapArgs -> assert



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
