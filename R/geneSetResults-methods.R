#' @name geneSetResults
#' @inherit acidgenerics::geneSetResults
#' @note Updated 2020-09-21.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea, deseq)
#' geneSetResults(
#'     object = fgsea,
#'     DESeqAnalysis = deseq,
#'     contrast = "condition_B_vs_A",
#'     collection = "h",
#'     set = "HALLMARK_P53_PATHWAY"
#' )
NULL



#' @rdname geneSetResults
#' @name geneSetResults
#' @importFrom acidgenerics geneSetResults
#' @usage geneSetResults(object, ...)
#' @export
NULL



## Updated 2020-09-21.
`geneSetResults,FGSEAList` <-  # nolint
    function(
        object,
        DESeqAnalysis,
        contrast,
        collection,
        set
    ) {
        deseq <- DESeqAnalysis
        validObject(object)
        validObject(deseq)
        assert(
            isString(contrast),
            isSubset(contrast, contrastNames(object)),
            isString(collection),
            isSubset(collection, collectionNames(object)),
            isString(set)
        )
        res <- results(
            object = deseq,
            i = contrast,
            extra = TRUE
        )
        genes <- geneSet(
            object = object,
            collection = collection,
            set = set
        )
        ddsSubset <- .matchGeneSet(
            object = as(deseq, "DESeqDataSet"),
            set = set,
            genes = genes
        )
        rownames <- rownames(ddsSubset)
        out <- res[rownames, ]
        rl <- RankedList(object)
        valueCol <- metadata(rl)[["value"]]
        assert(
            isSubset(valueCol, colnames(out)),
            isSubset(out[["geneName"]], names(rl[[contrast]]))
        )
        cli_alert(sprintf("Arranging by {.var %s} column.", valueCol))
        idx <- order(out[[valueCol]], decreasing = TRUE)
        out <- out[idx, ]
        out
    }



#' @rdname geneSetResults
#' @export
setMethod(
    f = "geneSetResults",
    signature = signature("FGSEAList"),
    definition = `geneSetResults,FGSEAList`
)
