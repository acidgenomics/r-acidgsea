#' @name geneSetResults
#' @inherit acidgenerics::geneSetResults
#' @note Updated 2020-09-23.
#' @inheritParams params
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#' geneSetResults(
#'     object = fgsea,
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



## Updated 2020-09-23.
`geneSetResults,FGSEAList` <-  # nolint
    function(
        object,
        contrast,
        collection,
        set
    ) {
        validObject(object)
        assert(
            isString(contrast),
            isSubset(contrast, contrastNames(object)),
            isString(collection),
            isSubset(collection, collectionNames(object)),
            isString(set)
        )
        deseq <- `DESeqAnalysis,FGSEAList`(object)
        res <- results(deseq, i = contrast, extra = TRUE)
        genes <- geneSet(object, collection = collection, set = set)
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
