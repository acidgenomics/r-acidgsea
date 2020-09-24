#' @name geneSetResults
#' @inherit acidgenerics::geneSetResults
#' @note Updated 2020-09-24.
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



## Updated 2020-09-24.
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
        genes <- geneSet(object, collection = collection, set = set)
        rownames <- .matchGenesToIDs(object, set = set, genes = genes)
        if (!hasLength(rownames)) return(NULL)
        deseq <- `DESeqAnalysis,FGSEAList`(object)
        suppressMessages({
            res <- results(object = deseq, i = contrast, extra = TRUE)
        })
        res <- res[rownames, ]
        res <- res[!is.na(res[["padj"]]), ]
        rl <- RankedList(object)
        valueCol <- metadata(rl)[["value"]]
        assert(
            isSubset(valueCol, colnames(res)),
            isSubset(res[["geneName"]], names(rl[[contrast]]))
        )
        idx <- order(res[[valueCol]], decreasing = TRUE)
        res <- res[idx, ]
        res
    }



#' @rdname geneSetResults
#' @export
setMethod(
    f = "geneSetResults",
    signature = signature("FGSEAList"),
    definition = `geneSetResults,FGSEAList`
)
