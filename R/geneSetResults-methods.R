#' @name geneSetResults
#' @inherit AcidGenerics::geneSetResults
#' @note Updated 2022-04-27.
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
#' geneSetResults(
#'     object = object,
#'     contrast = contrast,
#'     collection = collection,
#'     set = set
#' )
NULL



## Updated 2021-03-16.
`geneSetResults,FgseaList` <- # nolint
    function(object,
             contrast,
             collection,
             set) {
        validObject(object)
        assert(
            isString(contrast),
            isSubset(contrast, contrastNames(object)),
            isString(collection),
            isSubset(collection, collectionNames(object)),
            isString(set)
        )
        genes <- geneSet(object, collection = collection, set = set)
        rownames <- .matchGenesToIds(object, set = set, genes = genes)
        if (!hasLength(rownames)) {
            return(NULL)
        }
        deseq <- .getDESeqAnalysis(object)
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
    signature = signature(object = "FgseaList"),
    definition = `geneSetResults,FgseaList`
)
