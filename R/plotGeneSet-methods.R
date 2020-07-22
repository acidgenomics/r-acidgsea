## FIXME Rework internal code using fgsea::plotEnrichment as a guide.
## That code currently hard-codes theme_bw.



#' Plot gene set enrichment
#'
#' @name plotGeneSet
#' @note Updated 2020-07-22.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @seealso [fgsea::plotEnrichment()].
#'
#' @examples
#' ## This requires MSigDB to be installed at `${HOME}`.
#' if (isTRUE(dir.exists(file.path("~", "msigdb")))) {
#'     data(fgsea)
#'     plotGeneSet(
#'         object = fgsea,
#'         collection = "h",
#'         contrast = "condition_B_vs_A",
#'         set = "HALLMARK_P53_PATHWAY"
#'     )
#' }
NULL



`plotGeneSet,FGSEAList` <-
    function(
        object,
        collection,
        contrast,
        set
    ) {
        validObject(object)
        assert(
            isString(collection),
            isSubset(collection, names(object))
        )
        data <- object[[collection]]
        rankedList <- RankedList(object)
        assert(
            isString(contrast),
            isSubset(contrast, names(rankedList))
        )
        gmtFile <- metadata(object)[["gmtFiles"]][[collection]]
        assert(
            identical(names(data), names(rankedList)),
            isAFile(gmtFile)
        )
        pathways <- gmtPathways(gmt.file = gmtFile)
        assert(
            isString(set),
            isSubset(set, names(pathways))
        )
        pathway <- pathways[[set]]
        stats <- rankedList[[contrast]]
        p <- fgsea::plotEnrichment(pathway = pathway, stats = stats)
        assert(is(p, "ggplot"))
        p <- p + labs(
            title = set,
            subtitle = paste(collection, contrast, sep = " : ")
        )
        p
    }



#' @rdname plotGeneSet
#' @export
setMethod(
    f = "plotGeneSet",
    signature = signature("FGSEAList"),
    definition = `plotGeneSet,FGSEAList`
)
