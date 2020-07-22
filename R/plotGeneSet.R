#' Plot gene set enrichment
#'
#' @name plotGeneSet
#' @note Updated 2020-07-22.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
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
