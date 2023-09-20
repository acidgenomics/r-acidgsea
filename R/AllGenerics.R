#' @export
#' @rdname FGSEAList
setGeneric(
    name = "FGSEAList",
    def = function(object, ...) {
        standardGeneric("FGSEAList")
    }
)

#' @export
#' @name Gene2Symbol
#' @rdname Gene2Symbol
#' @usage Gene2Symbol(object, ...)
NULL

#' @export
#' @rdname RankedList
setGeneric(
    name = "RankedList",
    def = function(object, ...) {
        standardGeneric("RankedList")
    }
)

#' @aliases alphaThreshold<-
#' @export alphaThreshold alphaThreshold<-
#' @name alphaThreshold
#' @rdname alphaThreshold
#' @usage
#' alphaThreshold(object, ...)
#' alphaThreshold(object, ...) <- value
NULL

#' @aliases collectionNames<-
#' @export collectionNames collectionNames<-
#' @name collectionNames
#' @rdname collectionNames
#' @usage
#' collectionNames(object, ...)
#' collectionNames(object, ...) <- value
NULL

#' @export
#' @name combine
#' @rdname combine
#' @usage combine(x, y, ...)
NULL

#' @aliases contrastNames<-
#' @export contrastNames contrastNames<-
#' @name contrastNames
#' @rdname contrastNames
#' @usage
#' contrastNames(object, ...)
#' contrastNames(object, ...) <- value
NULL

#' @export
#' @name convertToHuman
#' @rdname convertToHuman
#' @usage convertToHuman(object, ...)
NULL

#' @export
#' @name enrichedGeneSets
#' @rdname enrichedGeneSets
#' @usage enrichedGeneSets(object, ...)
NULL

#' @export
#' @name export
#' @rdname export
#' @usage export(object, con, ...)
NULL

#' @export
#' @name geneSet
#' @rdname geneSet
#' @usage geneSet(object, ...)
NULL

#' @export
#' @name geneSetNames
#' @rdname geneSetNames
#' @usage geneSetNames(object, ...)
NULL

#' @export
#' @name geneSetResults
#' @rdname geneSetResults
#' @usage geneSetResults(object, ...)
NULL

#' @export
#' @name leadingEdge
#' @rdname leadingEdge
#' @usage leadingEdge(object, ...)
NULL

#' @export
#' @name markdownTables
#' @rdname markdownTables
#' @usage markdownTables(object, ...)
NULL

#' @aliases nesThreshold<-
#' @export nesThreshold nesThreshold<-
#' @name nesThreshold
#' @rdname nesThreshold
#' @usage
#' nesThreshold(object, ...)
#' nesThreshold(object, ...) <- value
NULL

#' @export
#' @name plotEnrichedGeneSets
#' @rdname plotEnrichedGeneSets
#' @usage plotEnrichedGeneSets(object, ...)
NULL

#' @export
#' @name plotEnrichedUpset
#' @rdname plotEnrichedUpset
#' @usage plotEnrichedUpset(object, ...)
NULL

#' @export
#' @name plotGeneSet
#' @rdname plotGeneSet
#' @usage plotGeneSet(object, ...)
NULL

#' @export
#' @name plotHeatmap
#' @rdname plotHeatmap
#' @usage plotHeatmap(object, ...)
NULL

#' @export
#' @name plotLFC
#' @rdname plotLFC
#' @usage plotLFC(object, ...)
NULL

#' @export
#' @name plotNES
#' @rdname plotNES
#' @usage plotNES(object, ...)
NULL

#' @export
#' @name results
#' @rdname results
#' @usage results(object, ...)
NULL

#' @export
#' @name updateObject
#' @rdname updateObject
#' @usage updateObject(object, ..., verbose = FALSE)
NULL
