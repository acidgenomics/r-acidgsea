#' @rdname FGSEAList
#' @export
setGeneric(
    name = "FGSEAList",
    def = function(object, ...) {
        standardGeneric("FGSEAList")
    }
)

#' @rdname RankedList
#' @export
setGeneric(
    name = "RankedList",
    def = function(object, ...) {
        standardGeneric("RankedList")
    }
)



#' @rdname Gene2Symbol
#' @name Gene2Symbol
#' @importFrom basejump Gene2Symbol
#' @usage Gene2Symbol(object, ...)
#' @export
NULL

#' @rdname alphaThreshold
#' @name alphaThreshold
#' @importFrom AcidGenerics alphaThreshold
#' @usage alphaThreshold(object, ...)
#' @export
NULL

#' @rdname alphaThreshold
#' @name alphaThreshold<-
#' @importFrom AcidGenerics alphaThreshold<-
#' @usage alphaThreshold(object, ...) <- value
#' @export
NULL

#' @rdname collectionNames
#' @name collectionNames
#' @importFrom AcidGenerics collectionNames
#' @usage collectionNames(object, ...)
#' @export
NULL

#' @rdname collectionNames
#' @name collectionNames<-
#' @importFrom AcidGenerics collectionNames<-
#' @usage collectionNames(object, ...) <- value
#' @export
NULL

#' @rdname combine
#' @name combine
#' @importFrom AcidGenerics combine
#' @usage combine(x, y, ...)
#' @export
NULL

#' @rdname contrastNames
#' @name contrastNames
#' @importFrom AcidGenerics contrastNames
#' @usage contrastNames(object, ...)
#' @export
NULL

#' @rdname contrastNames
#' @name contrastNames<-
#' @importFrom AcidGenerics contrastNames<-
#' @usage contrastNames(object, ...) <- value
#' @export
NULL

#' @rdname convertToHuman
#' @name convertToHuman
#' @importFrom AcidGenerics convertToHuman
#' @usage convertToHuman(object, ...)
#' @export
NULL

#' @rdname enrichedGeneSets
#' @name enrichedGeneSets
#' @importFrom AcidGenerics enrichedGeneSets
#' @usage enrichedGeneSets(object, ...)
#' @export
NULL

#' @rdname export
#' @name export
#' @importFrom AcidGenerics export
#' @usage export(object, con, format, ...)
#' @export
NULL

#' @rdname geneSet
#' @name geneSet
#' @importFrom AcidGenerics geneSet
#' @usage geneSet(object, ...)
#' @export
NULL

#' @rdname geneSetNames
#' @name geneSetNames
#' @importFrom AcidGenerics geneSetNames
#' @usage geneSetNames(object, ...)
#' @export
NULL

#' @rdname geneSetResults
#' @name geneSetResults
#' @importFrom AcidGenerics geneSetResults
#' @usage geneSetResults(object, ...)
#' @export
NULL

#' @rdname leadingEdge
#' @name leadingEdge
#' @importFrom AcidGenerics leadingEdge
#' @usage leadingEdge(object, ...)
#' @export
NULL

#' @rdname nesThreshold
#' @name nesThreshold
#' @importFrom AcidGenerics nesThreshold
#' @usage nesThreshold(object, ...)
#' @export
NULL

#' @rdname nesThreshold
#' @name nesThreshold<-
#' @importFrom AcidGenerics nesThreshold<-
#' @usage nesThreshold(object, ...) <- value
#' @export
NULL

#' @rdname plotEnrichedGeneSets
#' @name plotEnrichedGeneSets
#' @importFrom AcidGenerics plotEnrichedGeneSets
#' @usage plotEnrichedGeneSets(object, ...)
#' @export
NULL

#' @rdname plotEnrichedUpset
#' @name plotEnrichedUpset
#' @importFrom AcidGenerics plotEnrichedUpset
#' @usage plotEnrichedUpset(object, ...)
#' @export
NULL

#' @rdname plotGeneSet
#' @name plotGeneSet
#' @importFrom AcidGenerics plotGeneSet
#' @usage plotGeneSet(object, ...)
#' @export
NULL

#' @rdname plotHeatmap
#' @name plotHeatmap
#' @importFrom AcidGenerics plotHeatmap
#' @usage plotHeatmap(object, ...)
#' @export
NULL

#' @rdname plotLFC
#' @name plotLFC
#' @importFrom AcidGenerics plotLFC
#' @usage plotLFC(object, ...)
#' @export
NULL

#' @rdname plotNES
#' @name plotNES
#' @importFrom AcidGenerics plotNES
#' @usage plotNES(object, ...)
#' @export
NULL

#' @rdname results
#' @name results
#' @importFrom AcidGenerics results
#' @usage results(object, ...)
#' @export
NULL

#' @rdname topTables
#' @name topTables
#' @importFrom AcidGenerics topTables
#' @usage topTables(object, ...)
#' @export
NULL

#' @rdname updateObject
#' @name updateObject
#' @importFrom AcidGenerics updateObject
#' @usage updateObject(object, ..., verbose = FALSE)
#' @export
NULL
