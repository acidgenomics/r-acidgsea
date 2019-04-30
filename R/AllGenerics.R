#' @rdname RankedList
#' @export
setGeneric(
    name = "RankedList",
    def = function(object, ...) {
        standardGeneric("RankedList")
    }
)



#' @rdname collectionNames
#' @export
setGeneric(
    name = "collectionNames",
    def = function(object, ...) {
        standardGeneric("collectionNames")
    }
)



#' @rdname collectionNames
#' @export
setGeneric(
    name = "collectionNames<-",
    def = function(object, ..., value) {
        standardGeneric("collectionNames<-")
    }
)



#' @rdname contrastNames
#' @export
setGeneric(
    name = "contrastNames",
    def = function(object, ...) {
        standardGeneric("contrastNames")
    }
)



#' @rdname contrastNames
#' @export
setGeneric(
    name = "contrastNames<-",
    def = function(object, ..., value) {
        standardGeneric("contrastNames<-")
    }
)



#' @rdname enriched
#' @export
setGeneric(
    name = "enriched",
    def = function(object, ...) {
        standardGeneric("enriched")
    }
)



#' @rdname plotEnrichedUpset
#' @export
setGeneric(
    name = "plotEnrichedUpset",
    def = function(object, ...) {
        standardGeneric("plotEnrichedUpset")
    }
)



#' @rdname plotEnrichment
#' @export
setGeneric(
    name = "plotEnrichment",
    def = function(object, ...) {
        standardGeneric("plotEnrichment")
    }
)



#' @rdname plotGSEATable
#' @export
setGeneric(
    name = "plotGSEATable",
    def = function(object, ...) {
        standardGeneric("plotGSEATable")
    }
)
