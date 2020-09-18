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



## FIXME Move to acidgenerics.

#' @rdname convertToHuman
#' @export
setGeneric(
    name = "convertToHuman",
    def = function(object, ...) {
        standardGeneric("convertToHuman")
    }
)



## FIXME Move to acidgenerics.

#' @rdname geneSet
#' @export
setGeneric(
    name = "geneSet",
    def = function(object, ...) {
        standardGeneric("geneSet")
    }
)



## FIXME Move to acidgenerics.

#' @rdname leadingEdge
#' @export
setGeneric(
    name = "leadingEdge",
    def = function(object, ...) {
        standardGeneric("leadingEdge")
    }
)



## FIXME Move to acidgenerics.

#' @rdname nesThreshold
#' @export
setGeneric(
    name = "nesThreshold",
    def = function(object, ...) {
        standardGeneric("nesThreshold")
    }
)



## FIXME Move to acidgenerics.

#' @rdname nesThreshold
#' @export
setGeneric(
    name = "nesThreshold<-",
    def = function(object, ..., value) {
        standardGeneric("nesThreshold<-")
    }
)



## FIXME Move to acidgenerics.

#' @rdname plotGeneSet
#' @export
setGeneric(
    name = "plotGeneSet",
    def = function(object, ...) {
        standardGeneric("plotGeneSet")
    }
)
