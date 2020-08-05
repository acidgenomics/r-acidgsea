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



#' @rdname convertToHuman
#' @export
setGeneric(
    name = "convertToHuman",
    def = function(object, ...) {
        standardGeneric("convertToHuman")
    }
)



#' @rdname nesThreshold
#' @export
setGeneric(
    name = "nesThreshold",
    def = function(object, ...) {
        standardGeneric("nesThreshold")
    }
)



#' @rdname nesThreshold
#' @export
setGeneric(
    name = "nesThreshold<-",
    def = function(object, ..., value) {
        standardGeneric("nesThreshold<-")
    }
)



#' @rdname plotGeneSet
#' @export
setGeneric(
    name = "plotGeneSet",
    def = function(object, ...) {
        standardGeneric("plotGeneSet")
    }
)
