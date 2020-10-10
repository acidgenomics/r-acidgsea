#' @name alphaThreshold
#' @inherit AcidGenerics::alphaThreshold
#' @note Updated 2020-08-05.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' alphaThreshold(fgsea) <- 0.1
#' alphaThreshold(fgsea)
NULL



## Updated 2020-08-05.
`alphaThreshold,FGSEAList` <-  # nolint
    function(object) {
        value <- metadata(object)[["alpha"]]
        assert(isAlpha(value))
        value
    }



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature("FGSEAList"),
    definition = `alphaThreshold,FGSEAList`
)



## Updated 2020-08-05.
`alphaThreshold<-,FGSEAList,numeric` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["alpha"]] <- value
        object
    }



#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "FGSEAList",
        value = "numeric"
    ),
    definition = `alphaThreshold<-,FGSEAList,numeric`
)
