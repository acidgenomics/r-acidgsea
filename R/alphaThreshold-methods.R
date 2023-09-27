#' @name alphaThreshold
#' @inherit AcidGenerics::alphaThreshold
#' @note Updated 2022-04-27.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' alphaThreshold(object) <- 0.1
#' alphaThreshold(object)
NULL



## Updated 2020-08-05.
`alphaThreshold,FgseaList` <- # nolint
    function(object) {
        value <- metadata(object)[["alpha"]]
        assert(isAlpha(value))
        value
    }



## Updated 2020-08-05.
`alphaThreshold<-,FgseaList,numeric` <- # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["alpha"]] <- value
        object
    }



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature(object = "FgseaList"),
    definition = `alphaThreshold,FgseaList`
)

#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "FgseaList",
        value = "numeric"
    ),
    definition = `alphaThreshold<-,FgseaList,numeric`
)
