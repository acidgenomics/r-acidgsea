#' @name nesThreshold
#' @inherit AcidGenerics::nesThreshold
#' @note Updated 2022-04-27.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' nesThreshold(object) <- 0.1
#' nesThreshold(object)
NULL



## Updated 2020-08-05.
`nesThreshold,FGSEAList` <- # nolint
    function(object) {
        value <- metadata(object)[["nesThreshold"]]
        if (is.null(value)) {
            value <- 0L
        }
        assert(isNumber(value), isNonNegative(value))
        value
    }



## Updated 2020-08-05.
`nesThreshold<-,FGSEAList,numeric` <- # nolint
    function(object, value) {
        assert(isNumber(value), isNonNegative(value))
        metadata(object)[["nesThreshold"]] <- value
        object
    }



#' @rdname nesThreshold
#' @export
setMethod(
    f = "nesThreshold",
    signature = signature(object = "FGSEAList"),
    definition = `nesThreshold,FGSEAList`
)

#' @rdname nesThreshold
#' @export
setReplaceMethod(
    f = "nesThreshold",
    signature = signature(
        object = "FGSEAList",
        value = "numeric"
    ),
    definition = `nesThreshold<-,FGSEAList,numeric`
)
