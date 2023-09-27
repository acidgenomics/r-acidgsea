#' @name nesThreshold
#' @inherit AcidGenerics::nesThreshold
#' @note Updated 2022-04-27.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' nesThreshold(object) <- 0.1
#' nesThreshold(object)
NULL



## Updated 2020-08-05.
`nesThreshold,FgseaList` <- # nolint
    function(object) {
        value <- metadata(object)[["nesThreshold"]]
        if (is.null(value)) {
            value <- 0L
        }
        assert(isNumber(value), isNonNegative(value))
        value
    }



## Updated 2020-08-05.
`nesThreshold<-,FgseaList,numeric` <- # nolint
    function(object, value) {
        assert(isNumber(value), isNonNegative(value))
        metadata(object)[["nesThreshold"]] <- value
        object
    }



#' @rdname nesThreshold
#' @export
setMethod(
    f = "nesThreshold",
    signature = signature(object = "FgseaList"),
    definition = `nesThreshold,FgseaList`
)

#' @rdname nesThreshold
#' @export
setReplaceMethod(
    f = "nesThreshold",
    signature = signature(
        object = "FgseaList",
        value = "numeric"
    ),
    definition = `nesThreshold<-,FgseaList,numeric`
)
