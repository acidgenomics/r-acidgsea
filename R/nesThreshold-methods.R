#' Normalized enrichment score (NES) threshold
#'
#' @name nesThreshold
#' @note Updated 2020-08-05.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' nesThreshold(fgsea) <- 0.1
#' nesThreshold(fgsea)
NULL



## Updated 2020-08-05.
`nesThreshold,FGSEAList` <-  # nolint
    function(object) {
        value <- metadata(object)[["nesThreshold"]]
        if (is.null(value)) {
            value <- 0L
        }
        assert(isNumber(value), isNonNegative(value))
        value
    }



#' @rdname nesThreshold
#' @export
setMethod(
    f = "nesThreshold",
    signature = signature("FGSEAList"),
    definition = `nesThreshold,FGSEAList`
)



## Updated 2020-08-05.
`nesThreshold<-,FGSEAList,numeric` <-  # nolint
    function(object, value) {
        assert(isNumber(value), isNonNegative(value))
        metadata(object)[["nesThreshold"]] <- value
        object
    }



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
