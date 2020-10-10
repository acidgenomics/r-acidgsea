#' Show an object
#'
#' @name show
#' @inherit methods::show return
#' @note Updated 2020-09-22.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' show(fgsea)
NULL



## Updated 2020-09-22.
`show,FGSEAList` <-  # nolint
    function(object) {
        showHeader(object)
        list <- list(
            collectionNames = collectionNames(object),
            contrastNames = contrastNames(object),
            alphaThreshold = alphaThreshold(object),
            rankedList = metadata(RankedList(object))[["value"]]
        )
        nesThreshold <- nesThreshold(object)
        if (nesThreshold > 0L) {
            list[["nesThreshold"]] <- nesThreshold
        }
        showSlotInfo(list)
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("FGSEAList"),
    definition = `show,FGSEAList`
)
