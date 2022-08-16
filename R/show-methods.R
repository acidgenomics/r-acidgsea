#' Show an object
#'
#' @name show
#' @inherit methods::show return
#' @note Updated 2022-04-27.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' show(object)
NULL



## Updated 2022-08-16.
`show,FGSEAList` <- # nolint
    function(object) {
        showHeader(object)
        list <- list(
            "collectionNames" = collectionNames(object),
            "contrastNames" = contrastNames(object),
            "rankedList" = metadata(RankedList(object))[["value"]]
        )
        ## > list[["alphaThreshold"]] <- alphaThreshold(object)
        ## > nesThreshold <- nesThreshold(object)
        ## > if (nesThreshold > 0L) {
        ## >     list[["nesThreshold"]] <- nesThreshold
        ## > }
        showSlotInfo(list)
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "FGSEAList"),
    definition = `show,FGSEAList`
)
