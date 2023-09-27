#' Show an object
#'
#' @name show
#' @inherit methods::show return
#' @note Updated 2022-08-17.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' show(object)
NULL



## Updated 2022-08-17.
`show,FgseaList` <- # nolint
    function(object) {
        showHeader(object)
        list <- list(
            "collectionNames" = collectionNames(object),
            "contrastNames" = contrastNames(object),
            "rankedList" = metadata(RankedList(object))[["value"]],
            "alphaThreshold" = alphaThreshold(object)
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
    signature = signature(object = "FgseaList"),
    definition = `show,FgseaList`
)
