#' Show an object
#'
#' @name show
#' @inherit methods::show return
#' @note Updated 2020-03-18.
#'
#' @inheritParams acidroxygen::params
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' show(fgsea)
NULL



## Updated 2019-07-24.
`show,FGSEAList` <-  # nolint
    function(object) {
        showHeader(object)
        showSlotInfo(list(
            collectionNames = collectionNames(object),
            contrastNames = contrastNames(object)
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("FGSEAList"),
    definition = `show,FGSEAList`
)
