#' Show an object
#' @name show
#' @inherit acidroxygen::params
#' @examples
#' data(gsea)
#'
#' ## FGSEAList ====
#' show(gsea)
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
