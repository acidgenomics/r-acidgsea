#' @name show
#' @inherit basejump::show
#' @examples
#' data(gsea)
#'
#' ## FGSEAList ====
#' show(gsea)
NULL



show.FGSEAList <-  # nolint
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
    definition = show.FGSEAList
)
