#' @name show
#' @inherit basejump::show
#' @examples
#' data(gsea)
#'
#' ## FGSEAList ====
#' show(gsea)
NULL



# @seealso
# - `getMethod("show", "List")`.
# - `getMethod("classNameForDisplay", "SimpleList")`
# - `S4Vectors::classNameForDisplay()`.
# - `S4Vectors:::labeledLine()`.



# FIXME Export this as `showHeader()` in basejump package.
showHeader <- function(object) {
    class <- class(object)[[1L]]
    version <- as.character(metadata(object)[["version"]])
    assert(
        isString(class),
        isString(version)
    )
    cat(class, " ", version, "\n", sep = "")
}



show.FGSEAList <-  # nolint
    function(object) {
        showHeader(object)
        showSlotInfo(list(
            pathwayNames = pathwayNames(object),
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
