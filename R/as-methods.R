#' @importFrom methods coerce
#' @exportMethod coerce
setAs(
    from = "list",
    to = "FGSEAList",
    def = function(from) {
        new(Class = "FGSEAList", SimpleList(from))
    }
)
