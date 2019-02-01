#' @name coerce
#' @aliases as
#' @inherit methods::as
#' @importFrom methods coerce
#' @exportMethod coerce
#'
#' @return Object of new class.
#'
#' @seealso
#' - [methods::coerce()].
#' - [methods::canCoerce()].
#' - [methods::as()].
#'
#' @examples
#' # DESeqDataSet ====
#' x <- as(bcb_small, "DESeqDataSet")
#' names(S4Vectors::mcols(x))
#' class(x)
#' show(x)
#'
#' # RangedSummarizedExperiment ====
#' x <- as(bcb_small, "RangedSummarizedExperiment")
#' slotNames(x)
#' show(x)
#'
#' # SummarizedExperiment ====
#' # Coerce to RangedSummarizedExperiment first.
#' x <- as(bcb_small, "RangedSummarizedExperiment")
#' x <- as(x, "SummarizedExperiment")
#' class(x)
#' slotNames(x)
#' show(x)
NULL



#' @rdname coerce
#' @name coerce,list,FGSEAList-method
setAs(
    from = "list",
    to = "FGSEAList",
    def = function(from) {
        new(Class = "FGSEAList", SimpleList(from))
    }
)
