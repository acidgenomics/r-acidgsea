#' Gene set results
#'
#' @name geneSetResults
#' @note Updated 2020-09-21.
#'
#' @inheritParams params
#' @inheritParams acidroxygen::params
#'
#' @return `DataFrame`.
#'
#' @examples
#' data(fgsea)
#' geneSet(
#'     object = fgsea,
#'     collection = "h",
#'     set = "HALLMARK_P53_PATHWAY"
#' )
NULL



## Updated 2020-09-21.
`geneSetResults,FGSEAList` <-  # nolint
    function(object) {
        ## FIXME REWORK THIS.
        validObject(object)
    }
