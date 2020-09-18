#' Gene set
#'
#' @name geneSet
#' @note Updated 2020-09-18.
#'
#' @inheritParams params
#' @inheritParams acidroxygen::params
#'
#' @return `character`.
#'
#' @examples
#' data(fgsea)
#' geneSet(
#'     object = fgsea,
#'     collection = "h",
#'     set = "HALLMARK_P53_PATHWAY"
#' )
NULL



## Updated 2020-09-18.
`geneSet,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        set
    ) {
        validObject(object)
        assert(
            isString(collection),
            isString(set)
        )
        x <- metadata(object)[["collections"]][[collection]][[set]]
        assert(isCharacter(x))
        x
    }



#' @rdname geneSet
#' @export
setMethod(
    f = "geneSet",
    signature = signature("FGSEAList"),
    definition = `geneSet,FGSEAList`
)
