#' Gene set
#'
#' @name geneSet
#' @note Updated 2020-09-16.
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
#'     collection = 1L,
#'     set = 1L
#' )
NULL



`geneSet,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        set
    ) {
        validObject(object)
        assert(
            isScalar(collection),
            isScalar(set)
        )
        x <- metadata(object)[["gmt"]][[collection]][[set]]
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
