#' @name updateObject
#' @author Michael Steinbaugh
#' @inherit BiocGenerics::updateObject
#' @note Updated 2020-03-18.
#'
#' @inheritParams acidroxygen::params
#' @param alpha `number(1)`.
#'   Alpha level used for GSEA.
#'   Note that this is not necessarily the alpha level used to generate
#'   `DESeqResults` object.
#' @param verbose `logical(1)`.
#'   Whether information about the update should be reported.
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @examples
#' data(fgsea)
#' updateObject(fgsea)
NULL



#' @rdname updateObject
#' @name updateObject
#' @importFrom BiocGenerics updateObject
#' @usage updateObject(object, ..., verbose = FALSE)
#' @export
NULL



`updateObject,FGSEAList` <-  # nolint
    function(object, alpha, verbose = FALSE) {
        assert(isFlag(verbose))
        ## Slot alpha if undefined.
        if (!isSubset("alpha", names(metadata(object)))) {
            if (isTRUE(verbose)) {
                cli_alert_warning(
                    "Object does not contain alpha used to perform GSEA."
                )
            }
            if (missing(alpha)) {
                alpha <- 0.05
            }
            assert(isAlpha(alpha))
            if (isTRUE(verbose)) {
                cli_alert("Assigning alpha of ", alpha, " into 'metadata()'.")
            }
            metadata(object)[["alpha"]] <- alpha
        }
        validObject(object)
        object
    }



#' @rdname updateObject
#' @export
setMethod(
    f = "updateObject",
    signature = signature("FGSEAList"),
    definition = `updateObject,FGSEAList`
)
