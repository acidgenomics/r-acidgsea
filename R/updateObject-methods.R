#' @name updateObject
#' @author Michael Steinbaugh
#' @inherit BiocGenerics::updateObject
#' @note Updated 2020-09-16.
#'
#' @inheritParams acidroxygen::params
#' @param alphaThreshold `number(1)`.
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
#'
#' ## FGSEAList ====
#' updateObject(fgsea)
NULL



#' @rdname updateObject
#' @name updateObject
#' @importFrom BiocGenerics updateObject
#' @usage updateObject(object, ..., verbose = FALSE)
#' @export
NULL



## Updated 2020-09-17.
`updateObject,FGSEAList` <-  # nolint
    function(
        object,
        alphaThreshold = NULL,
        ...,
        verbose = FALSE
    ) {
        assert(isFlag(verbose))

        ## FIXME Rename gmtFiles to geneSetFiles.
        if (isSubset("gmtFiles", names(metadata(object)))) {
            metadata(object)[["geneSetFiles"]] <- metadata[["gmtFiles"]]
            metadata(object)[["gmtFiles"]] <- NULL
        }


        ## Slot gene set files if undefined.
        if (!isSubset("collections", names(metadata(object)))) {
            cli_alert("Importing gene set collections into object.")
            assert(isSubset("geneSetFiles", names(metadata(object))))
            geneSetFiles <- metadata(object)[["geneSetFiles"]]
            if (!allAreFiles(geneSetFiles)) {
                stop(sprintf(
                    "Required gene set files: %s.",
                    toString(geneSetFiles)
                ))
            }
            collections <- lapply(X = geneSetFiles, FUN = import)
            metadata(object)[["collections"]] <- collections
        }
        ## Slot alpha threshold if undefined.
        if (!isSubset("alpha", names(metadata(object)))) {
            if (isTRUE(verbose)) {
                cli_alert_warning(
                    "Object does not contain alpha used to perform GSEA."
                )
            }
            if (is.null(alphaThreshold)) {
                alphaThreshold <- 0.05
            }
            if (isTRUE(verbose)) {
                cli_alert(paste0(
                    "Assigning alpha of ", alphaThreshold,
                    " into {.fun alphaThreshold}."
                ))
            }
            alphaThreshold(object) <- alphaThreshold
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
