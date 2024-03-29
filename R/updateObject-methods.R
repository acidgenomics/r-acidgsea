#' Update object
#'
#' @name updateObject
#' @author Michael Steinbaugh
#' @note Updated 2022-04-27.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param alphaThreshold `number(1)`.
#' Alpha level used for GSEA.
#' Note that this is not necessarily the alpha level used to generate
#' `DESeqResults` object.
#'
#' @param deseq `DESeqAnalysis` or `NULL`.
#' Update object by slotting `DESeqAnalysis`, if necessary.
#' This was added to object in 2020-09.
#'
#' @param verbose `logical(1)`.
#' Whether information about the update should be reported.
#'
#' @return Modified object.
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' object <- updateObject(object)
#' print(object)
NULL



## Updated 2022-04-27.
`updateObject,FgseaList` <- # nolint
    function(object,
             deseq = NULL,
             alphaThreshold = NULL,
             ...,
             verbose = FALSE) {
        assert(isFlag(verbose))
        ## Slot DESeqAnalysis object, if necessary.
        if (is.null(metadata(object)[["deseq"]])) {
            assert(
                is(deseq, "DESeqAnalysis"),
                msg = sprintf(
                    paste(
                        "Define required '%s' object using '%s'",
                        "argument to update the object."
                    ),
                    "DESeqAnalysis", "deseq"
                )
            )
            metadata(object)[["deseq"]] <- deseq
            deseq <- NULL
        }
        assert(
            is.null(deseq),
            msg = "DESeqAnalysis is already defined in object."
        )
        ## Rename `gmtFiles` to `geneSetFiles`. Changed in v0.4 (2020-09).
        if (isSubset("gmtFiles", names(metadata(object)))) {
            metadata(object)[["geneSetFiles"]] <- metadata(object)[["gmtFiles"]]
            metadata(object)[["gmtFiles"]] <- NULL
        }
        ## Slot gene set files if undefined.
        if (!isSubset("collections", names(metadata(object)))) {
            if (isTRUE(verbose)) {
                alert("Importing gene set collections into object.")
            }
            assert(isSubset("geneSetFiles", names(metadata(object))))
            geneSetFiles <- metadata(object)[["geneSetFiles"]]
            assert(
                allAreFiles(geneSetFiles),
                msg = sprintf(
                    "Required gene set files: %s.",
                    toInlineString(geneSetFiles)
                )
            )
            suppressMessages({
                collections <- lapply(X = geneSetFiles, FUN = import)
            })
            metadata(object)[["collections"]] <- collections
        }
        ## Slot alpha threshold if undefined.
        if (!isSubset("alpha", names(metadata(object)))) {
            if (isTRUE(verbose)) {
                alertWarning(
                    "Object does not contain alpha used to perform GSEA."
                )
            }
            if (is.null(alphaThreshold)) {
                alphaThreshold <- 0.05
            }
            if (isTRUE(verbose)) {
                alert(sprintf(
                    "Assigning alpha of {.val %s} into {.fun %s}.",
                    as.character(alphaThreshold), "alphaThreshold"
                ))
            }
            alphaThreshold(object) <- alphaThreshold
            alphaThreshold <- NULL
        }
        assert(
            is.null(alphaThreshold),
            msg = sprintf(
                "'%s' is already defined in object.", "alphaThreshold"
            )
        )
        metadata(object) <- Filter(f = Negate(is.null), x = metadata(object))
        validObject(object)
        object
    }



#' @rdname updateObject
#' @export
setMethod(
    f = "updateObject",
    signature = signature(object = "FgseaList"),
    definition = `updateObject,FgseaList`
)
