#' @name updateObject
#' @author Michael Steinbaugh
#' @inherit BiocGenerics::updateObject
#' @note Updated 2021-02-17.
#'
#' @inheritParams AcidRoxygen::params
#' @param alphaThreshold `number(1)`.
#'   Alpha level used for GSEA.
#'   Note that this is not necessarily the alpha level used to generate
#'   `DESeqResults` object.
#' @param deseq `DESeqAnalysis` or `NULL`.
#'   Update object by slotting `DESeqAnalysis`, if necessary.
#'   This was added to object in 2020-09.
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



## Updated 2021-02-17.
`updateObject,FGSEAList` <-  # nolint
    function(
        object,
        deseq = NULL,
        alphaThreshold = NULL,
        ...,
        verbose = FALSE
    ) {
        assert(isFlag(verbose))
        ## Slot DESeqAnalysis object, if necessary.
        if (is.null(metadata(object)[["deseq"]])) {
            if (!is(deseq, "DESeqAnalysis")) {
                stop(paste(
                    "Define required DESeqAnalysis object using 'deseq'",
                    "argument to update the object."
                ))
            }
            assert(is(deseq, "DESeqAnalysis"))
            metadata(object)[["deseq"]] <- deseq
        } else if (!is.null(deseq)) {
            stop("DESeqAnalysis is already defined in object.")
        }
        ## Ensure the RankedList contains gene-to-symbol mappings.
        rl <- metadata(object)[["rankedList"]]
        assert(is(rl, "RankedList"))
        g2s <- metadata(rl)[["gene2symbol"]]
        if (!is(g2s, "Gene2Symbol")) {
            if (isTRUE(verbose)) {
                alert("Slotting Gene2Symbol in internal RankedList.")
            }
            suppressMessages({
                metadata(rl)[["gene2symbol"]] <-
                    Gene2Symbol(
                        object = as(deseq, "DESeqDataSet"),
                        format = "unmodified"
                    )
            })
            metadata(object)[["rankedList"]] <- rl
        }
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
            if (!allAreFiles(geneSetFiles)) {
                stop(sprintf(
                    "Required gene set files: %s.",
                    toString(geneSetFiles)
                ))
            }
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
                alert(paste0(
                    "Assigning alpha of ", alphaThreshold,
                    " into {.fun alphaThreshold}."
                ))
            }
            alphaThreshold(object) <- alphaThreshold
        } else if (!is.null(alphaThreshold)) {
            stop("alphaThreshold is already defined in object.")
        }
        metadata(object) <- Filter(f = Negate(is.null), x = metadata(object))
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
