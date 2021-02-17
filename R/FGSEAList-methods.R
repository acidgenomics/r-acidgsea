#' Parameterized fast gene set enrichment analysis (GSEA)
#'
#' Extends the functionality of [fgsea::fgsea()].
#'
#' @name FGSEAList
#' @note Updated 2020-09-23.
#'
#' @inheritParams RankedList
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param nPerm `integer(1)`.
#'   Number of permutations.
#'   Minimial possible nominal *P* value is about 1/`nPerm`.
#' @param minSize `integer(1)`.
#'   Minimal size of a gene set to test.
#'   All pathways below the threshold are excluded.
#' @param maxSize `integer(1)`/`Inf`.
#'   Maximal size of a gene set to test.
#'   All pathways above the threshold are excluded.
#' @param alphaThreshold `numeric(1)`.
#'   Alpha level cutoff.
#'   Stored internally in [alphaThreshold()].
#'   Applied only to plots and enriched gene set exports, but does not affect
#'   the actual GSEA enrichment calculation.
#' @param ... Arguments pass through to `RankedList` method.
#'
#' @return `FGSEAList`.
#'
#' @examples
#' data(deseq)
#' geneSetFiles <- system.file(
#'     "extdata",
#'     "msigdb",
#'     "7.0",
#'     "msigdb_v7.0_GMTs",
#'     "h.all.v7.0.symbols.gmt",
#'     package = "AcidGSEA",
#'     mustWork = TRUE
#' )
#' names(geneSetFiles) <- "h"
#' fgsea <- FGSEAList(
#'     object = deseq,
#'     geneSetFiles = geneSetFiles
#' )
#' print(fgsea)
NULL



## Updated 2020-09-25.
`FGSEAList,DESeqAnalysis` <-  # nolint
    function(
        object,
        geneSetFiles,
        value,
        nPerm = 1000L,
        minSize = 15L,
        maxSize = 500L,
        alphaThreshold = 0.05
    ) {
        validObject(object)
        assert(
            allAreFiles(geneSetFiles),
            hasNames(geneSetFiles),
            isInt(nPerm),
            isAlpha(alphaThreshold)
        )
        value <- match.arg(value)
        rankedList <- RankedList(object = object, value = value)
        validObject(rankedList)
        contrasts <- names(rankedList)
        stats <- as.list(rankedList)
        alert("Running parameterized fast GSEA.")
        txt("Gene set files:")
        ul(names(geneSetFiles))
        txt("Contrasts:")
        ul(contrasts)
        collections <- lapply(X = geneSetFiles, FUN = import)
        list <- mapply(
            name = names(collections),
            pathways = collections,
            FUN = function(name, pathways) {
                dl(c("Collection" = name))
                alertInfo(sprintf(
                    "Testing %d pathways.",
                    length(pathways)
                ))
                mapply(
                    contrast = contrasts,
                    stats = stats,
                    FUN = function(contrast, stats) {
                        dl(c("Contrast" = contrast))
                        suppressWarnings({
                            data <- fgsea::fgsea(
                                pathways = pathways,
                                stats = stats,
                                nperm = nPerm,
                                minSize = minSize,
                                maxSize = maxSize
                            )
                        })
                        assert(is(data, "data.table"))
                        data
                    },
                    SIMPLIFY = FALSE,
                    USE.NAMES = TRUE
                )
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        out <- SimpleList(list)
        metadata(out) <- list(
            version = .version,
            date = Sys.Date(),
            nPerm = nPerm,
            minSize = minSize,
            maxSize = maxSize,
            alpha = alphaThreshold,
            deseq = object,
            rankedList = rankedList,
            geneSetFiles = geneSetFiles,
            collections = collections,
            call = standardizeCall(),
            sessionInfo = session_info()
        )
        out
        new(Class = "FGSEAList", out)
    }

formals(`FGSEAList,DESeqAnalysis`)[["value"]] <- .rankedListValue



#' @rdname FGSEAList
#' @export
setMethod(
    f = "FGSEAList",
    signature = signature("DESeqAnalysis"),
    definition = `FGSEAList,DESeqAnalysis`
)
