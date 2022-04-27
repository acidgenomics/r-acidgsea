#' Parameterized fast gene set enrichment analysis (GSEA)
#'
#' Extends the functionality of [fgsea::fgsea()].
#'
#' @name FGSEAList
#' @note Updated 2021-10-19.
#'
#' @inheritParams RankedList
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param nPerm `integer(1)`.
#' Number of permutations.
#' Minimial possible nominal *P* value is about 1/`nPerm`.
#'
#' @param minSize `integer(1)`.
#' Minimal size of a gene set to test.
#' All pathways below the threshold are excluded.
#'
#' @param maxSize `integer(1)`/`Inf`.
#' Maximal size of a gene set to test.
#' All pathways above the threshold are excluded.
#'
#' @param alphaThreshold `numeric(1)`.
#' Alpha level cutoff.
#' Stored internally in [alphaThreshold()].
#' Applied only to plots and enriched gene set exports, but does not affect
#' the actual GSEA enrichment calculation.
#'
#' @return `FGSEAList`.
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#' geneSetFiles <- prepareGeneSetFiles(
#'     dir = system.file(
#'         "extdata",
#'         "msigdb",
#'         "7.0",
#'         "msigdb_v7.0_GMTs",
#'         package = "AcidGSEA"
#'     )
#' )
#' fgsea <- FGSEAList(
#'     object = deseq,
#'     geneSetFiles = geneSetFiles
#' )
#' print(fgsea)
NULL



## Updated 2021-10-20.
`FGSEAList,RankedList` <- # nolint
    function(object,
             geneSetFiles,
             nPerm = 1000L,
             minSize = 15L,
             maxSize = 500L,
             alphaThreshold = 0.05) {
        validObject(object)
        assert(
            allAreFiles(geneSetFiles),
            hasNames(geneSetFiles),
            isInt(nPerm),
            isAlpha(alphaThreshold)
        )
        contrasts <- names(object)
        stats <- as.list(object)
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
            "alpha" = alphaThreshold,
            "call" = standardizeCall(),
            "collections" = collections,
            "date" = Sys.Date(),
            "geneSetFiles" = geneSetFiles,
            "maxSize" = maxSize,
            "minSize" = minSize,
            "nPerm" = nPerm,
            "packageVersion" = .pkgVersion,
            "rankedList" = object,
            "sessionInfo" = session_info()
        )
        out
        new(Class = "FGSEAList", out)
    }



## Updated 2021-10-20.
`FGSEAList,DESeqResults` <- # nolint
    function(object,
             keyType = c("geneName", "geneId"),
             value = c("stat", "log2FoldChange", "padj"),
             rowRanges,
             ...) {
        validObject(object)
        rl <- RankedList(
            object = object,
            keyType = match.arg(keyType),
            value = match.arg(value),
            rowRanges = rowRanges
        )
        FGSEAList(rl, ...)
    }



## Updated 2021-10-20.
`FGSEAList,DESeqAnalysis` <- # nolint
    function(object,
             keyType = c("entrezId", "geneName"),
             value = c("stat", "log2FoldChange", "padj"),
             ...) {
        validObject(object)
        rl <- RankedList(
            object = object,
            keyType = match.arg(keyType),
            value = match.arg(value)
        )
        out <- FGSEAList(rl, ...)
        metadata(out)[["deseq"]] <- object
        out
    }



#' @rdname FGSEAList
#' @export
setMethod(
    f = "FGSEAList",
    signature = signature(object = "DESeqAnalysis"),
    definition = `FGSEAList,DESeqAnalysis`
)

#' @rdname FGSEAList
#' @export
setMethod(
    f = "FGSEAList",
    signature = signature(object = "DESeqResults"),
    definition = `FGSEAList,DESeqResults`
)

#' @rdname FGSEAList
#' @export
setMethod(
    f = "FGSEAList",
    signature = signature(object = "RankedList"),
    definition = `FGSEAList,RankedList`
)
