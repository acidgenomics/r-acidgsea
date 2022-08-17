#' Parameterized fast gene set enrichment analysis (GSEA)
#'
#' Extends the functionality of [fgsea::fgsea()].
#'
#' @name FGSEAList
#' @note Updated 2022-08-17.
#'
#' @inheritParams RankedList
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `FGSEAList`.
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#'
#' ## DESeqAnalysis ====
#' object <- deseq
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
#'     object = object,
#'     geneSetFiles = geneSetFiles
#' )
#' print(fgsea)
NULL



## Updated 2022-05-25.
`FGSEAList,RankedList` <- # nolint
    function(object,
             geneSetFiles,
             BPPARAM = BiocParallel::bpparam() # nolint
    ) {
        assert(
            validObject(object),
            allAreFiles(geneSetFiles),
            hasNames(geneSetFiles),
            isBiocParallelParam(BPPARAM)
        )
        contrasts <- names(object)
        stats <- as.list(object)
        alert("Running parameterized fast GSEA.")
        txt("Gene set files:")
        ul(names(geneSetFiles))
        txt("Contrasts:")
        ul(contrasts)
        collections <- lapply(X = geneSetFiles, FUN = import)
        list <- Map(
            name = names(collections),
            pathways = collections,
            f = function(name, pathways) {
                dl(c("Collection" = name))
                alertInfo(sprintf(
                    "Testing %d pathways.",
                    length(pathways)
                ))
                geneIds <- sort(unique(unlist(
                    x = pathways,
                    recursive = TRUE,
                    use.names = FALSE
                )))
                Map(
                    contrast = contrasts,
                    stats = stats,
                    f = function(contrast, stats) {
                        dl(c("Contrast" = contrast))
                        assert(areIntersectingSets(names(stats), geneIds))
                        suppressWarnings({
                            data <- fgsea::fgsea(
                                pathways = pathways,
                                stats = stats,
                                BPPARAM = BPPARAM
                            )
                        })
                        assert(is(data, "data.table"))
                        data
                    }
                )
            }
        )
        out <- SimpleList(list)
        metadata(out) <- list(
            "alpha" = 0.05,
            "call" = standardizeCall(),
            "collections" = collections,
            "date" = Sys.Date(),
            "geneSetFiles" = geneSetFiles,
            "packageVersion" = .pkgVersion,
            "rankedList" = object,
            "sessionInfo" = sessionInfo
        )
        new(Class = "FGSEAList", out)
    }



## Updated 2022-05-25.
`FGSEAList,DESeqResults` <- # nolint
    function(object,
             keyType,
             value = c("stat", "log2FoldChange"),
             rowRanges,
             proteinCodingOnly = FALSE,
             ...) {
        assert(validObject(object))
        rl <- RankedList(
            object = object,
            keyType = match.arg(keyType),
            value = match.arg(value),
            rowRanges = rowRanges,
            proteinCodingOnly = proteinCodingOnly
        )
        FGSEAList(rl, ...)
    }

formals(`FGSEAList,DESeqResults`)[["keyType"]] <- # nolint
    .keyType



## Updated 2022-05-25.
`FGSEAList,DESeqAnalysis` <- # nolint
    function(object,
             keyType,
             value = c("stat", "log2FoldChange"),
             proteinCodingOnly = FALSE,
             ...) {
        validObject(object)
        rl <- RankedList(
            object = object,
            keyType = match.arg(keyType),
            value = match.arg(value),
            proteinCodingOnly = proteinCodingOnly
        )
        out <- FGSEAList(rl, ...)
        metadata(out)[["deseq"]] <- object
        out
    }

formals(`FGSEAList,DESeqAnalysis`)[["keyType"]] <- # nolint
    .keyType



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
