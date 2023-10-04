## FIXME Rework this to just use parallel package.



#' Parameterized fast gene set enrichment analysis (GSEA)
#'
#' Extends the functionality of [fgsea::fgsea()].
#'
#' @name FgseaList
#' @note Updated 2023-09-20.
#'
#' @inheritParams RankedList
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `FgseaList`.
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
#' fgsea <- FgseaList(
#'     object = object,
#'     geneSetFiles = geneSetFiles
#' )
#' print(fgsea)
NULL



## Updated 2023-10-04.
`FgseaList,RankedList` <- # nolint
    ## nolint start
    function(object,
             geneSetFiles) {
        ## nolint end
        assert(
            validObject(object),
            allAreFiles(geneSetFiles),
            hasNames(geneSetFiles)
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
                                stats = stats
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
        new(Class = "FgseaList", out)
    }



## Updated 2022-05-25.
`FgseaList,DESeqResults` <- # nolint
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
        FgseaList(rl, ...)
    }

formals(`FgseaList,DESeqResults`)[["keyType"]] <- # nolint
    .keyType



## Updated 2022-05-25.
`FgseaList,DESeqAnalysis` <- # nolint
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
        out <- FgseaList(rl, ...)
        metadata(out)[["deseq"]] <- object
        out
    }

formals(`FgseaList,DESeqAnalysis`)[["keyType"]] <- # nolint
    .keyType



#' @rdname FgseaList
#' @export
setMethod(
    f = "FgseaList",
    signature = signature(object = "DESeqAnalysis"),
    definition = `FgseaList,DESeqAnalysis`
)

#' @rdname FgseaList
#' @export
setMethod(
    f = "FgseaList",
    signature = signature(object = "DESeqResults"),
    definition = `FgseaList,DESeqResults`
)

#' @rdname FgseaList
#' @export
setMethod(
    f = "FgseaList",
    signature = signature(object = "RankedList"),
    definition = `FgseaList,RankedList`
)
