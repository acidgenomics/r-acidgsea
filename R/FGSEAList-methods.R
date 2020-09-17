#' Parameterized fast gene set enrichment analysis (GSEA)
#'
#' Extends the functionality of [fgsea::fgsea()].
#'
#' @name FGSEAList
#' @note Updated 2020-09-17.
#'
#' @inheritParams acidroxygen::params
#' @param geneSetFiles `character`.
#'   Gene set file paths (i.e. GMT files).
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
#'
#' @return `FGSEAList`.
#'
#' @examples
#' ## > ## Copy example MSigDb files to `$HOME`.
#' ## > file.copy(
#' ## >     from = system.file(
#' ## >         "extdata", "msigdb",
#' ## >         package = "acidgsea",
#' ## >         mustWork = TRUE
#' ## >     ),
#' ## >     to = "~",
#' ## >     overwrite = FALSE,
#' ## >     recursive = TRUE
#' ## > )
#'
#' if (isTRUE(dir.exists(file.path("~", "msigdb")))) {
#'     data(fgsea)
#'     metadata <- S4Vectors::metadata
#'     rankedList <- metadata(fgsea)[["rankedList"]]
#'     geneSetFiles <- metadata(fgsea)[["geneSetFiles"]]
#'     fgsea <- FGSEAList(object = rankedList, geneSetFiles = geneSetFiles)
#'     print(fgsea)
#' }
NULL



## Updated 2020-09-17.
`FGSEAList,RankedList` <-  # nolint
    function(
        object,
        geneSetFiles,
        nPerm = 1000L,
        minSize = 15L,
        maxSize = 500L,
        alphaThreshold = 0.05,
        BPPARAM = BiocParallel::bpparam()  # nolint
    ) {
        assert(
            allAreFiles(geneSetFiles),
            hasNames(geneSetFiles),
            isInt(nPerm),
            isAlpha(alphaThreshold)
        )
        validObject(object)
        contrasts <- names(object)
        stats <- as.list(object)
        cli_alert("Running parameterized fast GSEA.")
        cli_text("Gene set files:")
        cli_ul(names(geneSetFiles))
        cli_text("Contrasts:")
        cli_ul(contrasts)
        collections <- lapply(X = geneSetFiles, FUN = import)
        list <- mapply(
            name = names(collections),
            pathways = collections,
            FUN = function(name, pathways) {
                cli_dl(c("Collection" = name))
                cli_alert_info(sprintf(
                    "Testing %d pathways.",
                    length(pathways)
                ))
                mapply(
                    contrast = contrasts,
                    stats = stats,
                    FUN = function(contrast, stats) {
                        cli_dl(c("Contrast" = contrast))
                        suppressWarnings({
                            data <- fgsea::fgsea(
                                pathways = pathways,
                                stats = stats,
                                nperm = nPerm,
                                minSize = minSize,
                                maxSize = maxSize,
                                BPPARAM = BPPARAM
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
            rankedList = object,
            geneSetFiles = geneSetFiles,
            collections = collections,
            call = standardizeCall(),
            sessionInfo = session_info()
        )
        new(Class = "FGSEAList", out)
    }



#' @rdname FGSEAList
#' @export
setMethod(
    f = "FGSEAList",
    signature = signature("RankedList"),
    definition = `FGSEAList,RankedList`
)
