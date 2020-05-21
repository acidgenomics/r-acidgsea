#' Parameterized fast gene set enrichment analysis (GSEA)
#'
#' Extends the functionality of [fgsea::fgsea()].
#'
#' @name FGSEAList
#' @note Updated 2020-05-20.
#'
#' @inheritParams acidroxygen::params
#' @param gmtFiles `character`.
#'   GMT file paths.
#' @param nPerm `integer(1)`.
#'   Number of permutations.
#'   Minimial possible nominal *P* value is about 1/`nPerm`.
#' @param minSize `integer(1)`.
#'   Minimal size of a gene set to test.
#'   All pathways below the threshold are excluded.
#' @param maxSize `integer(1)`/`Inf`.
#'   Maximal size of a gene set to test.
#'   All pathways above the threshold are excluded.
#' @param alpha `numeric(1)`.
#'   Alpha level cutoff. Stored internally in `metadata()`.
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
#'
#'     rankedList <- metadata(fgsea)[["rankedList"]]
#'     gmtFiles <- metadata(fgsea)[["gmtFiles"]]
#'
#'     fgsea <- FGSEAList(object = rankedList, gmtFiles = gmtFiles)
#'     print(fgsea)
#' }
NULL



## Updated 2020-05-20.
`FGSEAList,RankedList` <-  # nolint
    function(
        object,
        gmtFiles,
        nPerm = 1000L,
        minSize = 15L,
        maxSize = 500L,
        alpha = 0.05,
        BPPARAM = BiocParallel::bpparam()  # nolint
    ) {
        assert(
            all(isFile(gmtFiles)),
            hasNames(gmtFiles),
            isInt(nPerm),
            isAlpha(alpha)
        )
        validObject(object)
        cli_alert("Running parameterized fast GSEA.")
        cli_text("GMT files:")
        cli_ul(names(gmtFiles))
        cli_text("Contrasts:")
        cli_ul(names(object))
        list <- lapply(
            X = gmtFiles,
            FUN = function(gmtFile) {
                lapply(
                    X = object,
                    FUN = function(stats) {
                        pathways <- gmtPathways(gmt.file = gmtFile)
                        cli_dl(c("GMT file" = basename(gmtFile)))
                        cli_alert_info(sprintf(
                            "Testing against %d pathways.",
                            length(pathways)
                        ))
                        cli_alert_info(sprintf(
                            "Running using %d permutations.",
                            nPerm
                        ))
                        suppressWarnings(
                            data <- fgsea::fgsea(
                                pathways = pathways,
                                stats = stats,
                                nperm = nPerm,
                                minSize = minSize,
                                maxSize = maxSize,
                                BPPARAM = BPPARAM
                            )
                        )
                        assert(is(data, "data.table"))
                        data
                    }
                )
            }
        )
        out <- SimpleList(list)
        ## Stash useful metadata.
        metadata(out) <- list(
            version = .version,
            date = Sys.Date(),
            nPerm = nPerm,
            minSize = minSize,
            maxSize = maxSize,
            alpha = alpha,
            rankedList = object,
            gmtFiles = gmtFiles,
            call = standardizeCall(),
            sessionInfo = session_info()
        )
        ## Return.
        new(Class = "FGSEAList", out)
    }



#' @rdname FGSEAList
#' @export
setMethod(
    f = "FGSEAList",
    signature = signature("RankedList"),
    definition = `FGSEAList,RankedList`
)
