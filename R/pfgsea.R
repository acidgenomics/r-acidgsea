#' Parameterized fast gene set enrichment analysis (GSEA)
#'
#' Extends the functionality of [fgsea::fgsea()].
#'
#' @export
#'
#' @param rankedList `RankedList`.
#'   Ranked gene list.
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
#' @param BPPARAM BiocParallel parallelization parameter.
#'
#' @examples
#' data(gsea)
#' metadata <- S4Vectors::metadata
#'
#' rankedList <- metadata(gsea)[["rankedList"]]
#' gmtFiles <- metadata(gsea)[["gmtFiles"]]
#'
#' # Copy example MSigDb files to `$HOME`.
#' file.copy(
#'     from = system.file(
#'         "extdata", "msigdb",
#'         package = "pfgsea",
#'         mustWork = TRUE
#'     ),
#'     to = "~",
#'     overwrite = FALSE,
#'     recursive = TRUE
#' )
#'
#' x <- pfgsea(rankedList = rankedList, gmtFiles = gmtFiles)
#' print(x)

## Modified 2019-06-12.
pfgsea <- function(
    rankedList,
    gmtFiles,
    nPerm = 1000L,
    minSize = 15L,
    maxSize = 500L,
    alpha = 0.05,
    BPPARAM = bpparam()  # nolint
) {
    assert(
        is(rankedList, "RankedList"),
        all(isFile(gmtFiles)),
        hasNames(gmtFiles),
        isInt(nPerm),
        isAlpha(alpha)
    )
    validObject(rankedList)
    message(sprintf(
        fmt = paste(
            "Running parameterized fast GSEA...",
            "GMT files: %s",
            "Contrasts: %s",
            sep = "\n"
        ),
        toString(names(gmtFiles), width = 100L),
        toString(names(rankedList), width = 100L)
    ))
    list <- lapply(
        X = gmtFiles,
        FUN = function(gmtFile) {
            lapply(
                X = rankedList,
                FUN = function(stats) {
                    pathways <- gmtPathways(gmt.file = gmtFile)
                    message(sprintf(
                        fmt = paste(
                            "GMT file: %s",
                            "Testing against %d pathways.",
                            "Running using %d permutations.",
                            sep = "\n"
                        ),
                        basename(gmtFile),
                        length(pathways),
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
        rankedList = rankedList,
        gmtFiles = gmtFiles,
        call = match.call(),
        sessionInfo = session_info()
    )

    new(Class = "FGSEAList", out)
}
