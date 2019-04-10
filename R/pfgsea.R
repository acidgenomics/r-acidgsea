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
#' @param BPPARAM BiocParallel parallelization parameter.
#'
#' @examples
#' data(gsea)
#' rankedList <- metadata(gsea)[["rankedList"]]
#' gmtFiles <- metadata(gsea)[["gmtFiles"]]
#'
#' x <- pfgsea(rankedList = rankedList, gmtFiles = gmtFiles)
#' print(x)
pfgsea <- function(
    rankedList,
    gmtFiles,
    nPerm = 1000L,
    minSize = 15L,
    maxSize = 500L,
    BPPARAM = bpparam()
) {
    assert(
        is(rankedList, "RankedList"),
        all(isFile(gmtFiles)),
        hasNames(gmtFiles),
        isInt(nPerm)
    )
    validObject(rankedList)
    message(paste0(
        "Running parameterized fast GSEA...", "\n",
        "GMT files: ", toString(names(gmtFiles)), "\n",
        "Contrasts: ", toString(names(rankedList))
    ))
    list <- lapply(
        X = gmtFiles,
        FUN = function(gmtFile) {
            lapply(
                X = rankedList,
                FUN = function(stats) {
                    pathways <- gmtPathways(gmt.file = gmtFile)
                    message(paste0(
                        "GMT file: ", basename(gmtFile), "\n",
                        "Testing against ", length(pathways), " pathways.\n",
                        "Running using ", nPerm, " permutations."
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
    metadata(out)[["version"]] <- .version
    metadata(out)[["rankedList"]] <- rankedList
    metadata(out)[["gmtFiles"]] <- gmtFiles
    new(Class = "FGSEAList", out)
}
