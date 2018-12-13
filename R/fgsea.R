#' Fast Gene Set Enrichment Analysis
#'
#' Wrapper for `fgsea::fgsea` that loads the pathways automatically from a
#' GMT file, rather than requiring a separate call to `fgsea::gmtPathways`.
#'
#' @name fgsea
#' @inheritParams params
NULL



#' @describeIn fgsea Returns a `data.frame`/`data.table` of results.
#' @export
fgsea <- function(stats, gmtFile, ...) {
    assert(
        is.numeric(stats),
        isAFile(gmtFile)
    )
    pathways <- gmtPathways(gmt.file = gmtFile)
    message(paste0("GMT file: ", basename(gmtFile)))
    message(paste("Testing against", length(pathways), "pathways."))
    fgsea::fgsea(pathways = pathways, stats = stats, ...)
}



#' @describeIn fgsea Parameterized variant. Returns a `list` of results.
#' @export
pfgsea <- function(gmtFiles, statsList, ...) {
    assert(
        all(isFile(gmtFiles)),
        hasNames(gmtFiles),
        is.list(statsList),
        hasNames(statsList)
    )
    lapply(
        X = gmtFiles,
        FUN = function(gmtFile) {
            lapply(
                X = statsList,
                FUN = function(stats) {
                    assert(is.numeric(stats))
                    fgsea(stats = stats, gmtFile = gmtFile, ...)
                }
            )
        }
    )
}
