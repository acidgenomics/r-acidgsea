#' Fast Gene Set Enrichment Analysis
#'
#' Wrapper for [fgsea::fgsea()] that loads the pathways automatically from a
#' GMT file, rather than requiring a separate call to [fgsea::gmtPathways()].
#'
#' @name fgsea
#' @inherit fgsea::fgsea params return
#' @inheritParams params
NULL



#' @describeIn fgsea
#'   Returns a `data.frame`/`data.table` of results.
#' @export
fgsea <- function(stats, gmtFile) {
    assert(
        is.numeric(stats),
        isAFile(gmtFile)
    )
    pathways <- gmtPathways(gmt.file = gmtFile)
    message(paste0("GMT file: ", basename(gmtFile)))
    message(paste("Testing against", length(pathways), "pathways."))
    message(paste("Running using", nperm, "permutations."))
    # Suppressing the preranked stats warning here. It's not informative.
    suppressWarnings(
        do.call(
            what = fgsea::fgsea,
            args = matchArgsToDoCall(
                args = list(
                    stats = stats,
                    pathways = pathways
                ),
                removeFormals = "gmtFile"
            )
        )
    )
}

f1 <- formals(fgsea)
f2 <- formals(fgsea::fgsea)
f2 <- f2[setdiff(names(f2), c("pathways", "stats"))]
f <- c(f1, f2)
formals(fgsea) <- f



#' @describeIn fgsea
#'   Parameterized variant. Returns a `list` of results.
#' @export
pfgsea <- function(statsList, gmtFiles, ...) {
    assert(
        is.list(statsList),
        hasNames(statsList),
        all(isFile(gmtFiles)),
        hasNames(gmtFiles)
    )
    lapply(
        X = gmtFiles,
        FUN = function(gmtFile) {
            lapply(
                X = statsList,
                FUN = function(stats) {
                    assert(is.numeric(stats))
                    do.call(
                        what = fgsea,
                        args = list(
                            stats = stats,
                            gmtFile = gmtFile,
                            ...
                        )
                    )
                }
            )
        }
    )
}
