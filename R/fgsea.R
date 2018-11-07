#' Fast Gene Set Enrichment Analysis
#'
#' Wrapper for [fgsea::fgsea()] that loads the pathways automatically from a
#' GMT file, rather than requiring a separate call to [fgsea::gmtPathways()].
#'
#' @name fgsea
#' @inheritParams params
NULL



#' @describeIn fgsea Returns a `data.frame`/`data.table` of results.
#' @export
fgsea <- function(stats, gmtFile, ...) {
    assert_is_numeric(stats)
    assert_all_are_existing_files(gmtFile)
    pathways <- gmtPathways(gmt.file = gmtFile)

    message(paste0("GMT file: ", basename(gmtFile)))
    message(paste("Testing against", length(pathways), "pathways."))

    # Expecting warning about ranked tie, which isn't informative.
    suppressWarnings(
        fgsea::fgsea(
            pathways = pathways,
            stats = stats,
            ...
        )
    )
}



#' @describeIn fgsea Parameterized variant. Returns a `list` of results.
#' @export
pfgsea <- function(gmtFiles, statsList, ...) {
    assert_all_are_existing_files(gmtFiles)
    assert_has_names(gmtFiles)
    assert_is_list(statsList)
    assert_has_names(statsList)
    lapply(
        X = gmtFiles,
        FUN = function(gmtFile) {
            assert_is_a_string(gmtFile)
            assert_all_are_existing_files(gmtFile)
            lapply(
                X = statsList,
                FUN = function(stats) {
                    assert_is_numeric(stats)
                    fgsea(stats = stats, gmtFile = gmtFile, ...)
                }
            )
        }
    )
}
