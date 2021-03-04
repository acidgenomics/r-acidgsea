#' Prepare gene set files
#'
#' @details
#' For example, intended for MSigDb releases.
#'
#' @note Updated 2021-03-04.
#' @export
#'
#' @param dir `character(1)`.
#'   Directory name containing MSigDb release.
#' @param pattern `character(1)`.
#'   Glob string to use for pattern matching against gene set files.
#'   Passed to `list.files` internally.
#'   Intended primarily to match files from MSigDb.
#'
#' @examples
#' dir <- system.file(
#'     "extdata",
#'     "msigdb",
#'     "7.0",
#'     "msigdb_v7.0_GMTs",
#'     package = "AcidGSEA",
#'     mustWork = TRUE
#' )
#' files <- prepareGeneSetFiles(dir)
#' print(files)
prepareGeneSetFiles <- function(
    dir,
    pattern = "*.all.*.symbols.gmt"
) {
    assert(isString(pattern))
    dir <- realpath(dir)
    files <- sort(list.files(
        path = dir,
        pattern = pattern,
        full.names = TRUE,
        recursive = FALSE
    ))
    if (!hasLength(files)) {
        stop(sprintf(
            "Failed to locate GMT files matching '%s' pattern in '%s'.",
            pattern, dir
        ))
    }
    names <- vapply(
        X = strsplit(
            x = basename(files),
            split = ".",
            fixed = TRUE
        ),
        FUN = `[[`,
        1L,
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    names(files) <- names
    files
}
