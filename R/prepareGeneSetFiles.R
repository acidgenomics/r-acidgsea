#' Prepare gene set files
#'
#' @details
#'  Intended primarily to match GMT files from MSigDb.
#'
#' @note Updated 2021-10-20.
#' @export
#'
#' @param dir `character(1)`.
#'   Directory name containing MSigDb release.
#' @param keyType `character(1).
#'   Gene identifier format.
#'   Defaults to `"symbols"`, corresponding to gene names (e.g. `"TP53"`).
#'   Also supports Entrez identifiers, via `"entrez"` (e.g. `7157`).
#' @param ext `character(1)`.
#'   Gene set file extension.
#'   Case insensitive.
#' @param recursive `logical(1)`.
#'   Whether to search for gene set files recursively in `dir` argument.
#'
#' @seealso
#' - https://www.gsea-msigdb.org/gsea/msigdb/
#' - https://www.ncbi.nlm.nih.gov/gene/
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
    keyType = c("symbols", "entrez"),
    ext = "gmt",
    recursive = FALSE
) {
    assert(
        isADir(dir),
        isString(ext),
        isFlag(recursive)
    )
    keyType <- match.arg(keyType)
    dir <- realpath(dir)
    files <- sort(list.files(
        path = dir,
        pattern = paste0("*.", keyType, "\\.", ext, "$"),
        full.names = TRUE,
        recursive = recursive,
        ignore.case = TRUE
    ))
    files <- realpath(files)
    if (!hasLength(files)) {
        abort(sprintf(
            fmt = paste(
                "Failed to locate %s files matching",
                "{.arg %s} {.val %s} in {.path %s}."
            ),
            toupper(ext), "keyType", keyType, dir
        ))
    }
    alertInfo(sprintf(
        "Detected %d %s %s of {.var %s} {.val %s} in {.path %s}.",
        length(files),
        toupper(ext),
        ngettext(
            n = length(files),
            msg1 = "file",
            msg2 = "files"
        ),
        "keyType", keyType,
        dir
    ))
    names(files) <- snakeCase(basenameSansExt(files))
    files
}
