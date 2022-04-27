#' Prepare gene set files
#'
#' @details
#' Intended primarily to match GMT files from MSigDb.
#'
#' @note Updated 2021-10-20.
#' @export
#'
#' @inheritParams params
#'
#' @param dir `character(1)`.
#' Directory name containing MSigDb release.
#'
#' @param ext `character(1)`.
#' Gene set file extension.
#' Case insensitive.
#'
#' @param recursive `logical(1)`.
#' Whether to search for gene set files recursively in `dir` argument.
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
#' files <- prepareGeneSetFiles(dir, keyType = "geneName")
#' print(files)
prepareGeneSetFiles <-
    function(dir,
             keyType = c("geneName", "entrezId"),
             ext = "gmt",
             recursive = FALSE) {
    assert(
        isADir(dir),
        isString(ext),
        isFlag(recursive)
    )
    keyType <- match.arg(keyType)
    keyType2 <- switch(
        EXPR = keyType,
        "entrezId" = "entrez",
        "geneName" = "symbols"
    )
    dir <- realpath(dir)
    files <- sort(list.files(
        path = dir,
        pattern = paste0("*.", keyType2, "\\.", ext, "$"),
        full.names = TRUE,
        recursive = recursive,
        ignore.case = TRUE
    ))
    assert(
        hasLength(xxx),
        msg = sprintf(
            "Failed to detect any gene sets in {.dir %s}.",
            dir
        )
    )
    files <- realpath(files)
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
