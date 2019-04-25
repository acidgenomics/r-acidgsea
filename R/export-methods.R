#' @name export
#'
#' @inherit bioverbs::export
#'
#' @param x Object.
#' @param name Name.
#' @param dir Directory.
#' @param ... Additional arguments.
#'
#' @examples
#' data(gsea)
#' export(gsea, dir = "example")
#' list.files(file.path("example", "gsea"))
#' unlink("example", recursive = TRUE)
NULL



#' @rdname export
#' @name export
#' @importFrom bioverbs export
#' @usage export(x, ...)
#' @export
NULL



# On-disk structure: fgsea/mutant_vs_control/c1.csv
#
# S4 object is currently structured by:
# 1. Gene set (c1-c8, h).
# 2. Contrast.
#
# The object was structured in this manner to flow with the R Markdown template.
#
# However, when writing to disk, I think it makes more sense to organize by:
# 1. Contrast
# 2. Gene set.
#
# I'm considering restructuring the object to match this approach, and may apply
# this approach in a future update.
export.FGSEAList <- function(x, name = NULL, dir = ".") {
    validObject(x)

    call <- standardizeCall()
    assert(isString(name, nullOK = TRUE))
    if (is.null(name)) {
        name <- as.character(call[["x"]])
    }

    # Note that we're combining the dir with name, so we can set subdirectories
    # for each slotted data type (e.g. `DESeqDataSet`).
    dir <- initDir(file.path(dir, name))
    message(paste0("Exporting to ", dir, "."))

    files <- lapply(
        X = seq_len(length(x)),
        FUN = function(gmt) {
            contrasts <- x[[gmt]]
            files <- lapply(
                X = seq_len(length(contrasts)),
                FUN = function(contrast) {
                    data <- x[[gmt]][[contrast]]
                    file <- file.path(
                        dir,
                        names(x[[gmt]])[[contrast]],
                        paste0(names(x)[[gmt]], ".csv")
                    )
                    export(x = data, file = file)
                }
            )
            names(files) <- names(contrasts)
            files
        }
    )
    names(files) <- names(x)

    files
}



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("FGSEAList"),
    definition = export.FGSEAList
)
