#' @name export
#'
#' @inherit bioverbs::export
#'
#' @param x Object.
#' @param name Name.
#' @param dir Directory.
NULL



#' @importFrom bioverbs export
#' @aliases NULL
#' @export
bioverbs::export



# @seealso DESeqAnalysis:::export.DESeqAnalysis
export.FGSEAList <- function(x, name = NULL, dir = ".") {
    validObject(x)
    # The `getNameInParent()` method used in goalie doesn't work well inside
    # of an S4 method.
    call <- standardizeCall()
    assert(isString(name, nullOK = TRUE))
    if (is.null(name)) {
        name <- as.character(call[["x"]])
    }
    # Note that we're combining the dir with name, so we can set
    # subdirectories for each slotted data type (e.g. DESeqDataSet).
    dir <- initDir(file.path(dir, name))
    message(paste0("Exporting to ", dir, "."))
    # Naming system: fgsea_c1_mutant_vs_control.csv
    files <- lapply(
        X = seq_len(length(x)),
        FUN = function(gmt) {
            contrasts <- x[[gmt]]
            files <- lapply(
                X = seq_len(length(contrasts)),
                FUN = function(contrast) {
                    data <- x[[gmt]][[contrast]]
                    name <- paste(
                        "gsea",
                        names(x)[[gmt]],
                        names(x[[gmt]])[[contrast]],
                        sep = "_"
                    )
                    file <- file.path(dir, paste0(name, ".csv"))
                    export(x = data, file = file)
                }
            )
            names(files) <- names(contrasts)
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
