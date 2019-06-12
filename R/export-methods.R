#' @name export
#' @inherit bioverbs::export
#'
#' @inheritParams brio::export
#' @param ... Additional arguments.
#'
#' @examples
#' data(gsea)
#' export(gsea, dir = "example")
#' sort(list.files(file.path("example", "gsea")))
#' unlink("example", recursive = TRUE)
NULL



#' @rdname export
#' @name export
#' @importFrom bioverbs export
#' @usage export(object, ...)
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
#
# Modified 2019-06-12.
export.FGSEAList <-  # nolint
    function(object, name = NULL, dir = ".") {
        validObject(object)

        call <- standardizeCall()
        assert(isString(name, nullOK = TRUE))
        if (is.null(name)) {
            name <- as.character(call[["object"]])
        }

        # Note that we're combining the dir with name, so we can set
        # subdirectories for each slotted data type (e.g. `DESeqDataSet`).
        dir <- initDir(file.path(dir, name))
        message(paste0("Exporting to ", dir, "."))

        files <- lapply(
            X = seq_len(length(object)),
            FUN = function(gmt) {
                contrasts <- object[[gmt]]
                files <- lapply(
                    X = seq_len(length(contrasts)),
                    FUN = function(contrast) {
                        data <- object[[gmt]][[contrast]]
                        file <- file.path(
                            dir,
                            names(object[[gmt]])[[contrast]],
                            paste0(names(object)[[gmt]], ".csv")
                        )
                        export(object = data, file = file)
                    }
                )
                names(files) <- names(contrasts)
                files
            }
        )
        names(files) <- names(object)
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("FGSEAList"),
    definition = export.FGSEAList
)
