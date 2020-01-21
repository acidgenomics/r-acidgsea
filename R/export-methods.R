#' @name export
#' @inherit acidgenerics::export
#' @note Updated 2020-01-20.
#'
#' @section On-disk structure:
#'
#' Example: `fgsea/mutant_vs_control/c1.csv`
#'
#' S4 object is currently structured by:
#'
#' 1. Gene set (c1-c8, h).
#' 2. Contrast.
#'
#' The object was structured in this manner to flow with the R Markdown
#' template. However, when writing to disk, I think it makes more sense to
#' organize by:
#'
#' 1. Contrast
#' 2. Gene set.
#'
#' @inheritParams pipette::export
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
#' @importFrom acidgenerics export
#' @usage export(object, ...)
#' @export
NULL



`export,FGSEAList` <-  # nolint
    function(object, name = NULL, dir = ".") {
        validObject(object)
        call <- standardizeCall()
        assert(isString(name, nullOK = TRUE))
        if (is.null(name)) {
            name <- as.character(call[["object"]])
        }
        ## Note that we're combining the dir with name, so we can set
        ## subdirectories for each slotted data type (e.g. `DESeqDataSet`).
        dir <- initDir(file.path(dir, name))
        message(sprintf("Exporting to '%s'.", dir))
        files <- lapply(
            X = seq_len(length(object)),
            FUN = function(gmt) {
                contrasts <- object[[gmt]]
                files <- lapply(
                    X = seq_len(length(contrasts)),
                    FUN = function(contrast) {
                        data <- object[[gmt]][[contrast]]
                        assert(
                            is(data, "data.table"),
                            isSubset(
                                x = c(
                                    "pathway",
                                    "pval",
                                    "padj",
                                    "ES",
                                    "NES",
                                    "nMoreExtreme",
                                    "size",
                                    "leadingEdge"
                                ),
                                y = colnames(data)
                            )
                        )
                        data[["leadingEdge"]] <-
                            unlist(lapply(data[["leadingEdge"]], toString))
                        ## Coerce "leadingEdge" list column to string.
                        file <- file.path(
                            dir,
                            names(object[[gmt]])[[contrast]],
                            paste0(names(object)[[gmt]], ".csv")
                        )
                        assert(allAreAtomic(data))
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
    definition = `export,FGSEAList`
)
