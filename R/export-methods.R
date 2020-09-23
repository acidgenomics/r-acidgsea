#' @name export
#' @inherit acidgenerics::export
#' @note Updated 2020-09-23.
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
#' @param geneSetResults `logical(1)`.
#'   Export expression CSV files per gene set.
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' export(
#'     object = fgsea,
#'     dir = "example",
#'     geneSetResults = FALSE
#' )
#' sort(list.files(file.path("example", "fgsea")))
#' unlink("example", recursive = TRUE)
NULL



#' @rdname export
#' @name export
#' @importFrom acidgenerics export
#' @usage export(object, ...)
#' @export
NULL



## Updated 2020-09-23.
`export,FGSEAList` <-  # nolint
    function(
        object,
        name = NULL,
        dir = ".",
        geneSetResults = TRUE
    ) {
        validObject(object)
        call <- standardizeCall()
        assert(
            isString(name, nullOK = TRUE),
            isFlag(geneSetResults)
        )
        if (is.null(name)) {
            name <- as.character(call[["object"]])
        }
        ## Note that we're combining the dir with name, so we can set
        ## subdirectories for each slotted data type (e.g. `DESeqDataSet`).
        dir <- initDir(file.path(dir, name))
        cli_alert(sprintf("Exporting to '{.path %s}'.", dir))
        files <- lapply(
            X = collectionNames(object),
            FUN = function(collection) {
                files <- lapply(
                    X = contrastNames(object),
                    FUN = function(contrast) {
                        export(
                            object = results(
                                object = object,
                                contrast = contrast,
                                collection = collection
                            ),
                            file = file.path(
                                dir,
                                contrast,
                                paste0(collection, ".csv")
                            )
                        )
                        if (isTRUE(geneSetResults)) {
                            cli_alert("Exporting results per gene set.")
                            lapply(
                                X = geneSetNames(
                                    object = object,
                                    collection = collection
                                ),
                                FUN = function(set) {
                                    export(
                                        object = suppressMessages({
                                            geneSetResults(
                                                object = object,
                                                contrast = contrast,
                                                collection = collection,
                                                set = set
                                            )
                                        }),
                                        file = file.path(
                                            dir,
                                            contrast,
                                            collection,
                                            paste0(tolower(set), ".csv")
                                        )
                                    )
                                }
                            )
                        }
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
