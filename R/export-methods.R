#' @name export
#' @inherit AcidGenerics::export
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
#' @param geneSetResults `logical(1)` or `character`.
#'   Export per-gene set expression including log fold change values generated
#'   from DESeq2. Can be slow when processing all MSigDB collections, so
#'   disabled by default. Alternatively, can declare specific collections to
#'   process, as a `character` vector, such as `"h"` for the hallmark
#'   gene set collection.
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
#' @importFrom AcidGenerics export
#' @usage export(object, ...)
#' @export
NULL



## Updated 2020-09-23.
`export,FGSEAList` <-  # nolint
    function(
        object,
        name = NULL,
        dir = ".",
        geneSetResults = FALSE
    ) {
        validObject(object)
        call <- standardizeCall()
        assert(
            isString(name, nullOK = TRUE),
            isFlag(geneSetResults) || isCharacter(geneSetResults)
        )
        if (is.null(name)) {
            name <- as.character(call[["object"]])
        }
        dir <- initDir(file.path(dir, name))
        cli_alert(sprintf("Exporting to '{.path %s}'.", dir))
        contrastNames <- contrastNames(object)
        collectionNames <- collectionNames(object)
        ## Always export the FGSEA results per contrast / per collection.
        files <- lapply(
            X = contrastNames,
            FUN = function(contrast) {
                cli_alert(sprintf("Exporting results for {.var %s}.", contrast))
                files <- lapply(
                    X = collectionNames,
                    FUN = function(collection) {
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
                    }
                )
                names(files) <- collectionNames
                files
            }
        )
        names(files) <- contrastNames
        ## Optionally, export additional results in a nested collection
        ## directory, containing gene expression information from DESeqResults.
        if (!isFALSE(geneSetResults)) {
            if (isTRUE(geneSetResults)) {
                geneSetResults <- collectionNames
            }
            assert(isSubset(geneSetResults, collectionNames))
            lapply(
                X = contrastNames,
                FUN = function(contrast) {
                    lapply(
                        X = geneSetResults,
                        FUN = function(collection) {
                            cli_alert(sprintf(
                                "Exporting results for {.var %s} {.var %s}.",
                                contrast, collection
                            ))
                            sets <- geneSetNames(
                                object = object,
                                collection = collection
                            )
                            lapply(
                                X = sets,
                                FUN = function(set) {
                                    res <- geneSetResults(
                                        object = object,
                                        contrast = contrast,
                                        collection = collection,
                                        set = set
                                    )
                                    if (!hasLength(res)) return(NULL)
                                    export(
                                        object = res,
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
                    )
                }
            )
        }
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("FGSEAList"),
    definition = `export,FGSEAList`
)
