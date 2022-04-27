#' @name export
#'
#' @inherit pipette::export description return title
#' @note Updated 2022-03-11.
#'
#' @section On-disk structure:
#'
#' Example:
#'
#' ```r
#' file.path("object", "mutant_vs_control", "c1.csv")
#' ```
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
#' @inheritParams AcidExperiment::export
#' @param ... Additional arguments.
#'
#' @param geneSetResults `logical(1)` or `character`.
#' Export per-gene set expression including log fold change values generated
#' from DESeq2. Can be slow when processing all MSigDB collections, so
#' disabled by default. Alternatively, can declare specific collections to
#' process, as a `character` vector, such as `"h"` for the hallmark
#' gene set collection.
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' con <- file.path(tempdir(), "example")
#' export(
#'     object = object,
#'     con = con
#' )
#' unlink(con, recursive = TRUE)
NULL



## Updated 2021-10-15.
`export,FGSEAList` <- # nolint
    function(object,
             con,
             format, # NULL
             geneSetResults = FALSE,
             compress = getOption(
                 x = "acid.export.compress",
                 default = FALSE
             ),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        validObject(object)
        if (missing(format)) {
            format <- NULL
        }
        assert(
            isString(con),
            is.null(format),
            isFlag(geneSetResults) || isCharacter(geneSetResults),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        dir <- initDir(con)
        if (isFALSE(quiet)) {
            alert(sprintf(
                "Exporting {.cls %s} to '{.path %s}'.",
                "FGSEAList", dir
            ))
        }
        ext <- ".csv"
        if (isTRUE(compress)) {
            ext <- paste(ext, ".gz")
        }
        contrastNames <- contrastNames(object)
        collectionNames <- collectionNames(object)
        ## Always export the FGSEA results per contrast / per collection.
        files <- lapply(
            X = contrastNames,
            FUN = function(contrast) {
                if (isFALSE(quiet)) {
                    alert(sprintf(
                        "Exporting results for {.var %s}.", contrast
                    ))
                }
                files <- lapply(
                    X = collectionNames,
                    FUN = function(collection) {
                        res <- results(
                            object = object,
                            contrast = contrast,
                            collection = collection
                        )
                        if (!hasRows(res)) {
                            return(NULL)
                        }
                        export(
                            object = res,
                            con = file.path(
                                dir,
                                contrast,
                                paste0(collection, ext)
                            ),
                            overwrite = overwrite,
                            quiet = quiet
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
                            if (isFALSE(quiet)) {
                                alert(sprintf(
                                    paste(
                                        "Exporting results for",
                                        "{.var %s} {.var %s}."
                                    ),
                                    contrast, collection
                                ))
                            }
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
                                    if (!hasRows(res)) {
                                        return(NULL)
                                    }
                                    export(
                                        object = res,
                                        con = file.path(
                                            dir,
                                            contrast,
                                            collection,
                                            paste0(tolower(set), ext)
                                        ),
                                        overwrite = overwrite,
                                        quiet = quiet
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



## Updated 2021-10-15.
`export,FGSEAList,deprecated` <- # nolint
    methodFunction(
        f = "export",
        signature = signature(
            object = "SummarizedExperiment",
            con = "missingOrNULL",
            format = "missingOrNULL"
        ),
        package = "basejump"
    )



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "FGSEAList",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,FGSEAList`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "FGSEAList",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,FGSEAList,deprecated`
)
