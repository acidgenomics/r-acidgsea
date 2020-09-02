#' Fast GSEA list
#'
#' Class containing parameterized fast GSEA results.
#'
#' @export
#' @note Updated 2020-05-12.
#'
#' @return `FGSEAList`.
setClass(
    Class = "FGSEAList",
    contains = "SimpleList"
)
setValidity(
    Class = "FGSEAList",
    method = function(object) {
        validate(
            is.list(object[[1L]]),
            is(object[[1L]][[1L]], "data.table"),
            identical(
                x = colnames(object[[1L]][[1L]]),
                y = c(
                    "pathway",
                    "pval",
                    "padj",
                    "ES",
                    "NES",
                    "nMoreExtreme",
                    "size",
                    "leadingEdge"
                )
            ),
            isSubset(
                x = c(
                    ## "call"
                    ## "date"
                    ## "maxSize"
                    ## "minSize"
                    ## "nPerm"
                    ## "sessionInfo"
                    "alpha",
                    "gmtFiles",
                    "rankedList",
                    "version"
                ),
                y = names(metadata(object))
            ),
            isCharacter(metadata(object)[["gmtFiles"]]),
            identical(
                x = names(object),
                y = names(metadata(object)[["gmtFiles"]])
            ),
            allAreFiles(metadata(object)[["gmtFiles"]]),
            is(metadata(object)[["rankedList"]], "RankedList"),
            identical(
                x = names(object[[1L]]),
                y = names(metadata(object)[["rankedList"]])
            )
        )
    }
)



#' Ranked list of genes
#'
#' Class containing parameterized ranked gene lists.
#'
#' @export
#' @note Updated 2020-05-12.
#'
#' @return `RankedList`.
setClass(
    Class = "RankedList",
    contains = "SimpleList"
)
setValidity(
    Class = "RankedList",
    method = function(object) {
        validate(
            is.numeric(object[[1L]]),
            ## Check that this is sorted from high to low.
            identical(object[[1L]], sort(object[[1L]], decreasing = TRUE)),
            ## Rank vector must be named.
            hasNames(object[[1L]]),
            ## gene2symbol metadata is now optional, but still recommended.
            ## This check was removed to allow RankedList support for matrix.
            isSubset(
                x = c("value", "version"),
                y = names(metadata(object))
            ),
            is(metadata(object)[["version"]], "package_version"),
            isSubset(
                x = metadata(object)[["value"]],
                y = eval(formals(`RankedList,DESeqAnalysis`)[["value"]])
            )
        )
    }
)
