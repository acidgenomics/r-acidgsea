#' Fast GSEA list
#'
#' Class containing parameterized fast GSEA results.
#'
#' @export
#' @note Updated 2022-08-16.
#'
#' @return `FgseaList`.
setClass(
    Class = "FgseaList",
    contains = "SimpleList"
)
setValidity(
    Class = "FgseaList",
    method = function(object) {
        validate(
            is.list(object[[1L]]),
            is(object[[1L]][[1L]], "data.table"),
            isSubset(
                x = colnames(object[[1L]][[1L]]),
                y = c(
                    "pathway",
                    "pval",
                    "padj",
                    "ES",
                    "NES",
                    "size",
                    "leadingEdge",
                    ## New columns in Bioconductor 3.15:
                    "log2err",
                    ## Legacy columns:
                    "nMoreExtreme"
                )
            ),
            isSubset(
                x = c(
                    ## > "alpha",
                    ## > "call"
                    ## > "date"
                    ## > "deseq" (only created from DESeqAnalysis)
                    ## > "maxSize"
                    ## > "minSize"
                    ## > "nPerm"
                    ## > "packageName"
                    ## > "packageVersion"
                    ## > "sessionInfo"
                    "collections",
                    "geneSetFiles",
                    "rankedList"
                ),
                y = names(metadata(object))
            ),
            is.list(metadata(object)[["collections"]]),
            identical(
                x = names(object),
                y = names(metadata(object)[["collections"]])
            ),
            isCharacter(metadata(object)[["geneSetFiles"]]),
            identical(
                x = names(object),
                y = names(metadata(object)[["geneSetFiles"]])
            ),
            is(metadata(object)[["rankedList"]], "RankedList"),
            identical(
                x = names(object[[1L]]),
                y = names(metadata(object)[["rankedList"]])
            ),
            validObject(metadata(object)[["rankedList"]])
        )
    }
)



#' Ranked list of genes
#'
#' Class containing parameterized ranked gene lists.
#'
#' @export
#' @note Updated 2021-03-16.
#'
#' @return `RankedList`.
setClass(
    Class = "RankedList",
    contains = "SimpleList"
)
setValidity(
    Class = "RankedList",
    method = function(object) {
        ok <- validate(
            is.numeric(object[[1L]]),
            ## Check that this is sorted from high to low.
            identical(
                x = object[[1L]],
                y = sort(object[[1L]], decreasing = TRUE)
            ),
            ## Rank vector must be named.
            hasNames(object[[1L]]),
            isSubset(
                x = c(
                    "keyType",
                    "value"
                ),
                y = names(metadata(object))
            )
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)
