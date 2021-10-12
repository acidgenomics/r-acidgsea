#' Fast GSEA list
#'
#' Class containing parameterized fast GSEA results.
#'
#' @export
#' @note Updated 2021-03-16.
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
                    ## > "call"
                    ## > "date"
                    ## > "deseq" (only created from DESeqAnalysis)
                    ## > "maxSize"
                    ## > "minSize"
                    ## > "nPerm"
                    ## > "packageVersion"
                    ## > "sessionInfo"
                    "alpha",
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
            identical(object[[1L]], sort(object[[1L]], decreasing = TRUE)),
            ## Rank vector must be named.
            hasNames(object[[1L]]),
            isSubset(
                x = c(
                    ## > "keyType",
                    ## > "packageVersion",
                    "value"
                ),
                y = names(metadata(object))
            ),
            isSubset(
                x = metadata(object)[["value"]],
                y = eval(formals(`RankedList,DESeqAnalysis`)[["value"]])
            )
            ## > is(metadata(object)[["packageVersion"]], "package_version")
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        if (identical(metadata(object)[["keyType"]], "geneName")) {
            ok <- validate(
                isSubset(
                    x = "gene2symbol",
                    y = names(metadata(object))
                ),
                isSubset(
                    x = names(object[[1L]]),
                    y = metadata(object)[["gene2symbol"]][["geneName"]]
                )
            )
            if (!isTRUE(ok)) {
                return(ok)
            }
        }
        TRUE
    }
)
