#' Fast GSEA list
#'
#' Class containing parameterized fast GSEA results.
#'
#' @export
#' @note Updated 2020-09-23.
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
                    "collections",
                    ## > "deseq",  # 0.4.0
                    "geneSetFiles",
                    "rankedList",
                    "version"
                ),
                y = names(metadata(object))
            ),
            is.list(metadata(object)[["collections"]]),
            identical(
                x = names(object),
                y = names(metadata(object)[["collections"]])
            ),
            ## > is(metadata(object)[["deseq"]], "DESeqAnalysis"),  # 0.4.0
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
            is(
                metadata(metadata(object)[["rankedList"]])[["gene2symbol"]],
                "Gene2Symbol"
            )
        )
    }
)



#' Ranked list of genes
#'
#' Class containing parameterized ranked gene lists.
#'
#' @export
#' @note Updated 2020-09-23.
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
                x = c("gene2symbol", "value", "version"),
                y = names(metadata(object))
            ),
            ## These checks break backward compatiblity. Consider enabling in a
            ## future release for tighter checks.
            ## > is(metadata(object)[["gene2symbol"]], "Gene2Symbol"),
            ## > isSubset(
            ## >     x = names(object[[1L]]),
            ## >     y = metadata(object)[["gene2symbol"]][["geneName"]]
            ## > ),
            isSubset(
                x = metadata(object)[["value"]],
                y = eval(formals(`RankedList,DESeqAnalysis`)[["value"]])
            ),
            is(metadata(object)[["version"]], "package_version")
        )
    }
)
