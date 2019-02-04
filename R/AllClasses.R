#' Fast GSEA list
#'
#' Class containing parameterized fast GSEA results.
#'
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso [pfgsea()].
setClass(
    Class = "FGSEAList",
    contains = "SimpleList",
    validity = function(object) {
        validate(
            is.list(object[[1L]]),
            is(object[[1L]][[1L]], "data.table"),
            identical(
                colnames(object[[1L]][[1L]]),
                c(
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
                x = c("version", "rankedList", "gmtFiles"),
                y = names(metadata(object))
            ),
            is(metadata(object)[["rankedList"]], "RankedList"),
            isCharacter(metadata(object)[["gmtFiles"]])
        )
    }
)



#' Ranked list of genes
#'
#' Class containing parameterized ranked gene lists.
#'
#' @author Michael Steinbaugh
#' @export
setClass(
    Class = "RankedList",
    contains = "SimpleList",
    validity = function(object) {
        validate(
            is.numeric(object[[1L]]),
            # Check that this is sorted from high to low.
            identical(object[[1L]], sort(object[[1L]], decreasing = TRUE)),
            isSubset(
                x = c("gene2symbol", "value", "version"),
                y = names(metadata(object))
            ),
            is(metadata(object)[["version"]], "package_version"),
            isSubset(
                x = metadata(object)[["value"]],
                y = eval(formals(rankedList.DESeqAnalysis)[["value"]])
            )
        )
    }
)
