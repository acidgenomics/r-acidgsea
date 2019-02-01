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
            )
        )
    }
)



#' Ranked list of genes
#'
#' Class containing parameterized ranked gene lists.
#'
#' @author Michael Steinbaugh
#' @export
#'
#'
setClass(
    Class = "RankedList",
    contains = "SimpleList",
    validity = function(object) {
    }
)
