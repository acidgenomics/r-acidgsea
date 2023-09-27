#' Combine multiple objects
#'
#' @name combine
#' @note Updated 2022-04-27.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' x <- fgsea
#' y <- fgsea
#' contrastNames(x) <- paste0("x_", contrastNames(x))
#' contrastNames(y) <- paste0("y_", contrastNames(y))
#' c <- combine(x, y)
#' c
NULL



## Updated 2020-09-17.
`combine,FgseaList` <- # nolint
    function(x, y) {
        validObject(x)
        validObject(y)
        assert(
            identical(
                x = collectionNames(x),
                y = collectionNames(y)
            ),
            identical(
                x = metadata(x)[["collections"]],
                y = metadata(y)[["collections"]]
            ),
            identical(
                x = metadata(x)[["geneSetFiles"]],
                y = metadata(y)[["geneSetFiles"]]
            ),
            areDisjointSets(
                x = contrastNames(x),
                y = contrastNames(y)
            ),
            areDisjointSets(
                x = names(metadata(x)[["rankedList"]]),
                y = names(metadata(y)[["rankedList"]])
            )
        )
        ## Update the `listData` slot. Note that using `c()` directly here as
        ## `FUN` argument will prefix with `x.` and `y.`, which we don't want.
        listData <- Map(
            x = slot(x, "listData"),
            y = slot(y, "listData"),
            f = function(x, y) {
                c(x, y)
            }
        )
        ## Update the `rankedList` data stashed in `metadata`.
        rankedList <- c(
            metadata(x)[["rankedList"]],
            metadata(y)[["rankedList"]]
        )
        assert(identical(names(listData[[1L]]), names(rankedList)))
        ## Internally, let's work with `x` as primary object, renamed to `out`.
        out <- x
        slot(out, "listData") <- listData
        metadata(out)[["rankedList"]] <- rankedList
        validObject(out)
        out
    }



#' @rdname combine
#' @export
setMethod(
    f = "combine",
    signature = signature(
        x = "FgseaList",
        y = "FgseaList"
    ),
    definition = `combine,FgseaList`
)
