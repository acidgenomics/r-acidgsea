#' @name combine
#' @inherit BiocGenerics::combine description details return seealso title
#' @param ... Additional arguments
#' @examples
#' data(gsea)
#' x <- gsea
#' contrastNames(x) <- paste0("x_", contrastNames(x))
#' y <- gsea
#' contrastNames(y) <- paste0("y_", contrastNames(y))
#' c <- combine(x, y)
#' c
NULL



#' @rdname combine
#' @name combine
#' @importFrom BiocGenerics combine
#' @usage combine(x, y, ...)
#' @export
NULL



combine.FGSEAList <-  # nolint
    function(x, y) {
        validObject(x)
        validObject(y)

        # Require that pathway names are identical.
        assert(
            identical(pathwayNames(x), pathwayNames(y)),
            identical(
                x = metadata(x)[["gmtFiles"]],
                y = metadata(y)[["gmtFiles"]]
            )
        )

        # Don't allow intersection of contrast names. Require the user to rename
        # these first before attempting to combine.
        assert(
            areDisjointSets(contrastNames(x), contrastNames(y)),
            areDisjointSets(
                x = names(metadata(x)[["rankedList"]]),
                y = names(metadata(y)[["rankedList"]]),
            )
        )

        # Update the `listData` slot. Note that using `c()` directly here as
        # `FUN` argument will prefix with `x.` and `y.`, which we don't want.
        listData <- mapply(
            x = slot(x, "listData"),
            y = slot(y, "listData"),
            FUN = function(x, y) {
                c(x, y)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )

        # Update the `rankedList` data stashed in `metadata`.
        rankedList <- c(
            metadata(x)[["rankedList"]],
            metadata(y)[["rankedList"]]
        )

        assert(identical(names(listData[[1L]]), names(rankedList)))

        # Internally, let's work with `x` as primary object, renamed to `out`.
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
        x = "FGSEAList",
        y = "FGSEAList"
    ),
    definition = combine.FGSEAList
)
