#' @name Gene2Symbol
#' @inherit basejump::Gene2Symbol
#' @note Updated 2020-09-23.
#' @inheritParams params
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#' Gene2Symbol(fgsea)
NULL



#' @rdname Gene2Symbol
#' @name Gene2Symbol
#' @importFrom basejump Gene2Symbol
#' @usage Gene2Symbol(object, ...)
#' @export
NULL



## Updated 2020-09-23.
`Gene2Symbol,RankedList` <-  # nolint
    function(object) {
        validObject(object)
        g2s <- metadata(object)[["gene2symbol"]]
        assert(is(g2s, "Gene2Symbol"))
        validObject(g2s)
        ## Subset first keeping duplicates, so we can inform the user.
        keep <- g2s[["geneName"]] %in% names(object[[1L]])
        g2s <- g2s[keep, ]
        if (any(duplicated(g2s[["geneName"]]))) {
            n <- sum(duplicated(g2s[["geneName"]]))
            cli_alert_warning(sprintf(
                "%d duplicate gene %s detected. Returning first gene ID match.",
                n,
                ngettext(
                    n = n,
                    msg1 = "symbol",
                    msg2 = "symbols"
                )
            ))
        }
        ## Now keep only the first gene ID match.
        idx <- match(x = names(object[[1L]]), table = g2s[["geneName"]])
        assert(!any(is.na(idx)))
        g2s <- g2s[idx, ]
        rownames(g2s) <- NULL
        assert(is(g2s, "Gene2Symbol"))
        validObject(g2s)
        g2s
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("RankedList"),
    definition = `Gene2Symbol,RankedList`
)
