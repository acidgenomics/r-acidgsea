#' @name Gene2Symbol
#' @inherit AcidGenomes::Gene2Symbol
#' @note Updated 2022-04-27.
#' @inheritParams params
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' g2s <- Gene2Symbol(object)
#' print(g2s)
NULL



## Updated 2022-04-27.
`Gene2Symbol,FGSEAList` <- # nolint
    function(object) {
        validObject(object)
        assert(
            is(metadata(object)[["deseq"]], "DESeqAnalysis"),
            msg = sprintf(
                "{.var %s} not defined in object.",
                "DESeqAnalysis"
            )
        )
        deseq <- metadata(object)[["deseq"]]
        suppressMessages({
            g2s <- Gene2Symbol(
                object = as.DESeqDataSet(deseq),
                format = "unmodified"
            )
        })
        rl <- RankedList(object)[[1L]]
        ## Subset first keeping duplicates, so we can inform the user.
        keep <- g2s[["geneName"]] %in% names(rl)
        g2s <- g2s[keep, , drop = FALSE]
        if (hasDuplicates(g2s[["geneName"]])) {
            n <- sum(duplicated(g2s[["geneName"]]))
            alertWarning(sprintf(
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
        idx <- match(x = names(rl), table = g2s[["geneName"]])
        assert(!anyNA(idx))
        g2s <- g2s[idx, , drop = FALSE]
        rownames(g2s) <- NULL
        assert(is(g2s, "Gene2Symbol"))
        validObject(g2s)
        g2s
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature(object = "FGSEAList"),
    definition = `Gene2Symbol,FGSEAList`
)
