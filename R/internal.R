#' Get the top up- and down-regulated pathways from FGSEA results
#'
#' @note Updated 2020-03-18.
#' @noRd
#'
#' @param ... Passthrough arguments to `.enrichedGeneSets`.
#' @param n `integer(1)`.
#'   Number of upregulated and downregulated sets (each) to return.
#'
#' @examples
#' data(fgsea)
#' .headtail(
#'     object = fgsea[[1L]][[1L]],
#'     alphaThreshold = 0.9,
#'     nesThreshold = 1L,
#'     direction = "both",
#'     n = 2L,
#'     idCol = "pathway",
#'     alphaCol = "padj",
#'     nesCol = "NES"
#' )
.headtail <- function(..., direction, n) {
    assert(isInt(n))
    direction <- match.arg(direction, choices = c("both", "up", "down"))
    args <- list(...)
    ## Upregulated sets.
    if (isSubset(direction, c("both", "up"))) {
        up <- do.call(
            what = .enrichedGeneSets,
            args = c(args, direction = "up")
        )
        up <- head(x = up, n = n)
    } else {
        up <- character()
    }
    ## Downregulated sets.
    if (isSubset(direction, c("both", "down"))) {
        down <- do.call(
            what = .enrichedGeneSets,
            args = c(args, direction = "down")
        )
        down <- head(x = down, n = n)
    } else {
        down <- character()
    }
    ## Combine upregulated and downregulated sets.
    egs <- unique(c(up, rev(down)))
    egs
}



#' Match symbols in gene set to SummarizedExperiment (i.e. DESeqDataSet)
#'
#' Handle situation where DESeq object doesn't contain all symbols defined in
#' the gene set.
#'
#' @note Updated 2020-09-22.
#' @noRd
.matchGeneSet <- function(object, set, genes) {
    validObject(object)
    assert(
        is(object, "SummarizedExperiment"),
        isString(set),
        isCharacter(genes)
    )
    suppressMessages({
        g2s <- Gene2Symbol(object, format = "unmodified")
    })
    keep <- genes %in% g2s[["geneName"]]
    if (!all(keep)) {
        n <- sum(!keep)
        cli_alert_warning(sprintf(
            "%d %s in {.var %s} missing in {.var DESeqAnalysis}.",
            n,
            ngettext(
                n = n,
                msg1 = "gene",
                msg2 = "genes"
            ),
            set
        ))
        genes <- genes[keep]
    }
    rownames <- mapGenesToRownames(object = object, genes = genes)
    object <- object[rownames, , drop = FALSE]
    object
}
