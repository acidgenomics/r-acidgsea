## Get the top up- and down-regualted pathways from FGSEA results data frame.
## Updated 2019-08-28.
.headtail <- function(x, alpha, n) {
    assert(
        isSubset("pathway", colnames(x)),
        isAlpha(alpha),
        isInt(n)
    )
    x <- as(x, "DataFrame")
    ## Note that this sorts by adjusted P value, not desc NES.
    x <- .filterResults(x, alpha = alpha)
    if (!hasRows(x)) {
        return(character())  # nocov
    }
    ## Need to ensure we're arranging by:
    ## 1. NES (descending: positive to negative).
    ## 2. Adjusted P value.
    x <- x[order(-x[["NES"]], x[["padj"]]), , drop = FALSE]
    x <- x[["pathway"]]
    unique(c(head(x = x, n = n), tail(x = x, n = n)))
}
