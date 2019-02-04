# Get the top up- and down-regualted pathways from the FGSEA results data.table.
.headtail <- function(x, alpha, n) {
    assert(
        is(x, "data.table"),
        isSubset("pathway", colnames(x)),
        isAlpha(alpha),
        isInt(n)
    )
    # Note that this sorts by adjusted P value, not desc NES.
    x <- .filterResults(x, alpha = alpha)
    if (!hasRows(x)) {
        return(character())
    }
    # Need to ensure we're arranging by:
    # 1. NES (descending: positive to negative).
    # 2. Adjusted P value.
    x <- arrange(x, desc(!!sym("NES")), !!sym("padj"))
    x <- x[["pathway"]]
    unique(c(head(x = x, n = n), tail(x = x, n = n)))
}
