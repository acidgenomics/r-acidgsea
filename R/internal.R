## Filter results.
## Updated 2019-08-28.
.filterResults <- function(data, alpha) {
    assert(isAlpha(alpha))
    data <- as(data, "DataFrame")
    data <- data[data[["padj"]] < alpha, , drop = FALSE]
    data <- data[order(data[["padj"]], -data[["NES"]]), , drop = FALSE]
    data
}



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



## Get the leading edge genes for a GSEA contrast.
## Updated 2019-11-18.
.leadingEdge <- function(
    object,
    contrast,
    collection,
    set
) {
    assert(
        is(object, "FGSEAList"),
        isScalar(contrast),
        isScalar(collection),
        isString(set)
    )
    data <- object[[collection]][[contrast]]
    assert(is(data, "data.table"))
    ## Coerce to DataFrame, to use standard subsetting syntax.
    data <- as(data, "DataFrame")
    keep <- match(set, table = data[["pathway"]])
    if (!isInt(keep)) {
        stop(sprintf("Failed to match '%s' set.", set))
    }
    genes <- unlist(unname(data[keep, "leadingEdge"]))
    assert(isCharacter(genes))
    genes
}
