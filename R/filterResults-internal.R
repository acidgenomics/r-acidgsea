## Updated 2019-08-28.
.filterResults <- function(data, alpha) {
    assert(
        is(data, "data.table"),
        isAlpha(alpha)
    )
    data <- data[data[["padj"]] < alpha, ]
    data <- data[order(data[["padj"]], -data[["NES"]]), ]
    data
}
