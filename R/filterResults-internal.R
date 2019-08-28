## Updated 2019-08-28.
.filterResults <- function(data, alpha) {
    assert(isAlpha(alpha))
    data <- as(data, "DataFrame")
    data <- data[data[["padj"]] < alpha, , drop = FALSE]
    data <- data[order(data[["padj"]], -data[["NES"]]), , drop = FALSE]
    data
}
