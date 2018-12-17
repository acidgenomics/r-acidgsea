# TODO Work toward returning as `List` instead, so we can stash metadata in the
# `metadata()` slot. Particularly useful is including the value type here.



#' Prepare stats list for GSEA
#'
#' @name statsList
#' @inheritParams params
#'
#' @param value `character(1)`.
#'   Value type to use for GSEA. Recommending `"stat"` by default, which
#'   corresponds to the Wald test statistic for DESeq2. Shrunken log2 fold
#'   change (`"log2FoldChange"`) or BH-adjusted *P* values ("padj") are also
#'   acceptible.
#'
#' @examples
#' ## x <- statsList(object)
#' ## names(x)
NULL



statsList.DESeqAnalysis <- function(
    object,
    value = c("stat", "log2FoldChange", "padj")
) {
    validObject(object)
    value <- match.arg(value)

    # Extract the DESeqDataSet.
    dds <- as(object, "DESeqDataSet")

    # Extract the DESeqResults list.
    # Using shrunken LFC with `lfcShrink()` applied.
    results <- object@lfcShrink
    stopifnot(is(results, "list"))
    invisible(lapply(
        X = results,
        FUN = function(x) {
            assert(
                is(x, "DESeqResults"),
                identical(rownames(dds), rownames(x))
            )
        }
    ))

    # Note that we're averaging the value per gene symbol here.
    gene2symbol <- Gene2Symbol(dds, format = "long")

    # Get parameterized GSEA list values for each DESeqResults contrast.
    value <- sym(value)
    out <- lapply(
        X = results,
        FUN = function(data) {
            left_join(
                x = as_tibble(data, rownames = "rowname"),
                y = as_tibble(gene2symbol, rownames = "rowname"),
                by = "rowname"
            ) %>%
                select(!!!syms(c("geneName", value))) %>%
                na.omit() %>%
                distinct() %>%
                group_by(!!sym("geneName")) %>%
                summarize(!!value := mean(!!value)) %>%
                arrange(desc(!!value)) %>%
                deframe()
        }
    )
    names(out) <- names(results)
    out
}



#' @rdname statsList
#' @export
setMethod(
    f = "statsList",
    signature = signature("DESeqAnalysis"),
    definition = statsList.DESeqAnalysis
)
