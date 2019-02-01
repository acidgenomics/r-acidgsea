#' Prepare a ranked gene (stats) list for GSEA
#'
#' Return a parameterized ranked list for each differential expression contrast.
#'
#' @section Gene symbol multi-mapping:
#'
#' Multiple gene IDs can map to a gene symbol (e.g. *Homo sapiens* HGNC names).
#' In this event, we're averaging the stat values using `mean()` internally.
#'
#' @name rankedList
#' @inheritParams params
#'
#' @param value `character(1)`.
#'   Value type to use for GSEA. Recommending `"stat"` by default, which
#'   corresponds to the Wald test statistic for DESeq2. Shrunken log2 fold
#'   change (`"log2FoldChange"`) or BH-adjusted *P* values ("padj") are also
#'   acceptible.
#'
#' @return `RankedList`.
#'
#' @examples
#' ## x <- rankedList(object)
#' ## names(x)
NULL



#' @importFrom bioverbs rankedList
#' @aliases NULL
#' @export
NULL

#' @rdname rankedList
#' @usage NULL
#' @export
RankedList <- rankedList



rankedList.DESeqAnalysis <- function(
    object,
    value = c("stat", "log2FoldChange", "padj")
) {
    validObject(object)
    value <- match.arg(value)

    # Extract the DESeqDataSet.
    dds <- as(object, "DESeqDataSet")

    # Extract the DESeqResults list.
    if (value == "log2FoldChange") {
        # Note that we're requiring shrunken LFCs if the user wants to return
        # those values instead of using Wald test statistic.
        results <- object@lfcShrink
    } else {
        results <- object@results
    }
    assert(is(results, "list"))

    # Get the gene-to-symbol mappings in long format.
    # We're returning in long format so we can average the values for each
    # gene symbol, since for some genomes gene IDs multi-map to symbols.
    suppressMessages(
        gene2symbol <- Gene2Symbol(dds, format = "long")
    )

    # Get parameterized GSEA list values for each DESeqResults contrast.
    quovalue <- sym(value)
    list <- lapply(
        X = results,
        FUN = function(data) {
            left_join(
                x = as_tibble(data, rownames = "rowname"),
                y = as_tibble(gene2symbol, rownames = "rowname"),
                by = "rowname"
            ) %>%
                select(!!!syms(c("geneName", quovalue))) %>%
                na.omit() %>%
                distinct() %>%
                group_by(!!sym("geneName")) %>%
                summarize(!!quovalue := mean(!!quovalue)) %>%
                arrange(desc(!!quovalue)) %>%
                deframe()
        }
    )
    names(list) <- names(results)

    out <- SimpleList(list)
    metadata(out)[["version"]] <- .version
    metadata(out)[["value"]] <- value
    metadata(out)[["gene2symbol"]] <- metadata(gene2symbol)
    new(Class = "RankedList", out)
}



#' @rdname rankedList
#' @export
setMethod(
    f = "rankedList",
    signature = signature("DESeqAnalysis"),
    definition = rankedList.DESeqAnalysis
)
