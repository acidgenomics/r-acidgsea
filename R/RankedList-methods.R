#' Prepare a ranked gene (stats) list for GSEA
#'
#' Return a parameterized ranked list for each differential expression contrast.
#'
#' @section Gene symbol multi-mapping:
#'
#' Multiple gene IDs can map to a gene symbol (e.g. *Homo sapiens* HGNC names).
#' In this event, we're averaging the stat values using `mean()` internally.
#'
#' @name RankedList
#' @inheritParams params
#'
#' @param value `character(1)`.
#'   Value type to use for GSEA. Currently supported:
#'
#'   1. `stat`: Wald test statistic. This column is returned by `results()`
#'      but is removed in [DESeq2::lfcShrink()] return, currently.
#'   2. `log2FoldChange`: Shrunken log2 fold change. Note that this option
#'      requires `[DESeq2::lfcShrink()] return to be slotted.
#'   3. `padj`: Adjusted *P* value. This don't provide directional ranks, but
#'      is offered as a legacy option. Not generally recommended.
#'
#' @return `RankedList`.
#'
#' @examples
#' ## x <- RankedList(object)
#' ## names(x)
NULL



RankedList.DESeqAnalysis <- function(
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
                summarise(!!quovalue := mean(!!quovalue)) %>%
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



#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature("DESeqAnalysis"),
    definition = RankedList.DESeqAnalysis
)



RankedList.FGSEAList <- function(object) {
    validObject(object)
    metadata(object)[["rankedList"]]
}



#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature("FGSEAList"),
    definition = RankedList.FGSEAList
)
