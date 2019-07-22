#' @rdname RankedList-class
#' @name RankedList
#'
#' @title Prepare a ranked gene (stats) list for GSEA
#'
#' @description Return a parameterized ranked list for each differential
#'   expression contrast.
#'
#' @section Gene symbol multi-mapping:
#'
#' Multiple gene IDs can map to a gene symbol (e.g. *Homo sapiens* HGNC names).
#' In this event, we're averaging the stat values using `mean()` internally.
#'
#' @inheritParams params
#' @param value `character(1)`.
#'   Value type to use for GSEA. Currently supported:
#'
#'   1. `stat`: Wald test statistic. This column is returned by `results()`
#'      but is removed in [DESeq2::lfcShrink()] return, currently.
#'   2. `log2FoldChange`: Shrunken log2 fold change. Note that this option
#'      requires [DESeq2::lfcShrink()] return to be slotted.
#'   3. `padj`: Adjusted *P* value. This don't provide directional ranks, but
#'      is offered as a legacy option. Not generally recommended.
#'
#' @return `RankedList`.
#'
#' @examples
#' ## DESeqAnalysis ====
#' data(deseq, package = "DESeqAnalysis")
#' x <- RankedList(deseq)
#' print(x)
#'
#' ## FGSEAList ====
#' data(gsea, package = "pfgsea")
#' x <- RankedList(gsea)
#' print(x)
#'
#' ## matrix ====
#' data(mat, package = "acidtest")
#' x <- RankedList(mat)
#' print(x)
NULL


## This will work on any numeric matrix.
## Other options instead of df/list coercion (check benchmarks).
## https://stackoverflow.com/questions/6819804
## Updated 2019-07-17.
RankedList.matrix <-  # nolint
    function(object, value = "log2FoldChange") {
        assert(
            is.numeric(object),
            hasColnames(object),
            hasRownames(object)
        )
        list <- as.list(as.data.frame(object))
        list <- lapply(X = list, FUN = `names<-`, value = rownames(object))
        ## Sort the vectors from positive to negative.
        sorted <- lapply(X = list, FUN = sort, decreasing = TRUE)
        out <- SimpleList(sorted)
        metadata(out)[["version"]] <- .version
        metadata(out)[["value"]] <- value
        new(Class = "RankedList", out)
    }



#' @rdname RankedList-class
#' @export
setMethod(
    f = "RankedList",
    signature = signature("matrix"),
    definition = RankedList.matrix
)



## Updated 2019-07-17.
RankedList.DESeqAnalysis <-  # nolint
    function(
        object,
        value = c("stat", "log2FoldChange", "padj")
    ) {
        validObject(object)
        value <- match.arg(value)

        ## Extract the DESeqDataSet.
        dds <- as(object, "DESeqDataSet")

        ## Extract the DESeqResults list.
        if (value == "log2FoldChange") {
            ## Note that we're requiring shrunken LFCs if the user wants to
            ## return those values instead of using Wald test statistic.
            results <- object@lfcShrink
        } else {
            results <- object@results
        }
        assert(is(results, "list"))

        ## Get the gene-to-symbol mappings in long format.
        ## We're returning in long format so we can average the values for each
        ## gene symbol, since for some genomes gene IDs multi-map to symbols.
        suppressMessages(
            gene2symbol <- Gene2Symbol(dds, format = "unmodified")
        )

        ## Get parameterized GSEA list values for each DESeqResults contrast.
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



#' @rdname RankedList-class
#' @export
setMethod(
    f = "RankedList",
    signature = signature("DESeqAnalysis"),
    definition = RankedList.DESeqAnalysis
)



## Updated 2019-07-17.
RankedList.FGSEAList <-  # nolint
    function(object) {
        validObject(object)
        metadata(object)[["rankedList"]]
    }



#' @rdname RankedList-class
#' @export
setMethod(
    f = "RankedList",
    signature = signature("FGSEAList"),
    definition = RankedList.FGSEAList
)
