#' All generator functions
#' @include AllGenerics.R
#' @noRd
NULL



#' @name RankedList
#' @inherit RankedList-class title description return
#' @note Updated 2019-08-28.
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
#' data(matrix_lfc, package = "acidtest")
#' x <- RankedList(matrix_lfc)
#' print(x)
NULL



## This will work on any numeric matrix.
## Other options instead of df/list coercion (check benchmarks):
## https://stackoverflow.com/questions/6819804
## Updated 2019-07-17.
`RankedList,matrix` <-  # nolint
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
    definition = `RankedList,matrix`
)



## Updated 2019-08-28.
`RankedList,DESeqAnalysis` <-  # nolint
    function(
        object,
        value = c("stat", "log2FoldChange", "padj")
    ) {
        validObject(object)
        value <- match.arg(value)
        ## Extract the DESeqDataSet.
        dds <- as(object, "DESeqDataSet")
        ## Extract the DESeqResults list.
        if (identical(value, "log2FoldChange")) {
            ## Note that we're requiring shrunken LFCs if the user wants to
            ## return those values instead of using Wald test statistic.
            results <- slot(object, "lfcShrink")
        } else {
            results <- slot(object, "results")
        }
        assert(is(results, "list"))
        ## Get the gene-to-symbol mappings in long format.
        ## We're returning in long format so we can average the values for each
        ## gene symbol, since for some genomes gene IDs multi-map to symbols.
        suppressMessages(
            gene2symbol <- Gene2Symbol(dds, format = "unmodified")
        )
        gene2symbol <- as(gene2symbol, "DataFrame")
        gene2symbol[["rowname"]] <- rownames(gene2symbol)
        ## Get parameterized GSEA list values for each DESeqResults contrast.
        list <- lapply(
            X = results,
            FUN = function(data) {
                data <- as(data, "DataFrame")
                data[["rowname"]] <- rownames(data)
                data <- leftJoin(data, gene2symbol, by = "rowname")
                data <- data[, c("geneName", value), drop = FALSE]
                data <- data[complete.cases(data), , drop = FALSE]
                data <- unique(data)
                ## Average the value per gene symbol, if necessary.
                data[["geneName"]] <- as.factor(data[["geneName"]])
                if (any(duplicated(data[["geneName"]]))) {
                    rownames(data) <- NULL
                    dupes <- which(duplicated(data[["geneName"]]))
                    dupes <- as.character(data[["geneName"]][dupes])
                    dupes <- sort(unique(dupes))
                    message(sprintf(
                        fmt = "Averaging '%s' value for %d gene %s: %s.",
                        value,
                        length(dupes),
                        ngettext(
                            n = length(dupes),
                            msg1 = "symbol",
                            msg2 = "symbols"
                        ),
                        toString(dupes, width = 100L)
                    ))
                    split <- split(data, f = data[["geneName"]])
                    split <- SplitDataFrameList(lapply(
                        X = split,
                        FUN = function(x) {
                            if (nrow(x) == 1L) return(x)
                            args <- list()
                            args[["geneName"]] <- x[["geneName"]][[1L]]
                            args[[value]] <- mean(x[[value]])
                            do.call(what = DataFrame, args = args)
                        }
                    ))
                    data <- unsplit(split, f = unique(data[["geneName"]]))
                }
                assert(hasNoDuplicates(data[["geneName"]]))
                ## Arrange from positive to negative.
                data <- data[order(-data[[value]]), , drop = FALSE]
                out <- data[[value]]
                names(out) <- data[["geneName"]]
                out
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
    definition = `RankedList,DESeqAnalysis`
)



## Updated 2019-07-17.
`RankedList,FGSEAList` <-  # nolint
    function(object) {
        validObject(object)
        metadata(object)[["rankedList"]]
    }



#' @rdname RankedList-class
#' @export
setMethod(
    f = "RankedList",
    signature = signature("FGSEAList"),
    definition = `RankedList,FGSEAList`
)
