#' @name RankedList
#' @inherit RankedList-class title description return
#' @note Updated 2021-03-04.
#'
#' @section Gene symbol multi-mapping:
#'
#' Multiple gene IDs can map to a gene symbol (e.g. *Homo sapiens* HGNC names).
#' In this event, we're averaging the stat values using `mean()` internally.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param value `character(1)`.
#'   Value type to use for GSEA ranked list.
#'   Currently supported:
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
#' data(fgsea, package = "AcidGSEA")
#' x <- RankedList(fgsea)
#' print(x)
NULL



#' Return `SimpleList` to generate `RankedList`.
#'
#' @note Updated 2021-03-04.
#' @noRd
`RankedList,DataFrame` <-  # nolint
    function(
        object,
        gene2symbol,
        value
    ) {
        validObject(object)
        validObject(gene2symbol)
        assert(
            is(object, "DataFrame"),
            is(gene2symbol, "Gene2Symbol"),
            isSubset(value, colnames(object))
        )
        x <- as(object, "DataFrame")
        y <- as(gene2symbol, "DataFrame")
        ## Join the gene-to-symbol mappings, so we can convert Ensembl gene IDs
        ## to gene symbols, for use with GSEA MSigDb GMT files.
        x[["rowname"]] <- rownames(x)
        y[["rowname"]] <- rownames(y)
        x <- leftJoin(x, y, by = "rowname")
        x <- x[, c("geneName", value), drop = FALSE]
        x <- x[complete.cases(x), , drop = FALSE]
        x <- unique(x)
        ## Average the value per gene symbol, if necessary.
        x[["geneName"]] <- as.factor(x[["geneName"]])
        if (any(duplicated(x[["geneName"]]))) {
            rownames(x) <- NULL
            dupes <- which(duplicated(x[["geneName"]]))
            dupes <- as.character(x[["geneName"]][dupes])
            dupes <- sort(unique(dupes))
            alert(sprintf(
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
        }
        ## Split by the gene symbol.
        x <- split(x = x, f = x[["geneName"]])
        ## Calculate mean expression per symbol.
        x <- vapply(
            X = x[, value],
            FUN = mean,
            FUN.VALUE = numeric(1L)
        )
        ## Arrange from positive to negative.
        x <- sort(x, decreasing = TRUE)
        ## Return ranked list.
        out <- SimpleList(x)
        metadata(out) <- append(
            x = metadata(out),
            values = list(
                "gene2symbol" = gene2symbol,
                "packageVersion" = .pkgVersion,
                "value" = value
            )
        )
        out
    }



#' Automatically handle `DESeqResults` columns, passing to `DataFrame` method.
#'
#' @note Updated 2021-03-03.
#' @noRd
`RankedList,DESeqResults` <-  # nolint
    function(
        object,
        gene2symbol,
        value
    ) {
        validObject(object)
        assert(is(object, "DESeqResults"))
        out <- `RankedList,DataFrame`(
            object = as(object, "DataFrame"),
            gene2symbol = gene2symbol,
            value = match.arg(value)
        )
        ## Ensure return contains contrast name.
        name <- tryCatch(
            expr = contrastName(object),
            error = function(e) "unknown"
        )
        names(out) <- name
        out
    }

formals(`RankedList,DESeqResults`)[["value"]] <- .rankedListValue



#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature("DESeqResults"),
    definition = `RankedList,DESeqResults`
)



#' Primary `RankedList` generator.
#'
#' @note Updated 2021-02-16.
#' @noRd
`RankedList,DESeqAnalysis` <-  # nolint
    function(object, value) {
        validObject(object)
        value <- match.arg(value)
        ## Extract the DESeqResults list.
        if (identical(value, "log2FoldChange")) {
            ## Note that we're requiring shrunken LFCs if the user wants to
            ## return those values instead of using Wald test statistic.
            resultsList <- slot(object, "lfcShrink")
        } else {
            resultsList <- slot(object, "results")
        }
        assert(is(resultsList, "list"))
        ## Get the gene-to-symbol mappings. We're returning in long format so we
        ## can average the values for each gene symbol, since for some genomes
        ## gene IDs multi-map to symbols.
        suppressMessages({
            gene2symbol <- Gene2Symbol(
                object = as(object, "DESeqDataSet"),
                format = "unmodified"
            )
        })
        ## Get parameterized GSEA list values for each DESeqResults contrast.
        bplapply <- eval(.bplapply)
        list <- bplapply(
            X = resultsList,
            FUN = `RankedList,DESeqResults`,
            gene2symbol = gene2symbol,
            value = value
        )
        ## Extract the required metadata from the first slotted return object
        ## defined from DESeqResults method.
        meta <- metadata(list[[1L]])
        ## Now loop across the DESeqResults method return and unlist, so we
        ## don't have an additional nested list level.
        list <- lapply(
            X = list,
            FUN = unlist,
            recursive = FALSE,
            use.names = FALSE
        )
        out <- SimpleList(list)
        metadata(out) <- meta
        new(Class = "RankedList", out)
    }

formals(`RankedList,DESeqAnalysis`)[["value"]] <-
    formals(`RankedList,DESeqResults`)[["value"]]



#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature("DESeqAnalysis"),
    definition = `RankedList,DESeqAnalysis`
)



## Updated 2020-09-23.
`RankedList,FGSEAList` <-  # nolint
    function(object) {
        rl <- metadata(object)[["rankedList"]]
        assert(is(rl, "RankedList"))
        rl
    }



#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature("FGSEAList"),
    definition = `RankedList,FGSEAList`
)
