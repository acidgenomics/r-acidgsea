## NOTE Need to add support for RefSeq-aligned DESeq2 output.



#' @name RankedList
#' @inherit RankedList-class title description return
#' @note Updated 2022-05-25.
#'
#' @section Gene symbol multi-mapping:
#'
#' Multiple gene IDs can map to a gene symbol (e.g. *Homo sapiens* HGNC names).
#' In this event, we're averaging the values using `mean()` internally.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#'
#' @param proteinCodingOnly `logical(1)`.
#' Restrict to protein coding genes only.
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#' data(fgsea)
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' rl <- RankedList(object)
#' print(rl)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' rl <- RankedList(object)
#' print(rl)
NULL



#' Map DESeqResults to defined gene names (symbols)
#'
#' Join the gene-to-symbol mappings, so we can convert Ensembl gene identifiers
#' to gene symbols, for use with GSEA MSigDb GMT files.
#'
#' @note Updated 2022-04-27.
#' @noRd
.mapGeneNames <-
    function(object, rowRanges) {
        assert(
            is(object, "DataFrame"),
            is(rowRanges, "GenomicRanges"),
            identical(rownames(object), names(rowRanges))
        )
        suppressMessages({
            g2s <- Gene2Symbol(object = rowRanges, format = "unmodified")
        })
        x <- as(object, "DataFrame")
        y <- as(g2s, "DataFrame")
        cols <- setdiff(colnames(x), colnames(y))
        assert(hasLength(cols))
        x <- x[, cols]
        x[["rowname"]] <- rownames(x)
        y[["rowname"]] <- rownames(y)
        out <- leftJoin(x, y, by = "rowname")
        out[["rowname"]] <- NULL
        out
    }



## Updated 2022-04-27.
.mapEnsemblToEntrez <-
    function(object, rowRanges) {
        assert(
            is(object, "DataFrame"),
            is(rowRanges, "GenomicRanges"),
            identical(rownames(object), names(rowRanges))
        )
        rowData <- as.DataFrame(rowRanges)
        assert(
            isSubset("entrezId", colnames(rowData)),
            is(rowData[["entrezId"]], "List") ||
                is.list(rowData[["entrezId"]]),
            msg = sprintf(
                paste(
                    "{.cls %s} does not contain Entrez identifiers.",
                    "Re-run with {.arg %s} value other than {.val %s}."
                ),
                "rowRanges", "keyType", "entrezId"
            )
        )
        g2e <- IntegerList(rowData[["entrezId"]])
        names(g2e) <- rownames(rowData)
        keep <- !all(is.na(g2e))
        g2e <- g2e[keep, , drop = FALSE]
        ## For genes that don't map 1:1, use oldest Entrez identifier.
        g2e <- IntegerList(lapply(
            X = g2e,
            FUN = function(x) {
                head(sort(na.omit(x)), n = 1L)
            }
        ))
        g2e <- unlist(x = g2e, recursive = FALSE, use.names = TRUE)
        assert(
            is.integer(g2e),
            !anyNA(g2e)
        )
        idx <- match(x = names(g2e), table = rownames(object))
        assert(
            identical(length(idx), length(g2e)),
            !anyNA(idx)
        )
        if (length(idx) < nrow(object)) {
            pctKeep <- length(idx) / nrow(object)
            alertInfo(sprintf(
                "Mapping %s%% of genes from %s to %s (%d / %d).",
                prettyNum(
                    x = round(x = pctKeep * 100L, digits = 2L),
                    scientific = FALSE
                ),
                "Ensembl", "Entrez",
                length(idx), nrow(object)
            ))
            ## Fail if a certain number of genes don't pass threshold.
            assert(isInRange(x = pctKeep, lower = 0.5, upper = 1L))
        }
        out <- object[idx, , drop = FALSE]
        out[["ensemblId"]] <- names(g2e)
        out[["entrezId"]] <- unname(g2e)
        out
    }



## Updated 2022-05-25.
`.RankedList,DataFrame` <- # nolint
    function(object,
             rowRanges,
             keyType,
             value,
             proteinCodingOnly) {
        validObject(object)
        validObject(rowRanges)
        assert(
            is(object, "DataFrame"),
            is(rowRanges, "GenomicRanges"),
            isString(keyType),
            isSubset(keyType, colnames(mcols(rowRanges))),
            isSubset("geneId", colnames(mcols(rowRanges))),
            isString(value),
            isSubset(value, colnames(object)),
            hasRownames(object),
            identical(rownames(object), names(rowRanges)),
            isFlag(proteinCodingOnly)
        )
        object <- as(object, "DataFrame")
        rowRanges <- as(rowRanges, "GenomicRanges")
        organism <- organism(rowRanges)
        ensemblPattern <- "^ENSG[[:digit:]]{11}"
        if (any(isMatchingRegex(
            x = as.character(mcols(rowRanges)[["geneId"]]),
            pattern = ensemblPattern
        ))) {
            validSeqnames <- c(seq(from = 1L, to = 21L, by = 1L), "X", "Y", "MT")
            keep <- seqnames(rowRanges) %in% validSeqnames
            if (isTRUE(sum(keep) < length(keep))) {
                pctKeep <- sum(keep) / length(keep)
                alertInfo(sprintf(
                    "%s%% of genes mapped to primary seqnames (%d / %d).",
                    prettyNum(
                        x = round(x = pctKeep * 100L, digits = 2L),
                        scientific = FALSE
                    ),
                    sum(keep),
                    length(keep)
                ))
                assert(isInRange(x = pctKeep, lower = 0.7, upper = 1L))
                rowRanges <- rowRanges[keep]
            }
        }
        ## Restrict to protein coding genes only, if desired.
        if (isTRUE(proteinCodingOnly)) {
            assert(isSubset("geneBiotype", colnames(mcols(rowRanges))))
            keep <- mcols(rowRanges)[["geneBiotype"]] == "protein_coding"
            if (isTRUE(sum(keep) < length(keep))) {
                pctKeep <- sum(keep) / length(keep)
                alertInfo(sprintf(
                    "%s%% of genes are protein coding (%d / %d).",
                    prettyNum(
                        x = round(x = pctKeep * 100L, digits = 2L),
                        scientific = FALSE
                    ),
                    sum(keep),
                    length(keep)
                ))
                assert(isInRange(x = pctKeep, lower = 0.5, upper = 1L))
                rowRanges <- rowRanges[keep]
            }
        }
        assert(isSubset(names(rowRanges), rownames(object)))
        object <- object[names(rowRanges), , drop = FALSE]
        x <- switch(
            EXPR = keyType,
            "entrezId" = {
                .mapEnsemblToEntrez(
                    object = object,
                    rowRanges = rowRanges
                )
            },
            "geneName" = {
                .mapGeneNames(
                    object = object,
                    rowRanges = rowRanges
                )
            }
        )
        rownames(x) <- NULL
        x <- x[, c(keyType, value), drop = FALSE]
        x <- x[complete.cases(x), , drop = FALSE]
        x <- unique(x)
        ## Average the value per key (e.g. gene symbol), if necessary.
        if (anyDuplicated(x[[keyType]]) > 0L) {
            x[[keyType]] <- as.factor(x[[keyType]])
            dupes <- x[[keyType]][which(duplicated(x[[keyType]]))]
            dupes <- as.character(sort(unique(dupes)))
            alert(sprintf(
                fmt = "Averaging {.arg %s} for %d %s: %s.",
                value,
                length(dupes),
                ngettext(
                    n = length(dupes),
                    msg1 = "gene",
                    msg2 = "genes"
                ),
                toInlineString(dupes, n = 10L)
            ))
            x <- split(x = x, f = x[[keyType]])
            ## Calculate mean expression per key.
            out <- vapply(
                X = x[, value],
                FUN = mean,
                FUN.VALUE = numeric(1L),
                USE.NAMES = TRUE
            )
        } else {
            out <- x[[value]]
            names(out) <- x[[keyType]]
        }
        ## Arrange from positive to negative.
        out <- sort(out, decreasing = TRUE)
        ## Return ranked list.
        out <- SimpleList(out)
        metadata(out) <- list(
            "keyType" = keyType,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion,
            "value" = value
        )
        new(Class = "RankedList", out)
    }



## Updated 2022-04-27.
`RankedList,DESeqResults` <- # nolint
    function(object,
             rowRanges,
             keyType,
             value = c("stat", "log2FoldChange"),
             proteinCodingOnly = FALSE) {
        validObject(object)
        value <- match.arg(value)
        df <- as(object, "DataFrame")
        out <- RankedList(
            object = df,
            rowRanges = rowRanges,
            value = value,
            keyType = keyType,
            proteinCodingOnly = proteinCodingOnly
        )
        names(out) <- tryCatch(
            expr = {
                contrastName(object)
            },
            error = function(e) {
                NULL
            }
        )
        out
    }



## Updated 2022-04-27.
`RankedList,DESeqAnalysis` <- # nolint
    function(object,
             keyType = c("geneName", "entrezId"),
             value = c("stat", "log2FoldChange"),
             proteinCodingOnly = FALSE) {
        validObject(object)
        keyType <- match.arg(keyType)
        value <- match.arg(value)
        dds <- as(object, "DESeqDataSet")
        rowRanges <- rowRanges(dds)
        ## Extract the DESeqResults list. Note that we're requiring shrunken
        ## LFCs if the user wants to return those values instead of using the
        ## Wald test statistic ("stat").
        resultsList <- slot(
            object = object,
            name = ifelse(
                test = identical(value, "log2FoldChange"),
                yes = "lfcShrink",
                no = "results"
            )
        )
        assert(is(resultsList, "list"))
        ## Get parameterized GSEA list values for each DESeqResults contrast.
        bplapply <- eval(.bplapply)
        list <- bplapply(
            X = resultsList,
            FUN = RankedList,
            rowRanges = rowRanges,
            keyType = keyType,
            value = value,
            proteinCodingOnly = proteinCodingOnly
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



## Updated 2021-10-20.
`RankedList,FGSEAList` <- # nolint
    function(object) {
        rl <- metadata(object)[["rankedList"]]
        assert(
            is(rl, "RankedList"),
            msg = sprintf("{.cls %s} is not defined in object.", "RankedList")
        )
        rl
    }



#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature(object = "DataFrame"),
    definition = `.RankedList,DataFrame`
)

#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature(object = "DESeqAnalysis"),
    definition = `RankedList,DESeqAnalysis`
)

#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature(object = "DESeqResults"),
    definition = `RankedList,DESeqResults`
)

#' @rdname RankedList
#' @export
setMethod(
    f = "RankedList",
    signature = signature(object = "FGSEAList"),
    definition = `RankedList,FGSEAList`
)
