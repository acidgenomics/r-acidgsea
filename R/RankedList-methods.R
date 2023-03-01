#' @name RankedList
#' @inherit RankedList-class title description return
#' @note Updated 2023-03-01.
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



#' Filter row ranges to only contain protein coding genes
#'
#' @note Updated 2022-08-31.
#' @noRd
#'
#' @return `GRanges`.
.filterProteinCoding <-
    function(rowRanges, threshold = 0.25) {
        assert(
            is(rowRanges, "GenomicRanges"),
            isSubset("geneBiotype", colnames(mcols(rowRanges)))
        )
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
            assert(
                isInRange(x = pctKeep, lower = threshold, upper = 1L),
                msg = "Failed to map sufficient number of protein coding genes."
            )
            rowRanges <- rowRanges[keep]
        }
        rowRanges
    }



#' Filter row ranges to only keep primary seqnames
#'
#' @note Updated 2022-08-31.
#' @noRd
#'
#' @details
#' Currently only supporting Ensembl chromosomes naming conventions.
#'
#' @return `GRanges`.
.filterSeqnames <-
    function(rowRanges, threshold = 0.25) {
        assert(is(rowRanges, "GenomicRanges"))
        organism <- organism(rowRanges)
        if (!identical(organism, "Homo sapiens")) {
            return(rowRanges)
        }
        ## These are the conventions used by Ensembl and GENCODE.
        validSeqnames <- c(seq(from = 1L, to = 21L, by = 1L), "X", "Y", "MT")
        if (isSubset(validSeqnames, levels(seqnames(rowRanges)))) {
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
                assert(
                    isInRange(x = pctKeep, lower = threshold, upper = 1L),
                    msg = paste(
                        "Failed to map sufficient number",
                        "of genes to primary seqnames."
                    )
                )
                rowRanges <- rowRanges[keep]
            }
        }
        rowRanges
    }



#' Unlist and map gene identifiers 1:1
#'
#' @note Updated 2022-08-31.
#' @noRd
#'
#' @details
#' Function ensures 1:1 mapping without allowing duplicates.
#'
#' @return Named `vector`.
#' Contains `NA` for invalid matches.
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#'
#' ## DESeqAnalysis ====
#' rowRanges <- rowRanges(deseq@data)
#' x <- .unlistGenes(rowRanges = rowRanges, keyType = "ncbiGeneId")
.unlistGenes <-
    function(rowRanges,
             keyType,
             threshold = 0.25) {
        assert(
            is(rowRanges, "GenomicRanges"),
            isString(keyType),
            isSubset(keyType, colnames(mcols(rowRanges))),
            isAny(mcols(rowRanges)[[keyType]], c("List", "list"))
        )
        x <- mcols(rowRanges)[[keyType]]
        ## For genes that don't map 1:1, use oldest identifier.
        x <- lapply(
            X = x,
            FUN = function(x) {
                sort(x = x, decreasing = FALSE, na.last = TRUE)[[1L]]
            }
        )
        x <- unlist(x = x, recursive = FALSE, use.names = FALSE)
        assert(identical(length(x), length(rowRanges)))
        names(x) <- names(rowRanges)
        if (anyNA(x)) {
            n1 <- sum(!is.na(x))
            n2 <- length(rowRanges)
            pctKeep <- n1 / n2
            alertInfo(sprintf(
                "Mapping %s%% of genes to {.var %s} (%d / %d).",
                prettyNum(
                    x = round(x = pctKeep * 100L, digits = 2L),
                    scientific = FALSE
                ),
                keyType, n1, n2
            ))
            assert(
                isInRange(x = pctKeep, lower = threshold, upper = 1L),
                msg = "Failed to map sufficient number of gene identifiers."
            )
        }
        x
    }



#' Prepare RankedList
#'
#' @note Updated 2023-03-01.
#' @noRd
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#' object <- results(deseq, i = 1L, lfcShrink = FALSE)
#' rowRanges <- rowRanges(deseq@data)
`.RankedList,DataFrame` <- # nolint
    function(object,
             rowRanges,
             keyType,
             value,
             proteinCodingOnly) {
        assert(
            validObject(object),
            is(object, "DataFrame"),
            hasRownames(object),
            isAny(rowRanges, c("GenomicRanges", "NULL")),
            isString(keyType),
            isString(value),
            isSubset(value, colnames(object)),
            hasRownames(object),
            isFlag(proteinCodingOnly)
        )
        object <- as(object, "DataFrame")
        if (identical(keyType, "rowname")) {
            rowRanges <- NULL
        }
        if (is(rowRanges, "GenomicRanges")) {
            assert(
                validObject(rowRanges),
                identical(rownames(object), names(rowRanges))
            )
            rowRanges <- as(rowRanges, "GenomicRanges")
            rowRanges <- .filterSeqnames(rowRanges)
            if (isTRUE(proteinCodingOnly)) {
                rowRanges <- .filterProteinCoding(rowRanges)
            }
            assert(isSubset(names(rowRanges), rownames(object)))
            object <- object[names(rowRanges), , drop = FALSE]
            if (isAny(mcols(rowRanges)[[keyType]], c("List", "list"))) {
                keys <- .unlistGenes(rowRanges = rowRanges, keyType = keyType)
            } else if (
                !isSubset(keyType, colnames(mcols(rowRanges))) &&
                    isSubset(keyType, c("ensemblGeneId", "ncbiGeneId")) &&
                    isSubset("geneId", colnames(mcols(rowRanges)))
            ) {
                assert(
                    identical(
                        x = metadata(rowRanges)[["provider"]],
                        y = switch(
                            EXPR = keyType,
                            "ensemblGeneId" = "Ensembl",
                            "ncbiGeneId" = "RefSeq"
                        )
                    ),
                    msg = sprintf(
                        "Failed to detect {.var %s} in object.",
                        keyType
                    )
                )
                keys <- mcols(rowRanges)[["geneId"]]
            } else {
                assert(
                    isSubset(keyType, colnames(mcols(rowRanges))),
                    msg = sprintf(
                        "Failed to detect {.var %s} in object.",
                        keyType
                    )
                )
                keys <- mcols(rowRanges)[[keyType]]
            }
        } else {
            assert(
                identical(keyType, "rowname"),
                msg = sprintf(
                    paste(
                        "Only {.var %s} {.val %s} is supported",
                        "when {.var %s} is {.val %s}."
                    ),
                    "keyType", "rowname",
                    "rowRanges", "NULL"
                )
            )
            assert(
                isFALSE(proteinCodingOnly),
                msg = sprintf(
                    "{.var %s} for {.var %s}.",
                    "rowRanges", "proteinCodingOnly"
                )
            )
            keys <- rownames(object)
        }
        df <- DataFrame(
            "key" = unname(keys),
            "value" = object[[value]],
            row.names = rownames(object)
        )
        df <- decode(df)
        df <- df[complete.cases(df), , drop = FALSE]
        df <- unique(df)
        if (
            isSubset(keyType, c("ensemblId", "geneId")) &&
                any(grepl(x = df[["key"]], pattern = ".", fixed = TRUE))
        ) {
            suppressMessages({
                df[["key"]] <- stripGeneVersions(df[["key"]])
            })
        }
        ## Average the value per key (e.g. gene symbol), if necessary.
        if (hasDuplicates(df[["key"]])) {
            df[["key"]] <- as.factor(df[["key"]])
            dupes <- df[["key"]][which(duplicated(df[["key"]]))]
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
            spl <- split(x = df, f = df[["key"]])
            ## Calculate mean expression per key.
            out <- vapply(
                X = spl[, "value"],
                FUN = mean,
                FUN.VALUE = numeric(1L),
                USE.NAMES = TRUE
            )
        } else {
            out <- df[["value"]]
            names(out) <- as.character(df[["key"]])
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



## Updated 2022-05-26.
`RankedList,DESeqAnalysis` <- # nolint
    function(object,
             keyType,
             value = c("stat", "log2FoldChange"),
             proteinCodingOnly = FALSE) {
        assert(validObject(object))
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

formals(`RankedList,DESeqAnalysis`)[["keyType"]] <- # nolint
    .keyType



## Updated 2022-05-25.
`RankedList,DESeqResults` <- # nolint
    function(object,
             rowRanges,
             keyType,
             value = c("stat", "log2FoldChange"),
             proteinCodingOnly = FALSE) {
        assert(validObject(object))
        out <- `.RankedList,DataFrame`(
            object = as(object, "DataFrame"),
            rowRanges = rowRanges,
            value = match.arg(value),
            keyType = match.arg(keyType),
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

formals(`RankedList,DESeqResults`)[["keyType"]] <- # nolint
    .keyType



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
