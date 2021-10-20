## FIXME Allow the user to set how to handle duplicate identifiers / symbols
## here. Default behavior is to average.
## FIXME Need to add code coverage for duplicate handling.
## FIXME Increase the verbosity about matching here.
## FIXME Consider reworking how we handle Entrez identifier matching from
## input data that contains Ensembl identifiers.
## FIXME Can we add support for edgeR analysis here?
## FIXME Consider adding support for limma as well.
## FIXME Exclude identifiers that are scaffolds, etc.
## FIXME Ensure we exclude scaffolds and stuff that shouldn't be averaged here
## by default....
## FIXME Consider adding method support for matrix here, which is useful
## for a table of values across multiple contrasts.
## FIXME Rename keyType to "entrez" or "symbols" here....simpler
## FIXME Assign names from gene set files automatically.
## FIXME Need to rethink our geneId, entrezId approach here...
## FIXME Don't set the keyType as "geneName" or "geneId" here?.
## FIXME Rework this, consider not reexporting...too generic...
## FIXME Change the gene2symbol to rowData here.
## FIXME rowData MUST contain geneName, geneId, entrezId.
## FIXME Drop elements that map to scaffolds here...
## FIXME Check Entrez identifier metadata here...
## FIXME Can we get information on Entrez version for the gene sets here?
## Maybe we can improve our identifier matching a bit...
## FIXME Switch to using `EntrezGeneInfo()` approach here to map input against
## MSigDB sets...
## FIXMECensor Entrez identifiers that are no longer active.
## FIXME Always require rowRanges.
## FIXME Inform the user about what type of keyType we're using for matching.
## FIXME What do we do with Ensembl-to-Entrez matches that aren't 1:1?
## FIXME Consider defaulting here to entrezId.
## FIXME Define the RankedList pattern here based on geneId.
## FIXME geneId is too vague here, is this ever used?
## FIXME rename keyType to "entrez" and "symbols"
## FIXME Add support and code coverage for direct NCBI Entrez / RefSeq input.
## FIXME Add support for edgeR and vroom here, based on DataFrame.



#' @name RankedList
#' @inherit RankedList-class title description return
#' @note Updated 2021-10-20.
#'
#' @section Gene symbol multi-mapping:
#'
#' Multiple gene IDs can map to a gene symbol (e.g. *Homo sapiens* HGNC names).
#' In this event, we're averaging the values using `mean()` internally.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
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



## Assuming Ensembl/GENCODE-annotated input here.
## Support for direct NCBI Entrez/RefSeq-annotated input requires a future
## package update, and is currently considered an edge case.
## Updated 2021-10-20.
`.RankedList,DataFrame` <-  # nolint
    function(
        object,
        rowRanges,
        keyType = c("geneName", "entrezId"),
        value
    ) {
        validObject(object)
        validObject(rowRanges)
        ## FIXME Currently expecting Ensembl-processed input here.
        ## Direct NCBI Entrez/RefSeq-processed input requires package update.
        ensemblPattern <- "^ENSG[[:digit:]]{11}"
        ## FIXME Note that this approach should only be used for Ensembl.
        validSeqnames <- c(seq(from = 1L, to = 21L, by = 1L), "X", "Y", "MT")
        assert(
            is(object, "DataFrame"),
            is(rowRanges, "GenomicRanges"),
            identical(organism(rowRanges), "Homo sapiens"),
            any(grepl(pattern = ensemblPattern, x = names(rowRanges))),
            isSubset(validSeqnames, levels(seqnames(rowRanges))),
            isSubset("geneName", colnames(mcols(rowRanges))),
            isString(value),
            isSubset(value, colnames(object)),
            hasRownames(object),
            identical(rownames(object), names(rowRanges))
        )
        keyType <- match.arg(keyType)
        object <- as(object, "DataFrame")
        rowRanges <- as(rowRanges, "GenomicRanges")
        ## Discard genes that don't map to primary seqnames. This helps us
        ## remove clutter of unwanted haplotype scaffolds, etc.
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
            ## Fail if a certain number of genes don't pass threshold.
            assert(isInRange(x = pctKeep, lower = 0.5, upper = 1L))
        }
        rowRanges <- rowRanges[keep]
        rowRanges <- droplevels(rowRanges)
        assert(
            allAreMatchingRegex(
                x = names(rowRanges),
                pattern = ensemblPattern
            ),
            ## Currently only supporting GRCh38.
            ## Move on from GRCh37 already.
            allAreMatchingRegex(
                ## FIXME Import this from basejump, following update.
                x = GenomeInfoDb::genome(rowRanges),
                pattern = "^GRCh38"
            )
        )
        rowData <- as.DataFrame(rowRanges)
        object <- object[names(rowRanges), , drop = FALSE]
        switch(
            EXPR = keyType,
            "entrezId" = {
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
                    !any(is.na(g2e))
                )
                idx <- match(x = names(g2e), table = rownames(object))
                assert(
                    identical(length(idx), length(g2e)),
                    !any(is.na(idx))
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
                object <- object[idx, , drop = FALSE]
                object[[keyType]] <- unname(g2e)
            },
            "geneName" = {
                ## FIXME Rework this, not requiring Gene2Symbol...
                ## FIXME Work on the object names here...
                assert(
                    is(gene2symbol, "Gene2Symbol"),  # FIXME
                    hasRownames(gene2symbol)  # FIXME
                )
                x <- as(object, "DataFrame")
                y <- as(gene2symbol, "DataFrame")
                ## Join the gene-to-symbol mappings, so we can convert Ensembl
                ## gene identifiers to gene symbols, for use with GSEA MSigDb
                ## GMT files.
                cols <- setdiff(colnames(x), colnames(y))
                assert(hasLength(cols))
                x <- x[, cols]
                x[["rowname"]] <- rownames(x)
                y[["rowname"]] <- rownames(y)
                x <- leftJoin(x, y, by = "rowname")
            }
        )
        x <- object
        rownames(x) <- NULL
        x <- x[, c(keyType, value), drop = FALSE]
        x <- x[complete.cases(x), , drop = FALSE]
        x <- unique(x)
        ## Average the value per key (e.g. gene symbol), if necessary.
        if (any(duplicated(x[[keyType]]))) {
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
        if (identical(keyType, "entrezId")) {
            metadata(out)[["ensembl2Entrez"]] <- g2e
        }
        new(Class = "RankedList", out)
    }



## Updated 2021-10-20.
`RankedList,DESeqResults` <-  # nolint
    function(
        object,
        rowRanges,
        keyType = c("geneName", "entrezId"),
        value = c("stat", "log2FoldChange", "padj")
    ) {
        validObject(object)
        out <- RankedList(
            object = as(object, "DataFrame"),
            rowRanges = rowRanges,
            value = match.arg(value),
            keyType = match.arg(keyType)
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



## Updated 2021-10-20.
`RankedList,DESeqAnalysis` <-  # nolint
    function(
        object,
        keyType = c("geneName", "entrezId"),
        value = c("stat", "log2FoldChange", "padj")
    ) {
        validObject(object)
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
            keyType = match.arg(keyType),
            value = match.arg(value)
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
`RankedList,FGSEAList` <-  # nolint
    function(object) {
        rl <- metadata(object)[["rankedList"]]
        assert(
            is(rl, "RankedList"),
            msg = sprintf("{.cls %s} is not defined in object.", "RankedList")
        )
        rl
    }



## FIXME Does this work OK if we don't export?
## > #' @rdname RankedList
## > #' @export

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
