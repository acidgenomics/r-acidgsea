## NOTE Need to add support for RefSeq-aligned DESeq2 output.
## FIXME Add back support for "ensemblId" return...
## In this case, look for "ensemblId" column first, then "geneId", and in the
## case of geneId, perform identifier grep match...
## FIXME Also allow user to simply return "geneId" column?
##
## FIXME Need to add/improve support for these keyType arguments:
## - geneId
## - geneName
## - ensemblId
## - rowname



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



#' Detect default key type
#'
#' @note Updated 2022-05-25.
#' @noRd
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#'
#' ## DESeqAnalysis ====
#' .detectKeyType(mcols(rowRanges(deseq@data))[["geneId"]])
#' .detectKeyType(rownames(deseq))
.detectKeyType <- function(x) {
    if (any(grepl(pattern = "^ENS", x = x))) {
        return("ensembl")
    }
    if (any(grepl(pattern = "^[0-9]+$", x = x))) {
        return("entrez")
    }
    alertWarning("Unable to detect gene identifier key type.")
    NULL
}



#' Filter row ranges to only contain protein coding genes
#'
#' @note Updated 2022-05-26.
#' @noRd
#'
#' @return `GRanges`.
.filterProteinCoding <- function(rowRanges) {
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
        assert(isInRange(x = pctKeep, lower = 0.5, upper = 1L))
        rowRanges <- rowRanges[keep]
    }
    rowRanges
}



#' Filter row ranges to only keep primary seqnames
#'
#' @note Updated 2022-05-26.
#' @noRd
#'
#' @details
#' Currently only supporting Ensembl chromosomes naming conventions.
#' Planning on adding RefSeq support in a future update.
#'
#' @return `GRanges`.
.filterSeqnames <- function(rowRanges) {
    assert(is(rowRanges, "GenomicRanges"))
    organism <- organism(rowRanges)
    if (!identical(organism, "Homo sapiens")) {
        return(organism)
    }
    ## FIXME Work on adding support for RefSeq here too.
    validSeqnames <- c(
        seq(from = 1L, to = 21L, by = 1L),
        "X", "Y", "MT"
    )
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
            assert(isInRange(x = pctKeep, lower = 0.7, upper = 1L))
            rowRanges <- rowRanges[keep]
        }
    }
    rowRanges
}



#' Map gene identifiers from one type to another
#'
#' @note Updated 2022-05-26.
#' @noRd
#'
#' @details
#' Function ensures 1:1 mapping without allowing duplicates.
#'
#' @return `DataFrame`
#' Contains columns defined by `fromCol` and `toCol`.
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#'
#' ## DESeqAnalysis ====
#' object <- results(deseq, i = 1L, lfcShrink = FALSE)
#' rowRanges <- rowRanges(deseq@data)
#' x <- .mapGenes(
#'     object = object,
#'     rowRanges = rowRanges,
#'     fromCol = "geneId",
#'     toCol = "entrezId"
#' )
.mapGenes <-
    function(object,
             rowRanges,
             fromCol,
             toCol) {
        assert(
            is(object, "DataFrame"),
            is(rowRanges, "GenomicRanges"),
            identical(rownames(object), names(rowRanges)),
            isString(fromCol),
            isString(toCol)
        )
        rowData <- as.DataFrame(rowRanges)
        assert(
            isSubset(c(fromCol, toCol), colnames(rowData)),
            !is(rowData[[fromCol]], "List"),
            !anyNA(rowData[[fromCol]]),
            hasNoDuplicates(rowData[[fromCol]])
        )
        from <- decode(rowData[[fromCol]])
        to <- rowData[[toCol]]
        if (is(to, "List")) {
            ## For genes that don't map 1:1, use oldest identifier.
            to <- lapply(
                X = to,
                FUN = function(x) {
                    x <- na.omit(x)
                    if (identical(length(x), 0L)) {
                        return(NA)
                    }
                    sort(x)[[1L]]
                }
            )
            to <- unlist(x = to, recursive = FALSE, use.names = FALSE)
        } else {
            to <- decode(to)
        }
        assert(identical(length(from), length(to)))
        args <- list()
        args[[fromCol]] <- from
        args[[toCol]] <- to
        args[["row.names"]] <- rownames(object)
        map <- do.call(what = DataFrame, args = args)
        map2 <- map[complete.cases(map), ]
        if (nrow(map2) < nrow(object)) {
            pctKeep <- nrow(map2) / nrow(object)
            alertInfo(sprintf(
                "Mapping %s%% of genes from {.var %s} to {.var %s} (%d / %d).",
                prettyNum(
                    x = round(x = pctKeep * 100L, digits = 2L),
                    scientific = FALSE
                ),
                fromCol, toCol,
                nrow(map2), nrow(object)
            ))
            assert(
                isInRange(x = pctKeep, lower = 0.5, upper = 1L),
                msg = "Failed to map at least 50% of identifiers."
            )
        }
        map
    }



## FIXME Need to restrict keyType here, and provide handling options...
## FIXME Consider allowing NULL rowRanges input here.s
## FIXME In the event of proteinCodingOnly, need to require rowRanges...
## FIXME Need to figure out when to call .mapGenes internally here.

## Updated 2022-05-26.
`.RankedList,DataFrame` <- # nolint
    function(object,
             rowRanges,
             keyType,
             value,
             proteinCodingOnly) {
        assert(
            validObject(object),
            is(object, "DataFrame"),
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
                identical(rownames(object), names(rowRanges)),
                isSubset(keyType, colnames(mcols(rowRanges))),
                isSubset("geneId", colnames(mcols(rowRanges)))
            )
            rowRanges <- as(rowRanges, "GenomicRanges")
            rowRanges <- .filterSeqnames(rowRanges)
            if (isTRUE(proteinCodingOnly)) {
                rowRanges <- .filterProteinCoding(rowRanges)
            }
            assert(isSubset(names(rowRanges), rownames(object)))
            object <- object[names(rowRanges), , drop = FALSE]



            ## FIXME FIXME Need to apply mapping only if desired target
            ## is a nested List column.
            ## FIXME Need to handle edge case of "ensemblId" or "entrezId"
            ## desired, but only geneId or rowname is present...

            ## FIXME What to do for ensemblId handling here?
            ## AAAAARGH too confusing...

            switch(
                EXPR = keyType,
                "geneId" = {
                    ## FIXME Also consider allowing geneIdNoVersion?
                    map <- DataFrame(
                        "geneId" = mcols(rowRanges)[["geneId"]],
                        row.names = rownames(object)
                    )
                },
                {
                    map <- .mapGenes(
                        object = object,
                        rowRanges = rowRanges,
                        fromCol = "geneId",
                        toCol = keyType
                    )
                }
            )
            ## Now we need to apply mapping.
            ## FIXME Only do this when keyType is not "geneId".
            ## FIXME Otherwise just add the geneId...

        } else {
            assert(
                identical(keyType, "rowname"),
                isFALSE(proteinCodingOnly)
            )
            df <- DataFrame(
                "key" = rownames(object),
                "value" = object[[value]]
            )
        }




        ## FIXME Otherwise just create a dataframe with the desired values.
        ## FIXME Just set a DataFrame with rowname and value otherwise...
        ## then run complete cases....






        ## FIXME If user passes in `ensemblId` or `entrezId` and these
        ## columns aren't defined, see if we can get them from geneId.
        ## FIXME Don't need to check identifier type, just check seqnames.
        ## FIXME Rethink this approach, making more general...
        ## Only do this if the desired keyType is not the default keyType.
        ## FIXME Rethink this, using a more general `.mapGenes` approach.
        x <- switch(
            EXPR = keyType,
            "entrezId" = {
                .mapEnsemblToEntrez(
                    object = object,
                    rowRanges = rowRanges
                )
            },
            "geneName" = {
                ## FIXME Simplify this, to just use .mapGenes.
                .mapGeneNames(
                    object = object,
                    rowRanges = rowRanges
                )
            }
        )




        ## FIXME Rethink this when using "rownames" mapping approach.
        ## FIXME Consider reworking this approach, if we assign the keyType
        ## as the rownames in the mapping step...
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


## FIXME Allow NULL rowRanges input here.

## Updated 2022-05-25.
`RankedList,DESeqResults` <- # nolint
    function(object,
             rowRanges = NULL,
             keyType,
             value = c("stat", "log2FoldChange"),
             proteinCodingOnly = FALSE) {
        assert(validObject(object))
        df <- as(object, "DataFrame")
        out <- RankedList(
            object = df,
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



## Updated 2022-05-25.
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



## FIXME Don't export this method.

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
