#' @name convertToHuman
#' @inherit AcidGenerics::convertToHuman
#' @note Updated 2021-02-17.
#'
#' @inheritParams params
#' @param map `DataFrame`, or `NULL`.
#'   Ortholog mappings data frame returned by [matchHumanOrthologs()]. Since
#'   this function depends on the BioMart API and has a tendancy to time out,
#'   we're allowing passthrough of a cached object here instead. If left `NULL`,
#'   then `[matchHumanOrthologs()] will be called internally.
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#'
#' ## DESeqAnalysis ====
#' convertToHuman(deseq)
NULL



## Updated 2021-02-17.
`convertToHuman,DESeqAnalysis` <-  # nolint
    function(object, map = NULL) {
        validObject(object)
        assert(isAny(map, c("DataFrame", "NULL")))
        ## Break out the slots of the object.
        data <- as(object, "DESeqDataSet")
        transform <- as(object, "DESeqTransform")
        results <- slot(object, "results")
        lfcShrink <- slot(object, "lfcShrink")
        ## Attempt to use the row ranges to map gene identifiers.
        ## Don't assume gene identifiers are defined as row names.
        rr <- rowRanges(data)
        ## Enforcing strict camel case, as of 2021-02-16.
        if (!(identical(
            colnames(mcols(rr)),
            camelCase(colnames(mcols(rr)), strict = TRUE)
        ))) {
            alert("Reformatting mcols into strict lower camel case.")
            colnames(mcols(rr)) <- camelCase(colnames(mcols(rr)), strict = TRUE)
        }
        ## Get the organism and Ensembl release from DESeqDataSet.
        rrMeta <- metadata(rr)
        ## This step shouldn't get hit but it's useful to keep as a check.
        if (!isSubset(c("organism", "ensemblRelease"), names(rrMeta))) {
            ## nocov start
            stop(
                "Internal DESeqDataSet does not contain necessary metadata.\n",
                "Check 'metadata(rowRanges(data))'."
            )
            ## nocov end
        }
        organism <- rrMeta[["organism"]]
        ensemblRelease <- rrMeta[["ensemblRelease"]]
        assert(
            isString(organism),
            isScalarInteger(ensemblRelease)
        )
        alertInfo(sprintf(
            "%s (Ensembl %d) detected.",
            organism, ensemblRelease
        ))
        ## Early return on Homo sapiens.
        if (identical(organism, "Homo sapiens")) {
            alertWarning("Returning unmodified.")
            return(object)
        }
        ## Now we're ready to match the human orthologs.
        genes <- as.character(mcols(rr)[["geneId"]])
        if (!hasLength(genes)) {
            ## nocov start
            stop("'geneId' column not defined in DESeqDataSet 'rowRanges'.")
            ## nocov end
        }
        ## Note that this step can time out, so we're allowing map passthrough,
        ## which can help when working on multiple objects.
        if (is.null(map)) {
            ## FIXME This step is failing with mm_deseq.
            ## Need to update AcidGenomes and/or AcidPlyr to resolve this.
            map <- mapHumanOrthologs(
                genes = genes,
                organism = organism,
                ensemblRelease = ensemblRelease
            )
            assert(
                is(map, "DataFrame"),
                identical(
                    x = sort(colnames(map)),
                    y = c("geneId", "geneName", "hgncId", "hgncName")
                )
            )
        }
        ## Ensure that we rearrange the map return to match DESeqDataSet.
        map <- map[genes, , drop = FALSE]
        ## Reassign the row names to match the DESeqDataSet. This step is
        ## necessary in the event that Ensembl gene identifiers are not defined
        ## as the row names, but are defined in row ranges. This isn't very
        ## common but is accounted for in our unit testing.
        rownames(map) <- rownames(data)
        keep <- !is.na(map[["hgncId"]])
        assert(any(keep))
        alertInfo(sprintf(
            "Matched %d / %d genes to human orthologs.",
            sum(keep, na.rm = TRUE),
            nrow(map)
        ))
        ## Perform subset operations.
        filterList <- function(x) {
            lapply(
                X = x,
                FUN = function(x) {
                    x[keep, , drop = FALSE]
                }
            )
        }
        map <- map[keep, , drop = FALSE]
        data <- data[keep, , drop = FALSE]
        transform <- transform[keep, , drop = FALSE]
        results <- filterList(results)
        lfcShrink <- filterList(lfcShrink)
        ## Remap the human ortholog gene identifiers onto the row names.
        genes <- map[["hgncId"]]
        assignRownames <- function(x, value) {
            lapply(
                X = x,
                FUN = `rownames<-`,
                value = value
            )
        }
        rownames(data) <- genes
        rownames(transform) <- genes
        results <- assignRownames(results, genes)
        lfcShrink <- assignRownames(results, genes)
        ## Update gene-to-symbol mappings defined in rowRanges.
        mcols(rowRanges(data))[["geneId"]] <- map[["hgncId"]]
        mcols(rowRanges(data))[["geneName"]] <- map[["hgncName"]]
        mcols(rowRanges(transform))[["geneId"]] <- map[["hgncId"]]
        mcols(rowRanges(transform))[["geneName"]] <- map[["hgncName"]]
        out <- DESeqAnalysis(
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink
        )
        ## Ensure we're keeping any user-defined metadata.
        metadata <- metadata(object)
        metadata[["convertToHuman"]] <- TRUE
        metadata(out) <- metadata
        out
    }



#' @rdname convertToHuman
#' @export
setMethod(
    f = "convertToHuman",
    signature = signature(object = "DESeqAnalysis"),
    definition = `convertToHuman,DESeqAnalysis`
)
