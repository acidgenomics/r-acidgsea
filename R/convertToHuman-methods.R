#' @name convertToHuman
#' @inherit AcidGenerics::convertToHuman
#' @note Updated 2023-04-28.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param map `DFrame`, or `NULL`.
#' Ortholog mappings data frame returned by `mapHumanOrthologs()`. Since
#' this function depends on the BioMart API and has a tendancy to time out,
#' we're allowing passthrough of a cached object here instead. If left `NULL`,
#' then `mapHumanOrthologs()` will be called internally.
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' convertToHuman(object)
NULL



## Updated 2022-05-25.
`convertToHuman,DESeqAnalysis` <- # nolint
    function(object, map = NULL) {
        assert(
            validObject(object),
            isAny(map, c("DFrame", "NULL"))
        )
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
            colnames(mcols(rr)) <- camelCase(colnames(mcols(rr)), strict = TRUE)
        }
        ## Get the organism and Ensembl release from DESeqDataSet.
        rrMeta <- metadata(rr)
        assert(
            rrMeta[["provider"]] == "Ensembl",
            isOrganism(rrMeta[["organism"]]),
            isInt(rrMeta[["release"]]),
            msg = sprintf(
                "Internal %s doesn't contain necessary metadata.\nCheck '%s'.",
                "DESeqDataSet", "metadata(rowRanges(data))"
            )
        )
        organism <- rrMeta[["organism"]]
        ensemblRelease <- rrMeta[["release"]]
        alertInfo(sprintf(
            "{.emph %s} (%s %d) genome detected.",
            organism, "Ensembl", ensemblRelease
        ))
        ## Early return on Homo sapiens.
        if (identical(organism, "Homo sapiens")) {
            alertWarning("Returning unmodified.")
            return(object)
        }
        ## Now we're ready to match the human orthologs.
        genes <- as.character(mcols(rr)[["geneId"]])
        assert(
            hasLength(genes),
            msg = sprintf(
                "'%s' column not defined in %s '%s'.",
                "geneId", "DESeqDataSet", "rowRanges"
            )
        )
        ## Note that this step can time out, so we're allowing map passthrough,
        ## which can help when working on multiple objects.
        if (is.null(map)) {
            map <- mapHumanOrthologs(
                genes = genes,
                organism = organism,
                ensemblRelease = ensemblRelease
            )
            assert(
                is(map, "DFrame"),
                identical(
                    x = sort(colnames(map)),
                    y = c("geneId", "geneName", "humanGeneId", "humanGeneName")
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
        keep <- !is.na(map[["humanGeneId"]])
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
        genes <- map[["humanGeneId"]]
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
        mcols(rowRanges(data))[["geneId"]] <-
            map[["humanGeneId"]]
        mcols(rowRanges(data))[["geneName"]] <-
            map[["humanGeneName"]]
        mcols(rowRanges(transform))[["geneId"]] <-
            mcols(rowRanges(data))[["geneName"]]
        mcols(rowRanges(transform))[["geneName"]] <-
            mcols(rowRanges(data))[["geneName"]]
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
