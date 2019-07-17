#' Convert data set to use human orthologs
#'
#' @name convertToHuman
#'
#' @inheritParams params
#' @param map `DataFrame`, or `NULL`.
#'   Ortholog mappings data frame returned by [matchHumanOrthologs()]. Since
#'   this function depends on the BioMart API and has a tendancy to time out,
#'   we're allowing passthrough of a cached object here instead. If left `NULL`,
#'   then `[matchHumanOrthologs()] will be called internally.
#'
#' @return Modified object.
#'   Features (i.e. rownames) will be remapped to human genes.
#'
#' @examples
#' data(deseq, package = "DESeqAnalysis")
#' convertToHuman(deseq)
NULL



# Created 2019-06-12.
# Updated 2019-07-17.
convertToHuman.DESeqAnalysis <-  # nolint
    function(object, map = NULL) {
        validObject(object)
        assert(isAny(map, c("DataFrame", "NULL")))

        # Break out the slots of the object.
        data <- as(object, "DESeqDataSet")
        transform <- as(object, "DESeqTransform")
        results <- slot(object, "results")
        lfcShrink <- slot(object, "lfcShrink")
        genes <- rownames(data)

        # Get the organism and Ensembl release from DESeqDataSet.
        rrMeta <- metadata(rowRanges(data))
        # This step shouldn't get hit but it's useful to keep as a check.
        if (!isSubset(c("organism", "ensemblRelease"), names(rrMeta))) {
            # nocov start
            stop(paste(
                "Internal DESeqDataSet does not contain necessary metadata.",
                "Check `metadata(rowRanges(data))`."
            ))
            # nocov end
        }
        organism <- rrMeta[["organism"]]
        ensemblRelease <- rrMeta[["ensemblRelease"]]
        assert(
            isString(organism),
            isScalarInteger(ensemblRelease)
        )
        message(paste0(organism, " (Ensembl ", ensemblRelease, ") detected."))

        # Early return on Homo sapiens.
        if (organism == "Homo sapiens") {
            message("Returning unmodified.")
            return(object)
        }

        # Now we're ready to match the human orthologs.
        # Note that this step can time out, so we're allowing map passthrough.
        if (is.null(map)) {
            map <- matchHumanOrthologs(
                genes = genes,
                organism = organism,
                ensemblRelease = ensemblRelease
            )
            assert(
                is(map, "DataFrame"),
                identical(
                    x = sort(colnames(map)),
                    y = c("geneID", "geneName", "hgncID", "hgncName")
                )
            )

        }

        map <- as(map, "DataFrame")
        rownames(map) <- map[["geneID"]]
        map <- map[genes, , drop = FALSE]
        assert(identical(rownames(data), rownames(map)))
        keep <- !is.na(map[["hgncID"]])
        assert(any(keep))
        message(paste0(
            "Matched ",
            sum(keep, na.rm = TRUE), " / ", nrow(map),
            " genes to human orthologs."
        ))

        # Perform subset operations.
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

        # Remap the human ortholog gene identifiers onto the row names.
        genes <- map[["hgncID"]]
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

        # Update gene-to-symbol mappings defined in rowRanges.
        mcols(rowRanges(data))[["geneID"]] <- map[["hgncID"]]
        mcols(rowRanges(data))[["geneName"]] <- map[["hgncName"]]
        mcols(rowRanges(transform))[["geneID"]] <- map[["hgncID"]]
        mcols(rowRanges(transform))[["geneName"]] <- map[["hgncName"]]

        out <- DESeqAnalysis(
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink
        )

        # Ensure we're keeping any user-defined metadata.
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
    definition = convertToHuman.DESeqAnalysis
)
