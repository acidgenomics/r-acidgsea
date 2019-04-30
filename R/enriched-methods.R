#' Enriched pathways from gene set collections
#'
#' @name enriched
#' @inheritParams params
#' @param flatten `logical(1)`.
#'   Flatten nested "up"/"down" directional enrichment vector subsets.
#'   Recomended by default for UpSet plots.
#'
#' @return list.
#' Named list formatted as:
#'
#' 1. Gene set collection (e.g. "h" from MSigDb).
#' 2. Contrast (e.g. "dmso_r1881_vs_etoh").
#' 3. Direction (e.g. "down" or "up").
#'
#' @examples
#' data(gsea)
#' enriched(gsea)
NULL



# @seealso `DESeqAnalysis::plotDEGUpset()`, for looping inspiration.
enriched.FGSEAList <-  # nolint
    function(
        object,
        collection,
        alpha = 0.05,
        flatten = TRUE
    ) {
        validObject(object)
        assert(
            isSubset(collection, collectionNames(object)),
            isAlpha(alpha),
            isFlag(flatten)
        )
        collection <- object[[collection]]
        perContrast <- mapply(
            contrastName = names(collection),
            contrast = collection,
            FUN = function(contrastName, contrast) {
                data <- as_tibble(contrast)
                sig <- filter(data, padj < !!alpha)
                down <- sig %>%
                    filter(NES < 0) %>%
                    arrange(padj, NES) %>%
                    pull("pathway")
                up <- sig %>%
                    filter(NES > 0) %>%
                    arrange(padj, desc(NES)) %>%
                    pull("pathway")
                list(down = down, up = up)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )

        if (isTRUE(flatten)) {
            out <- do.call(what = c, args = perContrast)
            # Using "_" instead of "." for name concatenation.
            names(out) <- makeNames(names(out), unique = TRUE)
        } else {
            out <- perContrast
        }

        out
    }



#' @rdname enriched
#' @export
setMethod(
    f = "enriched",
    signature = signature("FGSEAList"),
    definition = enriched.FGSEAList
)
