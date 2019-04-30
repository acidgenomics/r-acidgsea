#' Enriched pathways
#'
#' Enriched pathways from gene set collections
#'
#' @name enrichedGeneSets
#' @inherit bioverbs::enrichedGeneSets
#'
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
#' enrichedGeneSets(gsea, collection = "h")
NULL



#' @rdname enrichedGeneSets
#' @name enrichedGeneSets
#' @importFrom bioverbs enrichedGeneSets
#' @usage enrichedGeneSets(object, ...)
#' @export
NULL



# @seealso `DESeqAnalysis::plotDEGUpset()`, for looping inspiration.
enrichedGeneSets.FGSEAList <-  # nolint
    function(
        object,
        collection,
        flatten = TRUE
    ) {
        validObject(object)
        alpha <- alpha(object)
        assert(
            isScalar(collection),
            isFlag(flatten),
            isAlpha(alpha)
        )
        collection <- object[[collection]]
        assert(
            is.list(collection),
            hasNames(collection)
        )
        perContrast <- mapply(
            contrastName = names(collection),
            contrast = collection,
            FUN = function(contrastName, contrast) {
                sig <- contrast %>%
                    as_tibble() %>%
                    filter(!!sym("padj") < !!alpha) %>%
                    arrange(!!sym("padj"))
                down <- sig %>%
                    filter(!!sym("NES") < 0L) %>%
                    arrange(!!!syms(c("padj", "NES"))) %>%
                    pull("pathway")
                up <- sig %>%
                    filter(!!sym("NES") > 0L) %>%
                    arrange(!!sym("padj"), desc(!!sym("NES"))) %>%
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



#' @rdname enrichedGeneSets
#' @export
setMethod(
    f = "enrichedGeneSets",
    signature = signature("FGSEAList"),
    definition = enrichedGeneSets.FGSEAList
)
