#' Enriched pathways
#'
#' Enriched pathways from gene set collections
#'
#' @name enrichedGeneSets
#' @inherit bioverbs::enrichedGeneSets
#' @note Updated 2019-08-28.
#'
#' @inheritParams params
#' @param flatten `logical(1)`.
#'   Flatten nested "up"/"down" directional enrichment vector subsets.
#'   Recomended by default for UpSet plots.
#' @param ... Additional arguments.
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



## @seealso `DESeqAnalysis::plotDEGUpset()`, for looping inspiration.
## Updated 2019-08-28.
`enrichedGeneSets,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        flatten = TRUE
    ) {
        validObject(object)
        alpha <- alphaThreshold(object)
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
            name = names(collection),
            data = collection,
            FUN = function(name, data) {
                assert(is(data, "data.table"))
                ## Subset significant enrichment.
                data <- data[data[["padj"]] < alpha, ]
                ## Upregulated.
                up <- data[data[["NES"]] > 0L, ]
                up <- up[order(up[["padj"]], -up[["NES"]]), ]
                up <- up[["pathway"]]
                ## Downregulated.
                down <- data[data[["NES"]] < 0L, ]
                down <- down[order(down[["padj"]], down[["NES"]]), ]
                down <- down[["pathway"]]
                list(up = up, down = down)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        if (isTRUE(flatten)) {
            out <- do.call(what = c, args = perContrast)
            ## Using "_" instead of "." for name concatenation.
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
    definition = `enrichedGeneSets,FGSEAList`
)
