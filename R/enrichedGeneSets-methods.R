#' Enriched gene sets
#'
#' Enriched pathways from gene set collections.
#'
#' @name enrichedGeneSets
#' @inherit acidgenerics::enrichedGeneSets
#' @note Updated 2020-03-18.
#'
#' @inheritParams acidroxygen::params
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
#' 2. Contrast (e.g. "condition_B_vs_A").
#' 3. Direction (e.g. "down" or "up").
#'
#' @seealso
#' - `DESeqAnalysis::deg`.
#'
#' @examples
#' data(gsea)
#' enrichedGeneSets(gsea, collection = "h", alpha = 0.6)
NULL



#' @rdname enrichedGeneSets
#' @name enrichedGeneSets
#' @importFrom acidgenerics enrichedGeneSets
#' @usage enrichedGeneSets(object, ...)
#' @export
NULL



## @seealso `DESeqAnalysis::plotDEGUpset()`, for looping inspiration.
## Updated 2020-03-18.
`enrichedGeneSets,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        alpha = NULL,
        nesThreshold = NULL,
        direction = c("both", "up", "down"),
        flatten = TRUE
    ) {
        validObject(object)
        if (is.null(alpha)) {
            alpha <- alphaThreshold(object)
        }
        if (is.null(nesThreshold)) {
            nesThreshold <- 0L
        }
        assert(
            isScalar(collection),
            isAlpha(alpha),
            isNumber(nesThreshold),
            isFlag(flatten)
        )
        direction <- match.arg(direction)
        nesThreshold <- abs(nesThreshold)
        collection <- object[[collection]]
        assert(
            is.list(collection),
            hasNames(collection)
        )
        perContrast <- mapply(
            object = collection,
            MoreArgs = list(
                alpha = alpha,
                nesThreshold = nesThreshold,
                direction = direction,
                idCol = "pathway",
                alphaCol = "padj",
                nesCol = "NES"
            ),
            FUN = .enrichedGeneSets,
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
