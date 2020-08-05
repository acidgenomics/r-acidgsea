#' Enriched gene sets
#'
#' Enriched pathways from gene set collections.
#'
#' @name enrichedGeneSets
#' @inherit acidgenerics::enrichedGeneSets
#' @note Updated 2020-08-05.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return list.
#' Named list containing significant gene sets per contrast.
#'
#' @seealso
#' - `DESeqAnalysis::deg()`.
#'
#' @examples
#' data(fgsea)
#' enrichedGeneSets(
#'     object = fgsea,
#'     collection = "h",
#'     alpha = 0.7,
#'     direction = "up"
#' )
NULL



#' @rdname enrichedGeneSets
#' @name enrichedGeneSets
#' @importFrom acidgenerics enrichedGeneSets
#' @usage enrichedGeneSets(object, ...)
#' @export
NULL



## @seealso `DESeqAnalysis::plotDEGUpset()`, for looping inspiration.
## Updated 2020-08-05.
`enrichedGeneSets,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        direction = c("both", "up", "down")
    ) {
        validObject(object)
        assert(
            isScalar(collection),
            isAlpha(alpha),
            isNumber(nesThreshold)
        )
        direction <- match.arg(direction)
        alpha <- alphaThreshold(object)
        nesThreshold <- nesThreshold(object)
        collection <- object[[collection]]
        assert(
            is.list(collection),
            hasNames(collection)
        )
        suppressMessages({
            out <- mapply(
                object = collection,
                MoreArgs = list(
                    alphaThreshold = alphaThreshold,
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
        })
        out
    }



#' @rdname enrichedGeneSets
#' @export
setMethod(
    f = "enrichedGeneSets",
    signature = signature("FGSEAList"),
    definition = `enrichedGeneSets,FGSEAList`
)
