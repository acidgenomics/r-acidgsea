#' Enriched gene sets
#'
#' Enriched pathways from gene set collections.
#'
#' @name enrichedGeneSets
#' @inherit AcidGenerics::enrichedGeneSets
#' @note Updated 2020-09-18.
#'
#' @inheritParams AcidRoxygen::params
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
#'
#' ## FGSEAList ====
#' alphaThreshold(fgsea) <- 0.7
#' enrichedGeneSets(
#'     object = fgsea,
#'     collection = "h",
#'     direction = "up"
#' )
NULL



#' Enriched gene sets
#'
#' Assuming fgsea input by default currently.
#'
#' @note Updated 2020-03-18.
#' @noRd
#'
#' @seealso `DESeqAnalysis::deg`.
#'
#' @examples
#' data(fgsea)
#' .enrichedGeneSets(
#'     object = fgsea[[1L]][[1L]],
#'     alphaThreshold = 0.9,
#'     nesThreshold = 1,
#'     direction = "down",
#'     idCol = "pathway",
#'     alphaCol = "padj",
#'     nesCol = "NES"
#' )
.enrichedGeneSets <- function(
    object,
    alphaThreshold,
    nesThreshold,
    direction,
    idCol,          # pathway
    alphaCol,       # padj
    nesCol          # NES
) {
    data <- as(object, "DataFrame")
    assert(
        isString(idCol),
        isString(alphaCol),
        isString(nesCol),
        isSubset(c(idCol, alphaCol, nesCol), colnames(data))
    )
    direction <- match.arg(direction, choices = c("both", "up", "down"))
    data <- data[, c(idCol, nesCol, alphaCol)]
    ## Apply alpha cutoff.
    keep <- which(data[[alphaCol]] < alphaThreshold)
    data <- data[keep, , drop = FALSE]
    ## Apply NES threshold cutoff.
    if (nesThreshold > 0L) {
        keep <- which(abs(data[[nesCol]]) > nesThreshold)
        data <- data[keep, , drop = FALSE]
    }
    ## Apply directional filtering.
    if (identical(direction, "up")) {
        keep <- which(data[[nesCol]] > 0L)
        data <- data[keep, , drop = FALSE]
    } else if (identical(direction, "down")) {
        keep <- which(data[[nesCol]] < 0L)
        data <- data[keep, , drop = FALSE]
    }
    ## Arrange table by adjusted P value and NES score.
    if (identical(direction, "down")) {
        data <- data[order(data[["padj"]], data[["NES"]]), , drop = FALSE]
    } else {
        ## Note that we're arranging from high (positive) to low (negative)
        ## NES value when direction is up or both.
        data <- data[order(data[["padj"]], -data[["NES"]]), , drop = FALSE]
    }
    egs <- data[[idCol]]
    status <- sprintf(
        fmt = "%d %s %s detected (alpha < %g; nes > %g).",
        length(egs),
        switch(
            EXPR = direction,
            up = "upregulated",
            down = "downregulated",
            both = "enriched"
        ),
        ngettext(
            n = length(egs),
            msg1 = "gene set",
            msg2 = "gene sets"
        ),
        alphaThreshold,
        nesThreshold
    )
    alertInfo(status)
    egs
}



## @seealso `DESeqAnalysis::plotDEGUpset()`, for looping inspiration.
## Updated 2020-09-18.
`enrichedGeneSets,FGSEAList` <-  # nolint
    function(
        object,
        collection,
        direction = c("both", "up", "down")
    ) {
        validObject(object)
        assert(isString(collection))
        direction <- match.arg(direction)
        alphaThreshold <- alphaThreshold(object)
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
