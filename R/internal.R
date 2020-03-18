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
#' .enrichedGeneSets(
#'     object = gsea[[1L]][[1L]],
#'     alpha = 0.9,
#'     nesThreshold = 1,
#'     direction = "down",
#'     idCol = "pathway",
#'     alphaCol = "padj",
#'     nesCol = "NES"
#' )
.enrichedGeneSets <- function(
    object,
    alpha,
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
    keep <- which(data[[alphaCol]] < alpha)
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
        fmt = "%d %s %s detected (alpha: %g; nes: %g).",
        length(egs),
        switch(
            EXPR = direction,
            up = "upregulated",
            down = "downregulated",
            both = "differentially expressed"
        ),
        ngettext(
            n = length(egs),
            msg1 = "gene set",
            msg2 = "gene sets"
        ),
        alpha,
        nesThreshold
    )
    cli_alert_info(status)
    egs
}



#' Get the top up- and down-regulated pathways from FGSEA results
#'
#' @note Updated 2020-03-18.
#' @noRd
#'
#' @param ... Passthrough arguments to `.enrichedGeneSets`.
#' @param n `integer(1)`.
#'   Number of upregulated and downregulated sets (each) to return.
#'
#' @examples
#' .headtail(
#'     object = gsea[[1L]][[1L]],
#'     alpha = 0.9,
#'     nesThreshold = 1L,
#'     direction = "both",
#'     n = 2L,
#'     idCol = "pathway",
#'     alphaCol = "padj",
#'     nesCol = "NES"
#' )
.headtail <- function(..., direction, n) {
    assert(isInt(n))
    direction <- match.arg(direction, choices = c("both", "up", "down"))
    args <- list(...)
    ## Upregulated sets.
    if (isSubset(direction, c("both", "up"))) {
        up <- do.call(
            what = .enrichedGeneSets,
            args = c(args, direction = "up")
        )
        up <- head(x = up, n = n)
    } else {
        up <- character()
    }
    ## Downregulated sets.
    if (isSubset(direction, c("both", "down"))) {
        down <- do.call(
            what = .enrichedGeneSets,
            args = c(args, direction = "down")
        )
        down <- head(x = down, n = n)
    } else {
        down <- character()
    }
    ## Combine upregulated and downregulated sets.
    egs <- unique(c(up, rev(down)))
    egs
}



#' Get the leading edge genes for a GSEA contrast
#' @note Updated 2019-11-18.
#' @noRd
.leadingEdge <- function(
    object,
    contrast,
    collection,
    set
) {
    assert(
        is(object, "FGSEAList"),
        isScalar(contrast),
        isScalar(collection),
        isString(set)
    )
    data <- object[[collection]][[contrast]]
    assert(is(data, "data.table"))
    ## Coerce to DataFrame, to use standard subsetting syntax.
    data <- as(data, "DataFrame")
    keep <- match(set, table = data[["pathway"]])
    if (!isInt(keep)) {
        stop(sprintf("Failed to match '%s' set.", set))
    }
    genes <- unlist(unname(data[keep, "leadingEdge"]))
    assert(isCharacter(genes))
    genes
}
