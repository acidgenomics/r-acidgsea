#' Enriched gene sets
#'
#' Assuming fgsea input by default currently.
#'
#' @note Updated 2020-03-18.
#' @noRd
#'
#' @seealso `DESeqAnalysis::deg`.
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



## FIXME This isn't capturing up and down correctly.
## FIXME Need to rework this.

#' Get the top up- and down-regulated pathways from FGSEA results
#' @note Updated 2020-03-18.
#' @noRd
.headtail <- function(
    object,
    alpha,
    nesThreshold,
    direction,
    n
) {
    ## This step is necessary to coerce `data.table`/`data.frame` from fgsea.
    object <- as(object, "DataFrame")
    assert(
        isSubset(c("pathway", "padj", "NES"), colnames(object)),
        isInt(n)
    )
    direction <- match.arg(direction, choices = c("both", "up", "down"))
    egs <- .enrichedGeneSets(
        object = object,
        alpha = alpha,
        nesThreshold = nesThreshold,
        direction = direction,
        idCol = idCol,
        alphaCol = alphaCol,
        nesCol = nesCol
    )
    if (!hasLength(egs)) {
        return(character())  # nocov
    }






    ## Need to ensure we're arranging by:
    ## 1. NES (descending: positive to negative).
    ## 2. Adjusted P value.
    object <- object[order(-object[["NES"]], object[["padj"]]), , drop = FALSE]
    ## Extract the pathway vector.
    x <- object[["pathway"]]



    ## FIXME This needs to check for positive, negative NES.

    ## Upregulated pathways.
    if (isSubset(direction, c("both", "up"))) {
        up <- head(x = x, n = n)
    } else {
        up <- character()
    }
    ## Downregulated pathways.
    if (isSubset(direction, c("both", "down"))) {
        down <- tail(x = x, n = n)
    } else {
        down <- character()
    }
    out <- unique(c(up, down))
    out
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
