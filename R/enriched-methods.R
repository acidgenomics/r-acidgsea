#' Enriched pathways
#'
#' @name enriched
#' @inheritParams params
#'
#' @return list.
#' Named list formatted as:
#'
#' 1. pathway (e.g. "h")
#' 2. contrast (e.g. "dmso_r1881_vs_etoh")
#' 3. direction (e.g. "down")
#'
#' @examples
#' data(gsea)
#' enriched(gsea)
NULL



# @seealso `DESeqAnalysis::plotDEGUpset()`, for looping inspiration.
enriched.FGSEAList <-  # nolint
    function(object, alpha = 0.05) {
        validObject(object)
        assert(isAlpha(alpha))
        mapply(
            pathwayName = names(object),
            pathway = object,
            FUN = function(pathwayName, pathway) {
                perContrast <- mapply(
                    contrastName = names(pathway),
                    contrast = pathway,
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
                out <- do.call(what = c, args = perContrast)
                # Using "_" instead of "." for name concatenation.
                names(out) <- makeNames(names(out), unique = TRUE)
                out
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
    }



#' @rdname enriched
#' @export
setMethod(
    f = "enriched",
    signature = signature("FGSEAList"),
    definition = enriched.FGSEAList
)
