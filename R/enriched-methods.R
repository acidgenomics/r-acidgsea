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
        perPathway <- mapply(
            pathwayName = names(object),
            pathway = object,
            FUN = function(pathwayName, pathway) {
                perContrast <- mapply(
                    contrastName = names(pathway),
                    contrast = pathway,
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
                do.call(what = c, args = perContrast)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        out <- do.call(what = c, args = perPathway)
        # Using "_" instead of "." for name concatenation.
        names(out) <- makeNames(names(out), unique = TRUE)
        out
    }



#' @rdname enriched
#' @export
setMethod(
    f = "enriched",
    signature = signature("FGSEAList"),
    definition = enriched.FGSEAList
)
