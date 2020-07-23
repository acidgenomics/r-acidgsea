#' Plot gene set enrichment
#'
#' @name plotGeneSet
#' @note Updated 2020-07-23.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param colors `character(5)`.
#'   Named character color vector indicating:
#'
#'   - Line color.
#'   - Minimum (negative) threshold line color.
#'   - Maximum (positive) threhsold color.
#'   - Y intercept at origin color.
#'   - Ticks color.
#'
#' @param ... Additional arguments.
#'
#' @seealso Modified version of [fgsea::plotEnrichment()].
#'
#' @examples
#' ## This requires MSigDB to be installed at `${HOME}`.
#' if (isTRUE(dir.exists(file.path("~", "msigdb")))) {
#'     data(fgsea)
#'     plotGeneSet(
#'         object = fgsea,
#'         collection = "h",
#'         contrast = "condition_B_vs_A",
#'         set = "HALLMARK_P53_PATHWAY"
#'     )
#' }
NULL



## Updated 2020-07-23.
`plotGeneSet,FGSEAList` <-
    function(
        object,
        collection,
        contrast,
        set,
        colors = c(
            line = "black",
            min = acidplots::purpleOrange(n = 2L)[[1L]],
            max = acidplots::purpleOrange(n = 2L)[[2L]],
            ticks = "black",
            yintercept = "black"
        )
    ) {
        validObject(object)
        assert(
            isCharacter(colors),
            areSetEqual(names(colors), names(eval(formals()[["colors"]])))
        )
        ## GSEA weight parameter. Refer to `fgsea::calcGseaStat` for details.
        gseaParam <- 1L
        assert(
            isString(collection),
            isSubset(collection, names(object))
        )
        data <- object[[collection]]
        rankedList <- RankedList(object)
        assert(
            isString(contrast),
            isSubset(contrast, names(rankedList))
        )
        gmtFile <- metadata(object)[["gmtFiles"]][[collection]]
        assert(
            identical(names(data), names(rankedList)),
            isAFile(gmtFile)
        )
        pathways <- gmtPathways(gmt.file = gmtFile)
        assert(
            isString(set),
            isSubset(set, names(pathways))
        )
        pathway <- pathways[[set]]
        stats <- rankedList[[contrast]]
        rnk <- rank(-stats)
        ord <- order(rnk)
        statsAdj <- stats[ord]
        statsAdj <- sign(statsAdj) * (abs(statsAdj) ^ gseaParam)
        statsAdj <- statsAdj / max(abs(statsAdj))
        pathway <- unname(as.vector(na.omit(match(pathway, names(statsAdj)))))
        pathway <- sort(pathway)
        gseaRes <- calcGseaStat(
            stats = statsAdj,
            selectedStats = pathway,
            gseaParam = 1L,
            returnAllExtremes = TRUE,
            returnLeadingEdge = FALSE,
            scoreType = "std"
        )
        bottoms <- gseaRes[["bottoms"]]
        tops <- gseaRes[["tops"]]
        n <- length(statsAdj)
        xs <- as.vector(rbind(pathway - 1L, pathway))
        ys <- as.vector(rbind(bottoms, tops))
        toPlot <- data.frame(x = c(0L, xs, n + 1L), y = c(0L, ys, 0L))
        diff <- (max(tops) - min(bottoms)) / 8L
        x <- NULL
        y <- NULL
        p <- ggplot(
            data = toPlot,
            mapping = aes(x = x, y = y)
        ) +
            geom_hline(
                yintercept = 0,
                color = colors[["yintercept"]],
                size = 0.5
            ) +
            geom_segment(
                data = data.frame(x = pathway),
                mapping = aes(
                    x = x,
                    y = -diff / 2L,
                    xend = x,
                    yend = diff / 2L
                ),
                color = colors[["ticks"]],
                size = 0.1
            ) +
            geom_line(
                color = colors[["line"]],
                size = 1L
            ) +
            geom_hline(
                yintercept = max(tops),
                color = colors[["max"]],
                linetype = "longdash",
                size = 0.5
            ) +
            geom_hline(
                yintercept = min(bottoms),
                color = colors[["min"]],
                linetype = "longdash",
                size = 0.5
            ) +
            theme(
                panel.border = element_blank(),
                panel.grid.minor = element_blank()
            ) +
            labs(
                title = set,
                subtitle = paste(collection, contrast, sep = " : "),
                x = "rank",
                y = "enrichment score"
            )
        p
    }



#' @rdname plotGeneSet
#' @export
setMethod(
    f = "plotGeneSet",
    signature = signature("FGSEAList"),
    definition = `plotGeneSet,FGSEAList`
)
