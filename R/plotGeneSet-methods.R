#' @name plotGeneSet
#' @inherit AcidGenerics::plotGeneSet
#' @note Updated 2023-08-15.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param colors `character(5)`.
#' Named character color vector indicating:
#'
#' - Line color.
#' - Minimum (negative) threshold line color.
#' - Maximum (positive) threhsold color.
#' - Y intercept at origin color.
#' - Ticks color.
#' @param ... Additional arguments.
#'
#' @seealso Modified version of `fgsea::plotEnrichment()`.
#'
#' @return `ggplot`.
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' contrast <- contrastNames(object)[[1L]]
#' collection <- collectionNames(object)[[1L]]
#' set <- geneSetNames(object = object, collection = collection)[[1L]]
#' plotGeneSet(
#'     object = object,
#'     collection = collection,
#'     contrast = contrast,
#'     set = set
#' )
NULL



## Updated 2023-03-23.
`plotGeneSet,FgseaList` <- # nolint
    function(object,
             collection,
             contrast,
             set,
             colors = c(
                 "line" = "black",
                 "min" = AcidPlots::purpleOrange(n = 2L)[[1L]],
                 "max" = AcidPlots::purpleOrange(n = 2L)[[2L]],
                 "ticks" = "black",
                 "yintercept" = "black"
             )) {
        validObject(object)
        assert(
            isString(collection),
            isString(contrast),
            isString(set),
            isCharacter(colors),
            areSetEqual(names(colors), names(eval(formals()[["colors"]])))
        )
        ## GSEA weight parameter. Refer to `fgsea::calcGseaStat` for details.
        gseaParam <- 1L
        data <- object[[collection]]
        assert(is.list(data))
        rankedList <- RankedList(object)
        stats <- rankedList[[contrast]]
        pathway <- geneSet(
            object = object,
            collection = collection,
            set = set
        )
        if (!areIntersectingSets(names(stats), pathway)) {
            alertWarning("No intersection in ranked list and gene set.")
            return(invisible(NULL))
        }
        rnk <- rank(-stats)
        ord <- order(rnk)
        statsAdj <- stats[ord]
        statsAdj <- sign(statsAdj) * (abs(statsAdj)^gseaParam)
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
        toPlot <- data.frame(
            "x" = c(0L, xs, n + 1L),
            "y" = c(0L, ys, 0L)
        )
        diff <- (max(tops) - min(bottoms)) / 8L
        padj <- object[[collection]][[contrast]][["padj"]][
            match(
                x = set,
                table = object[[collection]][[contrast]][["pathway"]]
            )
        ]
        assert(isAlpha(padj))
        padj2 <- formatC(padj, format = "e", digits = 2L)
        p <- ggplot(
            data = toPlot,
            mapping = aes(x = .data[["x"]], y = .data[["y"]])
        ) +
            geom_hline(
                yintercept = 0L,
                color = colors[["yintercept"]],
                size = 0.5
            ) +
            geom_segment(
                data = data.frame(x = pathway),
                mapping = aes(
                    x = .data[["x"]],
                    y = -diff / 2L,
                    xend = .data[["x"]],
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
            labs(
                title = set,
                subtitle = paste(
                    collection,
                    contrast,
                    paste("padj", padj2),
                    sep = " : "
                ),
                x = "rank",
                y = "enrichment score"
            )
        p
    }



#' @rdname plotGeneSet
#' @export
setMethod(
    f = "plotGeneSet",
    signature = signature(object = "FgseaList"),
    definition = `plotGeneSet,FgseaList`
)
