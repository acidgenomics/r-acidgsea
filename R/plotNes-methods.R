#' @name plotNes
#' @inherit AcidGenerics::plotNes
#' @note Updated 2023-08-15.
#'
#' @details
#' Only plots gene sets that pass adjusted *P* value cutoff, defined by
#' `alphaThreshold`.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#'
#' @param n `integer(1)` or `Inf`.
#' Number of gene sets (regardless of direction) to include in plot.
#'
#' @param ... Additional arguments.
#'
#' @seealso Inspired by example in Stephen Turner's
#' [DESeq to fgsea guide](https://stephenturner.github.io/deseq-to-fgsea/).
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' alphaThreshold(object) <- 0.9
#' contrast <- contrastNames(object)[[1L]]
#' collection <- collectionNames(object)[[1L]]
#' plotNes(
#'     object = object,
#'     contrast = contrast,
#'     collection = collection,
#'     n = 10L
#' )
NULL



## Updated 2023-08-15.
`plotNes,FgseaList` <- # nolint
    function(object,
             contrast,
             collection,
             n = Inf,
             flip = getOption(
                 x = "acid.flip",
                 default = TRUE
             ),
             labels = list(
                 "title" = TRUE,
                 "subtitle" = TRUE
             )) {
        validObject(object)
        assert(
            isScalar(contrast),
            isScalar(collection),
            isInt(n),
            isFlag(flip)
        )
        labels <- matchLabels(labels)
        if (!isString(contrast)) {
            contrast <- contrastNames(object)[[contrast]]
        }
        if (!isString(collection)) {
            collection <- collectionNames(object)[[collection]]
        }
        if (identical(contrast, "all")) {
            alert("Plotting multiple contrasts.")
            data <- .resultsForAllContrasts(
                object = object,
                collection = collection
            )
        } else {
            data <- results(
                object = object,
                contrast = contrast,
                collection = collection
            )
        }
        keep <- abs(data[["nes"]]) > 0L
        data <- data[keep, , drop = FALSE]
        alpha <- alphaThreshold(object)
        keep <- data[["padj"]] < alpha
        if (!any(keep)) {
            alertWarning("No gene sets passed significance cutoff.")
            return(invisible(NULL))
        }
        data <- data[keep, , drop = FALSE]
        if (is.finite(n)) {
            data <- data[
                order(data[["padj"]], -abs(data[["nes"]])), ,
                drop = FALSE
            ]
            data <- head(data, n = n)
        }
        ## Don't plot very large collections (e.g. "msigdb_v7_5_1_symbols").
        if (nrow(data) > 100L) {
            alertWarning("Collection is too large to plot.")
            return(invisible(NULL))
        }
        data[["direction"]] <- ifelse(
            test = data[["nes"]] > 0L,
            yes = "up",
            no = "down"
        )
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = reorder(.data[["pathway"]], .data[["nes"]]),
                y = .data[["nes"]]
            )
        ) +
            scale_alpha_identity()
        if (identical(contrast, "all")) {
            p <- p +
                geom_boxplot(
                    color = "gray50",
                    fill = NA,
                    outlier.color = NA,
                    show.legend = FALSE,
                    size = 0.25
                ) +
                geom_point(
                    mapping = aes(
                        color = .data[["contrast"]],
                        shape = .data[["isSignificant"]]
                    ),
                    show.legend = TRUE,
                    size = 2L
                ) +
                acid_scale_color_discrete() +
                scale_shape_manual(
                    values = c(
                        "FALSE" = 1L, # open circle
                        "TRUE" = 16L # filled circle
                    )
                )
        } else {
            p <- p +
                geom_col(
                    mapping = aes(
                        fill = .data[["direction"]]
                    ),
                    show.legend = FALSE
                ) +
                acid_scale_fill_discrete()
        }
        p <- p +
            geom_hline(
                color = "black",
                linetype = "dashed",
                linewidth = 0.5,
                yintercept = 0L
            )
        labels[["x"]] <- "pathway"
        labels[["y"]] <- "normalized enrichment score"
        if (isTRUE(labels[["title"]])) {
            labels[["title"]] <- ifelse(
                test = identical(contrast, "all"),
                yes = "multiple contrasts",
                no = contrast
            )
        }
        if (isTRUE(labels[["subtitle"]])) {
            subtitle <- paste("padj", "<", alpha)
            if (is.finite(n)) {
                subtitle <- paste(
                    subtitle,
                    paste("n", "=", n),
                    sep = " : "
                )
            }
            labels[["subtitle"]] <- subtitle
        }
        p <- p + do.call(what = labs, args = labels)
        ## Flip.
        if (isTRUE(flip)) {
            p <- p + coord_flip()
        }
        p
    }



#' @rdname plotNes
#' @export
setMethod(
    f = "plotNes",
    signature = signature(object = "FgseaList"),
    definition = `plotNes,FgseaList`
)
