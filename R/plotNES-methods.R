#' @name plotNES
#' @inherit AcidGenerics::plotNES
#' @note Updated 2023-03-23.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso Inspired by example in Stephen Turner's
#' [DESeq to fgsea guide](https://stephenturner.github.io/deseq-to-fgsea/).
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' object <- fgsea
#' contrast <- contrastNames(object)[[1L]]
#' collection <- collectionNames(object)[[1L]]
#' plotNES(
#'     object = object,
#'     contrast = contrast,
#'     collection = collection
#' )
NULL



## Updated 2023-03-23.
`plotNES,FGSEAList` <- # nolint
    function(object,
             contrast,
             collection,
             flip = getOption(
                 x = "acid.flip",
                 default = TRUE
             ),
             labels = list(
                 title = TRUE,
                 subtitle = NULL
             )) {
        validObject(object)
        assert(
            isScalar(contrast),
            isScalar(collection),
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
            stop("No gene sets passed significance cutoff.")
        }
        data <- data[keep, , drop = FALSE]
        data[["direction"]] <- ifelse(
            test = data[["nes"]] > 0L,
            yes = "up",
            no = "down"
        )
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = reorder(!!sym("pathway"), !!sym("nes")),
                y = !!sym("nes")
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
                        color = !!sym("contrast"),
                        shape = !!sym("isSignificant")
                    ),
                    show.legend = TRUE,
                    size = 2L
                ) +
                autoDiscreteColorScale() +
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
                        fill = !!sym("direction")
                    ),
                    show.legend = FALSE
                ) +
                autoDiscreteFillScale()
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
        p <- p + do.call(what = labs, args = labels)
        ## Flip.
        if (isTRUE(flip)) {
            p <- p + coord_flip()
        }
        p
    }



#' @rdname plotNES
#' @export
setMethod(
    f = "plotNES",
    signature = signature(object = "FGSEAList"),
    definition = `plotNES,FGSEAList`
)
