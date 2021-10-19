## FIXME Add an option here to plot all contrasts.
## FIXME Allow the user to input specific contrasts.
## FIXME Also allow the user to select which pathways to plot.
## This is super useful for non-hallmark gene sets...
## FIXME Allow user to pick specific gene sets from collection.



#' @name plotNES
#' @inherit AcidGenerics::plotNES
#' @note Updated 2021-10-19.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso Inspired by example in Stephen Turner's
#' [DESeq to fgsea guide](https://stephenturner.github.io/deseq-to-fgsea/).
#'
#' @examples
#' ## FGSEAList ====
#' data(fgsea)
#' plotNES(
#'     object = fgsea,
#'     contrast = "condition_B_vs_A",
#'     collection = "h"
#' )
NULL







## FIXME Allow the user to set title and subtitle here.
## FIXME Allow the user to select which contrasts to plot.

## Updated 2021-10-19.
`plotNES,FGSEAList` <-  # nolint
    function(
        object,
        contrast,
        collection,
        flip = getOption(
            x = "acid.flip",
            default = TRUE
        ),
        labels = list(
            title = TRUE,
            subtitle = NULL
        )
    ) {
        validObject(object)
        multiContrast <- FALSE
        ## FIXME Rework contrast input here, supporting multiple.
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







        ## FIXME Rework this as multi-contrast support.
        ## FIXME Rework this into a single call, and check for "contrast" column.
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
        data <- as_tibble(data)
        keep <- abs(data[["nes"]]) > 0L
        data <- data[keep, ]
        alpha <- alphaThreshold(object)
        data[["isSignificant"]] <- data[["padj"]] < alpha
        data[["opacity"]] <- ifelse(
            test = data[["isSignificant"]],
            yes = 1.0,
            no = 0.5
        )
        data[["direction"]] <- ifelse(
            test = {
                data[["nes"]] > 0L
            },
            yes = "up",
            no = "down"
        )
        p <- ggplot(
            data = data,
            mapping = aes(
                x = reorder(!!sym("pathway"), !!sym("nes")),
                y = !!sym("nes")
            )
        ) +
            ## See also:
            ## https://stackoverflow.com/questions/61200151/
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
                        "FALSE" = 1L,  # open circle
                        "TRUE" = 16L  # filled circle
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
                size = 0.5,
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
