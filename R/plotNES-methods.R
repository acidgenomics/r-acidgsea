## FIXME Add an option here to plot all contrasts.



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



## Updated 2021-10-19.
`plotNES,FGSEAList` <-  # nolint
    function(
        object,
        contrast,
        collection,
        flip = getOption(
            x = "acid.flip",
            default = TRUE
        )
    ) {
        validObject(object)
        assert(
            isString(contrast),
            isString(collection),
            isFlag(flip)
        )
        if (identical(contrast, "all")) {
            data <- do.call(
                what = rbind,
                args = lapply(
                    X = contrastNames(object),
                    FUN = function(contrast) {
                        df <- results(
                            object = object,
                            contrast = contrast,
                            collection = collection
                        )
                        df[["contrast"]] <- contrast
                        df
                    }
                )
            )
            data <- as_tibble(data)
            assert(isSubset(
                x = c("nes", "padj", "pathway"),
                y = colnames(data)
            ))




        } else {
            data <- results(
                object = object,
                contrast = contrast,
                collection = collection
            )
            data <- as_tibble(data)
            assert(isSubset(
                x = c("nes", "padj", "pathway"),
                y = colnames(data)
            ))
            alpha <- alphaThreshold(object)
            data[["sig"]] <- data[["padj"]] < alpha
            ## FIXME Rather than colors, can we selectively decrease the
            ## opacity of non-significant elements here?
            p <- ggplot(
                data = data,
                mapping = aes(
                    x = reorder(!!sym("pathway"), !!sym("nes")),
                    y = !!sym("nes")
                )) +
                geom_col(
                    mapping = aes(fill = !!sym("sig"))
                ) +
                labs(
                    x = "pathway",
                    y = "normalized enrichment score",
                    fill = paste0("padj < ", alpha),
                    title = collection
                )
        }





        ## Fill.
        p <- p + autoDiscreteFillScale()






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
